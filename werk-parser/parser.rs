use std::marker::PhantomData;

use werk_util::{AsDiagnostic as _, DiagnosticFileId, DiagnosticSourceMap, Offset, Span, Spanned};
use winnow::{
    Parser as _,
    ascii::{line_ending, till_line_ending},
    combinator::{alt, cut_err, delimited, empty, eof, opt, peek, preceded, repeat, seq},
    error::AddContext as _,
    stream::{Location, Stream as _},
    token::{any, none_of, one_of, take_while},
};

use crate::{
    ErrContext, Error, Failure, ModalErr,
    ast::{self, keyword, token, ws_ignore},
    fatal,
};

mod string;

pub use string::*;

pub type Input<'a> = winnow::stream::LocatingSlice<&'a str>;
pub type PResult<T> = Result<T, ModalErr>;

/// Shorthand trait.
pub(crate) trait Parser<'a, O>: winnow::Parser<Input<'a>, O, ModalErr> {
    fn with_token_span(self) -> SpannedTokenParser<Self, O>
    where
        Self: Sized,
    {
        SpannedTokenParser(self, std::marker::PhantomData)
    }

    fn token_span(self) -> TokenSpanParser<Self, O>
    where
        Self: Sized,
    {
        TokenSpanParser(self, std::marker::PhantomData)
    }

    fn while_parsing(self, thing: &'static str) -> WhileParsing<Self>
    where
        Self: Sized,
    {
        WhileParsing(self, thing)
    }

    fn expect(self, thing: &'static &'static str) -> OrFail<Self, O>
    where
        Self: Sized,
    {
        OrFail(self, Some(Failure::Expected(thing)), PhantomData)
    }

    fn or_fail(self, failure: Failure) -> OrFail<Self, O>
    where
        Self: Sized,
    {
        OrFail(self, Some(failure), PhantomData)
    }

    fn help(self, text: &'static str) -> Help<Self, O>
    where
        Self: Sized,
    {
        Help(self, text, PhantomData)
    }
}
impl<'a, P, O> Parser<'a, O> for P where P: winnow::Parser<Input<'a>, O, ModalErr> {}

/// Helper trait to parse the AST via type inference.
///
/// Philosophically, the reason `winnow` isn't designed like this might be
/// because it is not always the case that there is just a single rule
/// associated with a given AST node, but multiple rules can generate the same
/// AST node depending on context.
///
/// However, this parser aims to preserve full fidelity with the input,
/// including comments and whitespace, which means that any syntactical
/// difference must also be captured in the AST, which implies that there is, in
/// fact, a single rule per AST node.
pub trait Parse: Sized {
    fn parse(input: &mut Input) -> PResult<Self>;
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: &mut Input) -> PResult<Self> {
        T::parse(input).map(Box::new)
    }
}

pub fn parse<T: Parse>(input: &mut Input) -> Result<T, ModalErr> {
    T::parse(input)
}

pub fn parse_werk(source_code: &str) -> Result<ast::Root, Error> {
    root.parse(Input::new(source_code))
        .map_err(winnow::error::ParseError::into_inner)
}

pub fn parse_werk_with_diagnostics<S: DiagnosticSourceMap + ?Sized>(
    file_id: DiagnosticFileId,
    source_map: &S,
) -> Result<ast::Root, werk_util::Annotated<crate::ErrorInFile<Error>, &S>> {
    let source = source_map
        .get_source(file_id)
        .expect("file ID not found in source map");
    parse_werk(source.source)
        .map_err(|err| err.with_file(file_id).into_diagnostic_error(source_map))
}

fn root(input: &mut Input) -> PResult<ast::Root> {
    let ((), statements, decor_trailing, _) =
        statements_delimited(empty, parse, peek(eof)).parse_next(input)?;
    Ok(ast::Root {
        statements,
        ws_trailing: decor_trailing,
    })
}

#[allow(clippy::unnecessary_wraps)]
fn default<T: Default>(_input: &mut Input) -> PResult<T> {
    Ok(Default::default())
}

/// List of statements delimited by a statement separator, i.e.
/// newlines/comments or semicolon.
///
/// This parser also sets the "decoration" (leading and trailing
/// whitespace/comments) for each item, capturing doc comments.
///
/// This parser works differently from normal `delimited(open, items, close)`
/// because it eagerly checks the terminator after each item, rather than trying
/// to parse the item first and falling back to the terminator. This gives
/// slightly better error messages, but means that the `parse_next` must not
/// accept strings that start with `terminal`.
///
/// Each statement is separated by a statement separator: one or more newlines
/// (including comments), or semicolons.
fn statements_delimited<'a, Open, Item, Close, OpenParser, ParseNextItem, CloseParser>(
    mut initial: OpenParser,
    parse_next: ParseNextItem,
    mut terminal: CloseParser,
) -> impl Parser<'a, (Open, Vec<ast::BodyStmt<Item>>, ast::Whitespace, Close)>
where
    OpenParser: Parser<'a, Open>,
    CloseParser: Parser<'a, Close>,
    ParseNextItem: Parser<'a, Item>,
{
    let mut parse_next = parse_next;

    move |input: &mut Input<'a>| -> PResult<(Open, Vec<ast::BodyStmt<Item>>, ast::Whitespace, Close)> {
        let mut accum = Vec::new();

        let mut has_separator = true;

        let open = initial.parse_next(input)?;
        // Ignore all whitespace (including comments) until the first statement
        // or the terminator.
        let mut last_decor = whitespace_parsed.parse_next(input)?;

        loop {
            if let Ok(close) = terminal.parse_next(input) {
                return Ok((open, accum, last_decor.into_whitespace(), close));
            }

            if !has_separator {
                return Err(ModalErr::Error(Error::new(
                    Offset(input.previous_token_end() as u32),
                    Failure::Expected(&"semicolon or newline before next statement"),
                )));
            }

            let item = parse_next.parse_next(input)?;
            let preceding_whitespace = last_decor;
            let trailing;

            let whitespace_before_semicolon = whitespace_parsed.parse_next(input)?;
            let semicolon_and_whitespace = opt((parse, whitespace_parsed)).parse_next(input)?;

            if let Some((semicolon, whitespace_after_semicolon)) = semicolon_and_whitespace {
                // All whitespace before the semicolon is trailing for the item we just found.
                trailing = ast::Trailing {
                    ws: whitespace_before_semicolon.into_whitespace(),
                    token: Some(semicolon),
                };
                // Whitespace after the semicolon is the comment for the next item.
                last_decor = whitespace_after_semicolon;
                // Semicolon is a separator.
                has_separator = true;
            } else {
                trailing = ast::Trailing {
                    // Attribute the whitespace to the next item.
                    ws: ast::Whitespace(Span::from_offset_and_len(
                        whitespace_before_semicolon.span.start,
                        0,
                    )),
                    token: None,
                };
                has_separator = whitespace_before_semicolon.is_statement_separator();
                // No semicolon, so the whitespace between the statements is the
                // comment for the next item.
                last_decor = whitespace_before_semicolon;
            }

            accum.push(ast::BodyStmt {
                ws_pre: preceding_whitespace.into_whitespace(),
                statement: item,
                trailing,
            });
        }
    }
}

impl<T: Parse> Parse for ast::Body<T> {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (token_open, statements, decor_trailing, token_close) =
            statements_delimited(parse, parse, parse).parse_next(input)?;

        Ok(ast::Body {
            token_open,
            statements,
            ws_trailing: decor_trailing,
            token_close,
        })
    }
}

impl Parse for ast::RootStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        alt((
            parse.map(ast::RootStmt::Default),
            parse.map(ast::RootStmt::Config),
            parse.map(ast::RootStmt::Let),
            parse.map(ast::RootStmt::Task),
            parse.map(ast::RootStmt::Build),
            parse.map(ast::RootStmt::Include),
            fatal(Failure::Expected(&"statement"))
                .help("one of `default`, `let`, `task`, or `build`"),
        ))
        .parse_next(input)
    }
}

impl Parse for ast::DefaultStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        fn entry<'a, K: Parse, V: Parse>(
            token: keyword::Default,
            ws_1: ast::Whitespace,
        ) -> impl Parser<'a, ast::DefaultStmtEntry<K, V>> {
            move |input: &mut Input| {
                let (mut stmt, span) = seq! { ast::DefaultStmtEntry {
                    span: default,
                    token: default,
                    ws_1: default,
                    key: parse,
                    ws_2: whitespace,
                    token_eq: cut_err(parse).help("`default` statements look like this: default key = ..."),
                    ws_3: whitespace,
                    value: cut_err(parse),
                }}.with_token_span().parse_next(input)?;
                stmt.span = token.span().merge(span);
                stmt.token = token;
                stmt.ws_1 = ws_1;
                Ok(stmt)
            }
        }

        let token: keyword::Default = parse.parse_next(input)?;
        let ws_1 = whitespace_nonempty.parse_next(input)?;

        alt((
            entry(token, ws_1).map(ast::DefaultStmt::Target),
            entry(token, ws_1).map(ast::DefaultStmt::OutDir),
            entry(token, ws_1).map(ast::DefaultStmt::PrintCommands),
            entry(token, ws_1).map(ast::DefaultStmt::PrintFresh),
            entry(token, ws_1).map(ast::DefaultStmt::Quiet),
            entry(token, ws_1).map(ast::DefaultStmt::Loud),
            entry(token, ws_1).map(ast::DefaultStmt::Explain),
            entry(token, ws_1).map(ast::DefaultStmt::Verbose),
            entry(token, ws_1).map(ast::DefaultStmt::WatchDelay),
            entry(token, ws_1).map(ast::DefaultStmt::Jobs),
            entry(token, ws_1).map(ast::DefaultStmt::Edition),
            fatal(Failure::Expected(&"valid key for `default` statement")).help("one of `target`, `out-dir`, `print-commands`, `print-fresh`, `quiet`, `loud`, `explain`, `verbose`, `watch-delay`, `jobs`, or `edition`"),
        ))
        .parse_next(input)
    }
}

impl Parse for ast::ConfigBool {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (value, span) = alt((
            parse::<keyword::True>.value(true),
            parse::<keyword::False>.value(false),
        ))
        .with_token_span()
        .parse_next(input)?;
        Ok(ast::ConfigBool(span, value))
    }
}

impl Parse for ast::ConfigString {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (value, span) = escaped_string.with_token_span().parse_next(input)?;
        Ok(ast::ConfigString(span, value.into()))
    }
}

impl Parse for ast::ConfigInt {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (value, span) = winnow::ascii::dec_int
            .expect(&"integer")
            .with_token_span()
            .parse_next(input)?;
        Ok(ast::ConfigInt(span, value))
    }
}

impl Parse for ast::ConfigValue {
    fn parse(input: &mut Input) -> PResult<Self> {
        alt((
            parse.map(ast::ConfigValue::Bool),
            escaped_string.with_token_span().map(|(string, span)| {
                ast::ConfigValue::String(ast::ConfigString(span, string.into()))
            }),
        ))
        .expect(&"string literal or boolean value")
        .parse_next(input)
    }
}

impl Parse for ast::TaskRecipeStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        alt((
            parse.map(ast::TaskRecipeStmt::Let),
            parse.map(ast::TaskRecipeStmt::Build),
            parse_run_stmt::<true>.map(ast::TaskRecipeStmt::Run),
            parse.map(ast::TaskRecipeStmt::Spawn),
            parse.map(ast::TaskRecipeStmt::EnvRemove),
            parse.map(ast::TaskRecipeStmt::Env),
            parse.map(ast::TaskRecipeStmt::Info),
            parse.map(ast::TaskRecipeStmt::Warn),
            parse.map(ast::TaskRecipeStmt::SetCapture),
            parse.map(ast::TaskRecipeStmt::SetNoCapture),
            fatal(Failure::Expected(&"task recipe statement")).help(
                "could be one of `let`, `from`, `build`, `depfile`, `run`, `spawn`, `info`, or `warn` statement",
            ),
        ))
        .parse_next(input)
    }
}

impl Parse for ast::TaskRecipe {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut recipe, span) = seq! { ast::TaskRecipe {
            span: default,
            token_task: parse,
            ws_1: whitespace,
            name: cut_err(parse).help(
                "`task` must be followed by an identifier",
            ),
            ws_2: whitespace,
            body: parse,
        }}
        .with_token_span()
        .while_parsing("task recipe")
        .parse_next(input)?;
        recipe.span = span;
        Ok(recipe)
    }
}

impl Parse for ast::BuildRecipeStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        alt((
            parse.map(ast::BuildRecipeStmt::From),
            parse.map(ast::BuildRecipeStmt::Let),
            parse.map(ast::BuildRecipeStmt::Depfile),
            parse_run_stmt::<false>.map(ast::BuildRecipeStmt::Run),
            parse.map(ast::BuildRecipeStmt::EnvRemove),
            parse.map(ast::BuildRecipeStmt::Env),
            parse.map(ast::BuildRecipeStmt::Info),
            parse.map(ast::BuildRecipeStmt::Warn),
            parse.map(ast::BuildRecipeStmt::SetCapture),
            parse.map(ast::BuildRecipeStmt::SetNoCapture),
            fatal(Failure::Expected(&"build recipe statement")).help(
                "could be one of `let`, `from`, `build`, `depfile`, `run`, or `echo` statement",
            ),
        ))
        .parse_next(input)
    }
}

impl Parse for ast::BuildRecipe {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut recipe, span) = seq! { ast::BuildRecipe {
            span: default,
            token_build: parse,
            ws_1: whitespace,
            pattern: cut_err(parse).help(
                "`build` must be followed by a pattern literal",
            ).help("use string interpolation to use variables in recipe names"),
            ws_2: whitespace,
            body: parse,
        }}
        .with_token_span()
        .while_parsing("build recipe")
        .parse_next(input)?;
        recipe.span = span;
        Ok(recipe)
    }
}

impl Parse for ast::LetStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut stmt, span) = seq! { ast::LetStmt {
            span: default,
            token_let: parse,
            ws_1: cut_err(whitespace_nonempty).expect(&"whitespace after `let`"),
            ident: cut_err(parse).help("`let` must be followed by an identifier"),
            ws_2: whitespace,
            token_eq: cut_err(parse).help("`let <identifier>` must be followed by a `=`"),
            ws_3: whitespace,
            value: cut_err(parse),
        }}
        .with_token_span()
        .while_parsing("`let` statement")
        .parse_next(input)?;
        stmt.span = span;
        Ok(stmt)
    }
}

impl Parse for ast::ConfigStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut stmt, span) = seq! { ast::ConfigStmt {
            span: default,
            token_config: parse,
            ws_1: cut_err(whitespace_nonempty).expect(&"whitespace after `config`"),
            ident: cut_err(parse).help("`config` must be followed by an identifier"),
            ws_2: whitespace,
            token_eq: cut_err(parse).help("`config <identifier>` must be followed by a `=`"),
            ws_3: whitespace,
            value: cut_err(parse),
        }}
        .with_token_span()
        .while_parsing("`config` statement")
        .parse_next(input)?;
        stmt.span = span;
        Ok(stmt)
    }
}

impl Parse for ast::EnvStmt {
    fn parse(input: &mut Input) -> PResult<Self> {
        env_stmt(input)
    }
}

fn env_stmt(input: &mut Input) -> PResult<ast::EnvStmt> {
    fn env_stmt_inner(input: &mut Input) -> PResult<ast::EnvStmt> {
        let (token, ws_1, key, ws_2, token_eq, ws_3, value) = seq! {(
            parse,
            cut_err(whitespace_nonempty).expect(&"whitespace after `env`"),
            cut_err(parse)
                .help("`env` must be followed by a string")
                .help("consider using string interpolation to use variables in environment keys"),
            whitespace,
            cut_err(parse), // `=`
            whitespace,
            cut_err(parse),
        )}
        .while_parsing("`env` statement")
        .parse_next(input)?;

        Ok(ast::EnvStmt {
            span: Span::default(),
            token,
            ws_1,
            key,
            ws_2,
            token_eq,
            ws_3,
            value,
        })
    }

    let (mut stmt, span) = env_stmt_inner.with_token_span().parse_next(input)?;
    stmt.span = span;
    Ok(stmt)
}

/// `<keyword> <param>`
///
/// If the keyword is successfully parsed, parse the param with `cut_err(...)`,
/// so no backtracking.
fn kw_expr<'a, K, Param>(
    mut inner: impl Parser<'a, Param> + Copy,
) -> impl Parser<'a, ast::KwExpr<K, Param>>
where
    K: keyword::Keyword + Parse,
{
    move |input: &mut Input<'a>| {
        let (mut expr, span) = seq! { ast::KwExpr {
            span: default,
            token: parse,
            ws_1: whitespace_nonempty,
            param: cut_err(inner.by_ref()),
        }}
        .with_token_span()
        .while_parsing(K::TOKEN)
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

impl<T, Param> Parse for ast::KwExpr<T, Param>
where
    T: keyword::Keyword + Parse,
    Param: Parse,
{
    fn parse(input: &mut Input) -> PResult<Self> {
        kw_expr(parse).parse_next(input)
    }
}

impl Parse for ast::Expr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let expr = alt((
            parse.map(ast::Expr::StringExpr),
            parse.map(ast::Expr::List),
            parse.map(ast::Expr::Shell),
            parse.map(ast::Expr::Read),
            parse.map(ast::Expr::Glob),
            parse.map(ast::Expr::Which),
            parse.map(ast::Expr::Env),
            parse.map(ast::Expr::Error),
            parse.map(ast::Expr::Ident),
            parse.map(ast::Expr::SubExpr),
            fatal(Failure::Expected(&"expression"))
                .help("expressions must start with a value, or an `env`, `glob`, `which`, or `shell` operation")
        ))
        .parse_next(input)?;

        // Check if there is a `[...]` array lookup suffix.
        if let Some(IndexSuffix {
            ws_1,
            token_open,
            ws_2,
            index,
            ws_3,
            token_close,
        }) = opt(parse::<IndexSuffix>).parse_next(input)?
        {
            Ok(ast::Expr::Index(ast::IndexExpr {
                span: expr.span().merge(token_close.span()),
                target: Box::new(expr),
                ws_1,
                token_open,
                ws_2,
                index,
                ws_3,
                token_close,
            }))
        } else {
            Ok(expr)
        }
    }
}

struct IndexSuffix {
    pub ws_1: ast::Whitespace,
    pub token_open: token::BracketOpen,
    pub ws_2: ast::Whitespace,
    pub index: ast::IndexValue,
    pub ws_3: ast::Whitespace,
    pub token_close: token::BracketClose,
}

impl Parse for IndexSuffix {
    fn parse(input: &mut Input) -> PResult<Self> {
        seq! {IndexSuffix {
            ws_1: whitespace,
            token_open: parse,
            ws_2: whitespace,
            index: cut_err(parse),
            ws_3: whitespace,
            token_close: cut_err(parse)
        }}
        .parse_next(input)
    }
}

impl Parse for ast::IndexValue {
    fn parse(input: &mut Input) -> PResult<Self> {
        alt((
            parse.map(ast::IndexValue::Constant),
            cut_err(parse).map(|expr| ast::IndexValue::Expr(Box::new(expr))),
        ))
        .help("array indexing operator must be an integer constant or an expression")
        .parse_next(input)
    }
}

impl Parse for ast::ChainSubExpr {
    // "|" expression_tail
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut subexpr, span) = seq! { ast::ChainSubExpr {
            span: default,
            ws_1: whitespace,
            token_pipe: parse,
            ws_2: whitespace,
            expr: cut_err(expression_chain_op),
        }}
        .with_token_span()
        .parse_next(input)?;
        subexpr.span = span;
        Ok(subexpr)
    }
}

impl Parse for ast::ExprChain {
    fn parse(input: &mut Input) -> PResult<Self> {
        let atom = ast::Expr::parse(input)?;

        let (tail, tail_span): (Vec<_>, _) = repeat(0.., parse::<ast::ChainSubExpr>)
            .with_token_span()
            .parse_next(input)?;

        Ok(ast::ExprChain {
            span: atom.span().merge(tail_span),
            expr: atom,
            ops: tail,
        })
    }
}

impl Parse for ast::SubExpr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut expr, span) = seq! { ast::SubExpr {
            span: default,
            token_open: parse,
            ws_1: whitespace,
            expr: cut_err(parse),
            ws_2: whitespace,
            token_close: cut_err(parse),
        }}
        .with_token_span()
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

/// Expression after a `|` in an expression chain.
fn expression_chain_op(input: &mut Input) -> PResult<ast::ExprOp> {
    alt((
        alt((
            parse.map(ast::ExprOp::SubExpr),
            parse.map(ast::ExprOp::StringExpr),
            parse.map(ast::ExprOp::Match),
            parse.map(ast::ExprOp::Map),
            parse.map(ast::ExprOp::Flatten),
            parse.map(ast::ExprOp::Filter),
            parse.map(ast::ExprOp::FilterMatch),
            parse.map(ast::ExprOp::Discard),
            parse.map(ast::ExprOp::Join),
            parse.map(ast::ExprOp::Split),
            parse.map(ast::ExprOp::Dedup))),
        alt((parse.map(ast::ExprOp::Lines),
            parse.map(ast::ExprOp::Len),
            parse.map(ast::ExprOp::First),
            parse.map(ast::ExprOp::Last),
            parse.map(ast::ExprOp::Tail),
            parse.map(ast::ExprOp::Info),
            parse.map(ast::ExprOp::Warn),
            parse.map(ast::ExprOp::Error),
            parse.map(ast::ExprOp::AssertEq),
            parse.map(ast::ExprOp::AssertMatch))),
        fatal(Failure::Expected(&"a chaining expression"))
            .help("one of `join`, `flatten`, `map`, `match`, `env`, `glob`, `which`, `shell`, a string, or a sub-expression in parentheses")
    ))
    .parse_next(input)
}

fn parse_run_stmt<const ALLOW_SPAWN: bool>(input: &mut Input<'_>) -> PResult<ast::RunStmt> {
    kw_expr(parse_run_expr::<ALLOW_SPAWN>).parse_next(input)
}

fn parse_run_expr<const ALLOW_SPAWN: bool>(input: &mut Input<'_>) -> PResult<ast::RunExpr> {
    alt((
        parse.map(|string: ast::StringExpr| {
            ast::RunExpr::Shell(ast::ShellExpr {
                span: string.span,
                token: keyword::Keyword::with_span(string.span),
                ws_1: ws_ignore(),
                param: string,
            })
        }),
        parse_run_expr_list::<ALLOW_SPAWN>.map(ast::RunExpr::List),
        parse.map(ast::RunExpr::Shell),
        parse_spawn_expr::<ALLOW_SPAWN>.map(ast::RunExpr::Spawn),
        parse.map(ast::RunExpr::Info),
        parse.map(ast::RunExpr::Warn),
        parse.map(ast::RunExpr::Write),
        parse.map(ast::RunExpr::Copy),
        parse.map(ast::RunExpr::Delete),
        parse.map(ast::RunExpr::Touch),
        parse.map(ast::RunExpr::EnvRemove),
        parse.map(ast::RunExpr::Env),
        parse_run_expr_block::<ALLOW_SPAWN>.map(ast::RunExpr::Block),
        fatal(Failure::Expected(&"a run expression"))
            .help(if ALLOW_SPAWN {
                "one of `shell`, `spawn`, `info`, `warn`, `write`, `copy`, `delete`, `env`, `env-remove`, a string literal, a list, or a block"
            } else { "one of `shell`, `info`, `warn`, `write`, `copy`, `delete`, `env`, `env-remove`, a string literal, a list, or a block"} )
    )).parse_next(input)
}

fn parse_spawn_expr<const ALLOW_SPAWN: bool>(input: &mut Input) -> PResult<ast::SpawnExpr> {
    let expr = parse.parse_next(input)?;
    if ALLOW_SPAWN {
        Ok(expr)
    } else {
        Err(ModalErr::Error(
            Error::new(expr.span.start, Failure::Unexpected(&"spawn statement"))
                .with_hint("`spawn` is only allowed in `task` recipes"),
        ))
    }
}

fn parse_run_expr_list<const ALLOW_SPAWN: bool>(
    input: &mut Input<'_>,
) -> PResult<ast::ListExpr<ast::RunExpr>> {
    parse_list_expr(parse_run_expr::<ALLOW_SPAWN>).parse_next(input)
}

fn parse_run_expr_block<const ALLOW_SPAWN: bool>(
    input: &mut Input<'_>,
) -> PResult<ast::Body<ast::RunExpr>> {
    let (token_open, statements, decor_trailing, token_close) =
        statements_delimited(parse, parse_run_expr::<ALLOW_SPAWN>, parse).parse_next(input)?;

    Ok(ast::Body {
        token_open,
        statements,
        ws_trailing: decor_trailing,
        token_close,
    })
}

impl Parse for ast::WriteExpr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut expr, span) = seq! {ast::WriteExpr {
            span: default,
            token_write: parse,
            ws_1: whitespace,
            value: cut_err(parse),
            ws_2: whitespace,
            token_to: cut_err(parse),
            ws_3: whitespace,
            path: cut_err(parse),
        }}
        .with_token_span()
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

impl Parse for ast::CopyExpr {
    fn parse(input: &mut Input) -> PResult<Self> {
        let (mut expr, span) = seq! {ast::CopyExpr {
            span: default,
            token_copy: parse,
            ws_1: whitespace,
            src: cut_err(parse),
            ws_2: whitespace,
            token_to: cut_err(parse),
            ws_3: whitespace,
            dest: cut_err(parse),
        }}
        .with_token_span()
        .parse_next(input)?;
        expr.span = span;
        Ok(expr)
    }
}

impl Parse for ast::MatchBody {
    fn parse(input: &mut Input) -> PResult<Self> {
        struct MatchArmBraced(ast::MatchArm);
        impl Parse for MatchArmBraced {
            fn parse(input: &mut Input) -> PResult<Self> {
                let (mut arm, span) = seq! {ast::MatchArm {
            span: default,
            pattern: cut_err(parse).help("`match` arm must start with a pattern"),
            ws_1: whitespace,
            token_fat_arrow: cut_err(parse).help("pattern must be followed by `=>` in `match`"),
            ws_2: whitespace,
            expr: cut_err(parse).help("`=>` must be followed by an expression in `match`"),
        }}
        .with_token_span()
        .parse_next(input)?;
                arm.span = span;
                Ok(MatchArmBraced(arm))
            }
        }

        #[allow(dead_code)] // False positive!
        struct MatchArmSingle(ast::MatchArm);
        impl Parse for MatchArmSingle {
            fn parse(input: &mut Input) -> PResult<MatchArmSingle> {
                let (mut arm, span) = seq! {ast::MatchArm {
            span: default,
            pattern: cut_err(parse).help("`match` must be followed by a `{...}` block, or a single pattern"),
            ws_1: whitespace,
            token_fat_arrow: cut_err(parse).help("pattern must be followed by `=>` in `match`"),
            ws_2: whitespace,
            expr: cut_err(parse)
                .help("`=>` must be followed by a string literal in inline `match`")
                .map(ast::Expr::StringExpr)
                .map(Into::into),
        }}
        .with_token_span()
        .parse_next(input)?;
                arm.span = span;
                Ok(MatchArmSingle(arm))
            }
        }

        alt((
            preceded(
                peek(parse::<token::BraceOpen>),
                cut_err(parse::<ast::Body<MatchArmBraced>>).map(|body| {
                    ast::MatchBody::Braced(ast::Body {
                        token_open: body.token_open,
                        statements: body
                            .statements
                            .into_iter()
                            .map(|stmt| ast::BodyStmt {
                                ws_pre: stmt.ws_pre,
                                statement: stmt.statement.0,
                                trailing: stmt.trailing,
                            })
                            // Note: Guaranteed in-place.
                            .collect(),
                        ws_trailing: body.ws_trailing,
                        token_close: body.token_close,
                    })
                }),
            ),
            parse.map(|MatchArmSingle(arm)| ast::MatchBody::Single(Box::new(arm))),
        ))
        .parse_next(input)
    }
}

fn parse_list_expr<'a, Inner: Parser<'a, T>, T>(
    mut inner: Inner,
) -> impl Parser<'a, ast::ListExpr<T>> {
    move |input: &mut Input<'a>| {
        let token_open = parse::<token::BracketOpen>.parse_next(input)?;
        let mut accum = Vec::new();

        let mut has_separator = true;
        let mut last_decor = whitespace.parse_next(input)?;
        let mut end_of_last_item = input.checkpoint();

        loop {
            if let Ok(token_close) = parse::<token::BracketClose>.parse_next(input) {
                return Ok(ast::ListExpr {
                    span: token_open.span().merge(token_close.span()),
                    token_open,
                    items: accum,
                    ws_trailing: last_decor,
                    token_close,
                });
            }

            if !has_separator {
                input.reset(&end_of_last_item);
                return Err(ModalErr::Error(Error::new(
                    Offset(input.previous_token_end() as u32),
                    Failure::ExpectedChar(','),
                )));
            }

            let item = inner.parse_next(input)?;
            end_of_last_item = input.checkpoint();

            let whitespace_before_comma = whitespace.parse_next(input)?;
            let comma_and_whitespace = opt((parse, whitespace)).parse_next(input)?;

            let preceding_whitespace;
            let trailing;

            if let Some((token_comma, whitespace_after_comma)) = comma_and_whitespace {
                trailing = ast::Trailing {
                    ws: whitespace_before_comma,
                    token: Some(token_comma),
                };
                preceding_whitespace = last_decor;
                has_separator = true;
                last_decor = whitespace_after_comma;
            } else {
                trailing = ast::Trailing {
                    // Attribute the whitespace to the next item.
                    ws: ast::Whitespace(Span::from_offset_and_len(
                        whitespace_before_comma.0.start,
                        0,
                    )),
                    token: None,
                };
                preceding_whitespace = last_decor;
                has_separator = false;
                last_decor = whitespace_before_comma;
            }

            accum.push(ast::ListItem {
                ws_pre: preceding_whitespace,
                item,
                trailing,
            });
        }
    }
}

impl<T: Parse> Parse for ast::ListExpr<T> {
    fn parse(input: &mut Input) -> PResult<Self> {
        parse_list_expr(parse).parse_next(input)
    }
}

impl Parse for ast::Ident {
    fn parse(input: &mut Input) -> PResult<Self> {
        fn identifier_chars<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
            const KEYWORDS: &[&str] = &["let"];

            fn is_identifier_start(ch: char) -> bool {
                unicode_ident::is_xid_start(ch)
            }

            fn is_identifier_continue(ch: char) -> bool {
                // Allow kebab-case identifiers
                ch == '-' || unicode_ident::is_xid_continue(ch)
            }

            (
                take_while(1, is_identifier_start),
                take_while(0.., is_identifier_continue),
            )
                .take()
                .verify(|s| !KEYWORDS.contains(s))
                .expect(&"identifier")
                .parse_next(input)
        }

        let (ident, span) = identifier_chars.with_token_span().parse_next(input)?;
        Ok(ast::Ident::new(span, ident))
    }
}

fn escaped_string<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    fn escaped_string_char(input: &mut Input) -> PResult<()> {
        alt((none_of(['\\', '\"']).value(()), ('\\', any).value(()))).parse_next(input)
    }

    delimited(
        parse::<token::DoubleQuote>.expect(&"string literal"),
        repeat::<_, (), (), _, _>(0.., escaped_string_char).take(),
        cut_err(parse::<token::DoubleQuote>),
    )
    .parse_next(input)
}

fn until_eol_or_eof<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    match (till_line_ending, line_ending).take().parse_next(input) {
        Ok(comment) => Ok(comment),
        Err(ModalErr::Backtrack(..)) => Ok(input.finish()),
        Err(err) => Err(err),
    }
}

/// Whitespace (whitespace and comments) preceding a statement. The whitespace
/// can be used to separate statements. Does not include semicolons.
#[expect(clippy::unnecessary_wraps)] // This parser can never fail.
fn whitespace_parsed(input: &mut Input) -> PResult<ParsedWhitespace> {
    #[derive(Clone, Copy)]
    enum WsPart {
        Comment,
        /// Indentation etc.
        Whitespace,
        /// Newline characters, so this decoration can be used to separate
        /// statements.
        Newline,
    }

    let ws_part = alt((
        ('#', until_eol_or_eof).value(WsPart::Comment),
        one_of([' ', '\t', '\r']).value(WsPart::Whitespace),
        '\n'.value(WsPart::Newline),
    ));

    let mut parser = repeat(0.., ws_part)
        .fold(
            || (false, false),
            |(has_newlines, has_comments), part| match part {
                WsPart::Comment => (has_newlines, true),
                WsPart::Whitespace => (has_newlines, has_comments),
                WsPart::Newline => (true, has_comments),
            },
        )
        .with_token_span();

    match parser.parse_next(input) {
        Ok(((has_newlines, has_comments), span)) => Ok(ParsedWhitespace {
            span,
            has_newlines,
            has_comments,
        }),
        Err(_) => unreachable!("whitespace parser should never fail"),
    }
}

fn whitespace_parsed_nonempty(input: &mut Input) -> PResult<ParsedWhitespace> {
    whitespace_parsed
        .verify(|ws| !ws.span.is_empty())
        .expect(&"whitespace")
        .parse_next(input)
}

fn whitespace(input: &mut Input) -> PResult<ast::Whitespace> {
    whitespace_parsed
        .map(ParsedWhitespace::into_whitespace)
        .parse_next(input)
}

fn whitespace_nonempty(input: &mut Input) -> PResult<ast::Whitespace> {
    whitespace_parsed_nonempty
        .map(ParsedWhitespace::into_whitespace)
        .parse_next(input)
}

#[derive(Debug, PartialEq)]
struct ParsedWhitespace {
    pub span: Span,
    pub has_newlines: bool,
    pub has_comments: bool,
}

impl ParsedWhitespace {
    pub fn into_whitespace(self) -> ast::Whitespace {
        ast::Whitespace(self.span)
    }

    pub fn is_statement_separator(&self) -> bool {
        self.has_newlines | self.has_comments
    }
}

pub trait SetFailure {
    fn set_failure(&mut self, failure: Failure);
}

impl SetFailure for Error {
    #[inline]
    fn set_failure(&mut self, failure: Failure) {
        self.fail = failure;
    }
}

impl SetFailure for ModalErr {
    #[inline]
    fn set_failure(&mut self, failure: Failure) {
        match self {
            ModalErr::Backtrack(_, fail) => *fail = failure,
            ModalErr::Error(err) => err.fail = failure,
        }
    }
}

pub(crate) struct WhileParsing<P>(P, &'static str);
impl<'a, P, O> winnow::Parser<Input<'a>, O, ModalErr> for WhileParsing<P>
where
    P: Parser<'a, O>,
{
    fn parse_next(&mut self, input: &mut Input<'a>) -> PResult<O> {
        let checkpoint = input.checkpoint();
        self.0
            .parse_next(input)
            .map_err(|err| err.add_context(input, &checkpoint, ErrContext::WhileParsing(self.1)))
    }
}

pub(crate) struct SpannedTokenParser<P, O>(P, PhantomData<O>);
impl<'a, P, O> winnow::Parser<Input<'a>, (O, Span), ModalErr> for SpannedTokenParser<P, O>
where
    P: Parser<'a, O>,
{
    fn parse_next(&mut self, input: &mut Input<'a>) -> PResult<(O, Span)> {
        let start = input.current_token_start();
        self.0.parse_next(input).map(|value| {
            let end = input.previous_token_end();
            (value, Span::from(start..end))
        })
    }
}

pub(crate) struct TokenSpanParser<P, O>(P, std::marker::PhantomData<O>);
impl<'a, P, O> winnow::Parser<Input<'a>, Span, ModalErr> for TokenSpanParser<P, O>
where
    P: Parser<'a, O>,
{
    fn parse_next(&mut self, input: &mut Input<'a>) -> PResult<Span> {
        let start = input.current_token_start();
        self.0.parse_next(input).map(|_| {
            let end = input.previous_token_end();
            Span::from(start..end)
        })
    }
}

pub(crate) struct OrFail<P, O>(P, Option<Failure>, PhantomData<O>);
impl<'a, P, O> winnow::Parser<Input<'a>, O, ModalErr> for OrFail<P, O>
where
    P: Parser<'a, O>,
{
    fn parse_next(&mut self, input: &mut Input<'a>) -> PResult<O> {
        let mut result = self.0.parse_next(input);
        if let Err(ref mut err) = result {
            err.set_failure(self.1.take().expect("or_fail parser invoked twice"));
        }
        result
    }
}

pub(crate) struct Help<P, O>(P, &'static str, PhantomData<O>);
impl<'a, P, O, E> winnow::Parser<Input<'a>, O, E> for Help<P, O>
where
    P: winnow::Parser<Input<'a>, O, E>,
    E: winnow::error::ParserError<Input<'a>> + winnow::error::AddContext<Input<'a>, ErrContext>,
{
    fn parse_next(&mut self, input: &mut Input<'a>) -> Result<O, E> {
        let checkpoint = input.checkpoint();
        self.0
            .parse_next(input)
            .map_err(|err| err.add_context(input, &checkpoint, ErrContext::Hint(self.1)))
    }
}

#[cfg(test)]
mod tests {
    use super::Input;
    use crate::{
        ast::{
            self,
            keyword::{self, Keyword as _},
            trailing_ignore, ws, ws_ignore,
        },
        parser::{Offset, ParsedWhitespace, parse},
    };
    use werk_util::span;
    use winnow::Parser as _;

    #[test]
    fn which_expr() {
        let input = Input::new("which \"clang\"");
        assert_eq!(
            parse::<ast::Expr>.parse(input).unwrap(),
            ast::Expr::Which(ast::WhichExpr {
                span: span(0..13),
                token: keyword::Which(Offset(0)),
                ws_1: ws(5..6),
                param: ast::StringExpr {
                    span: span(6..13),
                    fragments: vec![ast::StringFragment::Literal("clang".into())]
                },
            })
        );
    }

    #[test]
    fn root_statements() {
        let input = Input::new(
            "default out-dir = \"../target\"\n\nlet cc = which \"clang\"\nlet ld = cc\n",
        );
        let root = super::root.parse(input).unwrap();
        assert_eq!(
            root,
            ast::Root {
                statements: vec![
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Default(ast::DefaultStmt::OutDir(
                            ast::DefaultStmtEntry {
                                span: span(0..29),
                                token: keyword::Default(Offset(0)),
                                ws_1: ws_ignore(),
                                key: keyword::OutDir(Offset(8)),
                                ws_2: ws_ignore(),
                                token_eq: ast::token::Token(Offset(16)),
                                ws_3: ws_ignore(),
                                value: ast::ConfigString(span(18..29), "../target".into()),
                            }
                        )),
                        trailing: trailing_ignore(),
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Let(ast::LetStmt {
                            span: span(31..53),
                            token_let: keyword::Let(Offset(31)),
                            ws_1: ws_ignore(),
                            ident: ast::Ident {
                                span: span(35..37),
                                ident: "cc".into(),
                            },
                            ws_2: ws_ignore(),
                            token_eq: ast::token::Token(Offset(38)),
                            ws_3: ws_ignore(),
                            value: ast::Expr::Which(ast::WhichExpr {
                                span: span(40..53),
                                token: ast::keyword::Which(Offset(40)),
                                ws_1: ws_ignore(),
                                param: ast::StringExpr {
                                    span: span(46..53),
                                    fragments: vec![ast::StringFragment::Literal("clang".into())]
                                },
                            })
                            .into(),
                        }),
                        trailing: trailing_ignore()
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Let(ast::LetStmt {
                            span: span(54..65),
                            token_let: keyword::Let(Offset(54)),
                            ws_1: ws_ignore(),
                            ident: ast::Ident {
                                span: span(58..60),
                                ident: "ld".into(),
                            },
                            ws_2: ws_ignore(),
                            token_eq: ast::token::Token(Offset(61)),
                            ws_3: ws_ignore(),
                            value: ast::Expr::Ident(ast::Ident {
                                span: span(63..65),
                                ident: "cc".into(),
                            })
                            .into(),
                        }),
                        trailing: trailing_ignore(),
                    }
                ],
                ws_trailing: ws_ignore(),
            }
        );
    }

    #[test]
    fn let_stmt() {
        let input = Input::new("let cc = which \"clang\"");
        assert_eq!(
            parse::<ast::LetStmt>.parse(input).unwrap(),
            ast::LetStmt {
                span: span(0..22),
                token_let: keyword::Let(Offset(0)),
                ws_1: ws(3..4),
                ident: ast::Ident {
                    span: span(4..6),
                    ident: "cc".into()
                },
                ws_2: ws(6..7),
                token_eq: ast::token::Token(Offset(7)),
                ws_3: ws(8..9),
                value: ast::Expr::Which(ast::WhichExpr {
                    span: span(9..22),
                    token: ast::keyword::Which(Offset(9)),
                    ws_1: ws(14..15),
                    param: ast::StringExpr {
                        span: span(15..22),
                        fragments: vec![ast::StringFragment::Literal("clang".into())]
                    },
                })
                .into(),
            }
        );
    }

    #[test]
    fn identifier() {
        let input = Input::new("hello");
        assert_eq!(
            parse::<ast::Ident>.parse(input).unwrap(),
            ast::Ident {
                span: span(0..5),
                ident: "hello".into()
            }
        );

        let input = Input::new("hello-world");
        assert_eq!(
            parse::<ast::Ident>.parse(input).unwrap(),
            ast::Ident {
                span: span(0..11),
                ident: "hello-world".into()
            }
        );

        let input = Input::new("hello world");
        assert_eq!(
            (parse, " world").parse(input).unwrap(),
            (
                ast::Ident {
                    span: span(0..5),
                    ident: "hello".into()
                },
                " world"
            )
        );
    }

    #[test]
    fn escaped_string() {
        assert_eq!(
            super::escaped_string
                .parse(Input::new(r#""hello, world""#))
                .unwrap(),
            "hello, world"
        );
        assert_eq!(
            super::escaped_string
                .parse(Input::new(r#""hello, \"world\"""#))
                .unwrap(),
            r#"hello, \"world\""#
        );
        assert_eq!(
            super::escaped_string
                .parse(Input::new(r#""hello, \\\"world\\\"""#))
                .unwrap(),
            r#"hello, \\\"world\\\""#
        );
    }

    #[test]
    fn whitespace_parsed() {
        assert_eq!(
            super::whitespace_parsed.parse(Input::new("  ")).unwrap(),
            ParsedWhitespace {
                span: span(0..2),
                has_newlines: false,
                has_comments: false
            }
        );
        assert_eq!(
            super::whitespace_parsed.parse(Input::new("#")).unwrap(),
            ParsedWhitespace {
                span: span(0..1),
                has_newlines: false,
                has_comments: true
            }
        );
        assert_eq!(
            super::whitespace_parsed
                .parse(Input::new("# comment\n"))
                .unwrap(),
            ParsedWhitespace {
                span: span(0..10),
                has_newlines: false,
                has_comments: true
            }
        );
        assert_eq!(
            super::whitespace_parsed.parse(Input::new("\n")).unwrap(),
            ParsedWhitespace {
                span: span(0..1),
                has_newlines: true,
                has_comments: false
            }
        );
        assert_eq!(
            super::whitespace_parsed.parse(Input::new("\r\n")).unwrap(),
            ParsedWhitespace {
                span: span(0..2),
                has_newlines: true,
                has_comments: false
            }
        );
    }

    #[test]
    fn filter_match() {
        assert_eq!(
            super::expression_chain_op
                .parse(Input::new("filter-match \"a\" => \"b\""))
                .unwrap(),
            ast::ExprOp::FilterMatch(ast::KwExpr {
                span: span(0..23),
                token: keyword::FilterMatch::with_span(span(0..12)),
                ws_1: ws_ignore(),
                param: ast::MatchBody::Single(Box::new(ast::MatchArm {
                    span: span(13..23),
                    pattern: ast::PatternExpr {
                        span: span(13..16),
                        fragments: vec![ast::PatternFragment::Literal("a".into())]
                    },
                    ws_1: ws_ignore(),
                    token_fat_arrow: keyword::FatArrow::with_span(span(17..19)),
                    ws_2: ws_ignore(),
                    expr: ast::Expr::StringExpr(ast::StringExpr {
                        span: span(20..23),
                        fragments: vec![ast::StringFragment::Literal("b".into())]
                    })
                    .into()
                }))
            })
        );
    }
}
