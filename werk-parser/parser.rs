use std::marker::PhantomData;

use winnow::{
    ascii::{line_ending, multispace1, till_line_ending},
    combinator::{
        alt, cut_err, delimited, empty, eof, fail, opt, peek, preceded, repeat, seq, terminated,
    },
    error::ErrMode,
    stream::Stream as _,
    token::{any, none_of, one_of, take_till, take_while},
    Parser,
};

use crate::{
    ast::{self, token, ws_ignore},
    parse_string::{
        path_interpolation, pattern_one_of, push_pattern_fragment, push_string_fragment,
        string_interpolation, StringFragment,
    },
    ErrContext, Expected,
};

mod span;

pub use span::*;

pub type Input<'a> = winnow::stream::Located<&'a str>;
pub type PError = crate::error::ParseError;
pub type PResult<T> = winnow::PResult<T, PError>;

pub fn parse_werk<'a>(source_code: &'a str) -> Result<crate::Document<'a>, crate::Error> {
    let root = root
        .parse(Input::new(source_code))
        .map_err(|err| crate::Error::Werk(span(err.offset()..err.offset()), err.into_inner()))?;
    Ok(crate::Document::new(root, source_code, None))
}

fn root<'a>(input: &mut Input<'a>) -> PResult<ast::Root<'a>> {
    let (_, statements, decor_trailing, _) =
        statements_delimited(empty, root_stmt, peek(eof)).parse_next(input)?;
    Ok(ast::Root {
        statements,
        ws_trailing: decor_trailing,
    })
}

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
) -> impl Parser<Input<'a>, (Open, Vec<ast::BodyStmt<Item>>, ast::Whitespace, Close), PError>
where
    OpenParser: Parser<Input<'a>, Open, PError>,
    CloseParser: Parser<Input<'a>, Close, PError>,
    ParseNextItem: Parser<Input<'a>, Item, PError>,
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
            match terminal.parse_next(input) {
                Ok(close) => {
                    return Ok((open, accum, last_decor.into_whitespace(), close));
                }
                Err(_) => (),
            }

            if !has_separator {
                return Err(Expected::Expected(
                    &"statement separator (comma or newline)",
                )
                .into());
            }

            let item = parse_next.parse_next(input)?;
            let preceding_whitespace = last_decor;
            let trailing_whitespace;

            let whitespace_before_semicolon = whitespace_parsed.parse_next(input)?;
            let semicolon_and_whitespace = opt((token, whitespace_parsed)).parse_next(input)?;

            if let Some((semicolon, whitespace_after_semicolon)) = semicolon_and_whitespace {
                // All whitespace before the semicolon is trailing for the item we just found.
                trailing_whitespace = Some((whitespace_before_semicolon.into_whitespace(), semicolon));
                // Whitespace after the semicolon is the comment for the next item.
                last_decor = whitespace_after_semicolon;
                // Semicolon is a separator.
                has_separator = true;
            } else {
                // No semicolon, so the whitespace between the statements is the
                // comment for the next item.
                trailing_whitespace = None;
                has_separator = whitespace_before_semicolon.is_statement_separator();
                last_decor = whitespace_before_semicolon;
            };

            accum.push(ast::BodyStmt {
                ws_pre: preceding_whitespace.into_whitespace(),
                statement: item,
                ws_trailing: trailing_whitespace,
            });
        }
    }
}

fn body<'a, T, TParser>(stmt_parser: TParser) -> impl Parser<Input<'a>, ast::Body<T>, PError>
where
    TParser: Parser<Input<'a>, T, PError> + Copy,
{
    move |input: &mut Input<'a>| -> PResult<ast::Body<T>> {
        let (token_open, statements, decor_trailing, token_close) =
            statements_delimited(token, stmt_parser, token).parse_next(input)?;

        Ok(ast::Body {
            token_open,
            statements,
            ws_trailing: decor_trailing,
            token_close,
        })
    }
}

fn root_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::RootStmt<'a>> {
    alt((
        config_stmt.map(ast::RootStmt::Config),
        let_stmt.map(ast::RootStmt::Let),
        task_recipe.map(ast::RootStmt::Task),
        build_recipe.map(ast::RootStmt::Build),
        cut_err(fail).context(Expected::Expected(
            &"`config`, `let`, `task`, or `build` statement",
        )),
    ))
    .parse_next(input)
}

fn config_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::ConfigStmt<'a>> {
    let (mut config, span) = seq! {ast::ConfigStmt {
        span: default,
        token_config: keyword::<token::Config>,
        ws_1: whitespace,
        ident: cut_err(identifier.context(Expected::Expected(
            &"`config` must be followed by an identifier",
        ))),
        ws_2: whitespace,
        token_eq: cut_err(token),
        ws_3: whitespace,
        value: cut_err(config_value(ident.ident)),
    }}
    .with_token_span()
    .parse_next(input)?;
    config.span = span;

    match config.ident.ident {
        "print-commands" => {
            if !matches!(config.value, ast::ConfigValue::Bool(_)) {
                return Err(ErrMode::Cut(
                    Expected::Expected(&"boolean value for `print-commands`").into(),
                ));
            }
        }
        "edition" => {
            if !matches!(config.value, ast::ConfigValue::String(_)) {
                return Err(ErrMode::Cut(
                    Expected::Expected(&"string literal for `edition`").into(),
                ));
            }
        }
        "out-dir" => {
            if !matches!(config.value, ast::ConfigValue::String(_)) {
                return Err(ErrMode::Cut(
                    Expected::Expected(&"string literal for `out-dir`").into(),
                ));
            }
        }
        "default" => {
            if !matches!(config.value, ast::ConfigValue::String(_)) {
                return Err(ErrMode::Cut(
                    Expected::Expected(&"string literal for `default`").into(),
                ));
            }
        }
        _ => {
            return Err(ErrMode::Cut(
                Expected::Expected(
                    &"config key, one of `out-dir`, `edition`, `print-commands`, or `default`",
                )
                .into(),
            ))
        }
    }

    Ok(config)
}

fn config_value<'a>(key: &str) -> impl Parser<Input<'a>, ast::ConfigValue<'a>, PError> + '_ {
    move |input: &mut Input<'a>| {
        if key == "print-commands" {
            alt((
                keyword::<token::True>.value(ast::ConfigValue::Bool(true)),
                keyword::<token::False>.value(ast::ConfigValue::Bool(false)),
                fail.context(Expected::Expected(
                    &"`true` or `false` for `print-commands`",
                )),
            ))
            .parse_next(input)
        } else {
            cut_err(escaped_string)
                .map(ast::ConfigValue::String)
                .parse_next(input)
        }
    }
}

fn task_recipe<'a>(input: &mut Input<'a>) -> PResult<ast::CommandRecipe<'a>> {
    fn task_recipe_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::TaskRecipeStmt<'a>> {
        alt((
            let_stmt.map(ast::TaskRecipeStmt::Let),
            build_stmt.map(ast::TaskRecipeStmt::Build),
            run_stmt.map(ast::TaskRecipeStmt::Run),
            info_expr.map(ast::TaskRecipeStmt::Info),
            warn_expr.map(ast::TaskRecipeStmt::Warn),
            cut_err(fail).context(Expected::Expected(
                &"`let`, `from`, `build`, `depfile`, `run`, or `echo` statement",
            )),
        ))
        .parse_next(input)
    }

    let (mut recipe, span) = seq! { ast::CommandRecipe {
        span: default,
        token_task: keyword::<token::Task>,
        ws_1: whitespace,
        name: cut_err(identifier.context(Expected::Expected(
            &"`task` must be followed by an identifier",
        ))),
        ws_2: whitespace,
        body: body(task_recipe_stmt),
    }}
    .with_token_span()
    .parse_next(input)?;
    recipe.span = span;
    Ok(recipe)
}

fn build_recipe<'a>(input: &mut Input<'a>) -> PResult<ast::BuildRecipe<'a>> {
    fn build_recipe_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::BuildRecipeStmt<'a>> {
        alt((
            from_stmt.map(ast::BuildRecipeStmt::From),
            let_stmt.map(ast::BuildRecipeStmt::Let),
            depfile_stmt.map(ast::BuildRecipeStmt::Depfile),
            run_stmt.map(ast::BuildRecipeStmt::Run),
            info_expr.map(ast::BuildRecipeStmt::Info),
            warn_expr.map(ast::BuildRecipeStmt::Warn),
            cut_err(fail).context(Expected::Expected(
                &"`let`, `from`, `build`, `depfile`, `run`, or `echo` statement",
            )),
        ))
        .parse_next(input)
    }

    let (mut recipe, span) = seq! { ast::BuildRecipe {
        span: default,
        token_build: keyword::<token::Build>,
        ws_1: whitespace,
        pattern: cut_err(pattern_expr.context(Expected::Expected(
            &"`build` must be followed by a string literal",
        ))),
        ws_2: whitespace,
        body: body(build_recipe_stmt),
    }}
    .with_token_span()
    .parse_next(input)?;
    recipe.span = span;
    Ok(recipe)
}

fn let_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::LetStmt<'a>> {
    fn let_stmt_inner<'a>(input: &mut Input<'a>) -> PResult<ast::LetStmt<'a>> {
        let (token_let, ws_1, ident, ws_2, token_eq, ws_3, value) = seq! {(
            keyword::<token::Let>,
            cut_err(whitespace_nonempty).context(Expected::Expected(&"whitespace after `let`")),
            cut_err(identifier.context(Expected::Expected(
                &"`let` must be followed by an identifier",
            ))),
            whitespace,
            cut_err(token), // `=`
            whitespace,
            cut_err(expression_chain),
        )}
        .parse_next(input)?;

        Ok(ast::LetStmt {
            span: Span::default(),
            token_let,
            ws_1,
            ident,
            ws_2,
            token_eq,
            ws_3,
            value,
        })
    }

    let (mut stmt, span) = let_stmt_inner.with_token_span().parse_next(input)?;
    stmt.span = span;
    Ok(stmt)
}

fn kw_expr<'a, T: token::Keyword, P, PParser>(
    param: PParser,
) -> impl Parser<Input<'a>, ast::KwExpr<T, P>, PError>
where
    PParser: Parser<Input<'a>, P, PError>,
{
    (keyword::<T>, whitespace_nonempty, cut_err(param))
        .context(ErrContext::KwExpr(T::TOKEN))
        .with_token_span()
        .map(|((token, ws_1, value), span)| ast::KwExpr {
            span,
            token,
            ws_1,
            param: value,
        })
}

fn from_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::FromStmt<'a>> {
    kw_expr(expression_chain).parse_next(input)
}

fn build_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::BuildStmt<'a>> {
    kw_expr(expression_chain).parse_next(input)
}

fn depfile_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::DepfileStmt<'a>> {
    kw_expr(expression_chain).parse_next(input)
}

fn run_stmt<'a>(input: &mut Input<'a>) -> PResult<ast::RunStmt<'a>> {
    kw_expr(run_expression).parse_next(input)
}

fn info_expr<'a>(input: &mut Input<'a>) -> PResult<ast::InfoExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn warn_expr<'a>(input: &mut Input<'a>) -> PResult<ast::WarnExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

/// Expression with chaining (`ast::ThenExpr`).
fn expression_chain<'a>(input: &mut Input<'a>) -> PResult<ast::Expr<'a>> {
    let expr = expression_head.parse_next(input)?;

    // "|" expression_tail
    fn expression_pipe<'a>(input: &mut Input<'a>) -> PResult<ast::ChainSubExpr<'a>> {
        let (mut subexpr, span) = seq! { ast::ChainSubExpr {
            span: default,
            ws_1: whitespace,
            token_pipe: token,
            ws_2: whitespace,
            expr: cut_err(expression_tail),
        }}
        .with_token_span()
        .parse_next(input)?;
        subexpr.span = span;
        Ok(subexpr)
    }

    let (tail, tail_span): (Vec<_>, _) = repeat(0.., expression_pipe)
        .with_token_span()
        .parse_next(input)?;

    if tail.is_empty() {
        Ok(expr)
    } else {
        Ok(ast::Expr::Chain(ast::ChainExpr {
            span: expr.span().merge(tail_span),
            head: Box::new(expr),
            tail,
        }))
    }
}

/// Expression with no chaining.
fn expression_head<'a>(input: &mut Input<'a>) -> PResult<ast::Expr<'a>> {
    alt((
        string_expr.map(ast::Expr::StringExpr),
        list_of(expression_chain).map(ast::Expr::List),
        shell_expr.map(ast::Expr::Shell),
        glob_expr.map(ast::Expr::Glob),
        which_expr.map(ast::Expr::Which),
        join_expr.map(ast::Expr::Join),
        env_expr.map(ast::Expr::Env),
        identifier.map(ast::Expr::Ident),
    ))
    .context(Expected::Expected(&"expression start; expression chains must start with a value, or an `env`, `glob`, `which`, or `shell` operation"))
    .parse_next(input)
}

/// Expression after a `|` in an expression chain.
fn expression_tail<'a>(input: &mut Input<'a>) -> PResult<ast::Expr<'a>> {
    alt((
        string_expr.map(ast::Expr::StringExpr),
        shell_expr.map(ast::Expr::Shell),
        glob_expr.map(ast::Expr::Glob),
        which_expr.map(ast::Expr::Which),
        join_expr.map(ast::Expr::Join),
        map_expr.map(ast::Expr::Map),
        keyword.map(ast::Expr::Flatten),
        kw_expr(pattern_expr).map(ast::Expr::Filter),
        kw_expr(cut_err(match_body)).map(ast::Expr::FilterMatch),
        kw_expr(pattern_expr).map(ast::Expr::Discard),
        kw_expr(pattern_expr).map(ast::Expr::Split),
        keyword.map(ast::Expr::Lines),
        env_expr.map(ast::Expr::Env),
        match_expr.map(ast::Expr::Match),
        info_expr.map(ast::Expr::Info),
        warn_expr.map(ast::Expr::Warn),
        error_expr.map(ast::Expr::Error),
        assert_eq_expr.map(ast::Expr::AssertEq),
    ))
    .context(Expected::Expected(&"a chaining operation; one of `join`, `flatten`, `map`, `match`, `env`, `glob`, `which`, `shell`, or a string expression"))
    .parse_next(input)
}

fn run_expression<'a>(input: &mut Input<'a>) -> PResult<ast::RunExpr<'a>> {
    alt((
        string_expr.map(|string| {
            ast::RunExpr::Shell(ast::ShellExpr {
                span: string.span,
                token: token::Keyword::with_span(string.span),
                ws_1: ws_ignore(),
                param: string,
            })
        }),
        list_of(run_expression).map(ast::RunExpr::List),
        shell_expr.map(ast::RunExpr::Shell),
        info_expr.map(ast::RunExpr::Info),
        warn_expr.map(ast::RunExpr::Warn),
        write_expr.map(ast::RunExpr::Write),
        copy_expr.map(ast::RunExpr::Copy),
        body(run_expression).map(ast::RunExpr::Block),
    ))
    .parse_next(input)
}

fn write_expr<'a>(input: &mut Input<'a>) -> PResult<ast::WriteExpr<'a>> {
    let (mut expr, span) = seq! {ast::WriteExpr {
        span: default,
        token_write: keyword,
        ws_1: whitespace,
        value: cut_err(expression_chain),
        ws_2: whitespace,
        token_to: cut_err(keyword),
        ws_3: whitespace,
        path: cut_err(expression_chain),
    }}
    .with_token_span()
    .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn copy_expr<'a>(input: &mut Input<'a>) -> PResult<ast::CopyExpr<'a>> {
    let (mut expr, span) = seq! {ast::CopyExpr {
        span: default,
        token_copy: keyword,
        ws_1: whitespace,
        src: cut_err(string_expr),
        ws_2: whitespace,
        token_to: cut_err(keyword),
        ws_3: whitespace,
        dest: cut_err(string_expr),
    }}
    .with_token_span()
    .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn shell_expr<'a>(input: &mut Input<'a>) -> PResult<ast::ShellExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn glob_expr<'a>(input: &mut Input<'a>) -> PResult<ast::GlobExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn which_expr<'a>(input: &mut Input<'a>) -> PResult<ast::WhichExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn join_expr<'a>(input: &mut Input<'a>) -> PResult<ast::JoinExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn map_expr<'a>(input: &mut Input<'a>) -> PResult<ast::MapExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn env_expr<'a>(input: &mut Input<'a>) -> PResult<ast::EnvExpr<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn match_expr<'a>(input: &mut Input<'a>) -> PResult<ast::MatchExpr<'a>> {
    let (mut expr, span) = seq! {ast::MatchExpr {
        span: default,
        token: keyword::<token::Match>,
        ws_1: whitespace,
        param: cut_err(match_body),
    }}
    .with_token_span()
    .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn match_body<'a>(input: &mut Input<'a>) -> PResult<ast::MatchBody<'a>> {
    fn match_arm_braced<'a>(input: &mut Input<'a>) -> PResult<ast::MatchArm<'a>> {
        let (mut arm, span) = seq! {ast::MatchArm {
            span: default,
            pattern: cut_err(pattern_expr.context(Expected::Expected(&"match arm must start with a pattern literal"))),
            ws_1: whitespace,
            token_fat_arrow: cut_err(keyword.context(Expected::Expected(&"match arm is missing a `=>`"))),
            ws_2: whitespace,
            expr: cut_err(expression_chain),
        }}
        .with_token_span()
        .parse_next(input)?;
        arm.span = span;
        Ok(arm)
    }

    fn match_arm_single<'a>(input: &mut Input<'a>) -> PResult<ast::MatchArm<'a>> {
        let (mut arm, span) = seq! {ast::MatchArm {
            span: default,
            pattern: cut_err(pattern_expr.context(Expected::Expected(&"inline match must start with a pattern literal"))),
            ws_1: whitespace,
            token_fat_arrow: cut_err(keyword.context(Expected::Expected(&"inline match is missing a `=>`"))),
            ws_2: whitespace,
            expr: cut_err(string_expr.context(ErrContext::StringExpr)).map(ast::Expr::StringExpr),
        }}
        .with_token_span()
        .parse_next(input)?;
        arm.span = span;
        Ok(arm)
    }

    alt((
        preceded(
            peek(token::<'{'>),
            cut_err(body(match_arm_braced)).map(ast::MatchBody::Braced),
        ),
        match_arm_single.map(Box::new).map(ast::MatchBody::Single),
    ))
    .context(Expected::Expected(
        &"match body { ... } or a single match arm",
    ))
    .parse_next(input)
}

fn error_expr<'a>(input: &mut Input<'a>) -> PResult<ast::ErrorStmt<'a>> {
    kw_expr(string_expr).parse_next(input)
}

fn assert_eq_expr<'a>(input: &mut Input<'a>) -> PResult<ast::AssertEqExpr<'a>> {
    kw_expr(expression_head.map(Box::new)).parse_next(input)
}

fn string_expr<'a>(input: &mut Input<'a>) -> PResult<ast::StringExpr<'a>> {
    let (mut expr, span) = delimited(
        '"'.context(Expected::Expected(&"string literal")),
        string_expr_inside_quotes,
        cut_err('"').context(Expected::ExpectedChar('"')),
    )
    .with_token_span()
    .context(Expected::Expected(&"string literal"))
    .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn pattern_expr<'a>(input: &mut Input<'a>) -> PResult<ast::PatternExpr<'a>> {
    let (mut expr, span) = delimited(
        '"'.context(Expected::Expected(&"pattern literal")),
        pattern_expr_inside_quotes,
        cut_err('"').context(Expected::ExpectedChar('"')),
    )
    .with_token_span()
    .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn string_expr_inside_quotes<'a>(input: &mut Input<'a>) -> PResult<ast::StringExpr<'a>> {
    let (mut expr, span) = repeat(0.., string_fragment)
        .fold(ast::StringExpr::default, |mut expr, fragment| {
            push_string_fragment(&mut expr, fragment);
            expr
        })
        .with_token_span()
        .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn pattern_expr_inside_quotes<'a>(input: &mut Input<'a>) -> PResult<ast::PatternExpr<'a>> {
    let (mut expr, span) = repeat(0.., pattern_fragment)
        .fold(ast::PatternExpr::default, |mut expr, fragment| {
            push_pattern_fragment(&mut expr, fragment);
            expr
        })
        .with_token_span()
        .parse_next(input)?;
    expr.span = span;
    Ok(expr)
}

fn string_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
        string_literal_fragment::<false>.map(StringFragment::Literal),
        escaped_char.map(StringFragment::EscapedChar),
        escaped_whitespace.value(StringFragment::EscapedWhitespace),
        string_interpolation.map(StringFragment::Interpolation),
        path_interpolation.map(StringFragment::Interpolation),
    ))
    .parse_next(input)
}

fn pattern_fragment<'a>(input: &mut Input<'a>) -> PResult<StringFragment<'a>> {
    // TODO: Consider escape sequences etc.
    alt((
        '%'.value(StringFragment::PatternStem),
        pattern_one_of.map(StringFragment::OneOf),
        string_literal_fragment::<true>.map(StringFragment::Literal),
        escaped_char.map(StringFragment::EscapedChar),
        escaped_whitespace.value(StringFragment::EscapedWhitespace),
        string_interpolation.map(StringFragment::Interpolation),
        path_interpolation.map(StringFragment::Interpolation),
    ))
    .parse_next(input)
}

fn string_literal_fragment<'a, const IS_PATTERN: bool>(input: &mut Input<'a>) -> PResult<&'a str> {
    let special = if IS_PATTERN {
        &['"', '\\', '{', '}', '<', '>', '(', ')', '%'] as &[char]
    } else {
        &['"', '\\', '{', '}', '<', '>'] as &[char]
    };

    take_till(1.., special)
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn escaped_char(input: &mut Input) -> PResult<char> {
    let escape_seq_char = winnow::combinator::dispatch! {
        any;
        '\\' => empty.value('\\'),
        '{' => empty.value('{'),
        '}' => empty.value('}'),
        '<' => empty.value('<'),
        '>' => empty.value('>'),
        '%' => empty.value('%'),
        '(' => empty.value('('),
        ')' => empty.value(')'),
        '"' => empty.value('"'),
        'n' => empty.value('\n'),
        'r' => empty.value('\r'),
        't' => empty.value('\t'),
        '0' => empty.value('\0'),
        _ => fail.context(Expected::Expected(&"valid escape sequence")),
    };

    preceded('\\', escape_seq_char).parse_next(input)
}

fn escaped_whitespace<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    preceded('\\', multispace1).parse_next(input)
}

fn list_of<'a, Item, ParseItem>(
    parse_item: ParseItem,
) -> impl Parser<Input<'a>, ast::ListExpr<Item>, PError>
where
    ParseItem: Parser<Input<'a>, Item, PError>,
{
    let mut parse_item = cut_err(parse_item);

    move |input: &mut Input<'a>| -> PResult<ast::ListExpr<Item>> {
        let token_open = token.parse_next(input)?;
        let mut accum = Vec::new();

        let mut has_separator = true;
        let mut last_decor = whitespace.parse_next(input)?;

        loop {
            match token.parse_next(input) {
                Ok(token_close) => {
                    return Ok(ast::ListExpr {
                        span: token_open.span().merge(token_close.span()),
                        token_open,
                        items: accum,
                        ws_trailing: last_decor,
                        token_close,
                    });
                }
                Err(_) => (),
            }

            if !has_separator {
                return Err(Expected::Expected(&"statement separator (comma or newline)").into());
            }

            let item = parse_item.parse_next(input)?;

            let whitespace_before_comma = whitespace.parse_next(input)?;
            let comma_and_whitespace = opt((token, whitespace)).parse_next(input)?;

            let preceding_whitespace;
            let trailing_whitespace;

            if let Some((token_comma, whitespace_after_comma)) = comma_and_whitespace {
                trailing_whitespace = Some((whitespace_before_comma, token_comma));
                preceding_whitespace = last_decor;
                has_separator = true;
                last_decor = whitespace_after_comma;
            } else {
                trailing_whitespace = None;
                preceding_whitespace = last_decor;
                has_separator = false;
                last_decor = whitespace_before_comma;
            }

            accum.push(ast::ListItem {
                ws_pre: preceding_whitespace,
                item,
                ws_trailing: trailing_whitespace,
            });
        }
    }
}

fn identifier<'a>(input: &mut Input<'a>) -> PResult<ast::Ident<'a>> {
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
            take_while(1, |ch| is_identifier_start(ch)),
            take_while(0.., |ch| is_identifier_continue(ch)),
        )
            .context(Expected::Expected(&"identifier"))
            .take()
            .verify(|s| !KEYWORDS.contains(s))
            .parse_next(input)
    }

    let (ident, span) = identifier_chars.with_token_span().parse_next(input)?;
    Ok(ast::Ident { span, ident })
}

fn keyword<T: ast::token::Keyword>(input: &mut Input) -> PResult<T> {
    let end_of_keyword = alt((
        any.verify(|c: &char| !c.is_alphanumeric()).value(()),
        eof.value(()),
    ));

    terminated(T::TOKEN, peek(end_of_keyword))
        .context(Expected::ExpectedKeyword(&T::TOKEN))
        .token_span()
        .map(T::with_span)
        .parse_next(input)
}

fn token<const CHAR: char>(input: &mut Input) -> PResult<ast::token::Token<CHAR>> {
    CHAR.token_span()
        .map(ast::token::Token::with_span)
        .parse_next(input)
}

fn escaped_string<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    fn escaped_string_char<'a>(input: &mut Input<'a>) -> PResult<()> {
        alt((none_of(['\\', '\"']).value(()), ('\\', any).value(()))).parse_next(input)
    }

    delimited(
        '"',
        repeat::<_, (), (), _, _>(0.., escaped_string_char).take(),
        '"',
    )
    .context(Expected::Expected(&"string literal"))
    .parse_next(input)
}

fn until_eol_or_eof<'a>(input: &mut Input<'a>) -> PResult<&'a str> {
    match (till_line_ending, line_ending).take().parse_next(input) {
        Ok(comment) => Ok(comment),
        Err(winnow::error::ErrMode::Backtrack(_)) => Ok(input.finish()),
        Err(err) => Err(err),
    }
}

/// Whitespace (whitespace and comments) preceding a statement. The whitespace
/// can be used to separate statements. Does not include semicolons.
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

    let ((has_newlines, has_comments), span) = repeat(0.., ws_part)
        .fold(
            || (false, false),
            |(has_newlines, has_comments), part| match part {
                WsPart::Comment => (has_newlines, true),
                WsPart::Whitespace => (has_newlines, has_comments),
                WsPart::Newline => (true, has_comments),
            },
        )
        .with_token_span()
        .parse_next(input)?;

    Ok(ParsedWhitespace {
        span,
        has_newlines,
        has_comments,
    })
}

fn whitespace_parsed_nonempty(input: &mut Input) -> PResult<ParsedWhitespace> {
    whitespace_parsed
        .verify(|ws| !ws.span.is_empty())
        .context(Expected::Expected(&"whitespace"))
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

pub(crate) trait TokenParserExt<'a, I, O, E>: winnow::Parser<I, O, E> {
    fn with_token_span(self) -> SpannedTokenParser<Self, I, O, E>
    where
        Self: Sized,
        I: winnow::stream::Stream + winnow::stream::Location,
    {
        SpannedTokenParser(self, std::marker::PhantomData)
    }

    fn token_span(self) -> TokenSpanParser<Self, I, O, E>
    where
        Self: Sized,
        I: winnow::stream::Stream + winnow::stream::Location,
    {
        TokenSpanParser(self, std::marker::PhantomData)
    }
}

impl<'a, I, O, E, T> TokenParserExt<'a, I, O, E> for T where T: winnow::Parser<I, O, E> {}

pub(crate) struct SpannedTokenParser<P, I, O, E>(P, PhantomData<(I, O, E)>);
impl<'a, I, P, O, E> winnow::Parser<I, (O, Span), E> for SpannedTokenParser<P, I, O, E>
where
    P: winnow::Parser<I, O, E>,
    I: winnow::stream::Location,
{
    fn parse_next(&mut self, input: &mut I) -> winnow::PResult<(O, Span), E> {
        let start = input.location();
        self.0.parse_next(input).map(|value| {
            let end = input.location();
            (value, span(start..end))
        })
    }
}

pub(crate) struct TokenSpanParser<P, I, O, E>(P, std::marker::PhantomData<(I, O, E)>);
impl<'a, I, P, O, E> winnow::Parser<I, Span, E> for TokenSpanParser<P, I, O, E>
where
    P: winnow::Parser<I, O, E>,
    I: winnow::stream::Location,
{
    fn parse_next(&mut self, input: &mut I) -> winnow::PResult<Span, E> {
        let start = input.location();
        self.0.parse_next(input).map(|_| {
            let end = input.location();
            span(start..end)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Input;
    use crate::{
        ast::{self, token::Keyword as _, ws, ws_ignore},
        parser::{span, token, Offset, ParsedWhitespace},
    };
    use winnow::Parser as _;

    #[test]
    fn which_expr() {
        let input = Input::new("which \"clang\"");
        assert_eq!(
            super::expression_chain.parse(input).unwrap(),
            ast::Expr::Which(ast::WhichExpr {
                span: span(0..13),
                token: ast::token::Which(Offset(0)),
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
        let input =
            Input::new("config out-dir = \"../target\"\n\nlet cc = which \"clang\"\nlet ld = cc\n");
        let root = super::root.parse(input).unwrap();
        assert_eq!(
            root,
            ast::Root {
                statements: vec![
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Config(ast::ConfigStmt {
                            span: span(0..28),
                            token_config: ast::token::Config(Offset(0)),
                            ws_1: ws_ignore(),
                            ident: ast::Ident {
                                span: span(7..14),
                                ident: "out-dir",
                            },
                            ws_2: ws_ignore(),
                            token_eq: ast::token::Token(Offset(15)),
                            ws_3: ws_ignore(),
                            value: ast::ConfigValue::String("../target"),
                        }),
                        ws_trailing: None,
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Let(ast::LetStmt {
                            span: span(30..52),
                            token_let: ast::token::Let(Offset(30)),
                            ws_1: ws_ignore(),
                            ident: ast::Ident {
                                span: span(34..36),
                                ident: "cc",
                            },
                            ws_2: ws_ignore(),
                            token_eq: ast::token::Token(Offset(37)),
                            ws_3: ws_ignore(),
                            value: ast::Expr::Which(ast::WhichExpr {
                                span: span(39..52),
                                token: ast::token::Which(Offset(39)),
                                ws_1: ws_ignore(),
                                param: ast::StringExpr {
                                    span: span(45..52),
                                    fragments: vec![ast::StringFragment::Literal("clang".into())]
                                },
                            }),
                        }),
                        ws_trailing: None
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::RootStmt::Let(ast::LetStmt {
                            span: span(53..64),
                            token_let: ast::token::Let(Offset(53)),
                            ws_1: ws_ignore(),
                            ident: ast::Ident {
                                span: span(57..59),
                                ident: "ld",
                            },
                            ws_2: ws_ignore(),
                            token_eq: ast::token::Token(Offset(60)),
                            ws_3: ws_ignore(),
                            value: ast::Expr::Ident(ast::Ident {
                                span: span(62..64),
                                ident: "cc",
                            }),
                        }),
                        ws_trailing: None,
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
            super::let_stmt.parse(input).unwrap(),
            ast::LetStmt {
                span: span(0..22),
                token_let: ast::token::Let(Offset(0)),
                ws_1: ws(3..4),
                ident: ast::Ident {
                    span: span(4..6),
                    ident: "cc"
                },
                ws_2: ws(6..7),
                token_eq: ast::token::Token(Offset(7)),
                ws_3: ws(8..9),
                value: ast::Expr::Which(ast::WhichExpr {
                    span: span(9..22),
                    token: ast::token::Which(Offset(9)),
                    ws_1: ws(14..15),
                    param: ast::StringExpr {
                        span: span(15..22),
                        fragments: vec![ast::StringFragment::Literal("clang".into())]
                    },
                }),
            }
        );
    }

    #[test]
    fn string_escape() {
        let input = "*.\\{c,cpp\\}";
        let expected = ast::StringExpr {
            span: span(0..input.len()),
            fragments: vec![ast::StringFragment::Literal("*.{c,cpp}".into())],
        };
        let result = super::string_expr_inside_quotes
            .parse(Input::new(input))
            .unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn string_expr() {
        let input = Input::new(r#""hello, world""#);
        assert_eq!(
            super::string_expr.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..14),
                fragments: vec![ast::StringFragment::Literal("hello, world".into())]
            }
        );

        let input = Input::new(r#""hello, \"world\"""#);
        assert_eq!(
            super::string_expr.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..18),
                fragments: vec![ast::StringFragment::Literal(r#"hello, "world""#.into())]
            }
        );

        let input = Input::new(r#""hello, \\\"world\\\"""#);
        assert_eq!(
            super::string_expr.parse(input).unwrap(),
            ast::StringExpr {
                span: span(0..22),
                fragments: vec![ast::StringFragment::Literal(r#"hello, \"world\""#.into())]
            }
        );

        let input = "hello %world% {name} <1:.ext1=.ext2>";
        let expected = ast::StringExpr {
            span: span(0..input.len()),
            fragments: vec![
                ast::StringFragment::Literal("hello %world% ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::Ident("name".into()),
                    options: None,
                }),
                ast::StringFragment::Literal(" ".into()),
                ast::StringFragment::Interpolation(ast::Interpolation {
                    stem: ast::InterpolationStem::CaptureGroup(1),
                    options: Some(Box::new(ast::InterpolationOptions {
                        ops: vec![
                            ast::InterpolationOp::ReplaceExtension(".ext1".into(), ".ext2".into()),
                            ast::InterpolationOp::ResolveOsPath,
                        ],
                        join: None,
                    })),
                }),
            ],
        };

        let result = super::string_expr_inside_quotes
            .parse(Input::new(input))
            .unwrap();
        assert_eq!(result, expected);
    }

    // #[test]
    // fn list_of_identifier() {
    //     let input = Input::new("[hello]");
    //     assert_eq!(
    //         super::list_of::<_, Vec<_>, _>(super::identifier)
    //             .parse(input)
    //             .unwrap(),
    //         vec![ast::Ident {
    //             span: span(1..6),
    //             ident: "hello".to_string()
    //         },]
    //     );

    //     let input = Input::new("[hello, world]");
    //     assert_eq!(
    //         super::list_of::<_, Vec<_>, _>(super::identifier)
    //             .parse(input)
    //             .unwrap(),
    //         vec![
    //             ast::Ident {
    //                 span: span(1..6),
    //                 ident: "hello".to_string()
    //             },
    //             ast::Ident {
    //                 span: span(8..13),
    //                 ident: "world".to_string()
    //             }
    //         ]
    //     );

    //     let input = Input::new("[ hello, world, ]");
    //     assert_eq!(
    //         super::list_of::<_, Vec<_>, _>(super::identifier)
    //             .parse(input)
    //             .unwrap(),
    //         vec![
    //             ast::Ident {
    //                 span: span(2..7),
    //                 ident: "hello".to_string()
    //             },
    //             ast::Ident {
    //                 span: span(9..14),
    //                 ident: "world".to_string()
    //             }
    //         ]
    //     );
    // }

    #[test]
    fn identifier() {
        let input = Input::new("hello");
        assert_eq!(
            super::identifier.parse(input).unwrap(),
            ast::Ident {
                span: span(0..5),
                ident: "hello"
            }
        );

        let input = Input::new("hello-world");
        assert_eq!(
            super::identifier.parse(input).unwrap(),
            ast::Ident {
                span: span(0..11),
                ident: "hello-world"
            }
        );

        let input = Input::new("hello world");
        assert_eq!(
            (super::identifier, " world").parse(input).unwrap(),
            (
                ast::Ident {
                    span: span(0..5),
                    ident: "hello"
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
            super::expression_tail
                .parse(Input::new("filter-match \"a\" => \"b\""))
                .unwrap(),
            ast::Expr::FilterMatch(ast::KwExpr {
                span: span(0..23),
                token: ast::token::FilterMatch::with_span(span(0..12)),
                ws_1: ws_ignore(),
                param: ast::MatchBody::Single(Box::new(ast::MatchArm {
                    span: span(13..23),
                    pattern: ast::PatternExpr {
                        span: span(13..16),
                        fragments: vec![ast::PatternFragment::Literal("a".into())]
                    },
                    ws_1: ws_ignore(),
                    token_fat_arrow: ast::token::FatArrow::with_span(span(17..19)),
                    ws_2: ws_ignore(),
                    expr: ast::Expr::StringExpr(ast::StringExpr {
                        span: span(20..23),
                        fragments: vec![ast::StringFragment::Literal("b".into())]
                    })
                }))
            })
        );
    }
}
