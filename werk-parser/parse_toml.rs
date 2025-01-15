use crate::{
    ast::{self, kw_ignore, token_ignore, ws_ignore},
    parse_string,
    parser::{Span, Spanned as _, SpannedValue},
    Error, LocatedError,
};

pub fn parse_toml<'a>(
    file_name: &'a std::path::Path,
    source: &'a str,
    document: &'a toml_edit::ImDocument<&'a str>,
) -> Result<crate::Document<'a>, LocatedError<'a, Error>> {
    parse_toml_document(document).map_err(|error| error.with_location(file_name, source))
}

#[derive(Default)]
struct SmuggledWhitespace(String);
impl SmuggledWhitespace {
    fn smuggle_decor(&mut self, decor: &str) -> ast::Whitespace {
        if decor.is_empty() {
            ws_ignore()
        } else {
            let start = self.0.len();
            let end = start + decor.len();
            self.0.push_str(decor);
            ast::Whitespace(Span::from(start..end))
        }
    }
}

pub fn parse_toml_document<'a>(
    toml: &'a toml_edit::ImDocument<&'a str>,
) -> Result<crate::Document<'a>, Error> {
    let mut smuggled_whitespace = SmuggledWhitespace::default();

    let root = toml.as_table();

    let mut statements = Vec::new();

    for (key, value) in root {
        let span = value.span().unwrap_or_default().into();

        match key {
            "config" => {
                let config_table = value.as_table().ok_or_else(|| Error::ExpectedTable(span))?;
                parse_config_table(config_table, &mut statements, &mut smuggled_whitespace)?;
            }
            "global" => {
                let global_table = value.as_table().ok_or_else(|| Error::ExpectedTable(span))?;
                for (key, value) in global_table {
                    let key_span = global_table
                        .key(key)
                        .and_then(toml_edit::Key::span)
                        .unwrap_or_default()
                        .into();
                    let ident = parse_ident(key_span, key)?;
                    let (decor, item) = parse_commented_item_expr(value)?;
                    statements.push(ast::BodyStmt {
                        ws_pre: smuggled_whitespace.smuggle_decor(decor),
                        statement: ast::RootStmt::Let(ast::LetStmt {
                            span,
                            token_let: kw_ignore(),
                            ws_1: ws_ignore(),
                            ident,
                            ws_2: ws_ignore(),
                            token_eq: token_ignore(),
                            ws_3: ws_ignore(),
                            value: item,
                        }),
                        ws_trailing: None,
                    });
                }
            }
            "command" => {
                let command = value.as_table().ok_or_else(|| Error::ExpectedTable(span))?;
                for (key, value) in command {
                    let key_span = command
                        .key(key)
                        .and_then(toml_edit::Key::span)
                        .unwrap_or_default()
                        .into();
                    let name = parse_ident(key_span, key)?;
                    let (decor, recipe) = parse_command_recipe(name, value)?;
                    statements.push(ast::BodyStmt {
                        ws_pre: smuggled_whitespace.smuggle_decor(decor),
                        statement: ast::RootStmt::Task(recipe),
                        ws_trailing: None,
                    });
                }
            }
            "build" => {
                let out = value.as_table().ok_or_else(|| Error::ExpectedTable(span))?;
                for (key, value) in out {
                    let key_span = out
                        .key(key)
                        .and_then(toml_edit::Key::span)
                        .unwrap_or_default()
                        .into();
                    let pattern = parse_pattern_expr(key_span, key)?;
                    let (decor, recipe) = parse_build_recipe(pattern, value)?;
                    statements.push(ast::BodyStmt {
                        ws_pre: smuggled_whitespace.smuggle_decor(decor),
                        statement: ast::RootStmt::Build(recipe),
                        ws_trailing: None,
                    });
                }
            }
            _ => {
                let span = root
                    .key(key)
                    .and_then(toml_edit::Key::span)
                    .unwrap_or_default();
                return Err(Error::InvalidKey(span.into()));
            }
        }
    }

    Ok(crate::Document::new(
        ast::Root {
            statements,
            ws_trailing: ws_ignore(),
        },
        toml.raw(),
        Some(smuggled_whitespace.0),
    ))
}

fn parse_config_table<'a>(
    table: &'a toml_edit::Table,
    config: &mut Vec<ast::BodyStmt<ast::RootStmt<'a>>>,
    smuggled_whitespace: &mut SmuggledWhitespace,
) -> Result<(), Error> {
    for (key, item) in table {
        let span = item.span().unwrap_or_default().into();
        let value = match key {
            "out-dir" | "edition" | "default" => {
                let Some(value) = item.as_str() else {
                    return Err(Error::ExpectedString(span));
                };
                ast::ConfigValue::String(ast::ConfigString(span, value.into()))
            }
            "print-commands" => {
                let Some(value) = item.as_bool() else {
                    return Err(Error::ExpectedString(span));
                };
                ast::ConfigValue::Bool(ast::ConfigBool(span, value))
            }
            _ => {
                let span = table
                    .key(key)
                    .and_then(toml_edit::Key::span)
                    .map_or(span, Into::into);
                return Err(Error::UnknownConfigKey(span));
            }
        };

        config.push(ast::BodyStmt {
            ws_pre: smuggled_whitespace.smuggle_decor(get_item_decor(item)),
            statement: ast::RootStmt::Config(ast::ConfigStmt {
                span,
                token_config: kw_ignore(),
                ws_1: ws_ignore(),
                ident: ast::Ident::new(span, key),
                ws_2: ws_ignore(),
                token_eq: token_ignore(),
                ws_3: ws_ignore(),
                value,
            }),
            ws_trailing: None,
        });
    }

    Ok(())
}

fn parse_ident(span: Span, s: &str) -> Result<ast::Ident, Error> {
    parse_string::parse_ident(s)
        .map(|ident| ast::Ident { span, ident })
        .map_err(|e| Error::InvalidIdent(span, e))
}

fn parse_string_expr(span: Span, s: &str) -> Result<ast::StringExpr, Error> {
    let mut expr =
        parse_string::parse_string_expr(s).map_err(|e| Error::InvalidStringExpr(span, e))?;
    expr.span = span;
    Ok(expr)
}

fn parse_pattern_expr(span: Span, s: &str) -> Result<ast::PatternExpr, Error> {
    let mut expr =
        parse_string::parse_pattern_expr(s).map_err(|e| Error::InvalidPatternExpr(span, e))?;
    expr.span = span;
    Ok(expr)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExprType {
    String,
    From,
    Env,
    Shell,
    Which,
    Glob,
    Error,
}

impl ExprType {
    pub fn all_strs() -> &'static [&'static str] {
        &["env", "shell", "which", "glob", "from", "error"]
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "string" => Some(Self::String),
            "env" => Some(Self::Env),
            "shell" => Some(Self::Shell),
            "which" => Some(Self::Which),
            "glob" => Some(Self::Glob),
            "from" => Some(Self::From),
            "error" => Some(Self::Error),
            _ => None,
        }
    }
}

impl std::fmt::Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ExprType::String => "string",
            ExprType::From => "from",
            ExprType::Env => "env",
            ExprType::Shell => "shell",
            ExprType::Which => "which",
            ExprType::Glob => "glob",
            ExprType::Error => "error",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RunExprType {
    Shell,
    Write,
    Copy,
    Echo,
}

impl RunExprType {
    pub fn all_strs() -> &'static [&'static str] {
        &["shell", "write", "copy", "echo"]
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "shell" => Some(Self::Shell),
            "write" => Some(Self::Write),
            "copy" => Some(Self::Copy),
            "echo" => Some(Self::Echo),
            _ => None,
        }
    }
}

impl std::fmt::Display for RunExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            RunExprType::Shell => "shell",
            RunExprType::Write => "write",
            RunExprType::Copy => "copy",
            RunExprType::Echo => "echo",
        })
    }
}

fn find_main_expr_type<T: toml_edit::TableLike + ?Sized>(
    span: Span,
    table: &T,
) -> Result<(SpannedValue<ExprType>, &toml_edit::Item), Error> {
    let mut found = None;
    let mut iter = table.iter();
    for (key, item) in iter.by_ref() {
        if let Some(ty) = ExprType::from_str(key) {
            let key_span = table.key(key).and_then(toml_edit::Key::span);
            found = Some((SpannedValue::new(key_span, ty), item));
            break;
        }
    }

    let Some(found) = found else {
        return Err(Error::ExpectedMainExpression(span));
    };

    for (tail, _) in iter {
        if let Some(duplicate) = ExprType::from_str(tail) {
            let key_span = table.key(tail).and_then(toml_edit::Key::span);
            return Err(Error::AmbiguousMainExpression(
                found.0,
                SpannedValue::new(key_span, duplicate),
            ));
        }
    }

    Ok(found)
}

fn find_main_run_expr_type<T: toml_edit::TableLike + ?Sized>(
    span: Span,
    table: &T,
) -> Result<(SpannedValue<RunExprType>, &toml_edit::Item), Error> {
    let mut found = None;
    let mut iter = table.iter();
    for (key, item) in iter.by_ref() {
        if let Some(ty) = RunExprType::from_str(key) {
            let key_span = table.key(key).and_then(toml_edit::Key::span);
            found = Some((SpannedValue::new(key_span, ty), item));
            break;
        }
    }

    let Some(found) = found else {
        return Err(Error::ExpectedMainExpression(span));
    };

    for (tail, _) in iter {
        if let Some(duplicate) = RunExprType::from_str(tail) {
            let key_span = table.key(tail).and_then(toml_edit::Key::span);
            return Err(Error::AmbiguousRunExpression(
                found.0,
                SpannedValue::new(key_span, duplicate),
            ));
        }
    }

    Ok(found)
}

#[allow(clippy::too_many_lines)]
fn parse_table_expr<T: toml_edit::TableLike + ?Sized>(
    span: Span,
    table: &T,
) -> Result<ast::Expr, Error> {
    let (expr_ty, item) = find_main_expr_type(span, table)?;
    let mut expr = match expr_ty.value {
        ExprType::String => parse_item_string_expr(item).map(ast::Expr::StringExpr)?,
        ExprType::From => {
            if let toml_edit::Item::Value(value @ toml_edit::Value::String(s)) = item {
                let ident = parse_ident(value.span().unwrap_or_default().into(), s.value())?;
                ast::Expr::Ident(ident)
            } else {
                parse_item_expr(item)?
            }
        }
        ExprType::Env => parse_item_string_expr(item).map(|value| {
            ast::Expr::Env(ast::EnvExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: value,
            })
        })?,
        ExprType::Shell => parse_item_string_expr(item).map(|value| {
            ast::Expr::Shell(ast::ShellExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: value,
            })
        })?,
        ExprType::Which => parse_item_string_expr(item).map(|value| {
            ast::Expr::Which(ast::WhichExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: value,
            })
        })?,
        ExprType::Glob => parse_item_string_expr(item).map(|value| {
            ast::Expr::Glob(ast::GlobExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: value,
            })
        })?,
        ExprType::Error => parse_item_string_expr(item).map(|value| {
            ast::Expr::Error(ast::ErrorExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: value,
            })
        })?,
    };

    // Chaining expressions
    for (key, item) in table.iter() {
        // Skip the main expression - note that duplicates and ambiguous
        // expressions have already been detected.
        if let Some(_ty) = ExprType::from_str(key) {
            continue;
        }

        let span = item.span().unwrap_or_default().into();

        let then = match key {
            "then" => ast::ExprOp::Map(ast::MapExpr {
                span: item.span().unwrap_or_default().into(),
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: parse_item_string_expr(item)?,
            }),
            "match" => {
                let Some(table) = item.as_table_like() else {
                    return Err(Error::ExpectedTable(span));
                };

                let mut arms = Vec::with_capacity(table.len());
                for (pattern, value) in table.iter() {
                    let value_span = value.span().unwrap_or_default().into();
                    let pattern = parse_pattern_expr(value_span, pattern)?;
                    let value = parse_item_expr(value)?;
                    arms.push(ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::MatchArm {
                            span,
                            pattern,
                            ws_1: ws_ignore(),
                            token_fat_arrow: kw_ignore(),
                            ws_2: ws_ignore(),
                            expr: value,
                        },
                        ws_trailing: None,
                    });
                }

                ast::ExprOp::Match(ast::MatchExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: ast::MatchBody::Braced(ast::Body {
                        token_open: ast::token::Token(span.start),
                        statements: arms,
                        ws_trailing: ws_ignore(),
                        token_close: ast::token::Token(span.end),
                    }),
                })
            }
            "join" => {
                let separator = parse_item_string_expr(item)?;
                ast::ExprOp::Join(ast::JoinExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: separator,
                })
            }
            "warn" => {
                let message = parse_item_string_expr(item)?;
                ast::ExprOp::Warn(ast::WarnExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: message,
                })
            }
            "info" => {
                let message = parse_item_string_expr(item)?;
                ast::ExprOp::Info(ast::InfoExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: message,
                })
            }
            "error" => {
                let message = parse_item_string_expr(item)?;
                ast::ExprOp::Error(ast::ErrorExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: message,
                })
            }
            "patsubst" => {
                let Some(table) = item.as_table_like() else {
                    return Err(Error::ExpectedTable(span));
                };

                let mut pattern = None;
                let mut replacement = None;

                for (key, item) in table.iter() {
                    match key {
                        "pattern" => pattern = Some(parse_item_pattern_expr(item)?),
                        "replacement" => replacement = Some(parse_item_string_expr(item)?),
                        _ => {
                            return Err(Error::InvalidKey(
                                table.key(key).and_then(toml_edit::Key::span).into(),
                            ))
                        }
                    }
                }

                let Some(pattern) = pattern else {
                    return Err(Error::ExpectedKey(span, &"pattern"));
                };
                let Some(replacement) = replacement else {
                    return Err(Error::ExpectedKey(span, &"replacement"));
                };

                ast::ExprOp::Match(ast::MatchExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: ast::MatchBody::Braced(ast::Body {
                        token_open: ast::token::Token(span.start),
                        statements: vec![ast::BodyStmt {
                            ws_pre: ws_ignore(),
                            statement: ast::MatchArm {
                                span,
                                pattern,
                                ws_1: ws_ignore(),
                                token_fat_arrow: kw_ignore(),
                                ws_2: ws_ignore(),
                                expr: ast::Expr::StringExpr(replacement),
                            },
                            ws_trailing: None,
                        }],
                        ws_trailing: ws_ignore(),
                        token_close: ast::token::Token(span.end),
                    }),
                })
            }
            _ => {
                return Err(Error::UnknownExpressionChain(
                    table.key(key).and_then(toml_edit::Key::span).into(),
                ))
            }
        };

        if let ast::Expr::Chain(ref mut chain_expr) = expr {
            chain_expr.tail.push(ast::ChainSubExpr {
                span,
                ws_1: ws_ignore(),
                token_pipe: token_ignore(),
                ws_2: ws_ignore(),
                expr: then,
            });
        } else {
            expr = ast::Expr::Chain(ast::ChainExpr {
                span,
                head: Box::new(expr),
                tail: vec![ast::ChainSubExpr {
                    span,
                    ws_1: ws_ignore(),
                    token_pipe: token_ignore(),
                    ws_2: ws_ignore(),
                    expr: then,
                }],
            });
        }
    }

    Ok(expr)
}

fn parse_value_expr(toml: &toml_edit::Value) -> Result<ast::Expr, Error> {
    let span = toml.span().unwrap_or_default().into();
    match toml {
        toml_edit::Value::String(formatted) => {
            parse_string_expr(span, formatted.value()).map(ast::Expr::StringExpr)
        }
        toml_edit::Value::Integer(_)
        | toml_edit::Value::Float(_)
        | toml_edit::Value::Boolean(_)
        | toml_edit::Value::Datetime(_) => Err(Error::ExpectedStringOrTable(span)),
        toml_edit::Value::Array(array) => {
            let mut items = Vec::with_capacity(array.len());
            for value in array {
                let item = parse_value_expr(value)?;
                items.push(ast::ListItem {
                    ws_pre: ws_ignore(),
                    item,
                    ws_trailing: None,
                });
            }
            Ok(ast::Expr::List(ast::ListExpr {
                span,
                token_open: token_ignore(),
                items,
                token_close: token_ignore(),
                ws_trailing: ws_ignore(),
            }))
        }
        toml_edit::Value::InlineTable(inline_table) => parse_table_expr(span, inline_table),
    }
}

fn parse_item_expr(toml: &toml_edit::Item) -> Result<ast::Expr, Error> {
    match toml {
        toml_edit::Item::None => Err(Error::ExpectedString(toml.span().into())),
        toml_edit::Item::Value(value) => parse_value_expr(value),
        toml_edit::Item::Table(table) => {
            parse_table_expr(table.span().unwrap_or_default().into(), table)
        }
        toml_edit::Item::ArrayOfTables(array_of_tables) => {
            let mut items = Vec::with_capacity(array_of_tables.len());
            for table in array_of_tables {
                let item = parse_table_expr(table.span().unwrap_or_default().into(), table)?;
                items.push(ast::ListItem {
                    ws_pre: ws_ignore(),
                    item,
                    ws_trailing: None,
                });
            }
            Ok(ast::Expr::List(ast::ListExpr {
                span: toml.span().into(),
                token_open: ast::token::Token::ignore(),
                items,
                token_close: ast::token::Token::ignore(),
                ws_trailing: ws_ignore(),
            }))
        }
    }
}

fn get_item_decor(toml: &toml_edit::Item) -> &str {
    let prefix = match toml {
        toml_edit::Item::None | toml_edit::Item::ArrayOfTables(_) => {
            return "";
        }
        toml_edit::Item::Value(value) => value.decor().prefix(),
        toml_edit::Item::Table(table) => table.decor().prefix(),
    };

    prefix.and_then(|decor| decor.as_str()).unwrap_or_default()
}

fn parse_commented_item_expr(toml: &toml_edit::Item) -> Result<(&str, ast::Expr<'_>), Error> {
    let pre = get_item_decor(toml);
    let expr = parse_item_expr(toml)?;
    Ok((pre, expr))
}

fn parse_item_run_expr(toml: &toml_edit::Item) -> Result<Vec<ast::RunExpr<'_>>, Error> {
    let mut vec = Vec::new();
    parse_item_run_exprs_into(toml, &mut vec)?;
    Ok(vec)
}

fn parse_item_run_exprs_into<'a>(
    toml: &'a toml_edit::Item,
    exprs: &mut Vec<ast::RunExpr<'a>>,
) -> Result<(), Error> {
    match toml {
        toml_edit::Item::None => Ok(()),
        toml_edit::Item::Value(value) => parse_value_run_exprs_into(value, exprs),
        toml_edit::Item::Table(table) => {
            exprs.push(parse_table_run_expr(
                table.span().unwrap_or_default().into(),
                table,
            )?);
            Ok(())
        }
        toml_edit::Item::ArrayOfTables(array_of_tables) => {
            for table in array_of_tables {
                let run_expr =
                    parse_table_run_expr(table.span().unwrap_or_default().into(), table)?;
                exprs.push(run_expr);
            }
            Ok(())
        }
    }
}

fn parse_value_run_exprs_into<'a>(
    value: &'a toml_edit::Value,
    exprs: &mut Vec<ast::RunExpr<'a>>,
) -> Result<(), Error> {
    let span = value.span().unwrap_or_default().into();

    match value {
        toml_edit::Value::String(formatted) => {
            let string = parse_string_expr(span, formatted.value())?;
            exprs.push(ast::RunExpr::Shell(ast::ShellExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: string,
            }));
            Ok(())
        }
        toml_edit::Value::Integer(_)
        | toml_edit::Value::Float(_)
        | toml_edit::Value::Boolean(_)
        | toml_edit::Value::Datetime(_) => Err(Error::ExpectedStringOrArray(span)),
        toml_edit::Value::Array(array) => {
            for element in array {
                parse_value_run_exprs_into(element, exprs)?;
            }
            Ok(())
        }
        toml_edit::Value::InlineTable(table) => {
            exprs.push(parse_table_run_expr(span, table)?);
            Ok(())
        }
    }
}

fn parse_table_run_expr<T: toml_edit::TableLike>(
    span: Span,
    table: &T,
) -> Result<ast::RunExpr<'_>, Error> {
    let (run_expr_ty, item) = find_main_run_expr_type(span, table)?;
    match run_expr_ty.value {
        RunExprType::Shell => {
            // TODO: Validate that there are no other keys.
            parse_item_string_expr(item).map(|command| {
                ast::RunExpr::Shell(ast::ShellExpr {
                    span,
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: command,
                })
            })
        }
        RunExprType::Write => {
            let target = parse_item_string_expr(item)?;
            if let Some(data) = table.get("data") {
                let data = parse_item_expr(data)?;
                Ok(ast::RunExpr::Write(ast::WriteExpr {
                    span,
                    token_write: ast::token::Keyword::ignore(),
                    ws_1: ws_ignore(),
                    path: ast::Expr::StringExpr(target),
                    ws_2: ws_ignore(),
                    token_to: kw_ignore(),
                    ws_3: ws_ignore(),
                    value: data,
                }))
            } else {
                Err(Error::ExpectedKey(span, &"data"))
            }
        }
        RunExprType::Copy => {
            let source = parse_item_string_expr(item)?;
            if let Some(destination) = table.get("to") {
                let to = parse_item_string_expr(destination)?;
                Ok(ast::RunExpr::Copy(ast::CopyExpr {
                    span,
                    token_copy: ast::token::Keyword::ignore(),
                    ws_1: ws_ignore(),
                    src: source,
                    ws_2: ws_ignore(),
                    token_to: kw_ignore(),
                    ws_3: ws_ignore(),
                    dest: to,
                }))
            } else {
                Err(Error::ExpectedKey(span, &"to"))
            }
        }
        RunExprType::Echo => {
            let message = parse_item_string_expr(item)?;
            Ok(ast::RunExpr::Info(ast::InfoExpr {
                span,
                token: ast::token::Keyword::ignore(),
                ws_1: ws_ignore(),
                param: message,
            }))
        }
    }
}

fn parse_item_string_expr(toml: &toml_edit::Item) -> Result<ast::StringExpr<'_>, Error> {
    let span = toml.span().into();
    match toml {
        toml_edit::Item::Value(toml_edit::Value::String(s)) => parse_string_expr(span, s.value()),
        _ => Err(Error::ExpectedString(span)),
    }
}

fn parse_item_pattern_expr(toml: &toml_edit::Item) -> Result<ast::PatternExpr<'_>, Error> {
    let span = toml.span().unwrap_or_default().into();
    match toml {
        toml_edit::Item::Value(toml_edit::Value::String(s)) => parse_pattern_expr(span, s.value()),
        _ => Err(Error::ExpectedString(span)),
    }
}

#[allow(clippy::too_many_lines)]
fn parse_command_recipe<'a>(
    name: ast::Ident<'a>,
    toml: &'a toml_edit::Item,
) -> Result<(&'a str, ast::CommandRecipe<'a>), Error> {
    let span = toml.span().unwrap_or_default().into();

    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(span));
    };

    let decor_pre = get_item_decor(toml);
    let mut build = None;
    let mut command = Vec::new();
    let mut pre_message = None;
    let mut post_message = None;
    let mut capture = None;

    for (key, value) in table.iter() {
        let span = value.span().unwrap_or_default().into();
        match key {
            "build" => {
                build = Some(parse_item_expr(value)?);
            }
            "command" => {
                command = parse_item_run_expr(value)?;
            }
            "pre-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(span));
                };
                pre_message = Some(parse_string_expr(span, value)?);
            }
            "post-message" => {
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(span));
                };
                post_message = Some(parse_string_expr(span, value)?);
            }
            "capture" => {
                let Some(value) = value.as_bool() else {
                    return Err(Error::ExpectedBool(span));
                };
                capture = Some(value);
            }
            _ => {
                let key_span = table.key(key).and_then(toml_edit::Key::span);
                return Err(Error::InvalidKey(key_span.into()));
            }
        }
    }

    let mut stmts = Vec::new();
    if let Some(build) = build {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::TaskRecipeStmt::Build(ast::BuildStmt {
                span: build.span(),
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: build,
            }),
            ws_trailing: None,
        });
    }

    if let Some(pre) = pre_message {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::TaskRecipeStmt::Info(ast::InfoExpr {
                span: pre.span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: pre,
            }),
            ws_trailing: None,
        });
    }

    if let Some(capture) = capture {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::TaskRecipeStmt::SetCapture(ast::KwExpr {
                span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: ast::ConfigBool(Span::ignore(), capture),
            }),
            ws_trailing: None,
        });
    }

    stmts.extend(command.into_iter().map(|value| ast::BodyStmt {
        ws_pre: ws_ignore(),
        statement: ast::TaskRecipeStmt::Run(ast::RunStmt {
            span: value.span(),
            token: kw_ignore(),
            ws_1: ws_ignore(),
            param: value,
        }),
        ws_trailing: None,
    }));

    if let Some(post) = post_message {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::TaskRecipeStmt::Info(ast::InfoExpr {
                span: post.span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: post,
            }),
            ws_trailing: None,
        });
    }

    Ok((
        decor_pre,
        ast::CommandRecipe {
            span,
            token_task: kw_ignore(),
            ws_1: ws_ignore(),
            name,
            ws_2: ws_ignore(),
            body: ast::Body {
                token_open: ast::token::Token(span.start),
                statements: stmts,
                ws_trailing: ws_ignore(),
                token_close: ast::token::Token(span.end),
            },
        },
    ))
}

#[allow(clippy::too_many_lines)]
fn parse_build_recipe<'a>(
    pattern: ast::PatternExpr<'a>,
    toml: &'a toml_edit::Item,
) -> Result<(&'a str, ast::BuildRecipe<'a>), Error> {
    let Some(table) = toml.as_table_like() else {
        return Err(Error::ExpectedTable(
            toml.span().map(Into::into).unwrap_or_default(),
        ));
    };

    let span = toml.span().unwrap_or_default().into();

    let decor_pre = get_item_decor(toml);
    let mut in_files = None;
    let mut depfile = None;
    let mut command = Vec::new();
    let mut pre_message = None;
    let mut post_message = None;

    for (key, value) in table.iter() {
        match key {
            "in" => {
                in_files = Some(parse_item_expr(value)?);
            }
            "depfile" | "depfiles" => {
                depfile = Some(parse_item_expr(value)?);
            }
            "pre-message" => {
                let span = value.span().unwrap_or_default().into();
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(span));
                };
                pre_message = Some(parse_string_expr(span, value)?);
            }
            "post-message" => {
                let span = value.span().unwrap_or_default().into();
                let Some(value) = value.as_str() else {
                    return Err(Error::ExpectedString(span));
                };
                post_message = Some(parse_string_expr(span, value)?);
            }
            "command" => {
                command = parse_item_run_expr(value)?;
            }
            _ => {
                let key_span = table.key(key).and_then(toml_edit::Key::span).into();
                return Err(Error::InvalidKey(key_span));
            }
        }
    }

    let mut stmts = Vec::new();

    if let Some(from) = in_files {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::BuildRecipeStmt::From(ast::FromStmt {
                span: from.span(),
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: from,
            }),
            ws_trailing: None,
        });
    }

    if let Some(depfile) = depfile {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::BuildRecipeStmt::Depfile(ast::DepfileStmt {
                span: depfile.span(),
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: depfile,
            }),
            ws_trailing: None,
        });
    }

    if let Some(pre) = pre_message {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::BuildRecipeStmt::Info(ast::InfoExpr {
                span: pre.span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: pre,
            }),
            ws_trailing: None,
        });
    }

    stmts.extend(command.into_iter().map(|value| ast::BodyStmt {
        ws_pre: ws_ignore(),
        statement: ast::BuildRecipeStmt::Run(ast::RunStmt {
            span: value.span(),
            token: kw_ignore(),
            ws_1: ws_ignore(),
            param: value,
        }),
        ws_trailing: None,
    }));

    if let Some(post) = post_message {
        stmts.push(ast::BodyStmt {
            ws_pre: ws_ignore(),
            statement: ast::BuildRecipeStmt::Info(ast::InfoExpr {
                span: post.span,
                token: kw_ignore(),
                ws_1: ws_ignore(),
                param: post,
            }),
            ws_trailing: None,
        });
    }

    Ok((
        decor_pre,
        ast::BuildRecipe {
            span,
            token_build: kw_ignore(),
            ws_1: ws_ignore(),
            pattern,
            ws_2: ws_ignore(),
            body: ast::Body {
                token_open: ast::token::Token(span.start),
                statements: stmts,
                ws_trailing: ws_ignore(),
                token_close: ast::token::Token(span.end),
            },
        },
    ))
}
