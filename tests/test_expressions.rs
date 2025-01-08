use std::sync::Arc;

use macro_rules_attribute::apply;
use werk_parser::{
    ast::{self, kw_ignore, token_ignore, ws_ignore},
    parse_string::{parse_pattern_expr, parse_string_expr},
    parser::Span,
    ParseError,
};
use werk_runner::{Metadata, Value, WhichError};

use tests::mock_io::*;

static EXPRESSIONS_TOML: &str = include_str!("../examples/expressions.toml");

fn parse_pattern_expr_ignore_span(s: &str) -> Result<ast::PatternExpr, ParseError> {
    let mut expr = parse_pattern_expr(s)?;
    expr.span = Span::ignore();
    Ok(expr)
}

fn parse_string_expr_ignore_span(s: &str) -> Result<ast::StringExpr, ParseError> {
    let mut expr = parse_string_expr(s)?;
    expr.span = Span::ignore();
    Ok(expr)
}

#[test]
fn parse_as_expected() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let ast = werk_parser::parse_toml("input".as_ref(), &EXPRESSIONS_TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    assert_eq!(ast.num_task_recipes(), 0);
    assert_eq!(ast.num_build_recipes(), 0);
    assert_eq!(
        ast.find_config("out-dir").unwrap().value,
        ast::ConfigValue::String("../target/werk-examples/expressions")
    );

    assert_eq!(
        ast.find_global("source-files").unwrap().value,
        ast::Expr::Glob(ast::GlobExpr {
            span: Span::ignore(),
            token: kw_ignore(),
            ws_1: ws_ignore(),
            param: ast::StringExpr::literal(Span::ignore(), "*.{c,cpp}")
        })
    );
    assert_eq!(
        ast.find_global("profile").unwrap().value,
        ast::Expr::Env(ast::EnvExpr {
            span: Span::ignore(),
            token: kw_ignore(),
            ws_1: ws_ignore(),
            param: ast::StringExpr::literal(Span::ignore(), "PROFILE")
        })
    );
    assert_eq!(
        ast.find_global("cc").unwrap().value,
        ast::Expr::Which(ast::WhichExpr {
            span: Span::ignore(),
            token: kw_ignore(),
            ws_1: ws_ignore(),
            param: ast::StringExpr::literal(Span::ignore(), "clang")
        })
    );
    assert_eq!(
        ast.find_global("object-files").unwrap().value,
        ast::Expr::Then(Box::new(ast::ThenExpr {
            span: Span::ignore(),
            expr: ast::Expr::Ident(ast::Ident::new(400..414, "source-files")),
            ws_1: ws_ignore(),
            token_fat_arrow: kw_ignore(),
            ws_2: ws_ignore(),
            then: ast::Expr::Match(ast::MatchExpr {
                span: Span::ignore(),
                token_match: kw_ignore(),
                ws_1: ws_ignore(),
                body: ast::Body {
                    token_open: token_ignore(),
                    statements: vec![ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::MatchArm {
                            span: Span::ignore(),
                            pattern: parse_pattern_expr_ignore_span("%.c").unwrap(),
                            ws_1: ws_ignore(),
                            token_fat_arrow: kw_ignore(),
                            ws_2: ws_ignore(),
                            expr: ast::Expr::StringExpr(
                                parse_string_expr_ignore_span("{%}.o").unwrap()
                            )
                        },
                        ws_trailing: None
                    }],
                    ws_trailing: ws_ignore(),
                    token_close: token_ignore()
                },
            })
        }))
    );
    assert_eq!(
        ast.find_global("cargo-profile").unwrap().value,
        ast::Expr::Then(Box::new(ast::ThenExpr {
            span: Span::ignore(),
            expr: ast::Expr::Ident(ast::Ident::new(Span::ignore(), "profile")),
            ws_1: ws_ignore(),
            token_fat_arrow: kw_ignore(),
            ws_2: ws_ignore(),
            then: ast::Expr::Match(ast::MatchExpr {
                span: Span::ignore(),
                token_match: kw_ignore(),
                ws_1: ws_ignore(),
                body: ast::Body {
                    token_open: token_ignore(),
                    statements: vec![ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::MatchArm {
                            span: Span::ignore(),
                            pattern: parse_pattern_expr_ignore_span("debug").unwrap(),
                            ws_1: ws_ignore(),
                            token_fat_arrow: kw_ignore(),
                            ws_2: ws_ignore(),
                            expr: ast::Expr::literal(Span::ignore(), "dev")
                        },
                        ws_trailing: None
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::MatchArm {
                            span: Span::ignore(),
                            pattern: parse_pattern_expr_ignore_span("release").unwrap(),
                            ws_1: ws_ignore(),
                            token_fat_arrow: kw_ignore(),
                            ws_2: ws_ignore(),
                            expr: ast::Expr::literal(Span::ignore(), "release")
                        },
                        ws_trailing: None
                    },
                    ast::BodyStmt {
                        ws_pre: ws_ignore(),
                        statement: ast::MatchArm {
                            span: Span::ignore(),
                            pattern: parse_pattern_expr_ignore_span("%").unwrap(),
                            ws_1: ws_ignore(),
                            token_fat_arrow: kw_ignore(),
                            ws_2: ws_ignore(),
                            expr: ast::Expr::Error(ast::ErrorExpr {
                                span: Span::ignore(),
                                token: kw_ignore(),
                                ws_1: ws_ignore(),
                                param: parse_string_expr_ignore_span(
                                    "invalid profile '{profile}'; expected \"debug\" or \"release\""
                                )
                                .unwrap()
                            }
                            )
                        },
                        ws_trailing: None
                    }
                    ],
                    ws_trailing: ws_ignore(),
                    token_close: token_ignore()
                }
            })
        }))
    );
    Ok(())
}

#[apply(smol_macros::test)]
async fn expressions() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let ast = werk_parser::parse_toml("input".as_ref(), &EXPRESSIONS_TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_program("clang", program_path("clang"), |_cmdline, _fs| {
                Ok(std::process::Output {
                    status: std::process::ExitStatus::default(),
                    stdout: Vec::from(b"clang output"),
                    stderr: vec![],
                })
            })
            .with_workspace_files([(
                "main.c",
                (
                    Metadata {
                        mtime: std::time::SystemTime::now(),
                        is_file: true,
                        is_symlink: false,
                    },
                    Vec::from(b"int main() { return 0; }\n"),
                ),
            )])
            .with_envs([("PROFILE", "debug")]),
    );
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;

    let globals = &workspace.manifest.globals;
    assert_eq!(globals["source-files"].value.value, ["/main.c"]);
    assert_eq!(globals["profile"].value.value, "debug");
    assert_eq!(
        globals["cc"].value.value,
        Value::String(program_path("clang").display().to_string())
    );
    assert_eq!(globals["object-files"].value.value, ["/main.o"]);
    assert_eq!(globals["cargo-profile"].value.value, "dev");

    Ok(())
}

#[apply(smol_macros::test)]
async fn fail_which() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let ast = werk_parser::parse_toml("input".as_ref(), &EXPRESSIONS_TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_workspace_files([(
                "main.c",
                (
                    Metadata {
                        mtime: std::time::SystemTime::now(),
                        is_file: true,
                        is_symlink: false,
                    },
                    Vec::from(b"int main() { return 0; }\n"),
                ),
            )])
            .with_envs([("PROFILE", "debug")]),
    );
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await;
    let Err(err) = workspace else {
        panic!("expected error")
    };

    assert_eq!(
        err,
        // Note: CommandNotFound is issued separately when failing as part of
        // eval or as part of building a recipe.
        werk_runner::Error::Eval(werk_runner::EvalError::CommandNotFound(
            Span::ignore(),
            String::from("clang"),
            WhichError::CannotFindBinaryPath
        ))
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn fail_custom_err() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let ast = werk_parser::parse_toml("input".as_ref(), &EXPRESSIONS_TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_program("clang", program_path("clang"), |_cmdline, _fs| {
                Ok(std::process::Output {
                    status: std::process::ExitStatus::default(),
                    stdout: Vec::from(b"clang output"),
                    stderr: vec![],
                })
            })
            .with_workspace_files([(
                "main.c",
                (
                    Metadata {
                        mtime: std::time::SystemTime::now(),
                        is_file: true,
                        is_symlink: false,
                    },
                    Vec::from(b"int main() { return 0; }\n"),
                ),
            )])
            .with_envs([("PROFILE", "nonexistent profile")]),
    );
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await;
    let Err(err) = workspace else {
        panic!("expected error")
    };

    assert_eq!(
        err,
        werk_runner::Error::Eval(werk_runner::EvalError::ErrorExpression(
            Span::ignore(),
            "invalid profile 'nonexistent profile'; expected \"debug\" or \"release\"".into()
        ))
    );

    Ok(())
}
