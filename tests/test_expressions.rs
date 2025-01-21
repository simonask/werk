use std::sync::Arc;

use werk_parser::{
    ast::{self, kw_ignore, token_ignore, ws_ignore},
    parse_string::{parse_pattern_expr, parse_string_expr},
    parser::Span,
    TomlParseError,
};
use werk_runner::{Value, WhichError};

use tests::mock_io::*;

static EXPRESSIONS_TOML: &str = include_str!("../examples/expressions.toml");

fn parse_pattern_expr_ignore_span(s: &str) -> Result<ast::PatternExpr, TomlParseError> {
    let mut expr = parse_pattern_expr(s)?;
    expr.span = Span::ignore();
    Ok(expr)
}

fn parse_string_expr_ignore_span(s: &str) -> Result<ast::StringExpr, TomlParseError> {
    let mut expr = parse_string_expr(s)?;
    expr.span = Span::ignore();
    Ok(expr)
}

#[test]
#[allow(clippy::too_many_lines)]
fn parse_as_expected() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let ast = werk_parser::parse_toml("input".as_ref(), EXPRESSIONS_TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    assert_eq!(ast.num_task_recipes(), 0);
    assert_eq!(ast.num_build_recipes(), 0);
    assert_eq!(
        ast.find_config("out-dir").unwrap().value,
        ast::ConfigValue::String(ast::ConfigString(
            Span::ignore(),
            "../target/werk-examples/expressions".into()
        ))
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
        ast::Expr::Chain(ast::ChainExpr {
            span: Span::ignore(),
            head: Box::new(ast::Expr::Ident(ast::Ident::new(
                Span::ignore(),
                "source-files"
            ))),
            tail: vec![ast::ChainSubExpr {
                span: Span::ignore(),
                ws_1: ws_ignore(),
                token_pipe: token_ignore(),
                ws_2: ws_ignore(),
                expr: ast::ExprOp::Match(ast::MatchExpr {
                    span: Span::ignore(),
                    token: kw_ignore(),
                    ws_1: ws_ignore(),
                    param: ast::MatchBody::Braced(ast::Body {
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
                    }),
                })
            }]
        })
    );
    assert_eq!(
        ast.find_global("cargo-profile").unwrap().value,
        ast::Expr::Chain(ast::ChainExpr {
            span: Span::ignore(),
            head: Box::new(ast::Expr::Ident(ast::Ident::new(Span::ignore(), "profile"))),
            tail: vec![
                ast::ChainSubExpr {
                    span: Span::ignore(),
                    ws_1: ws_ignore(),
                    token_pipe: token_ignore(),
                    ws_2: ws_ignore(),
                    expr: ast::ExprOp::Match(ast::MatchExpr {
                        span: Span::ignore(),
                        token: kw_ignore(),
                        ws_1: ws_ignore(),
                        param: ast::MatchBody::Braced(ast::Body {
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
                        })
                    })
                }
            ]
        })
    );
    Ok(())
}

#[test]
fn expressions() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let test = Test::new_toml(&toml)?;
    test.io
        .set_program("clang", program_path("clang"), |_cmdline, _fs| {
            Ok(std::process::Output {
                status: std::process::ExitStatus::default(),
                stdout: Vec::from(b"clang output"),
                stderr: vec![],
            })
        });
    test.io
        .set_workspace_file("main.c", b"int main() { return 0; }\n")
        .unwrap();
    test.io.set_env("PROFILE", "debug");
    let workspace = test.create_workspace(&[])?;

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

#[test]
fn fail_which() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let test = Test::new_toml(&toml)?;
    test.io.remove_program("clang");
    let workspace = test.create_workspace(&[]);
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

#[test]
fn fail_custom_err() -> anyhow::Result<()> {
    let toml = toml_edit::ImDocument::parse(EXPRESSIONS_TOML)?;
    let test = Test::new_toml(&toml)?;
    test.io
        .set_program("clang", program_path("clang"), |_cmdline, _fs| {
            Ok(std::process::Output {
                status: std::process::ExitStatus::default(),
                stdout: Vec::from(b"clang output"),
                stderr: vec![],
            })
        });
    test.io.set_env("PROFILE", "nonexistent profile");
    let workspace = test.create_workspace(&[]);
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

fn evaluate_global(source: &str, global_variable_name_to_check: &str) -> Value {
    let path = std::path::Path::new("test input");
    let ast = werk_parser::parse_werk(source)
        .map_err(|err| anyhow::Error::msg(err.with_location(path, source).to_string()))
        .unwrap();
    let render = Arc::new(MockRender::default());
    let io = Arc::new(MockIo::default().with_default_workspace_dir());
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*render,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(&[]),
    )
    .unwrap();
    workspace
        .manifest
        .globals
        .get(global_variable_name_to_check)
        .ok_or_else(|| anyhow::anyhow!("global variable not found"))
        .unwrap()
        .value
        .value
        .clone()
}

#[test]
fn local_var() {
    assert_eq!(evaluate_global("let a = \"a\"; let b = a;", "b"), "a");
}

#[test]
fn join() {
    assert_eq!(
        evaluate_global(
            r#"let a = ["a", "b"]; let joined = a | join "\n""#,
            "joined"
        ),
        "a\nb"
    );

    assert_eq!(
        evaluate_global(
            r#"let a = ["a", ["b", ["c"]]]; let joined = a | join "\n""#,
            "joined"
        ),
        "a\nb\nc"
    );
}

#[test]
fn match_expr_empty() {
    // Empty match is a no-op.
    assert_eq!(
        evaluate_global(
            r#"
    let input = "a";
    let result = input | match { }
"#,
            "result"
        ),
        "a"
    );

    assert_eq!(
        evaluate_global(
            r"
    let input = [];
    let result = input | match { }
",
            "result"
        ),
        Value::List(vec![])
    );
}

#[test]
fn match_expr_recursive() {
    // Empty match is a no-op.
    assert_eq!(
        evaluate_global(
            r#"
    let input = ["a", ["b"]];
    let result = input
        | match { "a" => "foo"; "b" => "bar" }
        | assert-eq ["foo", ["bar"]]
"#,
            "result"
        ),
        Value::List(vec![
            Value::from("foo"),
            Value::List(vec![Value::from("bar")])
        ])
    );
}

#[test]
fn map_recursive() {
    // Map a recursive list.
    assert_eq!(
        evaluate_global(
            r#"
    let input = ["a", ["b"]];
    let result = input
        | map "hello {}"
        | assert-eq ["hello a", ["hello b"]]
"#,
            "result"
        ),
        Value::List(vec![
            Value::from("hello a"),
            Value::List(vec![Value::from("hello b")])
        ])
    );

    // Map a single string
    assert_eq!(
        evaluate_global(
            r#"
    let input = "a";
    let result = input
        | map "hello {}"
        | assert-eq "hello a"
"#,
            "result"
        ),
        Value::String(String::from("hello a"))
    );
}
