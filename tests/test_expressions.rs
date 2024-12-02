use std::sync::Arc;

use werk_parser::{ast, parse_string::parse_pattern_expr, parse_string::parse_string_expr};
use werk_runner::{Metadata, WhichError};

#[path = "test_io.rs"]
mod test_io;
use test_io::*;

static EXPRESSIONS_TOML: &str = include_str!("../examples/expressions.toml");

#[test]
fn parse_as_expected() -> anyhow::Result<()> {
    let ast = werk_parser::parse_toml(&EXPRESSIONS_TOML)?;
    assert!(ast.commands.is_empty());
    assert!(ast.recipes.is_empty());
    assert_eq!(
        ast.config.output_directory.as_deref(),
        Some("../target/werk-examples/expressions")
    );

    assert_eq!(
        ast.global["source-files"],
        ast::Expr::Glob(ast::StringExpr::literal("*.c"))
    );
    assert_eq!(
        ast.global["profile"],
        ast::Expr::Env(ast::StringExpr::literal("PROFILE"))
    );
    assert_eq!(
        ast.global["cc"],
        ast::Expr::Which(ast::StringExpr::literal("clang"))
    );
    assert_eq!(
        ast.global["object-files"],
        ast::Expr::Patsubst(Box::new(ast::PatsubstExpr {
            input: ast::Expr::Ident(String::from("source-files")),
            pattern: parse_pattern_expr("%.c").unwrap(),
            replacement: parse_string_expr("{%}.o").unwrap(),
        }))
    );
    assert_eq!(
        ast.global["cargo-profile"],
        ast::Expr::Match(Box::new(ast::MatchExpr {
            input: ast::Expr::Ident(String::from("profile")),
            patterns: [
                (
                    parse_pattern_expr("debug").unwrap(),
                    ast::Expr::literal("dev")
                ),
                (
                    parse_pattern_expr("release").unwrap(),
                    ast::Expr::literal("release")
                ),
                (
                    parse_pattern_expr("%").unwrap(),
                    ast::Expr::Error(
                        parse_string_expr(
                            "invalid profile '{profile}'; expected \"debug\" or \"release\""
                        )
                        .unwrap()
                    )
                ),
            ]
            .into_iter()
            .collect()
        }))
    );
    Ok(())
}

#[tokio::test]
async fn expressions() -> anyhow::Result<()> {
    let ast = werk_parser::parse_toml(&EXPRESSIONS_TOML)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_program("clang", "/clang", |_cmdline| {
                Ok(std::process::Output {
                    status: std::process::ExitStatus::default(),
                    stdout: Vec::from(b"clang output"),
                    stderr: vec![],
                })
            })
            .with_filesystem([(
                "/main.c",
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
    let workspace =
        werk_runner::Workspace::new(&*io, "/".into(), "target".into(), Default::default()).await?;
    let runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher).await?;

    let globals = runner.globals();
    assert_eq!(globals["source-files"].value, ["/main.c"]);
    assert_eq!(globals["profile"].value, "debug");
    assert_eq!(globals["cc"].value, "/clang");
    assert_eq!(globals["object-files"].value, ["/main.o"]);
    assert_eq!(globals["cargo-profile"].value, "dev");

    Ok(())
}

#[tokio::test]
async fn fail_which() -> anyhow::Result<()> {
    let ast = werk_parser::parse_toml(&EXPRESSIONS_TOML)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_filesystem([(
                "/main.c",
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
    let workspace =
        werk_runner::Workspace::new(&*io, "/".into(), "target".into(), Default::default()).await?;
    let Err(err) = werk_runner::Runner::new(ast, io.clone(), workspace, watcher).await else {
        panic!("expected error")
    };

    assert_eq!(
        err,
        // Note: CommandNotFound is issued separately when failing as part of
        // eval or as part of building a recipe.
        werk_runner::Error::Eval(werk_runner::EvalError::CommandNotFound(
            String::from("clang"),
            WhichError::CannotFindBinaryPath
        ))
    );

    Ok(())
}

#[tokio::test]
async fn fail_custom_err() -> anyhow::Result<()> {
    let ast = werk_parser::parse_toml(&EXPRESSIONS_TOML)?;
    let watcher = Arc::new(MockWatcher::default());
    let io = Arc::new(
        MockIo::default()
            .with_program("clang", "/clang", |_cmdline| {
                Ok(std::process::Output {
                    status: std::process::ExitStatus::default(),
                    stdout: Vec::from(b"clang output"),
                    stderr: vec![],
                })
            })
            .with_filesystem([(
                "/main.c",
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
    let workspace =
        werk_runner::Workspace::new(&*io, "/".into(), "target".into(), Default::default()).await?;
    let Err(err) = werk_runner::Runner::new(ast, io.clone(), workspace, watcher).await else {
        panic!("expected error")
    };

    assert_eq!(
        err,
        werk_runner::Error::Eval(werk_runner::EvalError::ErrorExpression(
            "invalid profile 'nonexistent profile'; expected \"debug\" or \"release\"".into()
        ))
    );

    Ok(())
}
