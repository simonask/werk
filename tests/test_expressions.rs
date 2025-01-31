use std::sync::Arc;

use werk_runner::Value;

use tests::mock_io::*;
use werk_util::Symbol;

fn evaluate_global(source: &str, global_variable_name_to_check: &str) -> Value {
    let path = std::path::Path::new("test input");
    let ast = werk_parser::parse_werk(source)
        .map_err(|err| anyhow::Error::msg(err.with_location(path, source).to_string()))
        .unwrap();
    let render = Arc::new(MockRender::default());
    let io = Arc::new(MockIo::default());
    io.with_default_workspace_dir();
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
        .get(&Symbol::new(global_variable_name_to_check))
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
