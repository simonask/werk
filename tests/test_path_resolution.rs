use macro_rules_attribute::apply;
use tests::mock_io::*;
use werk_fs::Absolute;
use werk_runner::{Runner, TaskId, Value};
use werk_util::Symbol;

#[test]
fn test_path_resolution() {
    static WERK: &str = r#"
let exists = "foo";
let exists-not = "bar";

let exists-resolved = "<exists>"
let exists-explicit-out-dir = "<exists:out-dir>"
let exists-explicit-workspace = "<exists:workspace>"

let exists-not-resolved = "<exists-not>"
let exists-not-explicit-out-dir = "<exists-not:out-dir>"
let exists-not-explicit-workspace = "<exists-not:workspace>"
"#;

    _ = tracing_subscriber::fmt::try_init();

    let test = match Test::new(WERK) {
        Ok(test) => test,
        Err(err) => {
            eprintln!("{}", err.with_location(std::path::Path::new("input"), WERK));
            panic!("parse error")
        }
    };
    test.set_workspace_file(&["foo"], "foo").unwrap();
    let workspace = match test.create_workspace(&[]) {
        Ok(workspace) => workspace,
        Err(err) => {
            eprintln!(
                "{}",
                werk_parser::LocatedError {
                    file_name: std::path::Path::new("input"),
                    source_code: WERK,
                    error: err
                }
            );
            panic!("could not create workspace")
        }
    };
    let globals = &workspace.manifest.globals;
    assert_eq!(
        globals
            .get(&Symbol::new("exists-resolved"))
            .unwrap()
            .value
            .value,
        Value::String(test.workspace_path_str(["foo"]))
    );
    assert_eq!(
        globals
            .get(&Symbol::new("exists-explicit-out-dir"))
            .unwrap()
            .value
            .value,
        Value::String(test.output_path_str(["foo"]))
    );
    assert_eq!(
        globals
            .get(&Symbol::new("exists-explicit-workspace"))
            .unwrap()
            .value
            .value,
        Value::String(test.workspace_path_str(["foo"]))
    );

    assert_eq!(
        globals
            .get(&Symbol::new("exists-not-resolved"))
            .unwrap()
            .value
            .value,
        Value::String(test.output_path_str(["bar"]))
    );
    assert_eq!(
        globals
            .get(&Symbol::new("exists-not-explicit-out-dir"))
            .unwrap()
            .value
            .value,
        Value::String(test.output_path_str(["bar"]))
    );
    assert_eq!(
        globals
            .get(&Symbol::new("exists-not-explicit-workspace"))
            .unwrap()
            .value
            .value,
        Value::String(test.workspace_path_str(["bar"]))
    );
}

/// When a build recipe depends on a file with the same name, that is not
/// representable, and should cause a circular dependency error.
#[apply(smol_macros::test)]
async fn test_circular_dependency() {
    static WERK: &str = r#"
build "explicit" {
    from "explicit"
    run {
        # Cannot get here.
        copy "<in>" to "<out>"
    }
}
    "#;
    let test = Test::new(WERK).unwrap();
    let workspace = test.create_workspace(&[]).unwrap();
    let runner = Runner::new(&workspace);
    match runner.build_or_run("explicit").await {
        Ok(_) => panic!("expected circular dependency error"),
        Err(werk_runner::Error::CircularDependency(chain)) => {
            let id = TaskId::try_build("/explicit").unwrap();
            let chain = chain.into_inner();
            assert_eq!(chain, [id, id]);
        }
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

/// When a directory in the workspace has the same name as the target of a build
/// recipe, werk should not report a circular dependency
#[apply(smol_macros::test)]
async fn test_directory_name_collision() {
    static WERK: &str = r#"
# Directories should not participate in the lookup that happens as part
# of build recipe matching.
build "bar" {
    info "<out>"
}
build "foo" {
    info "<out>"
}

task build {
    build ["foo", "bar"]
}
"#;

    _ = tracing_subscriber::fmt::try_init();

    let test = Test::new(WERK).unwrap();
    test.set_workspace_dir(&["bar"]).unwrap();
    assert!(test.io.contains_dir(test.workspace_path(["bar"])));
    let workspace = test.create_workspace(&[]).unwrap();
    let runner = Runner::new(&workspace);
    match runner.build_or_run("build").await {
        Ok(_) => panic!("expected error"),
        Err(werk_runner::Error::Eval(werk_runner::EvalError::AmbiguousPathResolution(_, path))) => {
            assert_eq!(path, Absolute::try_from("/bar").unwrap());
        }
        Err(err) => panic!("unexpected error: {err}"),
    }
}
