use macro_rules_attribute::apply;
use stringleton::sym;
use tests::{mock_io::*, plan_build_and_get_status};
use werk_eval::{TaskName, Value};
use werk_fs::Absolute;
use werk_planner::{Planner, PlannerError};
use werk_util::Annotated;

stringleton::enable!(tests);

#[test]
fn test_path_resolution() {
    static WERK: &str = r#"
let exists = "foo";
let exists-not = "bar";

let exists-resolved = "<exists>"
let exists-not-resolved = "<exists-not>"
"#;

    _ = tracing_subscriber::fmt::try_init();

    let mut test = match Test::new(WERK) {
        Ok(test) => test,
        Err(err) => {
            eprintln!("{err}");
            panic!("parse error")
        }
    };
    test.set_workspace_file(&["foo"], "foo").unwrap();

    let foo_workspace = test.workspace_path_str(["foo"]);
    let bar_output = test.workspace_path_str(["bar"]);

    let workspace = match test.create_workspace() {
        Ok(workspace) => workspace,
        Err(err) => {
            eprintln!("{err}");
            panic!("could not create workspace")
        }
    };
    let globals = &workspace.manifest.global_variables;
    assert_eq!(
        globals.get(&sym!("exists-resolved")).unwrap().value,
        Value::from(foo_workspace.clone())
    );

    assert_eq!(
        globals.get(&sym!("exists-not-resolved")).unwrap().value,
        Value::from(bar_output.clone())
    );
}

/// When a build recipe depends on a file with the same name, that is not
/// representable, and should cause a circular dependency error.
#[test]
fn test_circular_dependency() {
    static WERK: &str = r#"
build "explicit" {
    from "explicit"
    run {
        # Cannot get here.
        copy "{in}" to "{out}"
    }
}
    "#;
    let mut test = Test::new(WERK).unwrap();
    let workspace = test.create_workspace().unwrap();
    let mut planner = Planner::new(&workspace.manifest);
    planner.add_goal_by_name("explicit").unwrap();
    match planner.plan(workspace).map_err(Annotated::into_inner) {
        Ok(_) => panic!("expected circular dependency error"),
        Err(PlannerError::CircularDependency(err)) => {
            let chain = err.chain;
            assert_eq!(
                chain,
                [String::from("/explicit"), String::from("/explicit")]
            );
        }
        Err(err) => panic!("unexpected error: {:?}", err),
    }
}

#[apply(smol_macros::test)]
async fn test_empty_out_dir() {
    static WERK: &str = r#"
build "bar" {
    info "<out>"
}
    "#;

    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK).unwrap();
    let expected_message = test.workspace_path_str(["bar"]);
    let workspace = test.create_workspace().unwrap();
    plan_build_and_get_status(workspace, "bar").await.unwrap();
    test.render.assert_did_see(&MockRenderEvent::Message(
        Some(TaskName::build(Absolute::try_from("/bar").unwrap())),
        expected_message,
    ));
}
