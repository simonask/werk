use tests::mock_io::*;
use werk_parser::parser::{parse, Input};
use werk_runner::{eval, Error, EvalError, RootScope, ShellCommandLine};
use werk_util::DiagnosticFileId;
use winnow::Parser as _;

#[test]
fn command_argument_splitting() {
    let mut test = Test::new(r#"let foo = "a"; let abc = ["a", "b", "c"]; let q = "\"""#).unwrap();
    test.io
        .set_program("a", program_path("a"), |_, _, _| Ok(empty_program_output()));
    let workspace = test.create_workspace().unwrap();
    let file = DiagnosticFileId(0);
    let vars = workspace.variables_per_file.get(&file).unwrap();

    // Simple literal command.
    let expr = parse.parse(Input::new(r#""a""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![],
        }
    );

    // Simple interpolation.
    let expr = parse.parse(Input::new(r#""{foo}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![],
        }
    );

    // One literal argument
    let expr = parse.parse(Input::new(r#""a b""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("b")],
        }
    );

    // Expand list without expansion as the first entry.
    let expr = parse.parse(Input::new(r#""a {abc}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a")],
        }
    );

    // Expand list with expansion as separate arguments.
    let expr = parse.parse(Input::new(r#""a {abc*}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a"), String::from("b"), String::from("c")],
        }
    );

    // ... unless there is a join separator
    let expr = parse.parse(Input::new(r#""a {abc,*}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a,b,c")],
        }
    );

    // ... or the argument is quoted.
    let expr = parse.parse(Input::new(r#""a \"{abc*}\"""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a b c")],
        }
    );

    // Support single-quoting, for things like `sh -c 'foo bar'`.
    let expr = parse.parse(Input::new(r#""a -c 'a b'""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("-c"), String::from("a b")],
        }
    );

    // Argument expansion within single quotes
    let expr = parse.parse(Input::new(r#""a -c '{abc*}'""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("-c"), String::from("a b c")],
        }
    );

    // Quotes in interpolated variables do not terminate a quoted argument.
    let expr = parse.parse(Input::new(r#""a \"{q}\"""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(workspace, vars), &expr, file).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("\"")],
        }
    );
}

#[test]
fn pathiness_double_resolve() {
    static WERK: &str = r#"
let foo = "foo"
let bar = "<foo>"
let baz = "<bar>"
    "#;
    let mut test = Test::new(WERK).unwrap();
    match test.create_workspace().map_err(|err| err.error) {
        Ok(_) => panic!("expected error"),
        Err(Error::Eval(EvalError::DoubleResolvePath(_))) => {}
        Err(err) => panic!("unexpected error: {err}"),
    }
}
