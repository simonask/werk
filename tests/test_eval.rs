use tests::mock_io::*;
use werk_parser::parser::{parse, Input};
use werk_runner::{eval, RootScope, ShellCommandLine};
use winnow::Parser as _;

#[test]
fn command_argument_splitting() {
    let test = Test::new(r#"let foo = "a"; let abc = ["a", "b", "c"]"#).unwrap();
    test.io
        .set_program("a", program_path("a"), |_, _, _| Ok(empty_program_output()));
    let workspace = test.create_workspace(&[]).unwrap();

    // Simple literal command.
    let expr = parse.parse(Input::new(r#""a""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![],
        }
    );

    // Simple interpolation.
    let expr = parse.parse(Input::new(r#""{foo}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![],
        }
    );

    // One literal argument
    let expr = parse.parse(Input::new(r#""a b""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("b")],
        }
    );

    // Expand list without expansion as the first entry.
    let expr = parse.parse(Input::new(r#""a {abc}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a")],
        }
    );

    // Expand list with expansion as separate arguments.
    let expr = parse.parse(Input::new(r#""a {abc*}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a"), String::from("b"), String::from("c")],
        }
    );

    // ... unless there is a join separator
    let expr = parse.parse(Input::new(r#""a {abc,*}""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a,b,c")],
        }
    );

    // ... or the argument is quoted.
    let expr = parse.parse(Input::new(r#""a \"{abc*}\"""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("a b c")],
        }
    );

    // Support single-quoting, for things like `sh -c 'foo bar'`.
    let expr = parse.parse(Input::new(r#""a -c 'a b'""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("-c"), String::from("a b")],
        }
    );

    // Argument expansion within single quotes
    let expr = parse.parse(Input::new(r#""a -c '{abc*}'""#)).unwrap();
    let cmd = eval::eval_shell_command(&RootScope::new(&workspace), &expr).unwrap();
    assert_eq!(
        cmd.value,
        ShellCommandLine {
            program: program_path("a"),
            arguments: vec![String::from("-c"), String::from("a b c")],
        }
    );
}
