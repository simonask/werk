use macro_rules_attribute::apply;
use tests::mock_io;

use mock_io::*;
use werk_fs::{Absolute, Path};
use werk_runner::{BuildStatus, Outdatedness, Reason, ShellCommandLine, TaskId};

static WERK: &str = r#"
let profile = env "PROFILE"
let cc = which "clang"
let write = which "write"

build "env-dep" {
    run "{write} {profile} <out>"
}

build "which-dep" {
    run "{cc}"
}

build "glob-dep" {
    from glob "*.c"
    run "{cc} <in*>"
}
"#;

static WERK_RECIPE_CHANGED: &str = r#"
let profile = env "PROFILE"
let cc = which "clang"
let write = which "write"

build "env-dep" {
    run "{write} {profile} <out>"
}

build "which-dep" {
    run "{cc} -o <out>"
}

build "glob-dep" {
    from glob "*.c"
    run "{cc} <in*>"
}
"#;

static WERK_GLOBAL: &str = r#"
let arg = "a"

build "output" {
    run {
        write arg to "{out}"
    }
}
"#;

static WERK_GLOBAL_CHANGED: &str = r#"
let args = ["b"]
let arg = "{args*}"

build "output" {
    run {
        write arg to "{out}"
    }
}
"#;

#[apply(smol_macros::test)]
async fn test_outdated_env() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let test = Test::new(WERK)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("env-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/env-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_read_env("PROFILE"));
    assert!(test.io.did_which("write"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec!["debug".into(), output_file("env-dep").display().to_string()],
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file("env-dep")
    ));

    // Change the environment!
    test.io.set_env("PROFILE", "release");
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("env-dep")?).await?;
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::new([Reason::Env(String::from("PROFILE")),])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_which() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let test = Test::new(WERK)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/which-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));

    // Change the environment!
    test.io
        .set_program("clang", program_path("path/to/clang"), |_cmd, _fs, _env| {
            Ok(std::process::Output {
                status: Default::default(),
                stdout: Default::default(),
                stderr: Default::default(),
            })
        });
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("path/to/clang"),
        arguments: vec![],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([
                Reason::Missing(Absolute::try_from("/which-dep")?),
                Reason::Which(String::from("clang"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_recipe_changed() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/which-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file(".werk-cache"),
    ));

    // Change the environment!
    test.io.clear_oplog();
    std::mem::drop(runner);

    // Initialize a new workspace.
    test.reload(WERK_RECIPE_CHANGED)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![
            String::from("-o"),
            output_file("which-dep").display().to_string()
        ],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([
                Reason::Missing(Absolute::try_from("/which-dep")?),
                Reason::RecipeChanged
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_glob() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let test = Test::new(WERK)?;
    test.io.set_workspace_file("a.c", "void foo() {}").unwrap();
    test.io
        .set_workspace_file("b.c", "int main() { return 0; }\n")
        .unwrap();
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/glob-dep").unwrap(),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/glob-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![workspace_file_str("a.c"), workspace_file_str("b.c")],
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(test.io.contains_file(output_file(".werk-cache")));

    // Change the environment!
    test.io.delete_file(workspace_file("b.c")).unwrap();
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![workspace_file_str("a.c")],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/glob-dep").unwrap(),
            Outdatedness::new([
                Reason::Missing(Absolute::try_from("/glob-dep")?),
                Reason::Glob(String::from("/*.c"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_define() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let test = Test::new(WERK)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("env-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/env-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_read_env("PROFILE"));
    assert!(test.io.did_which("write"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec!["debug".into(), output_file("env-dep").display().to_string()],
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    assert!(test.io.did_write_file(".werk-cache"));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &output_file("env-dep")
    ));

    // Override the `profile` variable manually.
    test.io.clear_oplog();

    // Initialize a new workspace with an overridden `profile` variable.
    let workspace = test.create_workspace(&[("profile", "release")])?;
    let runner = werk_runner::Runner::new(&workspace);
    let status = runner.build_file(Path::new("env-dep")?).await?;
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/env-dep").unwrap()),
            Outdatedness::new([Reason::Define(String::from("profile")),])
        )
    );
    // Because the variable was overridden, the expression should not be evaluated.
    assert!(!test.io.did_read_env("PROFILE"));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();

    // Initialize a new workspace with the same overridden `profile` variable,
    // which should then not trigger a rebuild.
    let workspace = test.create_workspace(&[("profile", "release")])?;
    let runner = werk_runner::Runner::new(&workspace);
    let status = runner.build_file(Path::new("env-dep")?).await?;
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/env-dep").unwrap()),
            Outdatedness::unchanged()
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_global_constant() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK_GLOBAL)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);
    let status = runner.build_file(Path::new("output")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/output").unwrap()),
            Outdatedness::new([Reason::Missing(Absolute::try_from("/output")?),])
        )
    );
    workspace.finalize().await?;
    std::mem::drop(runner);

    test.reload(WERK_GLOBAL_CHANGED)?;
    let workspace = test.create_workspace(&[])?;
    let runner = werk_runner::Runner::new(&workspace);
    let status = runner.build_file(Path::new("output")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/output").unwrap()),
            Outdatedness::new([
                Reason::GlobalChanged(String::from("arg")),
                Reason::GlobalChanged(String::from("args"))
            ])
        )
    );

    Ok(())
}
