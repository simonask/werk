use macro_rules_attribute::apply;
use tests::mock_io;

use mock_io::*;
use stringleton::Symbol;
use werk_fs::{Absolute, Path};
use werk_runner::{BuildStatus, Outdatedness, Reason, ShellCommandLine, TaskId};

static WERK: &str = r#"
config profile = env "PROFILE"
config cc = which "clang"
config write = which "write"

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
config profile = env "PROFILE"
config cc = which "clang"
config write = which "write"

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

fn anyhow_msg<E: ToString>(err: E) -> anyhow::Error {
    anyhow::Error::msg(err.to_string())
}

#[apply(smol_macros::test)]
async fn test_outdated_env() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("env-dep")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::missing(Absolute::try_from("/env-dep")?)
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_read_env("PROFILE"));
    assert!(test.did_which("write"));
    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec![
            "debug".into(),
            test.output_path(["env-dep"]).display().to_string()
        ],
    }));
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_write_output_file(&[".werk-cache"]));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path([".werk-cache"])
    ));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path(["env-dep"])
    ));

    // Change the environment!
    test.io.set_env("PROFILE", "release");
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("env-dep")?)
        .await
        .map_err(anyhow_msg)?;
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::new([Reason::Env(Symbol::from("PROFILE")),])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_which() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("which-dep")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::missing(Absolute::try_from("/which-dep")?)
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_which("clang"));
    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_write_output_file(&[".werk-cache"]));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path([".werk-cache"])
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
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("which-dep")?)
        .await
        .map_err(anyhow_msg)?;
    std::mem::drop(runner);

    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("path/to/clang"),
        arguments: vec![],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([
                Reason::missing(Absolute::try_from("/which-dep")?),
                Reason::Which(Symbol::from("clang"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_recipe_changed() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("which-dep")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([Reason::missing(Absolute::try_from("/which-dep")?),])
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_which("clang"));
    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
    }));
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_write_output_file(&[".werk-cache"]));

    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path([".werk-cache"]),
    ));

    // Change the environment!
    test.io.clear_oplog();

    // Initialize a new workspace.
    test.reload(WERK_RECIPE_CHANGED, &[]).map_err(anyhow_msg)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("which-dep")?)
        .await
        .map_err(anyhow_msg)?;
    std::mem::drop(runner);

    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![
            String::from("-o"),
            test.output_path(["which-dep"]).display().to_string()
        ],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/which-dep").unwrap(),
            Outdatedness::new([
                Reason::missing(Absolute::try_from("/which-dep")?),
                Reason::RecipeChanged
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_glob() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    test.set_workspace_file(&["a.c"], "void foo() {}").unwrap();
    test.set_workspace_file(&["b.c"], "int main() { return 0; }\n")
        .unwrap();
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("glob-dep")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/glob-dep").unwrap(),
            Outdatedness::new([Reason::missing(Absolute::try_from("/glob-dep")?),])
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_which("clang"));
    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![
            test.workspace_path_str(["a.c"]),
            test.workspace_path_str(["b.c"])
        ],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_write_output_file(&[".werk-cache"]));

    assert!(test.io.contains_file(test.output_path([".werk-cache"])));

    // Change the environment!
    test.io.delete_file(test.workspace_path(["b.c"])).unwrap();
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("glob-dep")?)
        .await
        .map_err(anyhow_msg)?;
    std::mem::drop(runner);

    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![test.workspace_path_str(["a.c"])],
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/glob-dep").unwrap(),
            Outdatedness::new([
                Reason::missing(Absolute::try_from("/glob-dep")?),
                Reason::Glob(Symbol::from("/*.c"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_define() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let mut test = Test::new(WERK)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);

    let status = runner
        .build_file(Path::new("env-dep")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::try_build("/env-dep").unwrap(),
            Outdatedness::new([Reason::missing(Absolute::try_from("/env-dep")?),])
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.did_read_env("PROFILE"));
    assert!(test.did_which("write"));
    assert!(test.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec![
            "debug".into(),
            test.output_path(["env-dep"]).display().to_string()
        ],
    }));

    assert!(test.did_write_output_file(&[".werk-cache"]));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path(&[".werk-cache"])
    ));
    assert!(contains_file(
        &test.io.filesystem.lock(),
        &test.output_path(&["env-dep"])
    ));

    // Override the `profile` variable manually.
    test.io.clear_oplog();

    // Initialize a new workspace with an overridden `profile` variable.
    let workspace = test
        .reload(WERK, &[("profile", "release")])
        .map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);
    let status = runner
        .build_file(Path::new("env-dep")?)
        .await
        .map_err(anyhow_msg)?;
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/env-dep").unwrap()),
            Outdatedness::new([Reason::Define(Symbol::from("profile")),])
        )
    );
    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    std::mem::drop(runner);

    // Because the variable was overridden, the expression should not be evaluated.
    assert!(!test.did_read_env("PROFILE"));

    // Initialize a new workspace with the same overridden `profile` variable,
    // which should then not trigger a rebuild.
    let workspace = test
        .reload(WERK, &[("profile", "release")])
        .map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);
    let status = runner
        .build_file(Path::new("env-dep")?)
        .await
        .map_err(anyhow_msg)?;
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
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);
    let status = runner
        .build_file(Path::new("output")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/output").unwrap()),
            Outdatedness::new([Reason::Missing(Absolute::symbolicate(Absolute::try_from(
                "/output"
            )?)),])
        )
    );
    workspace.finalize().await?;
    std::mem::drop(runner);

    test.reload(WERK_GLOBAL_CHANGED, &[]).map_err(anyhow_msg)?;
    let workspace = test.create_workspace().map_err(anyhow_msg)?;
    let runner = werk_runner::Runner::new(workspace);
    let status = runner
        .build_file(Path::new("output")?)
        .await
        .map_err(anyhow_msg)?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(Absolute::try_from("/output").unwrap()),
            Outdatedness::new([
                Reason::GlobalChanged(Symbol::from("arg")),
                Reason::GlobalChanged(Symbol::from("args"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_relaxed_depfile_outdatedness_issue_47() {
    _ = tracing_subscriber::fmt::try_init();

    static WERK_DEPFILE: &str = r#"
build "binary" {
    depfile "depfile.d"
    let contents = "foo"
    run "write {contents} <out>"
}"#;

    let mut test = Test::new(WERK_DEPFILE).unwrap();

    let depfile = format!(
        "{}: {}",
        test.output_path(["binary"]).display(),
        // File in the output directory should be considered as a dependency,
        // but the fact that it does not exist in the workspace should not force
        // the target to become outdated.
        test.output_path(["nonexistent_source.rs"]).display()
    );
    // The binary exists and is up to date.
    test.set_output_file(&["binary"], "foo").unwrap();
    // The depfile exists from a previous run.
    test.set_output_file(&["depfile.d"], &depfile).unwrap();

    let workspace = test.create_workspace().unwrap();
    let runner = werk_runner::Runner::new(workspace);
    let status = runner
        .build_file(werk_fs::Path::new("binary").unwrap())
        .await
        .unwrap();
    match status {
        BuildStatus::Complete(_task_id, outdatedness) => {
            assert!(
                outdatedness.is_unchanged(),
                "outdatedness = {:?}",
                outdatedness
            );
        }
        BuildStatus::Exists(..) | BuildStatus::Ignore(..) => {
            panic!("should have been built with a build rule")
        }
    }
}

#[apply(smol_macros::test)]
async fn test_touch() {
    _ = tracing_subscriber::fmt::try_init();

    static WERK_DEPFILE: &str = r#"
build "binary" {
    run { touch "<out>" }
}"#;

    let mut test = Test::new(WERK_DEPFILE).unwrap();
    let mtime = test.io.tick();
    let workspace = test.create_workspace().unwrap();
    let runner = werk_runner::Runner::new(workspace);
    runner
        .build_file(werk_fs::Path::new("binary").unwrap())
        .await
        .unwrap();
    std::mem::drop(runner);
    test.did_touch(test.output_path(["binary"]), mtime);
}
