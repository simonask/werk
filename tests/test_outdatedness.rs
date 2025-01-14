use macro_rules_attribute::apply;
use tests::mock_io;

use std::sync::Arc;

use mock_io::*;
use werk_fs::{Absolute, Path, PathBuf};
use werk_runner::{BuildStatus, Metadata, Outdatedness, Reason, ShellCommandLine, TaskId};

static TOML: &str = r#"
[global]
profile.env = "PROFILE"
cc.which = "clang"
write.which = "write"

[build.'env-dep']
command = "{write} {profile} <out>"

[build.'which-dep']
command = "{cc}"

[build.'glob-dep']
in.glob = "*.c"
command = "{cc} <in*>"
"#;

static TOML_RECIPE_CHANGED: &str = r#"
[global]
profile.env = "PROFILE"
cc.which = "clang"
write.which = "write"

[build.'env-dep']
command = "{write} {profile} <out>"

[build.'which-dep']
command = "{cc} -o <out>"

[build.'glob-dep']
in.glob = "*.c"
command = "{cc} <in*>"
"#;

#[apply(smol_macros::test)]
async fn test_outdated_env() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let toml = toml_edit::ImDocument::parse(TOML)?;
    let test = Test::new_toml(&toml)?;
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("env-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/env-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([Reason::Missing(PathBuf::try_from("/env-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_read_env("PROFILE"));
    assert!(test.io.did_which("write"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec!["debug".into(), output_file("env-dep").display().to_string()],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));
    assert!(contains_file(
        &*test.io.filesystem.lock(),
        &output_file("env-dep")
    ));

    // Change the environment!
    test.io.set_env("PROFILE", "release");
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("env-dep")?).await?;
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/env-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([Reason::Env(String::from("PROFILE")),])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_which() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let toml = toml_edit::ImDocument::parse(TOML)?;
    let test = Test::new_toml(&toml)?;
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/which-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([Reason::Missing(PathBuf::try_from("/which-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));

    // Change the environment!
    test.io
        .set_program("clang", program_path("path/to/clang"), |_cmd, _fs| {
            Ok(std::process::Output {
                status: Default::default(),
                stdout: Default::default(),
                stderr: Default::default(),
            })
        });
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("path/to/clang"),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/which-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/which-dep")?),
                Reason::Which(String::from("clang"))
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_recipe_changed() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let toml = toml_edit::ImDocument::parse(TOML)?;
    let mut test = Test::new_toml(&toml)?;
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/which-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([Reason::Missing(PathBuf::try_from("/which-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*test.io.filesystem.lock(),
        &output_file(".werk-cache"),
    ));

    // Change the environment!
    test.io.clear_oplog();
    std::mem::drop(runner);

    // Initialize a new workspace.
    let toml = toml_edit::ImDocument::parse(TOML_RECIPE_CHANGED)?;
    test.reload_toml(&toml)?;
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![
            String::from("-o"),
            output_file("which-dep").display().to_string()
        ],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/which-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/which-dep")?),
                Reason::RecipeChanged
            ])
        )
    );

    Ok(())
}

#[apply(smol_macros::test)]
async fn test_outdated_glob() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let toml = toml_edit::ImDocument::parse(TOML)?;
    let test = Test::new_toml(&toml)?;
    test.io.set_workspace_file("a.c", "void foo() {}").unwrap();
    test.io
        .set_workspace_file("b.c", "int main() { return 0; }\n")
        .unwrap();
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/glob-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([Reason::Missing(PathBuf::try_from("/glob-dep")?),])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_which("clang"));
    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![workspace_file_str("a.c"), workspace_file_str("b.c")],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(test.io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*test.io.filesystem.lock(),
        &output_file(".werk-cache")
    ));

    // Change the environment!
    test.io.delete_file(workspace_file("b.c")).unwrap();
    test.io.clear_oplog();

    // Initialize a new workspace.
    let workspace = test.create_workspace()?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert!(test.io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![workspace_file_str("a.c")],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::build(
                Absolute::new_unchecked(PathBuf::try_from("/glob-dep").unwrap()).into_boxed_path()
            ),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/glob-dep")?),
                Reason::Glob(String::from("/*.c"))
            ])
        )
    );

    Ok(())
}
