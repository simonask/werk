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

fn make_io_context() -> Arc<MockIo> {
    Arc::new(
        MockIo::default()
            .with_default_workspace_dir()
            .with_envs([("PROFILE", "debug")])
            .with_program("clang", program_path("clang"), |_cmd, _fs| {
                Ok(std::process::Output {
                    status: Default::default(),
                    stdout: Default::default(),
                    stderr: Default::default(),
                })
            })
            .with_program("write", program_path("write"), |cmdline, fs| {
                let contents = cmdline.arguments[0].as_str();
                let file = output_file(cmdline.arguments[1].as_str());
                tracing::trace!("write {}", file.display());
                insert_fs(
                    fs,
                    &file,
                    (
                        Metadata {
                            mtime: make_mtime(1),
                            is_file: true,
                            is_symlink: false,
                        },
                        contents.as_bytes().into(),
                    ),
                )
                .unwrap();
                Ok(std::process::Output {
                    status: Default::default(),
                    stdout: Default::default(),
                    stderr: Default::default(),
                })
            }),
    )
}

#[apply(smol_macros::test)]
async fn test_outdated_env() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let io = make_io_context();
    let watcher = Arc::new(MockWatcher::default());
    let toml = toml_edit::ImDocument::parse(TOML)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
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
    assert!(io.did_read_env("PROFILE"));
    assert!(io.did_which("write"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: program_path("write"),
        arguments: vec!["debug".into(), output_file("env-dep").display().to_string()],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*io.filesystem.lock(),
        &output_file(".werk-cache")
    ));
    assert!(contains_file(
        &*io.filesystem.lock(),
        &output_file("env-dep")
    ));

    // Change the environment!
    io.set_env("PROFILE", "release");
    io.clear_oplog();

    // Initialize a new workspace.
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
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

    let io = make_io_context();
    let watcher = Arc::new(MockWatcher::default());
    let toml = toml_edit::ImDocument::parse(TOML)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
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
    assert!(io.did_which("clang"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*io.filesystem.lock(),
        &output_file(".werk-cache")
    ));

    // Change the environment!
    io.set_program("clang", program_path("path/to/clang"), |_cmd, _fs| {
        Ok(std::process::Output {
            status: Default::default(),
            stdout: Default::default(),
            stderr: Default::default(),
        })
    });
    io.clear_oplog();

    // Initialize a new workspace.
    let toml = toml_edit::ImDocument::parse(TOML)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(io.did_run_during_build(&ShellCommandLine {
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

    let io = make_io_context();
    let watcher = Arc::new(MockWatcher::default());
    let toml = toml_edit::ImDocument::parse(TOML)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
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
    assert!(io.did_which("clang"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*io.filesystem.lock(),
        &output_file(".werk-cache"),
    ));

    // Change the environment!
    io.clear_oplog();

    // Initialize a new workspace.
    let toml = toml_edit::ImDocument::parse(TOML_RECIPE_CHANGED)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML_RECIPE_CHANGED, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(io.did_run_during_build(&ShellCommandLine {
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

    let io = make_io_context();
    io.set_workspace_file("a.c", "void foo() {}").unwrap();
    io.set_workspace_file("b.c", "int main() { return 0; }\n")
        .unwrap();

    let watcher = Arc::new(MockWatcher::default());
    let toml = toml_edit::ImDocument::parse(TOML)?;
    let ast = werk_parser::parse_toml("werk.toml".as_ref(), TOML, &toml)
        .map_err(|err| err.to_string())
        .map_err(anyhow::Error::msg)?;
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
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
    assert!(io.did_which("clang"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: program_path("clang"),
        arguments: vec![workspace_file_str("a.c"), workspace_file_str("b.c")],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    workspace.finalize().await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(".werk-cache"));

    assert!(contains_file(
        &*io.filesystem.lock(),
        &output_file(".werk-cache")
    ));

    // Change the environment!
    io.delete_file(workspace_file("b.c")).unwrap();
    io.clear_oplog();

    // Initialize a new workspace.
    let workspace = werk_runner::Workspace::new(
        &ast,
        &*io,
        &*watcher,
        test_workspace_dir().to_path_buf(),
        &test_workspace_settings(),
    )
    .await?;
    let runner = werk_runner::Runner::new(&workspace);

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert!(io.did_run_during_build(&ShellCommandLine {
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
