use tests::mock_io;

use std::sync::Arc;

use mock_io::*;
use werk_fs::{Path, PathBuf};
use werk_runner::{
    globset::GlobSet, BuildStatus, Metadata, Outdatedness, Reason, ShellCommandLine, TaskId,
    WorkspaceSettings,
};

static TOML: &str = r#"
[global]
profile.env = "PROFILE"
cc.which = "clang"
write.which = "write"

[build.'env-dep']
command = "{write} {profile} {out}"

[build.'which-dep']
command = "{cc}"

[build.'glob-dep']
in.glob = "*.c"
command = "{cc} <in*>"
"#;

fn make_io_context() -> Arc<MockIo> {
    Arc::new(
        MockIo::default()
            .with_envs([("PROFILE", "debug")])
            .with_program("clang", "/clang", |_cmd, _fs| {
                Ok(std::process::Output {
                    status: Default::default(),
                    stdout: Default::default(),
                    stderr: Default::default(),
                })
            })
            .with_program("write", "/write", |cmdline, fs| {
                let contents = cmdline.arguments[0].as_str();
                let file = cmdline.arguments[1].as_str();
                insert_fs(
                    fs,
                    &std::path::PathBuf::from(format!("/target{}", file)),
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

#[tokio::test]
async fn test_outdated_env() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let io = make_io_context();
    let watcher = Arc::new(MockWatcher::default());
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("env-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/env-dep").unwrap()),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/env-dep")?),
                Reason::Which(String::from("write"))
            ])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_read_env("PROFILE"));
    assert!(io.did_which("write"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: "/write".into(),
        arguments: vec!["debug".into(), "/env-dep".into()],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    runner.workspace().finalize(&*io).await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(&std::path::Path::new("target").join(".werk-cache")));

    assert!(contains_file(
        &*io.filesystem.lock(),
        std::path::Path::new("/target/.werk-cache")
    ));
    assert!(contains_file(
        &*io.filesystem.lock(),
        std::path::Path::new("/target/env-dep")
    ));

    // Change the environment!
    io.set_env("PROFILE", "release");
    io.clear_oplog();

    // Initialize a new workspace.
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("env-dep")?).await?;
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/env-dep").unwrap()),
            Outdatedness::new([Reason::Env(String::from("PROFILE")),])
        )
    );

    Ok(())
}

#[tokio::test]
async fn test_outdated_which() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let io = make_io_context();
    let watcher = Arc::new(MockWatcher::default());
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/which-dep").unwrap()),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/which-dep")?),
                Reason::Which(String::from("clang"))
            ])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_which("clang"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: "/clang".into(),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    runner.workspace().finalize(&*io).await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(&std::path::Path::new("target").join(".werk-cache")));

    assert!(contains_file(
        &*io.filesystem.lock(),
        std::path::Path::new("/target/.werk-cache")
    ));

    // Change the environment!
    io.set_program("clang", "/path/to/clang", |_cmd, _fs| {
        Ok(std::process::Output {
            status: Default::default(),
            stdout: Default::default(),
            stderr: Default::default(),
        })
    });
    io.clear_oplog();

    // Initialize a new workspace.
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("which-dep")?).await?;

    assert!(io.did_run_during_build(&ShellCommandLine {
        program: "/path/to/clang".into(),
        arguments: vec![],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/which-dep").unwrap()),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/which-dep")?),
                Reason::Which(String::from("clang"))
            ])
        )
    );

    Ok(())
}

#[tokio::test]
async fn test_outdated_glob() -> anyhow::Result<()> {
    _ = tracing_subscriber::fmt::try_init();

    let io = make_io_context();
    io.set_file("a.c", "void foo() {}").unwrap();
    io.set_file("b.c", "int main() { return 0; }\n").unwrap();

    let watcher = Arc::new(MockWatcher::default());
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/glob-dep").unwrap()),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/glob-dep")?),
                Reason::Glob(String::from("/*.c")),
                Reason::Which(String::from("clang")),
            ])
        )
    );
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_which("clang"));
    assert!(io.did_run_during_build(&ShellCommandLine {
        program: "/clang".into(),
        arguments: vec![String::from("/a.c"), String::from("/b.c")],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // Write .werk-cache.
    runner.workspace().finalize(&*io).await.unwrap();
    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert!(io.did_write_file(&std::path::Path::new("target").join(".werk-cache")));

    assert!(contains_file(
        &*io.filesystem.lock(),
        std::path::Path::new("/target/.werk-cache")
    ));

    // Change the environment!
    io.delete_file("b.c").unwrap();
    io.clear_oplog();

    // Initialize a new workspace.
    let ast = werk_parser::parse_toml(TOML)?;
    let workspace = werk_runner::Workspace::new(
        &*io,
        "/".into(),
        WorkspaceSettings::default().ignore_explicitly(
            GlobSet::builder()
                .add("/target/**".parse().unwrap())
                .build()
                .unwrap(),
        ),
    )
    .await?;
    let mut runner = werk_runner::Runner::new(ast, io.clone(), workspace, watcher.clone()).await?;

    let status = runner.build_file(Path::new("glob-dep")?).await?;

    assert!(io.did_run_during_build(&ShellCommandLine {
        program: "/clang".into(),
        arguments: vec![String::from("/a.c")],
        env: Default::default(),
        env_remove: Default::default()
    }));

    // println!("oplog = {:#?}", &*io.oplog.lock());
    assert_eq!(
        status,
        BuildStatus::Complete(
            TaskId::Build(PathBuf::try_from("/glob-dep").unwrap()),
            Outdatedness::new([
                Reason::Missing(PathBuf::try_from("/glob-dep")?),
                Reason::Glob(String::from("/*.c"))
            ])
        )
    );

    Ok(())
}
