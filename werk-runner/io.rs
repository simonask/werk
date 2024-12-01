use std::{
    future::Future,
    path::{Path, PathBuf},
    time::SystemTime,
};

use crate::{Error, GlobSettings, ShellCommandLine};

/// Convenience type alias.
pub type PinBox<T> = std::pin::Pin<Box<T>>;
/// Convenience type alias.
pub type PinBoxFut<'a, T> = PinBox<dyn Future<Output = T> + Send + 'a>;
/// Convenience type alias.
pub type BoxIter<'a, T> = Box<dyn Iterator<Item = T> + 'a>;

/// Abstract interface to the file system and OS.
///
/// All interactions with the file system and OS should go through this, which
/// in particular means that things like methods on `std::path::Path` should
/// avoid accessing the filesystem. For example, methods like `canonicalize()`
/// or `metadata()` do access the filesystem, and should be avoided.
///
/// This abstraction exists to allow for testing the runner in a controlled
/// environment.
pub trait Io: Send + Sync + 'static {
    /// Run a command as part of a build recipe. This will do nothing in dry-run
    /// mode.
    fn run_build_command<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Path,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>>;

    /// Run a command as part of evaluating the contents of a werk.toml file.
    /// This might still do something in dry-run mode.
    fn run_during_eval<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Path,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>>;

    fn which(&self, command: &str) -> Result<PathBuf, which::Error>;
    fn walk_directory<'a>(
        &'a self,
        path: &'a Path,
        settings: &'a GlobSettings,
        ignore_subdirs: &'a [&Path],
    ) -> Result<BoxIter<'a, Result<DirEntry, Error>>, Error>;
    fn metadata(&self, path: &Path) -> Result<Metadata, Error>;
    fn read_file<'a>(&'a self, path: &'a Path) -> PinBoxFut<'a, Result<Vec<u8>, std::io::Error>>;
    fn write_file<'a>(
        &'a self,
        path: &'a Path,
        data: &'a [u8],
    ) -> PinBoxFut<'a, Result<(), std::io::Error>>;
    fn create_parent_dirs<'a>(&'a self, path: &'a Path) -> PinBoxFut<'a, Result<(), Error>>;

    fn read_env(&self, name: &str) -> Option<String>;
}

#[derive(Debug, Clone)]
pub struct DirEntry {
    pub path: PathBuf,
    pub metadata: Metadata,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Metadata {
    pub mtime: SystemTime,
    pub is_file: bool,
    pub is_symlink: bool,
}

impl TryFrom<std::fs::Metadata> for Metadata {
    type Error = std::io::Error;

    fn try_from(metadata: std::fs::Metadata) -> Result<Self, Self::Error> {
        Ok(Metadata {
            mtime: metadata.modified()?,
            is_file: metadata.is_file(),
            is_symlink: metadata.file_type().is_symlink(),
        })
    }
}

pub struct RealSystem;
impl Io for RealSystem {
    fn run_build_command<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Path,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        Box::pin(async move {
            let mut command = tokio::process::Command::new(&command_line.program);
            command
                .args(
                    command_line
                        .arguments
                        .iter()
                        .filter(|s| !s.trim().is_empty()),
                )
                .envs(&command_line.env)
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                // All spawned commands always run in the project root.
                .current_dir(working_dir);

            for k in &command_line.env_remove {
                command.env_remove(k);
            }

            tracing::trace!("spawning {command:?}");
            let child = command.spawn()?;
            let output = child.wait_with_output().await?;
            Ok(output)
        })
    }

    fn run_during_eval<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Path,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        self.run_build_command(command_line, working_dir)
    }

    fn which(&self, program: &str) -> Result<PathBuf, which::Error> {
        which::which(program)
    }

    fn walk_directory<'a>(
        &'a self,
        path: &'a Path,
        settings: &'a GlobSettings,
        ignore_subdirs: &'a [&'a Path],
    ) -> Result<BoxIter<'a, Result<DirEntry, Error>>, Error> {
        let mut walker = ignore::WalkBuilder::new(path);
        walker
            .git_ignore(settings.git_ignore)
            .git_global(settings.git_ignore_global)
            .git_exclude(settings.git_ignore_exclude)
            .parents(settings.git_ignore_from_parents)
            .hidden(!settings.ignore_hidden);

        // Ignore the out_dir in globs.
        for ignore in ignore_subdirs {
            walker.add_ignore(ignore);
        }
        for ignore in &settings.ignore {
            walker.add_ignore(ignore);
        }

        let walker = walker.build();
        Ok(Box::new(walker.map(|entry| {
            let entry = entry?;
            Ok(DirEntry {
                path: entry.path().to_path_buf(),
                metadata: entry.metadata()?.try_into()?,
            })
        })))
    }

    fn metadata(&self, path: &Path) -> Result<Metadata, Error> {
        path.metadata()?.try_into().map_err(Into::into)
    }

    fn read_file<'a>(&'a self, path: &'a Path) -> PinBoxFut<'a, Result<Vec<u8>, std::io::Error>> {
        Box::pin(async move { tokio::fs::read(path).await.map_err(Into::into) })
    }

    fn write_file<'a>(
        &'a self,
        path: &'a Path,
        data: &'a [u8],
    ) -> PinBoxFut<'a, Result<(), std::io::Error>> {
        Box::pin(async move { tokio::fs::write(path, data).await })
    }

    fn create_parent_dirs<'a>(&'a self, path: &'a Path) -> PinBoxFut<'a, Result<(), Error>> {
        Box::pin(async move {
            let parent = path.parent().unwrap();
            let did_exist = parent.is_dir();
            tokio::fs::create_dir_all(&parent).await?;
            if !did_exist {
                tracing::info!("Created directory: {}", parent.display());
            }
            Ok(())
        })
    }

    fn read_env(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }
}
