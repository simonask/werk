use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

pub use ignore::WalkState;
use parking_lot::Mutex;
use werk_fs::Absolute;

use crate::{Error, GlobSettings, ShellCommandLine};

mod child;
pub use child::*;

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
    /// Run a command as part of a recipe. This will do nothing in dry-run mode.
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
    ) -> Result<Box<dyn Child>, std::io::Error>;

    /// Run a command as part of evaluating the contents of a werk.toml file.
    /// This might still do something in dry-run mode.
    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
    ) -> Result<std::process::Output, std::io::Error>;

    /// Determine the absolute filesystem path to a program.
    fn which(&self, command: &str) -> Result<Absolute<PathBuf>, which::Error>;

    /// Glob the workspace directory, adhering to the glob settings.
    ///
    /// If this function produces a path to a `.werk-cache` file, the
    /// `Workspace` constructor will fail.
    fn glob_workspace(
        &self,
        path: &Absolute<Path>,
        settings: &GlobSettings,
    ) -> Result<Vec<DirEntry>, Error>;

    /// Query the metadata of a filesystem path.
    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, Error>;

    /// Read a file from the filesystem.
    fn read_file(&self, path: &Absolute<Path>) -> Result<Vec<u8>, std::io::Error>;

    /// Write a file to the filesystem.
    fn write_file(&self, path: &Absolute<Path>, data: &[u8]) -> Result<(), std::io::Error>;

    /// Copy one file to another on the file system. Must do nothing in dry-run.
    /// May do nothing if the paths are equal.
    fn copy_file(&self, from: &Absolute<Path>, to: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Delete a file from the filesystem. Must do nothing in dry-run.
    fn delete_file(&self, path: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Create the parent directories of `path`, recursively.
    fn create_parent_dirs(&self, path: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Read environment variable.
    fn read_env(&self, name: &str) -> Option<String>;

    /// Is this object actually executing commands or not? The return value
    /// should be used for diagnostic purposes only, because the actual behavior
    /// of the runner is not affected by this.
    fn is_dry_run(&self) -> bool;
}

#[derive(Debug, Clone)]
pub struct DirEntry {
    pub path: Absolute<PathBuf>,
    pub metadata: Metadata,
}

impl TryFrom<ignore::DirEntry> for DirEntry {
    type Error = ignore::Error;

    #[inline]
    fn try_from(entry: ignore::DirEntry) -> Result<Self, Self::Error> {
        let metadata = entry.metadata()?.try_into()?;
        let path = entry.into_path();

        // `ignore` claims that this is always true.
        assert!(path.is_absolute());
        let path = Absolute::new_unchecked(path);

        Ok(DirEntry { path, metadata })
    }
}

impl TryFrom<std::fs::DirEntry> for DirEntry {
    type Error = std::io::Error;

    #[inline]
    fn try_from(entry: std::fs::DirEntry) -> Result<Self, Self::Error> {
        let metadata = entry.metadata()?.try_into()?;
        let path = entry.path();

        assert!(path.is_absolute());
        let path = Absolute::new_unchecked(path);

        Ok(DirEntry { path, metadata })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Metadata {
    pub mtime: SystemTime,
    pub is_file: bool,
    pub is_symlink: bool,
}

impl Metadata {
    #[inline]
    #[must_use]
    pub fn is_dir(&self) -> bool {
        !self.is_file
    }
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

#[derive(Default)]
pub struct RealSystem(());

impl RealSystem {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl Io for RealSystem {
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
    ) -> Result<Box<dyn Child>, std::io::Error> {
        let mut command = smol::process::Command::new(&*command_line.program);
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
        Ok(Box::new(child))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
    ) -> Result<std::process::Output, std::io::Error> {
        let mut command = std::process::Command::new(&*command_line.program);
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
        let output = child.wait_with_output()?;
        Ok(output)
    }

    fn which(&self, program: &str) -> Result<Absolute<PathBuf>, which::Error> {
        which::which(program).map(Absolute::new_unchecked)
    }

    fn glob_workspace(
        &self,
        path: &Absolute<Path>,
        settings: &GlobSettings,
    ) -> Result<Vec<DirEntry>, Error> {
        let GlobSettings {
            git_ignore,
            git_ignore_global,
            git_ignore_exclude,
            git_ignore_from_parents,
            dot_ignore,
            ignore_explicitly,
        } = settings.clone();

        let mut walker = ignore::WalkBuilder::new(path);
        walker
            .git_ignore(git_ignore)
            .git_global(git_ignore_global)
            .git_exclude(git_ignore_exclude)
            .ignore(dot_ignore)
            .parents(git_ignore_from_parents);

        walker.filter_entry(move |entry| !ignore_explicitly.is_match(entry.path()));

        let walker = walker.build_parallel();

        struct Builder<'s>(&'s Mutex<Result<Vec<DirEntry>, Error>>);
        impl<'s> ignore::ParallelVisitorBuilder<'s> for Builder<'s> {
            fn build(&mut self) -> Box<dyn ignore::ParallelVisitor + 's> {
                Box::new(Visitor(Ok(Vec::new()), self.0))
            }
        }

        struct Visitor<'s>(
            Result<Vec<DirEntry>, Error>,
            &'s Mutex<Result<Vec<DirEntry>, Error>>,
        );
        impl ignore::ParallelVisitor for Visitor<'_> {
            fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> WalkState {
                let Ok(ref mut entries) = self.0 else {
                    // Already errored.
                    return WalkState::Quit;
                };

                match entry.map_err(Into::into).and_then(TryInto::try_into) {
                    Ok(entry) => {
                        entries.push(entry);
                        WalkState::Continue
                    }
                    Err(err) => {
                        self.0 = Err(err.into());
                        WalkState::Quit
                    }
                }
            }
        }
        impl Drop for Visitor<'_> {
            fn drop(&mut self) {
                let mut results = self.1.lock();
                let Ok(ref mut entries) = &mut *results else {
                    // Already errored.
                    return;
                };

                match std::mem::replace(&mut self.0, Ok(Vec::new())) {
                    Ok(new_entries) => entries.extend(new_entries),
                    Err(err) => *results = Err(err),
                }
            }
        }

        let results = Mutex::new(Ok(Vec::new()));
        walker.visit(&mut Builder(&results));
        results.into_inner().map_err(Error::custom)
    }

    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, Error> {
        path.metadata()?.try_into().map_err(Into::into)
    }

    fn read_file(&self, path: &Absolute<Path>) -> Result<Vec<u8>, std::io::Error> {
        std::fs::read(path)
    }

    fn write_file(&self, path: &Absolute<Path>, data: &[u8]) -> Result<(), std::io::Error> {
        std::fs::write(path, data)
    }

    fn copy_file(&self, from: &Absolute<Path>, to: &Absolute<Path>) -> Result<(), std::io::Error> {
        std::fs::copy(from, to).map(|_| ())
    }

    fn delete_file(&self, path: &Absolute<Path>) -> Result<(), std::io::Error> {
        std::fs::remove_file(path)
    }

    fn create_parent_dirs(&self, path: &Absolute<Path>) -> Result<(), std::io::Error> {
        let parent = path.parent().unwrap();
        let did_exist = parent.is_dir();
        std::fs::create_dir_all(parent)?;
        if !did_exist {
            tracing::info!("Created directory: {}", parent.display());
        }
        Ok(())
    }

    fn read_env(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn is_dry_run(&self) -> bool {
        false
    }
}
