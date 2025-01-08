use std::{
    future::Future,
    path::{Path, PathBuf},
    time::SystemTime,
};

pub use ignore::WalkState;
use parking_lot::Mutex;
use werk_fs::Absolute;

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
        working_dir: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>>;

    /// Run a command as part of evaluating the contents of a werk.toml file.
    /// This might still do something in dry-run mode.
    fn run_during_eval<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>>;

    /// Determine the absolute filesystem path to a program.
    fn which(&self, command: &str) -> Result<Absolute<PathBuf>, which::Error>;

    /// Glob the workspace directory, adhering to the glob settings.
    ///
    /// If this function produces a path to a `.werk-cache` file, the
    /// `Workspace` constructor will fail.
    fn glob_workspace<'a>(
        &'a self,
        path: &'a Absolute<Path>,
        settings: &'a GlobSettings,
    ) -> PinBoxFut<'a, Result<Vec<DirEntry>, Error>>;

    /// Query the metadata of a filesystem path.
    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, Error>;

    /// Read a file from the filesystem.
    fn read_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<Vec<u8>, std::io::Error>>;

    /// Write a file to the filesystem.
    fn write_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
        data: &'a [u8],
    ) -> PinBoxFut<'a, Result<(), std::io::Error>>;

    /// Copy one file to another on the file system. Must do nothing in dry-run.
    /// May do nothing if the paths are equal.
    fn copy_file<'a>(
        &'a self,
        from: &'a Absolute<Path>,
        to: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>>;

    /// Delete a file from the filesystem. Must do nothing in dry-run.
    fn delete_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>>;

    /// Create the parent directories of `path`, recursively.
    fn create_parent_dirs<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>>;

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

        Ok(DirEntry { metadata, path })
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

        Ok(DirEntry { metadata, path })
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

pub struct RealSystem(());

impl RealSystem {
    #[inline]
    pub fn new() -> Self {
        Self(())
    }
}

impl Io for RealSystem {
    fn run_build_command<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        Box::pin(async move {
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
            let output = child.output().await?;
            Ok(output)
        })
    }

    fn run_during_eval<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        working_dir: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        self.run_build_command(command_line, working_dir)
    }

    fn which(&self, program: &str) -> Result<Absolute<PathBuf>, which::Error> {
        which::which(program).map(Absolute::new_unchecked)
    }

    fn glob_workspace<'a>(
        &'a self,
        path: &'a Absolute<Path>,
        settings: &'a GlobSettings,
    ) -> PinBoxFut<'a, Result<Vec<DirEntry>, Error>> {
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
                Box::new(Visitor(Ok(Vec::new()), &self.0))
            }
        }

        struct Visitor<'s>(
            Result<Vec<DirEntry>, Error>,
            &'s Mutex<Result<Vec<DirEntry>, Error>>,
        );
        impl<'s> ignore::ParallelVisitor for Visitor<'s> {
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
        impl<'s> Drop for Visitor<'s> {
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

        Box::pin(smol::unblock(move || {
            let results = Mutex::new(Ok(Vec::new()));
            walker.visit(&mut Builder(&results));
            results.into_inner().map_err(Error::custom)
        }))
    }

    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, Error> {
        path.metadata()?.try_into().map_err(Into::into)
    }

    fn read_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<Vec<u8>, std::io::Error>> {
        Box::pin(async move { smol::fs::read(path).await.map_err(Into::into) })
    }

    fn write_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
        data: &'a [u8],
    ) -> PinBoxFut<'a, Result<(), std::io::Error>> {
        Box::pin(async move { smol::fs::write(path, data).await })
    }

    fn copy_file<'a>(
        &'a self,
        from: &'a Absolute<Path>,
        to: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>> {
        Box::pin(async move { smol::fs::copy(from, to).await.map(|_| ()) })
    }

    fn delete_file<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>> {
        Box::pin(async move { smol::fs::remove_file(path).await.map(|_| ()) })
    }

    fn create_parent_dirs<'a>(
        &'a self,
        path: &'a Absolute<Path>,
    ) -> PinBoxFut<'a, Result<(), std::io::Error>> {
        Box::pin(async move {
            let parent = path.parent().unwrap();
            let did_exist = parent.is_dir();
            smol::fs::create_dir_all(&parent).await?;
            if !did_exist {
                tracing::info!("Created directory: {}", parent.display());
            }
            Ok(())
        })
    }

    fn read_env(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn is_dry_run(&self) -> bool {
        false
    }
}
