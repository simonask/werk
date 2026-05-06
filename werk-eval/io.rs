use std::{
    path::{Path, PathBuf},
    pin::Pin,
    time::SystemTime,
};

use futures::{AsyncRead, AsyncWrite};
use werk_fs::Absolute;

use crate::{Env, GlobError, GlobSettings, ShellCommandLine};

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
        env: &Env,
        forward_stdout: bool,
    ) -> Result<Box<dyn Child>, std::io::Error>;

    /// Run a command as part of evaluating the contents of a Werkfile. This
    /// might still do something in dry-run mode.
    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
        env: &Env,
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
    ) -> Result<Vec<DirEntry>, GlobError>;

    /// Query the metadata of a filesystem path.
    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, std::io::Error>;

    /// Read a file from the filesystem.
    fn read_file(&self, path: &Absolute<Path>) -> Result<Vec<u8>, std::io::Error>;

    /// Write a file to the filesystem.
    fn write_file(&self, path: &Absolute<Path>, data: &[u8]) -> Result<(), std::io::Error>;

    /// Copy one file to another on the file system. Must do nothing in dry-run.
    /// May do nothing if the paths are equal.
    fn copy_file(&self, from: &Absolute<Path>, to: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Delete a file from the filesystem. Must do nothing in dry-run.
    fn delete_file(&self, path: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Create a file, or update an existing file's mtime. (Equivalent to UNIX `touch`.)
    fn touch(&self, path: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Create the parent directories of `path`, recursively.
    fn create_parent_dirs(&self, path: &Absolute<Path>) -> Result<(), std::io::Error>;

    /// Read environment variable.
    fn read_env(&self, name: &str) -> Option<String>;

    /// Is this object actually executing commands or not? The return value
    /// should be used for diagnostic purposes only, because the actual behavior
    /// of the runner is not affected by this.
    fn is_dry_run(&self) -> bool;
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

pub trait Child: Send + Sync + Unpin {
    fn stdin(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncWrite>>;
    fn stderr(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncRead>>;

    fn take_stdin(&mut self) -> Option<Pin<Box<dyn AsyncWrite + Send>>>;
    fn take_stdout(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>>;
    fn take_stderr(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>>;

    /// Wait for the process to exit. Does NOT drop the stdin handle.
    fn status(
        &mut self,
    ) -> Pin<Box<dyn Future<Output = Result<std::process::ExitStatus, std::io::Error>> + Send>>;

    fn kill(&mut self) -> std::io::Result<()>;
}

#[derive(Debug, Clone)]
pub struct DirEntry {
    pub path: Absolute<PathBuf>,
    pub metadata: Metadata,
}

impl TryFrom<ignore::DirEntry> for DirEntry {
    type Error = ignore::Error;

    fn try_from(value: ignore::DirEntry) -> Result<Self, Self::Error> {
        Ok(Self {
            path: Absolute::new_unchecked(value.path().to_path_buf()),
            metadata: value.metadata().unwrap().try_into()?,
        })
    }
}
