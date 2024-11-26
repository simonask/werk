use std::sync::Arc;

use werk_fs::PathBuf;

use crate::{EvalError, OwnedDependencyChain, Pattern, ShellCommandLine, TaskId};

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] Arc<std::io::Error>),
    #[error(transparent)]
    CommandNotFound(#[from] which::Error),
    #[error("no rule to build target: {0}")]
    NoRuleToBuildTarget(String),
    #[error("circular dependency: {0}")]
    CircularDependency(OwnedDependencyChain),
    #[error("dependency failed: {0}: {1}")]
    DependencyFailed(TaskId, Arc<Error>),
    #[error("task was cancelled: {0}")]
    Cancelled(TaskId),
    #[error("eval error: {0}")]
    Eval(#[from] EvalError),
    #[error(transparent)]
    Walk(Arc<ignore::Error>),
    #[error(transparent)]
    Glob(Arc<globset::Error>),
    #[error("duplicate command: {0}")]
    DuplicateCommand(String),
    #[error("duplicate pattern: {0}")]
    DuplicateTarget(Pattern),
    #[error(transparent)]
    AmbiguousPattern(Arc<AmbiguousPatternError>),
    /// A shell command failed while executing a rule. Note that the
    /// stdout/stderr is a UI concern and only available through the
    /// `TrackRunner` interface.
    #[error("command failed: {0}")]
    CommandFailed(std::process::ExitStatus),
    #[error(transparent)]
    Custom(Arc<anyhow::Error>),
}

impl Error {
    pub fn custom<E: std::error::Error + Send + Sync + 'static>(err: E) -> Self {
        Self::Custom(Arc::new(anyhow::Error::new(err)))
    }
}

impl From<anyhow::Error> for Error {
    #[inline]
    fn from(err: anyhow::Error) -> Self {
        Self::Custom(Arc::new(err))
    }
}

impl From<std::io::Error> for Error {
    #[inline]
    fn from(err: std::io::Error) -> Self {
        Self::Io(Arc::new(err))
    }
}

impl From<ShellError> for Error {
    #[inline]
    fn from(err: ShellError) -> Self {
        Self::Eval(err.into())
    }
}

impl From<AmbiguousPatternError> for Error {
    #[inline]
    fn from(err: AmbiguousPatternError) -> Self {
        Self::AmbiguousPattern(Arc::new(err))
    }
}

impl From<globset::Error> for Error {
    #[inline]
    fn from(err: globset::Error) -> Self {
        Self::Glob(Arc::new(err))
    }
}

impl From<ignore::Error> for Error {
    #[inline]
    fn from(err: ignore::Error) -> Self {
        Self::Walk(Arc::new(err))
    }
}

impl From<werk_fs::PathError> for Error {
    #[inline]
    fn from(err: werk_fs::PathError) -> Self {
        Self::Eval(err.into())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("ambiguous pattern; both `{pattern1}` and `{pattern2}` would match `{path}`")]
pub struct AmbiguousPatternError {
    pub pattern1: String,
    pub pattern2: String,
    pub path: String,
}

#[derive(Debug, Clone)]
pub struct ShellError {
    pub command: ShellCommandLine,
    pub output: std::process::Output,
}

impl std::error::Error for ShellError {}

impl std::fmt::Display for ShellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "command failed: {}", self.command)?;
        if !self.output.stderr.is_empty() {
            write!(
                f,
                "\nstderr:\n{}",
                String::from_utf8_lossy(&self.output.stderr)
            )?;
        }
        Ok(())
    }
}
