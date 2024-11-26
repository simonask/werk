use std::sync::Arc;

use crate::{OwnedDependencyChain, Pattern, ShellCommandLine, TaskId, WhichError};

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] Arc<std::io::Error>),
    #[error(transparent)]
    CommandNotFound(#[from] WhichError),
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
    pub result: Arc<std::io::Result<std::process::Output>>,
}

impl std::error::Error for ShellError {}

impl std::fmt::Display for ShellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "command failed: {}", self.command)?;
        match &*self.result {
            Ok(ref output) => {
                if !output.stderr.is_empty() {
                    write!(f, "\nstderr:\n{}", String::from_utf8_lossy(&output.stderr))?;
                }
            }
            Err(ref err) => writeln!(f, "\nerror: {}", err)?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum EvalError {
    #[error("no pattern stem in this rule")]
    NoPatternStem,
    #[error("one-of patterns not allowed in this context")]
    IllegalOneOfPattern,
    #[error("no implied interpolation value in this context; provide an identifier or a capture group index")]
    NoImpliedValue,
    #[error("no capture group with index {0}")]
    NoSuchCaptureGroup(usize),
    #[error("no identifier with name {0}")]
    NoSuchIdentifier(String),
    #[error("unexpected list; perhaps a join operation `{{var*}}` is missing?")]
    UnexpectedList,
    #[error("pattern stems `{{%}}` cannot be interpolated in patterns")]
    PatternStemInterpolationInPattern,
    #[error("path resolution `<...>` interpolations cannot be used in patterns")]
    ResolvePathInPattern,
    #[error("join interpolations `{{...*}}` cannot be used in patterns")]
    JoinInPattern,
    #[error("unexpected list in pattern")]
    ListInPattern,
    #[error("invalid path interpolation within quotes; path arguments are automatically quoted")]
    PathWithinQuotes,
    #[error("empty command")]
    EmptyCommand,
    #[error("unterminated quote")]
    UnterminatedQuote,
    #[error("`glob` expressions are only allowed in variables")]
    UnexpectedGlob,
    #[error("`which` expressions are only allowed in variables")]
    UnexpectedWhich,
    #[error(transparent)]
    CommandNotFound(#[from] WhichError),
    #[error(transparent)]
    Glob(Arc<globset::Error>),
    /// Shell command failed during evaluation. Note: This error is not reported
    /// when executing commands as part of a rule, only when executing commands
    /// during evaluation (settings variables etc.)
    #[error(transparent)]
    Shell(Arc<ShellError>),
    #[error(transparent)]
    Path(#[from] werk_fs::PathError),
}

impl From<ShellError> for EvalError {
    fn from(err: ShellError) -> Self {
        EvalError::Shell(Arc::new(err))
    }
}

impl From<globset::Error> for EvalError {
    #[inline]
    fn from(err: globset::Error) -> Self {
        EvalError::Glob(Arc::new(err))
    }
}
