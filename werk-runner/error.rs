use std::sync::Arc;

use crate::{OwnedDependencyChain, Pattern, ShellCommandLine, TaskId};

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] Arc<std::io::Error>),
    #[error("command not found: {0}: {1}")]
    CommandNotFound(String, which::Error),
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
    #[error("cannot convert abstract paths to native OS paths yet; output directory has not been set in the [global] scope")]
    OutputDirectoryNotAvailable,
    #[error(".werk-cache file found in workspace; please add its directory to .gitignore")]
    ClobberedWorkspace(std::path::PathBuf),
    #[error(transparent)]
    Custom(Arc<anyhow::Error>),
}

impl Error {
    pub fn custom<E: std::error::Error + Send + Sync + 'static>(err: E) -> Self {
        Self::Custom(Arc::new(anyhow::Error::new(err)))
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(l0), Self::Io(r0)) => l0.kind() == r0.kind(),
            (Self::CommandNotFound(l0, l1), Self::CommandNotFound(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::NoRuleToBuildTarget(l0), Self::NoRuleToBuildTarget(r0)) => l0 == r0,
            (Self::CircularDependency(l0), Self::CircularDependency(r0)) => l0 == r0,
            (Self::DependencyFailed(l0, l1), Self::DependencyFailed(r0, r1)) => {
                l0 == r0 && l1 == r1
            }
            (Self::Cancelled(l0), Self::Cancelled(r0)) => l0 == r0,
            (Self::Eval(l0), Self::Eval(r0)) => l0 == r0,
            (Self::Walk(l0), Self::Walk(r0)) => l0.to_string() == r0.to_string(),
            (Self::Glob(l0), Self::Glob(r0)) => l0 == r0,
            (Self::DuplicateCommand(l0), Self::DuplicateCommand(r0)) => l0 == r0,
            (Self::DuplicateTarget(l0), Self::DuplicateTarget(r0)) => l0 == r0,
            (Self::AmbiguousPattern(l0), Self::AmbiguousPattern(r0)) => l0 == r0,
            (Self::CommandFailed(l0), Self::CommandFailed(r0)) => l0 == r0,
            (Self::ClobberedWorkspace(l0), Self::ClobberedWorkspace(r0)) => l0 == r0,
            (Self::Custom(l0), Self::Custom(r0)) => l0.to_string() == r0.to_string(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
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

#[derive(Debug, thiserror::Error, PartialEq)]
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

impl PartialEq for ShellError {
    fn eq(&self, other: &Self) -> bool {
        self.command == other.command
            && match (&*self.result, &*other.result) {
                (Ok(ref l), Ok(ref r)) => l == r,
                (Err(ref l), Err(ref r)) => l.kind() == r.kind(),
                _ => false,
            }
    }
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

#[derive(Debug, Clone, thiserror::Error, PartialEq)]
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
    #[error("`{0}` expressions are not allowed in this context")]
    UnexpectedExpressionType(&'static str),
    #[error("command not found: {0}: {1}")]
    CommandNotFound(String, which::Error),
    #[error(transparent)]
    Glob(Arc<globset::Error>),
    /// Shell command failed during evaluation. Note: This error is not reported
    /// when executing commands as part of a rule, only when executing commands
    /// during evaluation (settings variables etc.)
    #[error(transparent)]
    Shell(Arc<ShellError>),
    #[error(transparent)]
    Path(#[from] werk_fs::PathError),
    #[error("{0}")]
    ErrorExpression(String),
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
