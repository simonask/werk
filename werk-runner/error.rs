use std::sync::Arc;

use werk_parser::parser::Span;

use crate::{depfile::DepfileError, OwnedDependencyChain, ShellCommandLine, TaskId, Value};

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
    DuplicateTarget(String),
    #[error(transparent)]
    AmbiguousPattern(Arc<AmbiguousPatternError>),
    /// A shell command failed while executing a rule. Note that the
    /// stdout/stderr is a UI concern and only available through the
    /// `TrackRunner` interface.
    #[error("command failed: {0}")]
    CommandFailed(std::process::ExitStatus),
    #[error("cannot convert abstract paths to native OS paths yet; output directory has not been set in the [global] scope")]
    OutputDirectoryNotAvailable,
    #[error("depfile was not found: '{0}'; perhaps the rule to generate it writes to the wrong location?")]
    DepfileNotFound(werk_fs::PathBuf),
    #[error(transparent)]
    DepfileError(#[from] DepfileError),
    #[error(".werk-cache file found in workspace; please add its directory to .gitignore")]
    ClobberedWorkspace(std::path::PathBuf),
    #[error("invalid target path `{0}`: {1}")]
    InvalidTargetPath(String, werk_fs::PathError),
    #[error("invalid path in depfile `{0}`: {1}")]
    InvalidPathInDepfile(String, werk_fs::PathError),
    #[error(transparent)]
    Custom(Arc<anyhow::Error>),
}

impl Error {
    pub fn custom<E: std::error::Error + Send + Sync + 'static>(err: E) -> Self {
        Self::Custom(Arc::new(anyhow::Error::new(err)))
    }

    /// True when, even though an error occurred, the `.werk-cache` file should
    /// still be written.
    #[must_use]
    pub fn should_still_write_werk_cache(&self) -> bool {
        match self {
            Error::Io(_)
            | Error::CommandNotFound(..)
            | Error::NoRuleToBuildTarget(_)
            | Error::CircularDependency(_)
            | Error::DependencyFailed(..)
            | Error::CommandFailed(_)
            | Error::DepfileNotFound(_)
            | Error::DepfileError(_)
            | Error::Cancelled(_) => true,
            Error::Eval(_)
            | Error::Walk(_)
            | Error::Glob(_)
            | Error::DuplicateCommand(_)
            | Error::DuplicateTarget(_)
            | Error::AmbiguousPattern(_)
            | Error::OutputDirectoryNotAvailable
            | Error::ClobberedWorkspace(_)
            | Error::InvalidTargetPath(..)
            | Error::InvalidPathInDepfile(..)
            | Error::Custom(_) => false,
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(l0), Self::Io(r0)) => l0.kind() == r0.kind(),
            (Self::CommandNotFound(l0, l1), Self::CommandNotFound(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::CircularDependency(l0), Self::CircularDependency(r0)) => l0 == r0,
            (Self::DependencyFailed(l0, l1), Self::DependencyFailed(r0, r1)) => {
                l0 == r0 && l1 == r1
            }
            (Self::Cancelled(l0), Self::Cancelled(r0)) => l0 == r0,
            (Self::Eval(l0), Self::Eval(r0)) => l0 == r0,
            (Self::Walk(l0), Self::Walk(r0)) => l0.to_string() == r0.to_string(),
            (Self::Glob(l0), Self::Glob(r0)) => l0 == r0,
            (Self::NoRuleToBuildTarget(l0), Self::NoRuleToBuildTarget(r0))
            | (Self::DuplicateCommand(l0), Self::DuplicateCommand(r0))
            | (Self::DuplicateTarget(l0), Self::DuplicateTarget(r0)) => l0 == r0,
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
        write!(f, "command failed: {}", self.command.program.display())?;
        match &*self.result {
            Ok(ref output) => {
                if !output.stderr.is_empty() {
                    write!(f, "\nstderr:\n{}", String::from_utf8_lossy(&output.stderr))?;
                }
            }
            Err(ref err) => writeln!(f, "\nerror: {err}")?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum EvalError {
    #[error("invalid edition identifier; expected `v1`")]
    InvalidEdition(Span),
    #[error("expected a string value")]
    ExpectedConfigString(Span),
    #[error("expected a boolean value")]
    ExpectedConfigBool(Span),
    #[error("unknown config key")]
    UnknownConfigKey(Span),
    #[error("no pattern stem in this rule")]
    NoPatternStem(Span),
    #[error("one-of patterns not allowed in this context")]
    IllegalOneOfPattern(Span),
    #[error("duplicate pattern")]
    DuplicatePattern(Span, Span),
    #[error("no implied interpolation value in this context; provide an identifier or a capture group index")]
    NoImpliedValue(Span),
    #[error("no capture group with index {1}")]
    NoSuchCaptureGroup(Span, u32),
    #[error("no identifier with name {1}")]
    NoSuchIdentifier(Span, String),
    #[error("unexpected list; perhaps a join operation `{{var*}}` is missing?")]
    UnexpectedList(Span),
    #[error("pattern stems `{{%}}` cannot be interpolated in patterns")]
    PatternStemInterpolationInPattern(Span),
    #[error("path resolution `<...>` interpolations cannot be used in patterns")]
    ResolvePathInPattern(Span),
    #[error("join interpolations `{{...*}}` cannot be used in patterns")]
    JoinInPattern(Span),
    #[error("unexpected list in pattern")]
    ListInPattern(Span),
    #[error("invalid path interpolation within quotes; path arguments are automatically quoted")]
    PathWithinQuotes(Span),
    #[error("empty command")]
    EmptyCommand(Span),
    #[error("empty list")]
    EmptyList(Span),
    #[error("unterminated quote")]
    UnterminatedQuote(Span),
    #[error("`{1}` expressions are not allowed in this context")]
    UnexpectedExpressionType(Span, &'static str),
    #[error("command not found: {1}: {2}")]
    CommandNotFound(Span, String, which::Error),
    #[error("`which` expression resulted in a non-UTF-8 path: {}", .1.display())]
    NonUtf8Which(Span, std::path::PathBuf),
    #[error("`read` failed because file is not valid UTF-8: {}", .1.display())]
    NonUtf8Read(Span, std::path::PathBuf),
    #[error("{1}")]
    Glob(Span, Arc<globset::Error>),
    /// Shell command failed during evaluation. Note: This error is not reported
    /// when executing commands as part of a rule, only when executing commands
    /// during evaluation (settings variables etc.)
    #[error("{1}")]
    Shell(Span, Arc<ShellError>),
    #[error("{1}")]
    Path(Span, werk_fs::PathError),
    #[error("I/O error during evaluation: {1}")]
    Io(Span, IoError),
    #[error("{1}")]
    ErrorExpression(Span, String),
    #[error("assertion failed: {} != {}", .1 .0, .1 .1)]
    AssertionFailed(Span, Box<(Value, Value)>),
    #[error("assertion failed: \"{}\" does not match the pattern '{}'", .1 .0.escape_default(), .1 .1)]
    AssertionMatchFailed(Span, Box<(String, String)>),
}

impl werk_parser::DisplayError for EvalError {
    fn annotations(&self) -> Vec<werk_parser::DisplayAnnotation> {
        vec![werk_parser::DisplayAnnotation {
            level: annotate_snippets::Level::Error,
            message: self.to_string(),
            span: werk_parser::parser::Spanned::span(self),
        }]
    }
}

impl werk_parser::parser::Spanned for EvalError {
    #[inline]
    fn span(&self) -> Span {
        match self {
            EvalError::InvalidEdition(span)
            | EvalError::ExpectedConfigString(span)
            | EvalError::ExpectedConfigBool(span)
            | EvalError::UnknownConfigKey(span)
            | EvalError::NoPatternStem(span)
            | EvalError::IllegalOneOfPattern(span)
            | EvalError::DuplicatePattern(span, _)
            | EvalError::NoImpliedValue(span)
            | EvalError::NoSuchCaptureGroup(span, _)
            | EvalError::NoSuchIdentifier(span, _)
            | EvalError::UnexpectedList(span)
            | EvalError::PatternStemInterpolationInPattern(span)
            | EvalError::ResolvePathInPattern(span)
            | EvalError::JoinInPattern(span)
            | EvalError::ListInPattern(span)
            | EvalError::PathWithinQuotes(span)
            | EvalError::EmptyCommand(span)
            | EvalError::EmptyList(span)
            | EvalError::UnterminatedQuote(span)
            | EvalError::UnexpectedExpressionType(span, _)
            | EvalError::CommandNotFound(span, _, _)
            | EvalError::NonUtf8Which(span, _)
            | EvalError::NonUtf8Read(span, _)
            | EvalError::Glob(span, _)
            | EvalError::Shell(span, _)
            | EvalError::Path(span, _)
            | EvalError::Io(span, _)
            | EvalError::ErrorExpression(span, _)
            | EvalError::AssertionFailed(span, _)
            | EvalError::AssertionMatchFailed(span, _) => *span,
        }
    }
}

#[derive(Clone)]
pub struct IoError {
    pub error: Arc<std::io::Error>,
}

impl From<std::io::Error> for IoError {
    #[inline]
    fn from(error: std::io::Error) -> Self {
        Self {
            error: Arc::new(error),
        }
    }
}

impl std::fmt::Debug for IoError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.error, f)
    }
}

impl std::fmt::Display for IoError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&*self.error, f)
    }
}

impl PartialEq for IoError {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.error, &other.error) || self.error.kind() == other.error.kind()
    }
}
