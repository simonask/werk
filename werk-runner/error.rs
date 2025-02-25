use std::sync::Arc;

use werk_fs::Absolute;
use werk_util::{AnnotateLevelExt, DiagnosticFileId, DiagnosticSpan, Level};

use crate::{OwnedDependencyChain, ShellCommandLine, TaskId, Value, depfile::DepfileError};

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
    #[error(
        "cannot convert abstract paths to native OS paths yet; output directory has not been set in the [global] scope"
    )]
    OutputDirectoryNotAvailable,
    #[error(
        "depfile was not found: '{0}'; perhaps the rule to generate it writes to the wrong location?"
    )]
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

impl werk_util::AsDiagnostic for Error {
    fn as_diagnostic(&self) -> werk_util::Diagnostic {
        let level = Level::Error;

        let id = match self {
            Error::Eval(eval_error) => return eval_error.as_diagnostic(),
            Error::Io(..) => "R0001",
            Error::CommandNotFound(..) => "R0002",
            Error::NoRuleToBuildTarget(..) => "R0003",
            Error::CircularDependency(..) => "R0004",
            Error::DependencyFailed(..) => "R0005",
            Error::Cancelled(..) => "R0006",
            Error::Walk(..) => "R0007",
            Error::Glob(..) => "R0008",
            Error::DuplicateCommand(..) => "R0009",
            Error::DuplicateTarget(..) => "R0010",
            Error::AmbiguousPattern(..) => "R0011",
            Error::CommandFailed(..) => "R0012",
            Error::OutputDirectoryNotAvailable => "R0013",
            Error::DepfileNotFound(..) => "R0014",
            Error::DepfileError(..) => "R0015",
            Error::ClobberedWorkspace(..) => "R0016",
            Error::InvalidTargetPath(..) => "R0017",
            Error::InvalidPathInDepfile(..) => "R0018",
            Error::Custom(..) => "R9999",
        };

        let diag = level.diagnostic(id).title(self); // Use the Display impl from thiserror.

        // Additional context and help
        match self {
            Error::AmbiguousPattern(err) => diag.annotations([
                err.pattern1.annotation(Level::Note, "first pattern here"),
                err.pattern2.annotation(Level::Note, "second pattern here"),
            ]),
            _ => diag,
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
#[error("ambiguous pattern match: {path}")]
pub struct AmbiguousPatternError {
    pub pattern1: DiagnosticSpan,
    pub pattern2: DiagnosticSpan,
    pub path: String,
}

#[derive(Debug, thiserror::Error, PartialEq)]
#[error(
    "ambiguous path resolution: {path} exists in the workspace, but also matches a build recipe"
)]
pub struct AmbiguousPathError {
    pub path: Absolute<werk_fs::PathBuf>,
    pub build_recipe: DiagnosticSpan,
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
                (Ok(l), Ok(r)) => l == r,
                (Err(l), Err(r)) => l.kind() == r.kind(),
                _ => false,
            }
    }
}

impl std::error::Error for ShellError {}

impl std::fmt::Display for ShellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "command failed: {}", self.command.program.display())?;
        match &*self.result {
            Ok(output) => {
                if !output.stderr.is_empty() {
                    write!(f, "\nstderr:\n{}", String::from_utf8_lossy(&output.stderr))?;
                }
            }
            Err(err) => writeln!(f, "\nerror: {err}")?,
        }

        Ok(())
    }
}

#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum EvalError {
    #[error("invalid edition identifier; expected `v1`")]
    InvalidEdition(DiagnosticSpan),
    #[error("expected a string value")]
    ExpectedConfigString(DiagnosticSpan),
    #[error("expected a boolean value")]
    ExpectedConfigBool(DiagnosticSpan),
    #[error("unknown config key")]
    UnknownConfigKey(DiagnosticSpan),
    #[error("no pattern in scope that contains a pattern stem `%`")]
    NoPatternStem(DiagnosticSpan),
    #[error("one-of patterns not allowed in this context")]
    IllegalOneOfPattern(DiagnosticSpan),
    #[error("duplicate pattern")]
    DuplicatePattern(DiagnosticSpan, DiagnosticSpan),
    #[error("duplicate config statement")]
    DuplicateConfigStatement(DiagnosticSpan, DiagnosticSpan),
    #[error(
        "no implied interpolation value in this context; provide an identifier or a capture group index"
    )]
    NoImpliedValue(DiagnosticSpan),
    #[error("capture group with index {1} is out of bounds in the current scope")]
    NoSuchCaptureGroup(DiagnosticSpan, u32),
    #[error("no identifier with name {1}")]
    NoSuchIdentifier(DiagnosticSpan, String),
    #[error("unexpected list; perhaps a join operation `{{var*}}` is missing?")]
    UnexpectedList(DiagnosticSpan),
    #[error("pattern stems `{{%}}` cannot be interpolated in patterns")]
    PatternStemInterpolationInPattern(DiagnosticSpan),
    #[error("path resolution `<...>` interpolations cannot be used in patterns")]
    ResolvePathInPattern(DiagnosticSpan),
    #[error(
        "path resolution `<...>` of a string that already contains resolved paths, but is not a resolved path"
    )]
    DoubleResolvePath(DiagnosticSpan),
    #[error("join interpolations `{{...*}}` cannot be used in patterns")]
    JoinInPattern(DiagnosticSpan),
    #[error("unexpected list in pattern")]
    ListInPattern(DiagnosticSpan),
    #[error("invalid path interpolation within quotes; path arguments are automatically quoted")]
    PathWithinQuotes(DiagnosticSpan),
    #[error("empty command")]
    EmptyCommand(DiagnosticSpan),
    #[error("empty list")]
    EmptyList(DiagnosticSpan),
    #[error("unterminated quote in shell argument")]
    UnterminatedQuote(DiagnosticSpan),
    #[error("`{1}` expressions are not allowed in this context")]
    UnexpectedExpressionType(DiagnosticSpan, &'static str),
    #[error("expected an integer value, got \"{}\"", .1.escape_default())]
    ExpectedInt(DiagnosticSpan, String),
    #[error("index out of bounds: got {1}, and the array length is {2}")]
    IndexOutOfBounds(DiagnosticSpan, i32, usize),
    #[error("command not found: {1}: {2}")]
    CommandNotFound(DiagnosticSpan, String, which::Error),
    #[error("`which` expression resulted in a non-UTF-8 path: {}", .1.display())]
    NonUtf8Which(DiagnosticSpan, std::path::PathBuf),
    #[error("`read` failed because file is not valid UTF-8: {}", .1.display())]
    NonUtf8Read(DiagnosticSpan, std::path::PathBuf),
    #[error("{1}")]
    Glob(DiagnosticSpan, Arc<globset::Error>),
    /// Shell command failed during evaluation. Note: This error is not reported
    /// when executing commands as part of a rule, only when executing commands
    /// during evaluation (settings variables etc.)
    #[error("{1}")]
    Shell(DiagnosticSpan, Arc<ShellError>),
    #[error("{1}")]
    Path(DiagnosticSpan, werk_fs::PathError),
    #[error("I/O error during evaluation: {1}")]
    Io(DiagnosticSpan, IoError),
    #[error("{1}")]
    ErrorExpression(DiagnosticSpan, String),
    #[error("assertion failed: {} != {}", .1 .0, .1 .1)]
    AssertEqFailed(DiagnosticSpan, Box<(Value, Value)>),
    #[error("assertion failed: \"{}\" does not match the pattern '{}'", .1 .0.escape_default(), .1 .1)]
    AssertMatchFailed(DiagnosticSpan, Box<(String, String)>),
    #[error("assertion failed: {1}")]
    AssertCustomFailed(DiagnosticSpan, String),
    #[error("{1}")]
    AmbiguousPathResolution(DiagnosticSpan, Arc<AmbiguousPathError>),
    #[error("error including '{1}': {2}")]
    IncludeIoError(DiagnosticSpan, String, IoError),
    #[error("{1}")]
    IncludeError(DiagnosticSpan, Box<EvalError>),
    #[error("same file included twice: {2}")]
    IncludeDuplicate(
        DiagnosticSpan,
        Option<DiagnosticSpan>,
        Absolute<werk_fs::PathBuf>,
    ),
    #[error("`default` statements are not allowed in included files")]
    DefaultInInclude(DiagnosticSpan),
    #[error("{1}")]
    Parse(DiagnosticFileId, werk_parser::Error),
}

/// Error in an `include` statement.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum IncludeError {
    #[error("{1}")]
    Parse(DiagnosticFileId, werk_parser::Error),
    #[error(transparent)]
    Eval(#[from] EvalError),
}

impl EvalError {
    #[inline]
    fn span(&self) -> DiagnosticSpan {
        match self {
            EvalError::InvalidEdition(span)
            | EvalError::ExpectedConfigString(span)
            | EvalError::ExpectedConfigBool(span)
            | EvalError::UnknownConfigKey(span)
            | EvalError::NoPatternStem(span)
            | EvalError::IllegalOneOfPattern(span)
            | EvalError::DuplicatePattern(span, _)
            | EvalError::DuplicateConfigStatement(span, _)
            | EvalError::NoImpliedValue(span)
            | EvalError::NoSuchCaptureGroup(span, _)
            | EvalError::NoSuchIdentifier(span, _)
            | EvalError::UnexpectedList(span)
            | EvalError::PatternStemInterpolationInPattern(span)
            | EvalError::ResolvePathInPattern(span)
            | EvalError::DoubleResolvePath(span)
            | EvalError::JoinInPattern(span)
            | EvalError::ListInPattern(span)
            | EvalError::PathWithinQuotes(span)
            | EvalError::EmptyCommand(span)
            | EvalError::EmptyList(span)
            | EvalError::UnterminatedQuote(span)
            | EvalError::UnexpectedExpressionType(span, _)
            | EvalError::ExpectedInt(span, _)
            | EvalError::IndexOutOfBounds(span, _, _)
            | EvalError::CommandNotFound(span, _, _)
            | EvalError::NonUtf8Which(span, _)
            | EvalError::NonUtf8Read(span, _)
            | EvalError::Glob(span, _)
            | EvalError::Shell(span, _)
            | EvalError::Path(span, _)
            | EvalError::Io(span, _)
            | EvalError::ErrorExpression(span, _)
            | EvalError::AssertEqFailed(span, _)
            | EvalError::AssertMatchFailed(span, _)
            | EvalError::AssertCustomFailed(span, _)
            | EvalError::AmbiguousPathResolution(span, _)
            | EvalError::IncludeIoError(span, ..)
            | EvalError::IncludeError(span, ..)
            | EvalError::IncludeDuplicate(span, ..)
            | EvalError::DefaultInInclude(span) => *span,
            EvalError::Parse(file, err) => file.span(err.span()),
        }
    }
}

impl werk_util::AsDiagnostic for EvalError {
    fn as_diagnostic(&self) -> werk_util::Diagnostic {
        let level = Level::Error;
        let span = self.span();

        let id = match self {
            EvalError::InvalidEdition(..) => "E0001",
            EvalError::ExpectedConfigString(..) => "E0002",
            EvalError::ExpectedConfigBool(..) => "E0003",
            EvalError::UnknownConfigKey(..) => "E0004",
            EvalError::NoPatternStem(..) => "E0005",
            EvalError::IllegalOneOfPattern(..) => "E0006",
            EvalError::DuplicatePattern(..) => "E0007",
            EvalError::DuplicateConfigStatement(..) => "E0033",
            EvalError::NoImpliedValue(..) => "E0008",
            EvalError::NoSuchCaptureGroup(..) => "E0009",
            EvalError::NoSuchIdentifier(..) => "E0010",
            EvalError::UnexpectedList(..) => "E0011",
            EvalError::PatternStemInterpolationInPattern(..) => "E0012",
            EvalError::ResolvePathInPattern(..) => "E0013",
            EvalError::DoubleResolvePath(..) => "E0034",
            EvalError::JoinInPattern(..) => "E0014",
            EvalError::ListInPattern(..) => "E0015",
            EvalError::PathWithinQuotes(..) => "E0016",
            EvalError::EmptyCommand(..) => "E0017",
            EvalError::EmptyList(..) => "E0018",
            EvalError::UnterminatedQuote(..) => "E0019",
            EvalError::UnexpectedExpressionType(..) => "E0020",
            EvalError::ExpectedInt(..) => "E0028",
            EvalError::IndexOutOfBounds(..) => "E0037",
            EvalError::CommandNotFound(..) => "E0021",
            EvalError::NonUtf8Which(..) => "E0022",
            EvalError::NonUtf8Read(..) => "E0023",
            EvalError::Glob(..) => "E0024",
            EvalError::Shell(..) => "E0025",
            EvalError::Path(..) => "E0026",
            EvalError::Io(..) | EvalError::IncludeIoError(..) => "E0027",
            EvalError::ErrorExpression(..) => "E9999",
            EvalError::AssertEqFailed(..) => "E0029",
            EvalError::AssertMatchFailed(..) => "E0030",
            EvalError::AssertCustomFailed(..) => "E0031",
            EvalError::AmbiguousPathResolution(..) => "E0032",
            EvalError::IncludeError(_, err) => {
                return err
                    .as_diagnostic()
                    .annotation(span.annotation(Level::Note, "included here"));
            }
            EvalError::IncludeDuplicate(..) => "E0035",
            EvalError::DefaultInInclude(..) => "E0036",
            EvalError::Parse(file, err) => {
                return err.with_file_ref(*file).as_diagnostic();
            }
        };

        let diag = level
            .diagnostic(id)
            .title(self) // Use the `Display` implementation from thiserror.
            .annotation(span.annotation(level, self));

        // Help messages and additional context.
        match self {
            EvalError::NoSuchCaptureGroup(..) => diag.footer(
                "pattern capture groups are zero-indexed, starting from 0",
            ),
            EvalError::AmbiguousPathResolution(_, err) => diag
                .annotation(err.build_recipe.annotation(Level::Note, "matched this build recipe"))
                .footer("use `<...:out-dir>` or `<...:workspace>` to disambiguate between paths in the workspace and the output directory"),
            EvalError::DuplicateConfigStatement(_, previous_span) => diag
                .annotation(previous_span.annotation(Level::Note, "previous config statement here")),
            EvalError::ExpectedInt(..) => diag.footer("integers are stringly typed in array index operations, so \"0\" is the first element"),
            EvalError::IndexOutOfBounds(..) => diag.footer("arrays are indexed from zero, and negative indices refer to elements from the end of the array"),
            EvalError::IncludeDuplicate(_, Some(previous_span), _) => diag.annotation(previous_span.annotation(Level::Note, "already included here")),
            EvalError::DefaultInInclude(_) => diag.footer("move `default` statements to the top-level Werkfile"),
            _ => diag,
        }
    }
}

/// Clonable `std::io::Error`
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

impl std::error::Error for IoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.error.source()
    }
}
