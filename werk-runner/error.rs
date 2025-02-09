use std::sync::Arc;

use werk_fs::Absolute;
use werk_parser::parser::Span;
use werk_util::{DiagnosticFileId, DiagnosticSnippet};

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

impl werk_util::Diagnostic for Error {
    fn id_prefix(&self) -> &'static str {
        if let Error::Eval(ref err) = self {
            err.id_prefix()
        } else {
            "R"
        }
    }

    fn level(&self) -> annotate_snippets::Level {
        if let Error::Eval(ref err) = self {
            err.level()
        } else {
            annotate_snippets::Level::Error
        }
    }

    fn id(&self) -> u32 {
        match self {
            Error::Io(_) => 1,
            Error::CommandNotFound(..) => 2,
            Error::NoRuleToBuildTarget(_) => 3,
            Error::CircularDependency(..) => 4,
            Error::DependencyFailed(..) => 5,
            Error::Cancelled(..) => 6,
            Error::Eval(ref err) => err.id(),
            Error::Walk(..) => 7,
            Error::Glob(..) => 8,
            Error::DuplicateCommand(_) => 9,
            Error::DuplicateTarget(_) => 10,
            Error::AmbiguousPattern(..) => 11,
            Error::CommandFailed(..) => 12,
            Error::OutputDirectoryNotAvailable => 13,
            Error::DepfileNotFound(..) => 14,
            Error::DepfileError(..) => 15,
            Error::ClobberedWorkspace(..) => 16,
            Error::InvalidTargetPath(..) => 17,
            Error::InvalidPathInDepfile(..) => 18,
            Error::Custom(..) => 9999,
        }
    }

    fn title(&self) -> String {
        match self {
            Error::Eval(eval_error) => eval_error.title(),
            _ => self.to_string(),
        }
    }

    fn snippet(&self) -> Option<DiagnosticSnippet> {
        if let Error::Eval(ref err) = self {
            err.snippet()
        } else {
            None
        }
    }

    fn context_snippets(&self) -> Vec<DiagnosticSnippet> {
        match self {
            Error::Eval(ref err) => err.context_snippets(),
            Error::AmbiguousPattern(ref err) => {
                vec![
                    DiagnosticSnippet {
                        file_id: DiagnosticFileId::default(), // TODO
                        span: err.pattern1.into(),
                        message: String::from("first pattern here"),
                        info: vec![],
                    },
                    DiagnosticSnippet {
                        file_id: DiagnosticFileId::default(), // TODO
                        span: err.pattern2.into(),
                        message: String::from("second pattern here"),
                        info: vec![],
                    },
                ]
            }
            _ => vec![],
        }
    }

    fn help(&self) -> Vec<String> {
        if let Error::Eval(ref err) = self {
            err.help()
        } else {
            vec![]
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
#[error("ambiguous pattern match: {path}")]
pub struct AmbiguousPatternError {
    pub pattern1: Span,
    pub pattern2: Span,
    pub path: String,
}

#[derive(Debug, thiserror::Error, PartialEq)]
#[error(
    "ambiguous path resolution: {path} exists in the workspace, but also matches a build recipe"
)]
pub struct AmbiguousPathError {
    pub path: Absolute<werk_fs::PathBuf>,
    pub build_recipe: Span,
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
    #[error("duplicate config statement")]
    DuplicateConfigStatement(Span, Span),
    #[error("no implied interpolation value in this context; provide an identifier or a capture group index")]
    NoImpliedValue(Span),
    #[error("capture group with index {1} is out of bounds in the current scope")]
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
    #[error("unterminated quote in shell argument")]
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
    AssertEqFailed(Span, Box<(Value, Value)>),
    #[error("assertion failed: \"{}\" does not match the pattern '{}'", .1 .0.escape_default(), .1 .1)]
    AssertMatchFailed(Span, Box<(String, String)>),
    #[error("assertion failed: {1}")]
    AssertCustomFailed(Span, String),
    #[error("{1}")]
    AmbiguousPathResolution(Span, Arc<AmbiguousPathError>),
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
            | EvalError::DuplicateConfigStatement(span, _)
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
            | EvalError::AssertEqFailed(span, _)
            | EvalError::AssertMatchFailed(span, _)
            | EvalError::AssertCustomFailed(span, _)
            | EvalError::AmbiguousPathResolution(span, _) => *span,
        }
    }
}

impl werk_util::Diagnostic for EvalError {
    fn id_prefix(&self) -> &'static str {
        "E"
    }

    fn level(&self) -> annotate_snippets::Level {
        annotate_snippets::Level::Error
    }

    fn id(&self) -> u32 {
        match self {
            EvalError::InvalidEdition(..) => 1,
            EvalError::ExpectedConfigString(..) => 2,
            EvalError::ExpectedConfigBool(..) => 3,
            EvalError::UnknownConfigKey(..) => 4,
            EvalError::NoPatternStem(..) => 5,
            EvalError::IllegalOneOfPattern(..) => 6,
            EvalError::DuplicatePattern(..) => 7,
            EvalError::DuplicateConfigStatement(..) => 33,
            EvalError::NoImpliedValue(..) => 8,
            EvalError::NoSuchCaptureGroup(..) => 9,
            EvalError::NoSuchIdentifier(..) => 10,
            EvalError::UnexpectedList(..) => 11,
            EvalError::PatternStemInterpolationInPattern(..) => 12,
            EvalError::ResolvePathInPattern(..) => 13,
            EvalError::JoinInPattern(..) => 14,
            EvalError::ListInPattern(..) => 15,
            EvalError::PathWithinQuotes(..) => 16,
            EvalError::EmptyCommand(..) => 17,
            EvalError::EmptyList(..) => 18,
            EvalError::UnterminatedQuote(..) => 19,
            EvalError::UnexpectedExpressionType(..) => 20,
            EvalError::CommandNotFound(..) => 21,
            EvalError::NonUtf8Which(..) => 22,
            EvalError::NonUtf8Read(..) => 23,
            EvalError::Glob(..) => 24,
            EvalError::Shell(..) => 25,
            EvalError::Path(..) => 26,
            EvalError::Io(..) => 27,
            EvalError::ErrorExpression(..) => 28,
            EvalError::AssertEqFailed(..) => 29,
            EvalError::AssertMatchFailed(..) => 30,
            EvalError::AssertCustomFailed(..) => 31,
            EvalError::AmbiguousPathResolution(..) => 32,
        }
    }

    fn title(&self) -> String {
        self.to_string()
    }

    fn snippet(&self) -> Option<DiagnosticSnippet> {
        use werk_parser::parser::Spanned;
        Some(DiagnosticSnippet {
            file_id: werk_util::DiagnosticFileId::default(), // TODO
            span: self.span().into(),
            message: self.to_string(),
            info: vec![],
        })
    }

    fn context_snippets(&self) -> Vec<DiagnosticSnippet> {
        match self {
            EvalError::AmbiguousPathResolution(_, err) => {
                vec![DiagnosticSnippet {
                    file_id: DiagnosticFileId::default(), // TODO: might come from another file
                    span: err.build_recipe.into(),
                    message: String::from("matched this build recipe"),
                    info: vec![],
                }]
            }
            EvalError::DuplicateConfigStatement(_, previous_span) => {
                vec![DiagnosticSnippet {
                    file_id: DiagnosticFileId::default(), // TODO: Might come from another file,
                    span: (*previous_span).into(),
                    message: String::from("previous config statement here"),
                    info: vec![],
                }]
            }
            _ => vec![],
        }
    }

    fn help(&self) -> Vec<String> {
        match self {
            EvalError::NoSuchCaptureGroup(..) => vec![String::from(
                "pattern capture groups are zero-indexed, starting from 0",
            )],
            EvalError::AmbiguousPathResolution(..) => vec![String::from(
                "use `<...:out-dir>` or `<...:workspace>` to disambiguate between paths in the workspace and the output directory",
            )],
            _ => vec![],
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
