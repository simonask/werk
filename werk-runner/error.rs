use std::sync::Arc;

use annotate_snippets::{AnnotationKind, Snippet};
use werk_eval::{EvalError, TaskId};
use werk_util::{DiagnosticSourceMap, DiagnosticSpan, Level};

use crate::{OwnedDependencyChain, depfile::DepfileError};

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
    AmbiguousPattern(Arc<werk_eval::AmbiguousPatternError>),
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

impl From<werk_eval::AmbiguousPatternError> for Error {
    #[inline]
    fn from(err: werk_eval::AmbiguousPatternError) -> Self {
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

impl From<werk_eval::GlobError> for Error {
    fn from(value: werk_eval::GlobError) -> Self {
        match value {
            werk_eval::GlobError::Glob(error) => error.into(),
            werk_eval::GlobError::Ignore(error) => error.into(),
        }
    }
}

impl werk_util::AsDiagnostic for Error {
    fn as_diagnostic<'a>(
        &'a self,
        source_map: &'a dyn DiagnosticSourceMap,
    ) -> Vec<annotate_snippets::Group<'a>> {
        let id = match self {
            Error::Eval(eval_error) => return eval_error.as_diagnostic(source_map),
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

        // Use the Display impl from thiserror.
        let mut diag = vec![annotate_snippets::Group::with_title(
            Level::ERROR.primary_title(self.to_string()).id(id),
        )];

        // Additional context and help
        if let Error::AmbiguousPattern(err) = self {
            let first_source = source_map
                .get_source(err.pattern1.file)
                .expect("valid file ID");
            let second_source = source_map
                .get_source(err.pattern2.file)
                .expect("valid file ID");
            diag.push(
                Level::NOTE.secondary_title("first pattern here").element(
                    Snippet::source(first_source.source)
                        .path(first_source.file)
                        .annotation(AnnotationKind::Context.span(err.pattern1.span.into())),
                ),
            );
            diag.push(
                Level::NOTE.secondary_title("second pattern here").element(
                    Snippet::source(second_source.source)
                        .path(second_source.file)
                        .annotation(AnnotationKind::Context.span(err.pattern2.span.into())),
                ),
            );
        }

        diag
    }
}
