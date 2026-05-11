use annotate_snippets::{AnnotationKind, Snippet};
use std::sync::Arc;
use werk_eval::TaskName;
use werk_planner::PlannerError;
use werk_util::{DiagnosticSourceMap, DiagnosticSpan, IoError, Level};

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] IoError),
    #[error(transparent)]
    Planner(#[from] PlannerError),
    #[error("command not found: {0}: {1}")]
    CommandNotFound(String, which::Error),
    #[error("dependency failed: {0}: {1}")]
    DependencyFailed(TaskName, Arc<Error>),
    #[error("task was cancelled: {0}")]
    Cancelled(TaskName),
    #[error(transparent)]
    Walk(Arc<ignore::Error>),
    #[error(transparent)]
    Glob(Arc<globset::Error>),
    #[error("duplicate command: {0}")]
    DuplicateCommand(String),
    #[error("duplicate pattern: {0}")]
    DuplicateTarget(String),
    /// A shell command failed while executing a rule. Note that the
    /// stdout/stderr is a UI concern and only available through the
    /// `TrackRunner` interface.
    #[error("command failed: {0}")]
    CommandFailed(std::process::ExitStatus),
    #[error("build recipe target is a directory: {path}")]
    TargetIsADirectory {
        span: DiagnosticSpan,
        path: werk_fs::PathBuf,
    },
    #[error("cannot {op} because the path is protected: {}", path.display())]
    ProtectedPath {
        span: Option<DiagnosticSpan>,
        op: &'static str,
        path: std::path::PathBuf,
    },
    #[error("cannot {op} beyond the root of the workspace: {}", path.display())]
    WriteBeyondRoot {
        span: Option<DiagnosticSpan>,
        op: &'static str,
        path: std::path::PathBuf,
    },
    #[error("invalid target path `{0}`: {1}")]
    InvalidTargetPath(String, werk_fs::PathError),
    #[error("invalid path in depfile `{0}`: {1}")]
    InvalidPathInDepfile(String, werk_fs::PathError),
    #[error("{0}")]
    MustMigrate(String),
    #[error(transparent)]
    Custom(Arc<anyhow::Error>),
}

impl Error {
    pub fn custom<E: std::error::Error + Send + Sync + 'static>(err: E) -> Self {
        Self::Custom(Arc::new(anyhow::Error::new(err)))
    }

    #[must_use]
    pub fn eval(err: werk_eval::EvalError) -> Self {
        Self::Planner(PlannerError::Evaluation(err))
    }

    /// True when, even though an error occurred, the `.werk-cache` file should
    /// still be written.
    #[must_use]
    pub fn should_still_write_werk_cache(&self) -> bool {
        match self {
            Error::Io(_)
            | Error::CommandNotFound(..)
            | Error::DependencyFailed(..)
            | Error::CommandFailed(_)
            | Error::Cancelled(_) => true,
            Error::Walk(_)
            | Error::Glob(_)
            | Error::DuplicateCommand(_)
            | Error::DuplicateTarget(_)
            | Error::Planner(_)
            | Error::TargetIsADirectory { .. }
            | Error::InvalidTargetPath(..)
            | Error::InvalidPathInDepfile(..)
            | Error::MustMigrate(..)
            | Error::ProtectedPath { .. }
            | Error::WriteBeyondRoot { .. }
            | Error::Custom(_) => false,
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(l0), Self::Io(r0)) => l0 == r0,
            (Self::Planner(l0), Self::Planner(r0)) => l0 == r0,
            (Self::CommandNotFound(l0, l1), Self::CommandNotFound(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::DependencyFailed(l0, l1), Self::DependencyFailed(r0, r1)) => {
                l0 == r0 && l1 == r1
            }
            (Self::Cancelled(l0), Self::Cancelled(r0)) => l0 == r0,
            (Self::Walk(l0), Self::Walk(r0)) => l0.to_string() == r0.to_string(),
            (Self::Glob(l0), Self::Glob(r0)) => l0 == r0,
            (Self::DuplicateCommand(l0), Self::DuplicateCommand(r0))
            | (Self::DuplicateTarget(l0), Self::DuplicateTarget(r0)) => l0 == r0,
            (Self::CommandFailed(l0), Self::CommandFailed(r0)) => l0 == r0,
            (
                Self::TargetIsADirectory { path: lhs, .. },
                Self::TargetIsADirectory { path: rhs, .. },
            ) => lhs == rhs,
            (Self::ProtectedPath { path: lhs, .. }, Self::ProtectedPath { path: rhs, .. })
            | (Self::WriteBeyondRoot { path: lhs, .. }, Self::WriteBeyondRoot { path: rhs, .. }) => {
                lhs == rhs
            }
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

impl From<werk_planner::AmbiguousPatternError> for Error {
    #[inline]
    fn from(err: werk_planner::AmbiguousPatternError) -> Self {
        Self::Planner(werk_planner::PlannerError::AmbiguousPattern(err))
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
        }
    }
}

impl werk_util::AsDiagnostic for Error {
    fn as_diagnostic<'a>(
        &'a self,
        source_map: &'a dyn DiagnosticSourceMap,
    ) -> Vec<annotate_snippets::Group<'a>> {
        let id = match self {
            Error::Io(..) => "R0001",
            Error::CommandNotFound(..) => "R0002",
            Error::Planner(planner_error) => return planner_error.as_diagnostic(source_map),
            Error::DependencyFailed(..) => "R0005",
            Error::Cancelled(..) => "R0006",
            Error::Walk(..) => "R0007",
            Error::Glob(..) => "R0008",
            Error::DuplicateCommand(..) => "R0009",
            Error::DuplicateTarget(..) => "R0010",
            Error::CommandFailed(..) => "R0012",
            Error::TargetIsADirectory { .. } => "R0052",
            Error::InvalidTargetPath(..) => "R0017",
            Error::InvalidPathInDepfile(..) => "R0018",
            Error::Custom(..) | Error::MustMigrate(_) => "R9999",
            Error::ProtectedPath { .. } => "R0050",
            Error::WriteBeyondRoot { .. } => "R0051",
        };

        // Use the Display impl from thiserror.
        let diag = annotate_snippets::Group::with_title(
            Level::ERROR.primary_title(self.to_string()).id(id),
        );

        let diag =
            match self {
                Error::TargetIsADirectory { span, path } => {
                    let source_file = source_map.get_source(span.file).expect("invalid file ID");
                    diag.element(
                        Snippet::source(source_file.source)
                            .path(source_file.file)
                            .annotation(AnnotationKind::Context.span(span.span.into()).label(
                                format!("this build recipe target matches the directory '{path}'"),
                            )),
                    )
                }
                _ => diag,
            };

        vec![diag]
    }
}
