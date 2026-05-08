use annotate_snippets::{AnnotationKind, Snippet};
use literator::Literator as _;
use werk_eval::EvalError;
use werk_util::{AsDiagnostic, DiagnosticSpan, Level};

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum PlannerError {
    #[error(transparent)]
    AmbiguousPattern(#[from] AmbiguousPatternError),
    #[error("no rule to build target: {0}")]
    NoRuleToBuildTarget(String),
    #[error(transparent)]
    CircularDependency(#[from] CircularDependencyError),
    #[error(transparent)]
    Evaluation(#[from] EvalError),
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
#[error("ambiguous pattern match: {path}")]
pub struct AmbiguousPatternError {
    pub pattern1: DiagnosticSpan,
    pub pattern2: DiagnosticSpan,
    pub path: String,
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
#[error("circular dependency: {}", .chain.iter().join(" -> "))]
pub struct CircularDependencyError {
    pub chain: Vec<String>,
}

impl AsDiagnostic for PlannerError {
    fn as_diagnostic<'a>(
        &'a self,
        source_map: &'a dyn werk_util::DiagnosticSourceMap,
    ) -> Vec<annotate_snippets::Group<'a>> {
        let id = match self {
            PlannerError::AmbiguousPattern(..) => "R0011",
            PlannerError::NoRuleToBuildTarget(_) => "R0003",
            PlannerError::CircularDependency(..) => "R0004",
            PlannerError::Evaluation(eval_error) => return eval_error.as_diagnostic(source_map),
        };

        // Use the Display impl from thiserror.
        let mut diag = vec![annotate_snippets::Group::with_title(
            Level::ERROR.primary_title(self.to_string()).id(id),
        )];

        // Additional context and help
        if let PlannerError::AmbiguousPattern(err) = self {
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
