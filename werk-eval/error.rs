use std::sync::Arc;

use annotate_snippets::{AnnotationKind, Snippet};
use werk_fs::Absolute;
use werk_util::{DiagnosticFileId, DiagnosticSourceMap, DiagnosticSpan, IoError, Level};

use crate::{ShellCommandLine, Value};

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
    PathResolution(DiagnosticSpan, ResolvePathError),
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
    #[error("{}", .0.error)]
    Parse(werk_parser::ErrorInFile<werk_parser::Error>),
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
            | EvalError::PathResolution(span, _)
            | EvalError::IncludeIoError(span, ..)
            | EvalError::IncludeError(span, ..)
            | EvalError::IncludeDuplicate(span, ..)
            | EvalError::DefaultInInclude(span) => *span,
            EvalError::Parse(err) => err.file.span(err.error.span()),
        }
    }
}

impl werk_util::AsDiagnostic for EvalError {
    fn as_diagnostic<'a>(
        &'a self,
        source_map: &'a dyn DiagnosticSourceMap,
    ) -> Vec<annotate_snippets::Group<'a>> {
        let span = self.span();
        let source = source_map.get_source(span.file).expect("valid file ID");

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
            EvalError::PathResolution(_, ResolvePathError::Illegal) => "E0013",
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
            EvalError::PathResolution(_, ResolvePathError::Ambiguous(_)) => "E0032",
            EvalError::IncludeError(_, err) => {
                let mut groups = err.as_diagnostic(source_map);
                groups.push(
                    Level::NOTE.secondary_title("included here").element(
                        Snippet::source(source.source)
                            .path(source.file)
                            .annotation(AnnotationKind::Primary.span(span.span.into())),
                    ),
                );
                return groups;
            }
            EvalError::IncludeDuplicate(..) => "E0035",
            EvalError::DefaultInInclude(..) => "E0036",
            EvalError::Parse(err) => {
                return err.as_diagnostic(source_map);
            }
        };

        let title = self.to_string();

        let diag = Level::ERROR
            .primary_title(title.clone()) // Use the `Display` implementation from thiserror.
            .id(id)
            .element(
                Snippet::source(source.source)
                    .path(source.file)
                    .annotation(AnnotationKind::Primary.span(span.span.into()).label(title)),
            );

        // Help messages and additional context.
        let diag = match self {
            EvalError::NoSuchCaptureGroup(..) => diag.element(Level::HELP.message(
                "pattern capture groups are zero-indexed, starting from 0",
            )),
            EvalError::PathResolution(_, ResolvePathError::Ambiguous(err)) => {
                let previous_definition_file = source_map.get_source(err.build_recipe.file).expect("invalid file ID");
                diag
                .element(
                    Snippet::source(previous_definition_file.source)
                        .path(previous_definition_file.file)
                        .annotation(AnnotationKind::Context.span(err.build_recipe.span.into()).label("matched this build recipe")))
                .element(Level::HELP.message("use `<...:out-dir>` or `<...:workspace>` to disambiguate between paths in the workspace and the output directory"))
            },
            EvalError::DuplicateConfigStatement(_, previous_span) => {
                let previous_definition_file = source_map.get_source(previous_span.file).expect("invalid file ID");
                diag
                .element(Snippet::source(previous_definition_file.source).path(previous_definition_file.file).annotation(AnnotationKind::Context.span(previous_span.span.into()).label("previous config statement here")))
            },
            EvalError::ExpectedInt(..) => diag.element(Level::HELP.message("integers are stringly typed in array index operations, so \"0\" is the first element")),
            EvalError::IndexOutOfBounds(..) => diag.element(Level::HELP.message("arrays are indexed from zero, and negative indices refer to elements from the end of the array")),
            EvalError::IncludeDuplicate(_, Some(previous_span), _) => {
                let previous_definition_file = source_map.get_source(previous_span.file).expect("invalid file ID");
                diag
                .element(Snippet::source(previous_definition_file.source).path(previous_definition_file.file).annotation(AnnotationKind::Context.span(previous_span.span.into()).label("already included here")))
            },
            EvalError::DefaultInInclude(_) => diag.element(Level::HELP.message("move `default` statements to the top-level Werkfile")),
            _ => diag,
        };

        vec![diag]
    }
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
#[error(
    "ambiguous path resolution: {path} exists in the workspace, but also matches a build recipe"
)]
pub struct AmbiguousPathError {
    pub path: Absolute<werk_fs::PathBuf>,
    pub build_recipe: DiagnosticSpan,
}

#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum ResolvePathError {
    #[error("{0}")]
    Ambiguous(AmbiguousPathError),
    #[error("path resolution `<...>` interpolations cannot be used in patterns")]
    Illegal,
}
