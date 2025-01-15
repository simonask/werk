use std::sync::Arc;

use winnow::{
    error::{AddContext, FromExternalError},
    stream::Stream,
};

use crate::{
    parse_toml::{ExprType, RunExprType},
    parser::{Span, SpannedValue},
};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Toml(Box<toml_edit::TomlError>),
    #[error("{1}")]
    Werk(Span, ParseError),
    #[error("invalid key")]
    InvalidKey(Span),
    #[error("expected table")]
    ExpectedTable(Span),
    #[error("expected string")]
    ExpectedString(Span),
    #[error("expected boolean")]
    ExpectedBool(Span),
    #[error("expected string or table")]
    ExpectedStringOrTable(Span),
    #[error("expected string or array")]
    ExpectedStringOrArray(Span),
    #[error("expected integer")]
    ExpectedInteger(Span),
    #[error("expected key '{1}' in table expression")]
    ExpectedKey(Span, &'static &'static str),
    #[error("expression table contain a root expression, one of: {}", ExprType::all_strs().join(", "))]
    ExpectedMainExpression(Span),
    #[error("expression table can only contain one root expression, found: {} and {}", &**.0, &**.1)]
    AmbiguousMainExpression(SpannedValue<ExprType>, SpannedValue<ExprType>),
    #[error("expression table can only contain one root expression, found: {} and {}", &**.0, &**.1)]
    AmbiguousRunExpression(SpannedValue<RunExprType>, SpannedValue<RunExprType>),
    #[error("unknown chaining expression")]
    UnknownExpressionChain(Span),
    #[error("invalid identifier: {1}")]
    InvalidIdent(Span, TomlParseError),
    #[error("invalid string expression: {1}")]
    InvalidStringExpr(Span, TomlParseError),
    #[error("invalid pattern expression: {1}")]
    InvalidPatternExpr(Span, TomlParseError),
    #[error("unknown config key")]
    UnknownConfigKey(Span),
}

impl From<toml_edit::TomlError> for Error {
    #[inline]
    fn from(value: toml_edit::TomlError) -> Self {
        Self::Toml(Box::new(value))
    }
}

impl Error {
    pub fn with_location<'a>(
        self,
        file_name: &'a std::path::Path,
        source_code: &'a str,
    ) -> LocatedError<'a, Self> {
        LocatedError {
            file_name,
            source_code,
            error: self,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Error::Toml(toml_error) => toml_error.span().map(Into::into).unwrap_or(Span::ignore()),
            Error::Werk(span, _) => *span,
            Error::InvalidKey(span)
            | Error::ExpectedTable(span)
            | Error::ExpectedString(span)
            | Error::ExpectedBool(span)
            | Error::ExpectedStringOrTable(span)
            | Error::ExpectedStringOrArray(span)
            | Error::ExpectedInteger(span)
            | Error::ExpectedKey(span, _)
            | Error::ExpectedMainExpression(span)
            | Error::UnknownExpressionChain(span)
            | Error::InvalidIdent(span, ..)
            | Error::InvalidStringExpr(span, ..)
            | Error::InvalidPatternExpr(span, ..)
            | Error::UnknownConfigKey(span)
            | Error::AmbiguousMainExpression(_, SpannedValue { span, .. })
            | Error::AmbiguousRunExpression(_, SpannedValue { span, .. }) => *span,
        }
    }
}

#[derive(Debug)]
pub struct LocatedError<'a, E> {
    pub file_name: &'a std::path::Path,
    pub source_code: &'a str,
    pub error: E,
}

impl std::fmt::Display for LocatedError<'_, Error> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use annotate_snippets::{Level, Snippet};

        let error_string;
        let file_name = self.file_name.display().to_string();

        let make_snippet = || {
            Snippet::source(self.source_code)
                .origin(&file_name)
                .fold(true)
        };

        let message = match self.error {
            Error::Toml(ref toml_error) => {
                if let Some(span) = toml_error.span() {
                    error_string = toml_error.to_string();
                    Level::Error.title("error parsing TOML").snippet(
                        make_snippet().annotation(Level::Error.span(span).label(&error_string)),
                    )
                } else {
                    error_string = format!("error parsing TOML: {toml_error}");
                    Level::Error.title(&error_string)
                }
            }
            Error::Werk(span, ref werk_error) => {
                error_string = werk_error.to_string();
                Level::Error.title("error parsing werk file").snippet(
                    make_snippet().annotation(Level::Error.span(span.into()).label(&error_string)),
                )
            }
            Error::InvalidKey(span) => Level::Error.title("invalid key").snippet(
                make_snippet().annotation(Level::Error.span(span.into()).label("invalid key")),
            ),
            Error::ExpectedTable(span) => Level::Error.title("expected table").snippet(
                make_snippet().annotation(Level::Error.span(span.into()).label("expected table")),
            ),
            Error::ExpectedString(span) => Level::Error.title("expected string").snippet(
                make_snippet().annotation(Level::Error.span(span.into()).label("expected string")),
            ),
            Error::ExpectedBool(span) => Level::Error.title("expected boolean").snippet(
                make_snippet().annotation(Level::Error.span(span.into()).label("expected boolean")),
            ),
            Error::ExpectedStringOrTable(span) => {
                Level::Error.title("expected string or table").snippet(
                    make_snippet().annotation(
                        Level::Error
                            .span(span.into())
                            .label("expected string or table"),
                    ),
                )
            }
            Error::ExpectedStringOrArray(span) => {
                Level::Error.title("expected string or array").snippet(
                    make_snippet().annotation(
                        Level::Error
                            .span(span.into())
                            .label("expected string or array"),
                    ),
                )
            }
            Error::ExpectedInteger(span) => Level::Error.title("expected integer").snippet(
                make_snippet().annotation(Level::Error.span(span.into()).label("expected integer")),
            ),
            Error::ExpectedKey(span, expected) => {
                error_string = format!("expected key `{expected}` in table expression");
                Level::Error.title(&error_string).snippet(
                    make_snippet().annotation(
                        Level::Error
                            .span(span.into())
                            .label("in this table expression"),
                    ),
                )
            }
            Error::ExpectedMainExpression(span) => {
                error_string = format!(
                    "expression table must contain a root expression: {}",
                    ExprType::all_strs().join(", ")
                );
                Level::Error.title(&error_string).snippet(
                    make_snippet().annotation(
                        Level::Error
                            .span(span.into())
                            .label("must contain a root expression"),
                    ),
                )
            }
            Error::AmbiguousMainExpression(ref first, ref second) => {
                Level::Error.title("ambiguous main expression").snippet(
                    make_snippet()
                        .annotation(
                            Level::Note
                                .span(first.span.into())
                                .label("first expression type"),
                        )
                        .annotation(
                            Level::Error
                                .span(second.span.into())
                                .label("second expression type"),
                        ),
                )
            }
            Error::AmbiguousRunExpression(ref first, ref second) => {
                Level::Error.title("ambiguous run expression").snippet(
                    make_snippet()
                        .annotation(
                            Level::Note
                                .span(first.span.into())
                                .label("first expression type"),
                        )
                        .annotation(
                            Level::Error
                                .span(second.span.into())
                                .label("second expression type"),
                        ),
                )
            }
            Error::UnknownExpressionChain(span) => {
                Level::Error.title("unknown chaining expression").snippet(
                    make_snippet().annotation(
                        Level::Error
                            .span(span.into())
                            .label("unknown chaining expression"),
                    ),
                )
            }
            Error::InvalidIdent(span, ref err) => {
                error_string = err.to_string();
                Level::Error.title("invalid identifier").snippet(
                    make_snippet().annotation(Level::Error.span(span.into()).label(&error_string)),
                )
            }
            Error::InvalidStringExpr(span, ref err) => {
                error_string = err.to_string();
                Level::Error.title("invalid string expression").snippet(
                    make_snippet().annotation(Level::Error.span(span.into()).label(&error_string)),
                )
            }
            Error::InvalidPatternExpr(span, ref err) => {
                error_string = err.to_string();
                Level::Error.title("invalid pattern expression").snippet(
                    make_snippet().annotation(Level::Error.span(span.into()).label(&error_string)),
                )
            }
            Error::UnknownConfigKey(span) => Level::Error.title("unknown config key").snippet(
                make_snippet()
                    .annotation(Level::Error.span(span.into()).label("unknown config key")),
            ),
        };

        let renderer = annotate_snippets::Renderer::styled();
        let render = renderer.render(message);
        render.fmt(f)
    }
}

impl std::error::Error for LocatedError<'_, Error> {}

impl LocatedError<'_, Error> {
    pub fn render(&self) -> String {
        self.to_string()
    }
}

impl<E> std::ops::Deref for LocatedError<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.error
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TomlParseError {
    #[error("empty identifier")]
    EmptyIdentifier,
    #[error("invalid char in identifier: {0}")]
    InvalidIdentifier(char),
    #[error(transparent)]
    InvalidExpr(StringExprParseError),
}

#[derive(Debug)]
pub struct StringExprParseError {
    pub offset: usize,
    pub error: ParseError,
}

impl std::fmt::Display for StringExprParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "string expression parse error at offset {}: {}",
            self.offset, self.error
        )
    }
}

impl std::error::Error for StringExprParseError {}

impl<T> From<winnow::error::ParseError<T, ParseError>> for StringExprParseError {
    fn from(value: winnow::error::ParseError<T, ParseError>) -> Self {
        Self {
            offset: value.offset(),
            error: value.into_inner(),
        }
    }
}

impl<T> From<winnow::error::ParseError<T, ParseError>> for TomlParseError {
    fn from(value: winnow::error::ParseError<T, ParseError>) -> Self {
        Self::InvalidExpr(value.into())
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub stack: Option<Box<Vec<ErrContext>>>,
    pub expected: Expected,
}

impl ParseError {
    #[inline]
    pub fn new(expected: Expected) -> Self {
        Self {
            stack: None,
            expected,
        }
    }

    #[inline]
    pub fn stack(&self) -> &[ErrContext] {
        if let Some(stack) = self.stack.as_deref() {
            stack
        } else {
            &[]
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, entry: ErrContext) {
        if let Some(ref mut stack) = self.stack {
            stack.push(entry);
        } else {
            self.stack = Some(Box::new(vec![entry]));
        }
    }
}

impl From<Expected> for ParseError {
    #[inline]
    fn from(expected: Expected) -> Self {
        Self::new(expected)
    }
}

impl<I: winnow::stream::Stream> winnow::error::ParserError<I> for ParseError {
    fn from_error_kind(_input: &I, kind: winnow::error::ErrorKind) -> Self {
        Self {
            stack: None,
            expected: Expected::Internal(kind),
        }
    }

    fn append(
        self,
        _input: &I,
        _token_start: &I::Checkpoint,
        _kind: winnow::error::ErrorKind,
    ) -> Self {
        self
    }
}

impl<I: winnow::stream::Stream> AddContext<I, Expected> for ParseError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        context: Expected,
    ) -> Self {
        self.expected = context;
        self
    }
}

impl<I: winnow::stream::Stream> AddContext<I, ErrContext> for ParseError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        context: ErrContext,
    ) -> Self {
        self.push(context);
        self
    }
}

impl<I: winnow::stream::Stream, E: std::error::Error + Send + Sync + 'static>
    FromExternalError<I, E> for ParseError
{
    fn from_external_error(_input: &I, _kind: winnow::error::ErrorKind, e: E) -> Self {
        ParseError {
            expected: Expected::External(Box::new(Arc::new(e))),
            stack: None,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expected)?;
        for entry in self.stack() {
            write!(f, "\n    {entry}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Expected {
    #[error("{0}")]
    Internal(winnow::error::ErrorKind),
    /// "expected ..."
    #[error("expected {0}")]
    Expected(&'static &'static str),
    #[error("expected keyword `{0}`")]
    ExpectedKeyword(&'static &'static str),
    #[error("expected character {0}")]
    ExpectedChar(char),
    #[error(transparent)]
    ValidRegex(Arc<regex::Error>),
    #[error(transparent)]
    External(Box<Arc<dyn std::error::Error + Send + Sync>>),
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrContext {
    #[error("in {0} expression")]
    KwExpr(&'static str),
    #[error("in string expression")]
    StringExpr,
    #[error("in pattern expression")]
    PatternExpr,
}
