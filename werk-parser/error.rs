use std::sync::Arc;

use winnow::stream::Location;

use crate::parser::{Input, Offset, Span};

/// Owned version of `annotate_snippets::Message`.
pub trait DisplayError: std::fmt::Display {
    fn annotations(&self) -> Vec<DisplayAnnotation>;
}

/// Owned version of `annotate_snippets::Annotation`.
///
/// Note that parser errors pertain to a specific location in the input, so the
/// parser will only ever need one snippet per error.
pub struct DisplayAnnotation {
    pub level: annotate_snippets::Level,
    pub message: String,
    pub span: Span,
}

impl Error {
    #[must_use]
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

    #[must_use]
    pub fn span(&self) -> Span {
        Span::from_offset_and_len(self.offset, 0)
    }
}

#[derive(Debug)]
pub struct LocatedError<'a, E> {
    pub file_name: &'a std::path::Path,
    pub source_code: &'a str,
    pub error: E,
}

impl<E: DisplayError> std::fmt::Display for LocatedError<'_, E> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use annotate_snippets::Level;

        // Collect all annotations into an owned structure, because
        // `annotate-snippets` requires references, but we want to operate in
        // terms of `Display`.
        let annotations = self.error.annotations();

        let title = self.error.to_string();
        let origin = self.file_name.to_string_lossy();
        let mut message = Level::Error.title(&title);
        if !annotations.is_empty() {
            message = message.snippet(
                annotate_snippets::Snippet::source(self.source_code)
                    .origin(&origin)
                    .fold(true)
                    .annotations(
                        annotations
                            .iter()
                            .map(|a| a.level.span(a.span.into()).label(&a.message)),
                    ),
            );
        }

        let renderer = annotate_snippets::Renderer::styled();
        let render = renderer.render(message);
        render.fmt(f)
    }
}

impl<E: DisplayError + std::fmt::Debug> std::error::Error for LocatedError<'_, E> {}

impl LocatedError<'_, Error> {
    #[must_use]
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

#[derive(Debug)]
pub struct Error {
    pub context: Option<Box<Vec<ErrContext>>>,
    pub fail: Failure,
    pub offset: Offset,
}

impl Error {
    #[inline]
    #[must_use]
    pub fn new(offset: Offset, fail: Failure) -> Self {
        Self {
            context: None,
            fail,
            offset,
        }
    }

    #[inline]
    #[must_use]
    pub fn context(&self) -> &[ErrContext] {
        if let Some(stack) = self.context.as_deref() {
            stack
        } else {
            &[]
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, entry: ErrContext) {
        if let Some(ref mut stack) = self.context {
            stack.push(entry);
        } else {
            self.context = Some(Box::new(vec![entry]));
        }
    }
}

impl DisplayError for Error {
    fn annotations(&self) -> Vec<DisplayAnnotation> {
        let mut annotations = self.context().iter().map(Into::into).collect::<Vec<_>>();
        annotations.push(DisplayAnnotation {
            level: annotate_snippets::Level::Error,
            message: self.to_string(),
            span: self.offset.into(),
        });
        annotations
    }
}

impl From<&ErrContext> for DisplayAnnotation {
    fn from(context: &ErrContext) -> Self {
        use annotate_snippets::Level;
        let (offset, level) = match *context {
            ErrContext::Error(offset, _) => (offset, Level::Error),
            ErrContext::WhileParsing(offset, _) => (offset, Level::Info),
            ErrContext::Hint(offset, _) => (offset, Level::Help),
            ErrContext::Note(offset, _) => (offset, Level::Note),
        };
        DisplayAnnotation {
            level,
            message: context.to_string(),
            span: offset.into(),
        }
    }
}

impl winnow::error::ParserError<Input<'_>> for Error {
    fn from_error_kind(input: &Input<'_>, kind: winnow::error::ErrorKind) -> Self {
        let offset = Offset(input.location() as u32);
        Self {
            context: None,
            fail: Failure::Internal(kind),
            offset,
        }
    }

    fn append(
        self,
        _input: &Input<'_>,
        _token_start: &<Input<'_> as winnow::stream::Stream>::Checkpoint,
        _kind: winnow::error::ErrorKind,
    ) -> Self {
        self
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fail)
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Failure {
    #[error("{0}")]
    Internal(winnow::error::ErrorKind),
    /// "expected ..."
    #[error("expected {0}")]
    Expected(&'static &'static str),
    #[error("expected keyword `{0}`")]
    ExpectedKeyword(&'static &'static str),
    #[error("invalid escape sequence: {0:?}")]
    InvalidEscapeChar(char),
    #[error("expected character {0}")]
    ExpectedChar(char),
    #[error(transparent)]
    ValidRegex(Arc<regex::Error>),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
}

impl winnow::error::FromExternalError<Input<'_>, std::num::ParseIntError> for Error {
    fn from_external_error(
        input: &Input<'_>,
        _kind: winnow::error::ErrorKind,
        e: std::num::ParseIntError,
    ) -> Self {
        let location = input.location();
        Error {
            context: None,
            fail: Failure::ParseInt(e),
            offset: Offset(location as u32),
        }
    }
}

#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum ErrContext {
    #[error("{1}")]
    Error(Offset, &'static str),
    #[error("while parsing {}", .1)]
    WhileParsing(Offset, &'static str),
    #[error("{1}")]
    Hint(Offset, &'static str),
    #[error("{1}")]
    Note(Offset, &'static str),
}

pub(crate) fn fatal<'a, O>(failure: Failure) -> impl winnow::Parser<Input<'a>, O, Error> {
    let mut failure = Some(failure);
    fatal_with(move || failure.take().expect("fatal parser invoked multiple times"))
}

pub(crate) fn fatal_with<'a, O, F>(with: F) -> impl winnow::Parser<Input<'a>, O, Error>
where
    F: FnOnce() -> Failure,
{
    let mut with = Some(with);
    move |input: &mut Input<'a>| {
        let location = input.location();
        Err(winnow::error::ErrMode::Cut(Error {
            context: None,
            fail: with.take().expect("fatal parser invoked multiple times")(),
            offset: Offset(location as u32),
        }))
    }
}
