use std::ops::Range;

pub use annotate_snippets::Level;
use indexmap::IndexMap;

pub trait AnnotateLevelExt {
    fn diagnostic(self, id: &'static str) -> Diagnostic;
    fn annotation(
        self,
        span: impl Into<Range<usize>>,
        message: impl std::fmt::Display,
    ) -> DiagnosticAnnotation;
}

impl AnnotateLevelExt for Level {
    #[inline]
    #[must_use]
    fn diagnostic(self, id: &'static str) -> Diagnostic {
        Diagnostic {
            id,
            level: self,
            title: String::new(),
            snippets: vec![],
            footer: vec![],
        }
    }

    #[must_use]
    fn annotation(
        self,
        span: impl Into<Range<usize>>,
        message: impl std::fmt::Display,
    ) -> DiagnosticAnnotation {
        DiagnosticAnnotation {
            span: span.into(),
            level: self,
            message: message.to_string(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Annotated<T, R> {
    pub repository: R,
    pub error: T,
}

impl<T, R> Annotated<T, R> {
    pub fn map_err<F: FnOnce(T) -> U, U>(self, f: F) -> Annotated<U, R> {
        Annotated {
            repository: self.repository,
            error: f(self.error),
        }
    }
}

impl<T: AsDiagnostic, R: DiagnosticFileRepository + Sized> Annotated<T, R> {
    pub fn display<'a>(
        &'a self,
        renderer: &'a annotate_snippets::Renderer,
    ) -> impl std::fmt::Display + 'a {
        self.error.display(&self.repository, renderer)
    }
}

impl<T: std::fmt::Debug, R> std::fmt::Debug for Annotated<T, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl<T: AsDiagnostic, R: DiagnosticFileRepository> std::fmt::Display for Annotated<T, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let renderer = annotate_snippets::Renderer::styled();
        let display = self.display(&renderer);
        display.fmt(f)
    }
}

impl<T, R> std::error::Error for Annotated<T, R>
where
    T: AsDiagnostic + std::error::Error,
    R: DiagnosticFileRepository,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.error.source()
    }
}

/// An arbitrary ID for a file in a diagnostic.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DiagnosticFileId(pub u32);

impl DiagnosticFileId {
    #[must_use]
    pub fn snippet(
        self,
        annotations: impl IntoIterator<Item = DiagnosticAnnotation>,
    ) -> DiagnosticSnippet {
        DiagnosticSnippet {
            file_id: self,
            annotations: annotations.into_iter().collect(),
        }
    }
}

pub struct DiagnosticLocation {
    pub file: DiagnosticFileId,
    pub span: Range<usize>,
}

/// A source file used in diagnostics reporting.
///
/// The full location of a diagnostic is the source + span.
#[derive(Clone, Copy, Debug)]
pub struct DiagnosticSource<'a> {
    pub file: &'a str,
    pub source: &'a str,
}

impl<'a> DiagnosticSource<'a> {
    #[inline]
    #[must_use]
    pub fn new(file: &'a std::path::Path, source: &'a str) -> Self {
        Self {
            file: file.to_str().unwrap_or("<invalid UTF-8 in path"),
            source,
        }
    }
}

pub trait DiagnosticFileRepository {
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>>;
}

#[derive(Clone, Debug, Default)]
pub struct DiagnosticFileSourceMap {
    map: IndexMap<String, String>,
}

impl DiagnosticFileSourceMap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn insert(&mut self, file: String, source: String) -> DiagnosticFileId {
        let (index, _) = self.map.insert_full(file, source);
        DiagnosticFileId(index.try_into().unwrap())
    }
}

impl DiagnosticFileRepository for DiagnosticFileSourceMap {
    #[inline]
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        self.map
            .get_index(id.0 as usize)
            .map(|(file, source)| DiagnosticSource { file, source })
    }
}

impl DiagnosticFileRepository for DiagnosticSource<'_> {
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        if id == DiagnosticFileId(0) {
            Some(*self)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub id: &'static str,
    pub level: Level,
    /// Title on the first line.
    pub title: String,
    /// Snippets and annotations.
    pub snippets: Vec<DiagnosticSnippet>,
    /// Help strings in the footer.
    pub footer: Vec<String>,
}

impl Diagnostic {
    #[must_use]
    pub fn id(mut self, id: &'static str) -> Self {
        self.id = id;
        self
    }

    #[must_use]
    pub fn title(mut self, title: impl std::fmt::Display) -> Self {
        self.title = title.to_string();
        self
    }

    #[must_use]
    pub fn snippets(mut self, snippets: impl IntoIterator<Item = DiagnosticSnippet>) -> Self {
        self.snippets.extend(snippets);
        self
    }

    #[must_use]
    pub fn snippet(mut self, snippet: DiagnosticSnippet) -> Self {
        self.snippets.push(snippet);
        self
    }

    #[must_use]
    pub fn footers(mut self, footers: impl IntoIterator<Item = String>) -> Self {
        self.footer.extend(footers);
        self
    }

    #[must_use]
    pub fn footer(mut self, footer: impl std::fmt::Display) -> Self {
        self.footer.push(footer.to_string());
        self
    }
}

impl AsRef<Diagnostic> for Diagnostic {
    #[inline]
    fn as_ref(&self) -> &Diagnostic {
        self
    }
}

impl Diagnostic {
    #[inline]
    #[must_use]
    pub fn new(id: &'static str) -> Self {
        Self {
            id,
            level: Level::Error,
            title: String::new(),
            snippets: Vec::new(),
            footer: Vec::new(),
        }
    }

    pub fn display<'a>(
        &'a self,
        source_files: &'a dyn DiagnosticFileRepository,
        renderer: &'a annotate_snippets::Renderer,
    ) -> impl std::fmt::Display + 'a {
        Display(self, source_files, renderer)
    }
}

pub trait AsDiagnostic {
    fn as_diagnostic(&self) -> Diagnostic;

    fn display<'a>(
        &'a self,
        source_files: &'a dyn DiagnosticFileRepository,
        render: &'a annotate_snippets::Renderer,
    ) -> impl std::fmt::Display + 'a {
        Display(self.as_diagnostic(), source_files, render)
    }

    fn into_diagnostic_error<R: DiagnosticFileRepository>(
        self,
        source_files: R,
    ) -> Annotated<Self, R>
    where
        Self: Sized,
    {
        Annotated {
            repository: source_files,
            error: self,
        }
    }
}

struct Display<'a, D>(
    D,
    &'a dyn DiagnosticFileRepository,
    &'a annotate_snippets::Renderer,
);
impl<D: AsRef<Diagnostic>> std::fmt::Display for Display<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let diag = self.0.as_ref();
        let message = diag
            .level
            .title(&diag.title)
            .id(diag.id)
            .snippets(diag.snippets.iter().filter_map(|snippet| {
                let source = self.1.get_source(snippet.file_id)?;
                let annotations = snippet.annotations.iter().map(|annotation| {
                    annotation
                        .level
                        .span(annotation.span.clone())
                        .label(&annotation.message)
                });
                Some(
                    annotate_snippets::Snippet::source(source.source)
                        .origin(source.file)
                        .fold(true)
                        .annotations(annotations),
                )
            }))
            .footers(
                diag.footer
                    .iter()
                    .map(|footer| annotate_snippets::Level::Help.title(footer)),
            );
        let rendered = self.2.render(message);
        rendered.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticSnippet {
    pub file_id: DiagnosticFileId,
    pub annotations: Vec<DiagnosticAnnotation>,
}

#[derive(Debug, Clone)]
pub struct DiagnosticAnnotation {
    pub span: Range<usize>,
    pub level: Level,
    pub message: String,
}

pub struct DiagnosticAnnotationInfo {
    pub span: Range<usize>,
    pub message: String,
}
