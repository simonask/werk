pub use annotate_snippets::Level;
use indexmap::{IndexMap, map::Entry};

use crate::DiagnosticSpan;

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

impl<T: AsDiagnostic, R: DiagnosticSourceMap + Sized> Annotated<T, R> {
    pub fn display<'a>(
        &'a self,
        renderer: &'a annotate_snippets::Renderer,
    ) -> impl std::fmt::Display + 'a {
        Display(&self.error, &self.repository, renderer)
    }
}

impl<T: std::fmt::Debug, R> std::fmt::Debug for Annotated<T, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl<T: AsDiagnostic, R: DiagnosticSourceMap> std::fmt::Display for Annotated<T, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let renderer = annotate_snippets::Renderer::styled();
        let display = self.display(&renderer);
        display.fmt(f)
    }
}

impl<T, R> std::error::Error for Annotated<T, R>
where
    T: AsDiagnostic + std::error::Error,
    R: DiagnosticSourceMap,
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
    pub fn span(self, span: impl Into<crate::Span>) -> DiagnosticSpan {
        DiagnosticSpan {
            file: self,
            span: span.into(),
        }
    }
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

pub trait DiagnosticSourceMap {
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>>;
}

impl<T: DiagnosticSourceMap + ?Sized> DiagnosticSourceMap for &T {
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        T::get_source(self, id)
    }
}

#[derive(Clone, Debug, Default)]
pub struct DiagnosticMainSourceMap {
    map: IndexMap<String, SourceMapEntry>,
}

#[derive(Clone, Debug, Default)]
struct SourceMapEntry {
    pub source: String,
    pub included_from: Option<DiagnosticSpan>,
}

impl DiagnosticMainSourceMap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    #[must_use]
    pub fn insert(
        &mut self,
        file: String,
        source: String,
        included_from: Option<DiagnosticSpan>,
    ) -> DiagnosticFileId {
        let (index, _) = self.map.insert_full(
            file,
            SourceMapEntry {
                source,
                included_from,
            },
        );
        DiagnosticFileId(index.try_into().unwrap())
    }

    #[inline]
    pub fn insert_check_duplicate(
        &mut self,
        file: String,
        source: String,
        included_from: Option<DiagnosticSpan>,
    ) -> Result<DiagnosticFileId, Option<DiagnosticSpan>> {
        match self.map.entry(file) {
            Entry::Occupied(occupied_entry) => Err(occupied_entry.get().included_from),
            Entry::Vacant(vacant_entry) => {
                let index = vacant_entry.index();
                vacant_entry.insert(SourceMapEntry {
                    source,
                    included_from,
                });
                Ok(DiagnosticFileId(index.try_into().unwrap()))
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn get_included_at(&self, id: DiagnosticFileId) -> Option<DiagnosticSpan> {
        self.map
            .get_index(id.0 as usize)
            .and_then(|(_, entry)| entry.included_from)
    }

    #[inline]
    pub fn clear(&mut self) {
        self.map.clear();
    }
}

impl DiagnosticSourceMap for DiagnosticMainSourceMap {
    #[inline]
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        self.map
            .get_index(id.0 as usize)
            .map(|(file, SourceMapEntry { source, .. })| DiagnosticSource { file, source })
    }
}

#[derive(Default)]
pub struct DiagnosticSecondarySourceMap {
    map: Vec<Option<(String, String)>>,
}

impl DiagnosticSecondarySourceMap {
    #[inline]
    pub fn insert(&mut self, id: DiagnosticFileId, file: String, source: String) {
        let index = id.0 as usize;

        // Extend the map with empty entries.
        for _ in self.map.len()..=index {
            self.map.push(None);
        }

        self.map[index] = Some((file, source));
    }

    #[inline]
    pub fn clear(&mut self) {
        self.map.clear();
    }
}

impl DiagnosticSourceMap for DiagnosticSecondarySourceMap {
    #[inline]
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        self.map.get(id.0 as usize).and_then(|opt| {
            opt.as_ref()
                .map(|(file, source)| DiagnosticSource { file, source })
        })
    }
}

impl DiagnosticSourceMap for DiagnosticSource<'_> {
    fn get_source(&self, id: DiagnosticFileId) -> Option<DiagnosticSource<'_>> {
        if id == DiagnosticFileId(0) {
            Some(*self)
        } else {
            None
        }
    }
}

pub trait AsDiagnostic {
    fn as_diagnostic<'a>(
        &'a self,
        source_map: &'a dyn DiagnosticSourceMap,
    ) -> Vec<annotate_snippets::Group<'a>>;

    fn into_diagnostic_error<R: DiagnosticSourceMap>(self, source_files: R) -> Annotated<Self, R>
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
    &'a D,
    &'a dyn DiagnosticSourceMap,
    &'a annotate_snippets::Renderer,
);
impl<D: AsDiagnostic> std::fmt::Display for Display<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let report = self.0.as_diagnostic(self.1);
        let rendered = self.2.render(&report);
        f.write_str(&rendered)
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticAnnotation {
    pub span: DiagnosticSpan,
    pub level: Level<'static>,
    pub message: String,
}
