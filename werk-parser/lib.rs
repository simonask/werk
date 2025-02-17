#![allow(clippy::cast_possible_truncation)]

pub mod ast;
mod error;
pub mod parser;
mod pattern;

pub use error::*;
pub use parser::{parse_werk, parse_werk_with_diagnostics};
pub use pattern::*;

pub fn extract_doc_comment<S: werk_util::DiagnosticSourceMap + ?Sized>(
    source_map: &S,
    file: werk_util::DiagnosticFileId,
    whitespace: ast::Whitespace,
) -> &str {
    let range: std::ops::Range<usize> = whitespace.0.into();
    source_map
        .get_source(file)
        .and_then(|source| source.source.get(range))
        .and_then(|s| s.trim().lines().map(str::trim).next())
        .unwrap_or("")
}

pub fn extract_whitespace<S: werk_util::DiagnosticSourceMap + ?Sized>(
    source_map: &S,
    file: werk_util::DiagnosticFileId,
    whitespace: ast::Whitespace,
) -> &str {
    let range: std::ops::Range<usize> = whitespace.0.into();
    source_map
        .get_source(file)
        .and_then(|source| source.source.get(range))
        .unwrap_or("")
}
