#![allow(clippy::cast_possible_truncation)]

pub mod ast;
mod document;
mod error;
pub mod parse_string;
mod parse_toml;
pub mod parser;
mod pattern;
mod semantic_hash;

pub use document::*;
pub use error::*;
pub use parse_toml::{parse_toml, parse_toml_document};
pub use parser::parse_werk;
pub use pattern::*;
pub use semantic_hash::*;
