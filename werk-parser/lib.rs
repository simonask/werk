#![allow(clippy::cast_possible_truncation)]

pub mod ast;
mod document;
mod error;
pub mod parser;
mod pattern;
mod semantic_hash;

pub use document::*;
pub use error::*;
pub use parser::parse_werk;
pub use pattern::*;
pub use semantic_hash::*;
