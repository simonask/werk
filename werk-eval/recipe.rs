use stringleton::Symbol;
use werk_fs::Absolute;
use werk_parser::ast;
use werk_util::{DiagnosticSpan, hash128::Hash128};

use crate::{Pattern, PatternMatchData};

pub enum RecipeMatch<'a> {
    Task(&'a TaskRecipe),
    Build(BuildRecipeMatch<'a>),
}

pub struct BuildRecipeMatch<'a> {
    pub recipe: &'a BuildRecipe,
    pub match_data: PatternMatchData,
    pub target_file: Box<Absolute<werk_fs::Path>>,
}

#[derive(Debug)]
pub struct TaskRecipe {
    pub span: DiagnosticSpan,
    pub name: Symbol,
    pub doc_comment: String,
    pub ast: ast::TaskRecipe,
    pub hash: Hash128,
}

#[derive(Debug)]
pub struct BuildRecipe {
    pub span: DiagnosticSpan,
    pub pattern: Pattern,
    pub doc_comment: String,
    pub ast: ast::BuildRecipe,
    pub hash: Hash128,
}
