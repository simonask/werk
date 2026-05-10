use stringleton::Symbol;
use werk_eval::{Pattern, PatternMatchData};
use werk_fs::{Absolute, SymPath};
use werk_parser::ast;
use werk_util::{DiagnosticSpan, hash128::Hash128};

#[derive(Clone, Debug)]
pub enum RecipeMatch<'a> {
    Task(&'a TaskRecipe),
    Build(BuildRecipeMatch<'a>),
}

impl PartialEq for RecipeMatch<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Task(l0), Self::Task(r0)) => std::ptr::eq(l0, r0),
            (Self::Build(l0), Self::Build(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for RecipeMatch<'_> {}

impl std::hash::Hash for RecipeMatch<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            RecipeMatch::Task(task_recipe) => {
                std::ptr::from_ref::<TaskRecipe>(*task_recipe).hash(state);
            }
            RecipeMatch::Build(build_recipe_match) => build_recipe_match.hash(state),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BuildRecipeMatch<'a> {
    pub recipe: &'a BuildRecipe,
    pub match_data: PatternMatchData,
    pub target_file: Absolute<SymPath>,
}

impl PartialEq for BuildRecipeMatch<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.recipe, other.recipe)
            && self.match_data == other.match_data
            && self.target_file == other.target_file
    }
}

impl Eq for BuildRecipeMatch<'_> {}

impl std::hash::Hash for BuildRecipeMatch<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::from_ref::<BuildRecipe>(self.recipe).hash(state);
        self.match_data.hash(state);
        self.target_file.hash(state);
    }
}

#[derive(Debug, PartialEq)]
pub struct TaskRecipe {
    pub span: DiagnosticSpan,
    pub name: Symbol,
    pub doc_comment: String,
    pub ast: ast::TaskRecipe,
    pub hash: Hash128,
}

#[derive(PartialEq, Debug)]
pub struct BuildRecipe {
    pub span: DiagnosticSpan,
    pub pattern: Pattern,
    pub doc_comment: String,
    pub ast: ast::BuildRecipe,
    pub hash: Hash128,
}
