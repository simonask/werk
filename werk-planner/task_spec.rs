use werk_eval::TaskName;
use werk_fs::Absolute;

use crate::RecipeMatch;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TaskSpec<'a> {
    /// The task name matched a build recipe.
    Recipe(RecipeMatch<'a>),
    /// The task name did not match a build recipe, so check if the file exists.
    CheckExists(Absolute<werk_fs::SymPath>),
    /// Check if the file exists, but don't emit an error if it doesn't. This
    /// applies to dependencies discovered through depfiles, where the depfile
    /// may be outdated (from a previous build).
    ///
    /// If the file does not exist, or if it is newer than the task output, the
    /// task will be considered outdated.
    CheckExistsRelaxed(Absolute<werk_fs::SymPath>),
}

impl TaskSpec<'_> {
    #[must_use]
    pub fn name(&self) -> TaskName {
        match self {
            TaskSpec::Recipe(recipe_match) => match recipe_match {
                RecipeMatch::Task(task_recipe) => TaskName::Task(task_recipe.name),
                RecipeMatch::Build(build_recipe_match) => {
                    TaskName::Build(build_recipe_match.target_file)
                }
            },
            TaskSpec::CheckExists(absolute) | TaskSpec::CheckExistsRelaxed(absolute) => {
                TaskName::Build(*absolute)
            }
        }
    }
}
