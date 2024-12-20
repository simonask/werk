use werk_fs::Path;
use werk_parser::ast;

use crate::{
    eval_pattern_builder, AmbiguousPatternError, Error, EvalError, Pattern, PatternMatch,
    PatternMatchData, PatternSet, RootScope, TaskId,
};

pub struct Recipes {
    pub ast: ast::Root,
    pub build_recipe_patterns: PatternSet,
}

pub enum RecipeMatch<'a> {
    Command {
        index: usize,
        name: &'a str,
        recipe: &'a ast::CommandRecipe,
    },
    Build {
        index: usize,
        recipe: &'a ast::BuildRecipe,
        pattern_match: PatternMatch<'a>,
        target_file: werk_fs::PathBuf,
    },
}

#[derive(Debug, Clone)]
pub enum RecipeMatchData {
    Command {
        index: usize,
        name: String,
    },
    Build {
        index: usize,
        pattern_match: PatternMatchData,
        target_file: werk_fs::PathBuf,
    },
}

impl std::fmt::Display for RecipeMatchData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecipeMatchData::Command { name, .. } => f.write_str(name),
            RecipeMatchData::Build { target_file, .. } => f.write_str(target_file.as_str()),
        }
    }
}

impl From<RecipeMatch<'_>> for RecipeMatchData {
    fn from(match_: RecipeMatch) -> Self {
        match match_ {
            RecipeMatch::Command { index, name, .. } => RecipeMatchData::Command {
                index,
                name: name.to_string(),
            },
            RecipeMatch::Build {
                index,
                pattern_match,
                target_file,
                ..
            } => RecipeMatchData::Build {
                index,
                pattern_match: pattern_match.data,
                target_file,
            },
        }
    }
}

impl Recipes {
    pub async fn new(root: ast::Root, global_scope: &RootScope<'_>) -> Result<Self, EvalError> {
        // Compile patterns.
        let mut build_recipe_patterns = PatternSet::default();
        for (pattern_expr, _) in root.recipes.iter() {
            let mut pattern_builder = eval_pattern_builder(global_scope, pattern_expr)?;
            // Ensure that build recipe patterns are absolute paths.
            pattern_builder.ensure_absolute_path();
            build_recipe_patterns.push(pattern_builder.build());
        }

        Ok(Self {
            ast: root,
            build_recipe_patterns,
        })
    }

    pub fn get_matched(&self, match_data: RecipeMatchData) -> RecipeMatch {
        match match_data {
            RecipeMatchData::Command { index, .. } => {
                let (name, recipe) = self
                    .ast
                    .commands
                    .get_index(index)
                    .expect("invalid command recipe index");
                RecipeMatch::Command {
                    index,
                    name,
                    recipe,
                }
            }
            RecipeMatchData::Build {
                index,
                pattern_match,
                target_file,
            } => {
                let recipe = &self.ast.recipes[index];
                RecipeMatch::Build {
                    index,
                    recipe,
                    pattern_match: PatternMatch::from_pattern_and_data(
                        &self.build_recipe_patterns[index],
                        pattern_match,
                    ),
                    target_file,
                }
            }
        }
    }

    pub fn build_recipes<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a Pattern, &'a ast::Commented<ast::BuildRecipe>)> {
        self.build_recipe_patterns
            .iter()
            .zip(self.ast.recipes.values())
    }

    #[inline]
    pub fn match_command_recipe<'b>(&'b self, name: &str) -> Option<RecipeMatch<'b>> {
        let (index, name, recipe) = self.ast.commands.get_full(name)?;
        Some(RecipeMatch::Command {
            index,
            name,
            recipe,
        })
    }

    pub fn match_build_recipe<'b>(&'b self, path: &Path) -> Result<Option<RecipeMatch<'b>>, Error> {
        let path = path.absolutize(Path::ROOT)?;
        tracing::trace!("Looking for build matching '{path}'");
        let best_match = self.build_recipe_patterns.best_match_path(&path)?;
        let Some((recipe_index, pattern_match)) = best_match else {
            return Ok(None);
        };
        let recipe = &self.ast.recipes[recipe_index];

        tracing::debug!("Found match: {path} -> {pattern_match}");
        let target_file = pattern_match.to_path_buf();
        Ok(Some(RecipeMatch::Build {
            index: recipe_index,
            pattern_match,
            recipe,
            target_file,
        }))
    }

    /// Match either a command or build recipe, returning an error if there is
    /// ambiguity. If there is no recipe that can build the target, returns
    /// `Ok(None)`.
    pub fn match_recipe_by_name<'b>(
        &'b self,
        name: &str,
    ) -> Result<Option<RecipeMatch<'b>>, Error> {
        let command = self.match_command_recipe(name);

        if let Ok(path) = Path::new(name) {
            let path = path.absolutize(Path::ROOT)?;
            tracing::debug!("looking for build rule: {path}");
            if let Some(out_match) = self.match_build_recipe(&path)? {
                // Check if we found a command with the same name.
                if command.is_some() {
                    return Err(AmbiguousPatternError {
                        pattern1: out_match.pattern_name(),
                        pattern2: name.to_owned(),
                        path: name.to_owned(),
                    }
                    .into());
                } else {
                    return Ok(Some(out_match));
                }
            }
        }

        Ok(command)
    }
}

impl RecipeMatch<'_> {
    pub fn is_verbatim(&self) -> bool {
        match self {
            RecipeMatch::Command { .. } => true,
            RecipeMatch::Build { pattern_match, .. } => pattern_match.is_verbatim(),
        }
    }

    pub fn stem(&self) -> Option<&str> {
        match self {
            RecipeMatch::Command { .. } => None,
            RecipeMatch::Build { pattern_match, .. } => pattern_match.stem(),
        }
    }

    pub fn pattern_name(&self) -> String {
        match self {
            RecipeMatch::Command { name, .. } => name.to_string(),
            RecipeMatch::Build { pattern_match, .. } => pattern_match.pattern.to_string(),
        }
    }

    pub fn capture_group(&self, group: usize) -> Option<&str> {
        match self {
            RecipeMatch::Command { .. } => None,
            RecipeMatch::Build { pattern_match, .. } => pattern_match.capture_group(group),
        }
    }

    #[inline]
    pub fn to_task_id(&self) -> TaskId {
        match self {
            RecipeMatch::Command { name, .. } => TaskId::command(name.to_string()),
            RecipeMatch::Build { target_file, .. } => TaskId::build(target_file.clone()),
        }
    }
}

impl RecipeMatchData {
    #[inline]
    pub fn to_task_id(&self) -> TaskId {
        match self {
            RecipeMatchData::Command { name, .. } => TaskId::command(name),
            RecipeMatchData::Build { target_file, .. } => TaskId::build(target_file.clone()),
        }
    }
}
