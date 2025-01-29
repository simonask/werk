use indexmap::IndexMap;
use werk_fs::Absolute;
use werk_parser::{ast, parser::Span};

use crate::{cache::Hash128, EvalError, GlobalVariables, Pattern, PatternMatchData};

type Result<T, E = EvalError> = std::result::Result<T, E>;

/// Representation of the `Werkfile` manifest, partially evaluated.
///
/// - Global variables are fully evaluated.
/// - Recipe patterns are fully evaluated.
/// - Doc comments for all global items are gathered.
/// - Recipe bodies are *not* evaluated, and refer directly into the AST.
#[derive(Default)]
pub struct Manifest<'a> {
    pub globals: GlobalVariables,
    pub task_recipes: IndexMap<&'a str, TaskRecipe<'a>>,
    pub build_recipes: Vec<BuildRecipe<'a>>,
}

impl<'a> Manifest<'a> {
    #[inline]
    #[must_use]
    pub fn match_task_recipe(&self, name: &str) -> Option<&TaskRecipe<'a>> {
        self.task_recipes.get(name)
    }

    pub fn match_build_recipe<'b>(
        &'b self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<BuildRecipeMatch<'b>>, crate::Error> {
        let matches = self.build_recipes.iter().filter_map(|recipe| {
            recipe
                .pattern
                .match_whole_path(path)
                .map(|match_data| (recipe, match_data))
        });

        let mut best_match = None;

        for (candidate_recipe, candidate_pattern_match) in matches {
            match best_match {
                None => {
                    // No match yet, pick this candidate.
                    best_match = Some((candidate_recipe, candidate_pattern_match));
                }
                Some((best_recipe, ref best_data)) => {
                    match (best_data.stem(), candidate_pattern_match.stem()) {
                        (None, Some(_)) => {
                            // Best match is exact, do nothing.
                        }
                        (Some(_), None) => {
                            // Candidate is exact, do nothing.
                            best_match = Some((candidate_recipe, candidate_pattern_match));
                        }
                        (Some(best_stem), Some(candidate_stem))
                            if candidate_stem.len() < best_stem.len() =>
                        {
                            // Candidate has a shorter stem, so it's better.
                            best_match = Some((candidate_recipe, candidate_pattern_match));
                        }
                        (Some(best_stem), Some(candidate_stem))
                            if candidate_stem.len() > best_stem.len() =>
                        {
                            // Candidate has a longer stem, so it's worse; do nothing.
                        }
                        _ => {
                            // Candidate and best have the same length, or both are
                            // exact.
                            return Err(crate::AmbiguousPatternError {
                                pattern1: best_recipe.pattern.to_string(),
                                pattern2: candidate_recipe.pattern.to_string(),
                                path: path.to_string(),
                            }
                            .into());
                        }
                    }
                }
            }
        }

        Ok(best_match.map(|(recipe, match_data)| BuildRecipeMatch {
            recipe,
            match_data,
            target_file: path.to_owned().into_boxed_path(),
        }))
    }

    pub fn match_recipe_by_name<'b>(
        &'b self,
        name: &str,
    ) -> Result<Option<RecipeMatch<'b>>, crate::Error> {
        let task = self.match_task_recipe(name);

        if let Ok(path) = werk_fs::Path::new(name) {
            if let Ok(path) = path.normalize() {
                if let Some(build_recipe_match) = self.match_build_recipe(&path)? {
                    if task.is_some() {
                        return Err(crate::AmbiguousPatternError {
                            pattern1: build_recipe_match.recipe.pattern.to_string(),
                            pattern2: name.to_owned(),
                            path: name.to_owned(),
                        }
                        .into());
                    }

                    return Ok(Some(RecipeMatch::Build(build_recipe_match)));
                }
            }
        }

        Ok(task.map(RecipeMatch::Task))
    }
}

pub enum RecipeMatch<'a> {
    Task(&'a TaskRecipe<'a>),
    Build(BuildRecipeMatch<'a>),
}

pub struct BuildRecipeMatch<'a> {
    pub recipe: &'a BuildRecipe<'a>,
    pub match_data: PatternMatchData,
    pub target_file: Box<Absolute<werk_fs::Path>>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Edition {
    #[default]
    V1,
}

#[derive(Debug)]
pub struct TaskRecipe<'a> {
    pub span: Span,
    pub name: &'a str,
    pub doc_comment: String,
    pub body: &'a [ast::BodyStmt<ast::TaskRecipeStmt<'a>>],
    pub hash: Hash128,
}

#[derive(Debug)]
pub struct BuildRecipe<'a> {
    pub span: Span,
    pub pattern: Pattern<'a>,
    pub doc_comment: String,
    pub body: &'a [ast::BodyStmt<ast::BuildRecipeStmt<'a>>],
    pub hash: Hash128,
}

#[derive(Debug, Default, PartialEq)]
pub struct Config {
    pub edition: Edition,
    pub output_directory: Option<String>,
    pub print_commands: Option<bool>,
    pub default_target: Option<String>,
}

impl Config {
    pub fn new(doc: &werk_parser::Document) -> Result<Self> {
        let mut config = Self::default();
        for stmt in &doc.root.statements {
            let ast::RootStmt::Config(ref config_stmt) = stmt.statement else {
                continue;
            };

            match &*config_stmt.ident.ident {
                "edition" => {
                    let edition = match config_stmt.value {
                        ast::ConfigValue::String(ast::ConfigString(_, ref edition))
                            if edition == "v1" =>
                        {
                            Edition::V1
                        }
                        _ => return Err(EvalError::InvalidEdition(config_stmt.span)),
                    };
                    config.edition = edition;
                }
                "out-dir" | "output-directory" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::String(ast::ConfigString(_, ref value)) => {
                            value.to_string()
                        }
                        ast::ConfigValue::Bool(_) => {
                            return Err(EvalError::ExpectedConfigString(config_stmt.span))
                        }
                    };
                    config.output_directory = Some(value);
                }
                "print-commands" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::Bool(ast::ConfigBool(_, ref value)) => *value,
                        ast::ConfigValue::String(_) => {
                            return Err(EvalError::ExpectedConfigBool(config_stmt.span))
                        }
                    };
                    config.print_commands = Some(value);
                }
                "default" | "default-target" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::String(ast::ConfigString(_, ref value)) => {
                            value.to_string()
                        }
                        ast::ConfigValue::Bool(_) => {
                            return Err(EvalError::ExpectedConfigString(config_stmt.span))
                        }
                    };
                    config.default_target = Some(value);
                }
                _ => return Err(EvalError::UnknownConfigKey(config_stmt.ident.span)),
            }
        }

        Ok(config)
    }
}
