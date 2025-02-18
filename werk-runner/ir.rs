use indexmap::IndexMap;
use werk_fs::Absolute;
use werk_parser::ast;
use werk_util::{DiagnosticFileId, DiagnosticMainSourceMap, DiagnosticSpan, Symbol};

use crate::{
    cache::Hash128, AmbiguousPatternError, ConfigVar, EvalError, LocalVariables, Pattern,
    PatternMatchData, Value,
};

type Result<T, E = EvalError> = std::result::Result<T, E>;

/// Representation of the `Werkfile` manifest, partially evaluated.
///
/// - Global variables are fully evaluated.
/// - Recipe patterns are fully evaluated.
/// - Doc comments for all global items are gathered.
/// - Recipe bodies are *not* evaluated, and refer directly into the AST.
#[derive(Default)]
pub struct Manifest {
    /// `config` variables and their value at the point of declaration,
    /// collected across all included source files.
    pub config_variables: IndexMap<Symbol, ConfigVar>,
    /// `let` statements in the global scope.
    pub global_variables: LocalVariables,
    pub task_recipes: IndexMap<&'static str, TaskRecipe>,
    pub build_recipes: Vec<BuildRecipe>,
    /// Populated by `include` statements during evaluation of the root scope in
    /// the werkfile.
    pub source_map: DiagnosticMainSourceMap,
}

impl Manifest {
    pub fn set_config(
        &mut self,
        name: Symbol,
        value: Value,
        span: DiagnosticSpan,
        comment: String,
    ) -> Result<(), EvalError> {
        if let Some(previous_value) = self.config_variables.insert(
            name,
            ConfigVar {
                value,
                comment,
                span,
            },
        ) {
            return Err(EvalError::DuplicateConfigStatement(
                span,
                previous_value.span,
            ));
        }
        Ok(())
    }

    #[inline]
    #[must_use]
    pub fn match_task_recipe(&self, name: &str) -> Option<&TaskRecipe> {
        self.task_recipes.get(name)
    }

    pub fn match_build_recipe<'b>(
        &'b self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<BuildRecipeMatch<'b>>, AmbiguousPatternError> {
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
                            return Err(AmbiguousPatternError {
                                pattern1: best_recipe.pattern.span,
                                pattern2: candidate_recipe.pattern.span,
                                path: path.to_string(),
                            });
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
                    if let Some(task) = task {
                        return Err(crate::AmbiguousPatternError {
                            pattern1: build_recipe_match.recipe.pattern.span,
                            pattern2: task.ast.name.span.with_file(task.span.file),
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
    Task(&'a TaskRecipe),
    Build(BuildRecipeMatch<'a>),
}

pub struct BuildRecipeMatch<'a> {
    pub recipe: &'a BuildRecipe,
    pub match_data: PatternMatchData,
    pub target_file: Box<Absolute<werk_fs::Path>>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Edition {
    #[default]
    V1,
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

#[derive(Debug, Default, PartialEq)]
pub struct Defaults {
    pub output_directory: Option<String>,
    pub print_commands: Option<bool>,
    pub print_fresh: Option<bool>,
    pub quiet: Option<bool>,
    pub loud: Option<bool>,
    pub explain: Option<bool>,
    pub verbose: Option<bool>,
    pub watch_delay: Option<i32>,
    pub jobs: Option<i32>,
    pub edition: Edition,
}

impl Defaults {
    pub fn new(ast: &ast::Root, file: DiagnosticFileId) -> Result<Self> {
        let mut defaults = Self::default();
        for stmt in &ast.statements {
            let ast::RootStmt::Default(ref stmt) = stmt.statement else {
                continue;
            };

            match stmt {
                ast::DefaultStmt::Target(_) => continue, // Evaluated on workspace creation.
                ast::DefaultStmt::OutDir(entry) => {
                    defaults.output_directory = Some(entry.value.1.clone());
                }
                ast::DefaultStmt::PrintCommands(entry) => {
                    defaults.print_commands = Some(entry.value.1);
                }
                ast::DefaultStmt::PrintFresh(entry) => defaults.print_fresh = Some(entry.value.1),
                ast::DefaultStmt::Quiet(entry) => defaults.quiet = Some(entry.value.1),
                ast::DefaultStmt::Loud(entry) => defaults.loud = Some(entry.value.1),
                ast::DefaultStmt::Explain(entry) => defaults.explain = Some(entry.value.1),
                ast::DefaultStmt::Verbose(entry) => defaults.verbose = Some(entry.value.1),
                ast::DefaultStmt::WatchDelay(entry) => defaults.watch_delay = Some(entry.value.1),
                ast::DefaultStmt::Jobs(entry) => defaults.jobs = Some(entry.value.1),
                ast::DefaultStmt::Edition(entry) => {
                    if entry.value.1 == "v1" {
                        defaults.edition = Edition::V1;
                    } else {
                        return Err(EvalError::InvalidEdition(file.span(entry.value.0)));
                    }
                }
            }
        }

        Ok(defaults)
    }
}

impl werk_util::DiagnosticSourceMap for Manifest {
    #[inline]
    fn get_source(
        &self,
        id: werk_util::DiagnosticFileId,
    ) -> Option<werk_util::DiagnosticSource<'_>> {
        self.source_map.get_source(id)
    }
}
