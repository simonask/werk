use indexmap::IndexMap;
use werk_fs::Absolute;
use werk_parser::{ast, parser::Span};

use crate::{
    cache::Hash128, compute_stable_hash, compute_stable_semantic_hash, eval, eval_pattern_builder,
    Eval, EvalError, GlobalVar, GlobalVariables, Io, Pattern, PatternMatchData, RootScope,
    UsedVariable, Value, Workspace,
};

type Result<T, E = EvalError> = std::result::Result<T, E>;

pub struct Document<'a> {
    pub globals: GlobalVariables,
    pub task_recipes: IndexMap<&'a str, TaskRecipe<'a>>,
    pub build_recipes: Vec<BuildRecipe<'a>>,
}

impl<'a> Document<'a> {
    /// Evaluate global variables, tasks, and recipe patterns. Also gathers
    /// documentation for each global item.
    pub async fn compile(
        doc: werk_parser::Document<'a>,
        io: &dyn Io,
        workspace: &Workspace,
        watcher: &dyn crate::Watcher,
    ) -> Result<Self> {
        let mut task_recipes = IndexMap::new();
        let mut build_recipes = Vec::new();
        let mut globals = GlobalVariables::default();

        let smuggled_whitespace = doc.smuggled_whitespace;
        let source = doc.source;
        let get_doc_comment = |ws: ast::Whitespace| {
            let range = ws.0.start.0 as usize..ws.0.end.0 as usize;
            if let Some(ref smuggled) = smuggled_whitespace {
                smuggled.get(range).unwrap_or_default().trim()
            } else {
                source.get(range).unwrap_or_default().trim()
            }
        };

        for stmt in doc.root.statements {
            let doc_comment = get_doc_comment(stmt.ws_pre).to_string();

            match stmt.statement {
                ast::RootStmt::Config(_) => continue,
                ast::RootStmt::Let(let_stmt) => {
                    if let Some(global_override) = workspace.defines.get(let_stmt.ident.ident) {
                        tracing::trace!(
                            "overriding global var `{}` with `{}`",
                            let_stmt.ident,
                            global_override
                        );
                        globals.insert(
                            let_stmt.ident.ident.to_owned(),
                            GlobalVar {
                                value: Eval::using_var(
                                    Value::String(global_override.clone()),
                                    UsedVariable::Define(
                                        let_stmt.ident.ident.to_owned(),
                                        compute_stable_hash(global_override),
                                    ),
                                ),
                                comment: doc_comment,
                            },
                        );
                    } else {
                        let scope = RootScope::new(&globals, workspace, watcher);
                        let value = eval(&scope, &*io, &let_stmt.value).await?;
                        tracing::trace!("(global) let `{}` = {:?}", let_stmt.ident, value);
                        globals.insert(
                            let_stmt.ident.ident.to_owned(),
                            GlobalVar {
                                value,
                                comment: doc_comment,
                            },
                        );
                    }
                }
                ast::RootStmt::Task(command_recipe) => {
                    let hash = compute_stable_semantic_hash(&command_recipe);
                    task_recipes.insert(
                        command_recipe.name.ident,
                        TaskRecipe {
                            span: command_recipe.span,
                            name: command_recipe.name.ident,
                            doc_comment,
                            body: command_recipe.body.statements,
                            hash,
                        },
                    );
                }
                ast::RootStmt::Build(build_recipe) => {
                    let hash = compute_stable_semantic_hash(&build_recipe);
                    let scope = RootScope::new(&globals, workspace, watcher);
                    let mut pattern_builder =
                        eval_pattern_builder(&scope, &build_recipe.pattern)?.value;

                    // TODO: Consider if it isn't better to do this while matching recipes.
                    pattern_builder.ensure_absolute_path();

                    build_recipes.push(BuildRecipe {
                        span: build_recipe.span,
                        pattern: pattern_builder.build(),
                        doc_comment,
                        body: build_recipe.body.statements,
                        hash,
                    });
                }
            }
        }

        // Warn about defines set on the command-line that have no effect.
        for (key, _) in &workspace.defines {
            if !globals.contains_key(key) {
                watcher.warning(None, &format!("Unused define: {key}"));
            }
        }

        Ok(Self {
            globals,
            task_recipes,
            build_recipes,
        })
    }

    #[inline]
    pub fn match_task_recipe(&self, name: &str) -> Option<&TaskRecipe<'a>> {
        self.task_recipes.get(name)
    }

    pub fn match_build_recipe<'b>(
        &'b self,
        path: &Absolute<werk_fs::Path>,
    ) -> Result<Option<BuildRecipeMatch<'b>>, crate::Error> {
        let matches = self.build_recipes.iter().filter_map(|recipe| {
            if let Some(match_data) = recipe.pattern.match_path(&path) {
                Some((recipe, match_data))
            } else {
                None
            }
        });

        let mut best_match = None;

        for (candidate_recipe, candidate_pattern_match) in matches {
            match best_match {
                None => {
                    // No match yet, pick this candidate.
                    best_match = Some((candidate_recipe, candidate_pattern_match));
                }
                Some((ref best_recipe, ref best_data)) => {
                    match (best_data.stem(), candidate_pattern_match.stem()) {
                        (None, Some(_)) => {
                            // Best match is exact, do nothing.
                        }
                        (Some(_), None) => {
                            // Candidate is exact, do nothing.
                            best_match = Some((candidate_recipe, candidate_pattern_match))
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
            if let Some(path) = Absolute::try_new_ref(path) {
                if let Some(build_recipe_match) = self.match_build_recipe(path)? {
                    if task.is_some() {
                        return Err(crate::AmbiguousPatternError {
                            pattern1: build_recipe_match.recipe.pattern.to_string(),
                            pattern2: name.to_owned(),
                            path: name.to_owned(),
                        }
                        .into());
                    } else {
                        return Ok(Some(RecipeMatch::Build(build_recipe_match)));
                    }
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
    pub body: Vec<ast::BodyStmt<ast::TaskRecipeStmt<'a>>>,
    pub hash: Hash128,
}

#[derive(Debug)]
pub struct BuildRecipe<'a> {
    pub span: Span,
    pub pattern: Pattern<'a>,
    pub doc_comment: String,
    pub body: Vec<ast::BodyStmt<ast::BuildRecipeStmt<'a>>>,
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

            match config_stmt.ident.ident {
                "edition" => {
                    let edition = match config_stmt.value {
                        ast::ConfigValue::String("v1") => Edition::V1,
                        _ => return Err(EvalError::InvalidEdition(config_stmt.span)),
                    };
                    config.edition = edition;
                }
                "out-dir" | "output-directory" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::String(value) => value.to_string(),
                        _ => return Err(EvalError::ExpectedConfigString(config_stmt.span)),
                    };
                    config.output_directory = Some(value);
                }
                "print-commands" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::Bool(value) => value,
                        _ => return Err(EvalError::ExpectedConfigBool(config_stmt.span)),
                    };
                    config.print_commands = Some(value);
                }
                "default" | "default-target" => {
                    let value = match config_stmt.value {
                        ast::ConfigValue::String(value) => value.to_string(),
                        _ => return Err(EvalError::ExpectedConfigString(config_stmt.span)),
                    };
                    config.default_target = Some(value);
                }
                _ => return Err(EvalError::UnknownConfigKey(config_stmt.ident.span)),
            }
        }

        Ok(config)
    }
}
