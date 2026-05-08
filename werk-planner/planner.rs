use ahash::HashSet;
use stringleton::sym;
use werk_depfile::Depfile;
use werk_eval::{Eval, Scope, TaskName, Value, Warning};
use werk_fs::{Absolute, Normalize};
use werk_util::{Annotated, AsDiagnostic, DiagnosticSpan, Spanned as _};

use crate::{
    AmbiguousPatternError, BuildRecipeMatch, BuildRecipeScope, EvaluatedTask, Manifest,
    PlannerError, RecipeMatch, TaskGraph, TaskId, TaskRecipe, TaskRecipeScope, TaskSpec,
};

pub struct Planner<'a> {
    graph: TaskGraph<'a>,
    manifest: &'a Manifest,

    pending_goals: Vec<TaskId>,
    /// Dependencies discovered through depfiles, which shouldn't raise errors
    /// in case of cycles or other reasons.
    tentative_dependencies: Vec<(TaskId, TaskId)>,
}

impl<'a> Planner<'a> {
    #[must_use] 
    pub fn new(manifest: &'a Manifest) -> Self {
        Self {
            graph: TaskGraph::default(),
            manifest,
            pending_goals: vec![],
            tentative_dependencies: vec![],
        }
    }

    pub fn add_goal_by_name(&mut self, name: &str) -> Result<TaskId, Annotated<'a, PlannerError>> {
        let task_recipe_match = self.manifest.match_task_recipe(name);

        if let Ok(path) = werk_fs::Path::new(name).and_then(|p| p.absolutize(werk_fs::Path::ROOT)) {
            if let Some(build_recipe_match) =
                self.manifest.match_build_recipe(&path).map_err(|e| {
                    PlannerError::AmbiguousPattern(e).into_diagnostic_error(self.manifest)
                })?
            {
                // Check for name collisions between task and build recipes.
                if let Some(task_recipe) = task_recipe_match {
                    return Err(PlannerError::AmbiguousPattern(AmbiguousPatternError {
                        pattern1: task_recipe.span,
                        pattern2: build_recipe_match.recipe.span,
                        path: path.to_string(),
                    })
                    .into_diagnostic_error(self.manifest));
                }

                return Ok(
                    self.add_goal_by_spec(TaskSpec::Recipe(RecipeMatch::Build(build_recipe_match)))
                );
            } else if task_recipe_match.is_none() {
                return Ok(
                    self.add_goal_by_spec(TaskSpec::CheckExists(Absolute::symbolicate(path)))
                );
            }
        }

        match task_recipe_match {
            Some(task_recipe) => {
                Ok(self.add_goal_by_spec(TaskSpec::Recipe(RecipeMatch::Task(task_recipe))))
            }
            // This branch only gets hit if the name was not also a valid path.
            None => Err(PlannerError::NoRuleToBuildTarget(name.to_string())
                .into_diagnostic_error(self.manifest)),
        }
    }

    fn add_goal_by_spec(&mut self, spec: TaskSpec<'a>) -> TaskId {
        let task_id = self.graph.get_or_insert_task_spec(spec);
        self.pending_goals.push(task_id);
        task_id
    }

    /// Discovers all dependencies between tasks and builds a task graph.
    ///
    /// This fully evaluates all statements in all tasks, but does not execute commands.
    pub fn plan(
        mut self,
        global_scope: &dyn Scope,
    ) -> Result<TaskGraph<'a>, Annotated<'a, PlannerError>> {
        while let Some(task_id) = self.pending_goals.pop() {
            if self.graph.is_evaluated(task_id) {
                continue;
            }

            let spec = self.graph.get_task_spec(task_id).clone();
            match spec {
                TaskSpec::Recipe(RecipeMatch::Build(build_recipe_match)) => {
                    tracing::trace!("Planning build recipe: {}", build_recipe_match.target_file);
                    self.evaluate_and_plan_build_recipe(task_id, build_recipe_match, global_scope)?;
                }
                TaskSpec::Recipe(RecipeMatch::Task(task_recipe_match)) => {
                    tracing::trace!("Planning task recipe: {}", task_recipe_match.name);
                    self.evaluate_and_plan_task_recipe(task_id, task_recipe_match, global_scope)?;
                }
                TaskSpec::CheckExists(path) | TaskSpec::CheckExistsRelaxed(path) => {
                    tracing::trace!("Planning check-exists: {path}");
                    self.graph
                        .set_evaluated(task_id, EvaluatedTask::CheckExists);
                }
            }
        }

        self.graph.check_circular_dependencies().map_err(|e| {
            PlannerError::CircularDependency(e).into_diagnostic_error(self.manifest)
        })?;

        let mut tentative_dependency_scratch = HashSet::default();
        for (task_id, potential_dependency) in self.tentative_dependencies.drain(..) {
            if self.graph.would_cause_circular_dependency(
                task_id,
                potential_dependency,
                &mut tentative_dependency_scratch,
            ) {
                global_scope.warning(&Warning::custom(None, format_args!("Ignoring dependency discovered via depfile, because it would cause a circular dependency: {} -> {}", self.graph.get_task_spec(task_id).name(), self.graph.get_task_spec(potential_dependency).name())));
            } else {
                self.graph.add_dependency(task_id, potential_dependency);
            }
        }

        Ok(self.graph)
    }

    fn evaluate_and_plan_task_recipe(
        &mut self,
        task_id: TaskId,
        task_recipe_match: &'a TaskRecipe,
        global_scope: &dyn Scope,
    ) -> Result<(), Annotated<'a, PlannerError>> {
        let mut scope = TaskRecipeScope::new(global_scope, TaskName::Task(task_recipe_match.name));
        let evaluated = werk_eval::eval_task_recipe_statements(
            &mut scope,
            &task_recipe_match.ast.body.statements,
            task_recipe_match.span.file,
        )
        .map_err(|e| PlannerError::Evaluation(e).into_diagnostic_error(self.manifest))?;
        // Discover dependencies and create new tasks for them.
        for dep in &evaluated.build {
            let dep_id = self.add_goal_by_name(&dep.string)?;
            self.graph.add_dependency(task_id, dep_id);
        }

        self.graph
            .set_evaluated(task_id, EvaluatedTask::Task(evaluated));
        Ok(())
    }

    fn evaluate_and_plan_build_recipe(
        &mut self,
        task_id: TaskId,
        build_recipe_match: BuildRecipeMatch<'a>,
        global_scope: &dyn Scope,
    ) -> Result<(), Annotated<'a, PlannerError>> {
        let mut scope = BuildRecipeScope::new(
            global_scope,
            TaskName::Build(build_recipe_match.target_file),
            &build_recipe_match,
        );
        scope.set(
            sym!(out),
            Eval::inherent(Value::from(build_recipe_match.target_file.to_string())),
        );

        let evaluated = werk_eval::eval_build_recipe_statements(
            &mut scope,
            &build_recipe_match.recipe.ast.body.statements,
            build_recipe_match.recipe.span.file,
        )
        .map_err(|e| PlannerError::Evaluation(e).into_diagnostic_error(self.manifest))?;
        // Discover dependencies and create new tasks for them.
        for dep in &evaluated.explicit_dependencies {
            let dep_id = self.add_goal_by_name(&dep.string)?;
            self.graph.add_dependency(task_id, dep_id);
        }
        if let Some(depfile) = evaluated.depfile.as_ref() {
            // For diagnostics, find the `depfile` statement.
            let depfile_span = build_recipe_match
                .recipe
                .ast
                .body
                .statements
                .iter()
                .find_map(|stmt| match stmt.statement {
                    werk_parser::ast::BuildRecipeStmt::Depfile(ref kw_expr) => Some(kw_expr.span),
                    _ => None,
                })
                .unwrap_or(build_recipe_match.recipe.ast.token_build.span());

            self.evaluate_build_recipe_depfile(
                task_id,
                depfile_span.with_file(build_recipe_match.recipe.span.file),
                depfile,
                &scope,
            )?;
        }
        self.graph
            .set_evaluated(task_id, EvaluatedTask::Build(evaluated));
        Ok(())
    }

    fn evaluate_build_recipe_depfile(
        &mut self,
        task_id: TaskId,
        span: DiagnosticSpan,
        depfile_path: &str,
        scope: &BuildRecipeScope<'_>,
    ) -> Result<(), Annotated<'a, PlannerError>> {
        let Ok(depfile_path) = werk_fs::Path::new(depfile_path) else {
            scope.warning(&Warning::custom(
                Some(span),
                format_args!("invalid depfile path '{depfile_path}'; ignoring"),
            ));
            return Ok(());
        };
        let depfile_path = match depfile_path.absolutize(werk_fs::Path::ROOT) {
            Ok(depfile_path) => depfile_path,
            Err(err) => {
                scope.warning(&Warning::custom(
                    Some(span),
                    format_args!(
                        "error absolutizing depfile path '{depfile_path}': {err}; ignoring"
                    ),
                ));
                return Ok(());
            }
        };

        match self.manifest.match_build_recipe(&depfile_path) {
            Ok(Some(recipe_match)) => {
                let depfile_task_id =
                    self.add_goal_by_spec(TaskSpec::Recipe(RecipeMatch::Build(recipe_match)));
                self.graph.add_dependency(task_id, depfile_task_id);
            }
            Ok(None) => (),
            Err(err) => {
                scope.warning(&Warning::custom(
                    Some(span),
                    format_args!("ambiguous depfile recipe: {err}; ignoring"),
                ));
            }
        }

        let depfile_os_path = match scope
            .resolve_path(&depfile_path, werk_eval::ResolvePathMode::OutDir)
        {
            Ok(depfile_os_path) => depfile_os_path,
            Err(err) => {
                scope.warning(&Warning::custom(
                    Some(span),
                    format_args!("error resolving depfile path '{depfile_path}': {err}; ignoring"),
                ));
                return Ok(());
            }
        };

        let depfile_contents = match scope.io().read_file(&depfile_os_path) {
            Ok(depfile_contents) => depfile_contents,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(err) => {
                scope.warning(&Warning::custom(
                    Some(span),
                    format_args!("error reading depfile '{depfile_path}': {err}; ignoring"),
                ));
                return Ok(());
            }
        };

        let depfile = match Depfile::parse(&depfile_contents) {
            Ok(depfile) => depfile,
            Err(err) => {
                scope.warning(&Warning::custom(
                    Some(span),
                    format_args!("error parsing depfile '{depfile_path}': {err}; ignoring"),
                ));
                return Ok(());
            }
        };

        for dep in &depfile.deps {
            let dep_os_path = match dep.as_path().normalize() {
                Ok(dep_path) => dep_path,
                Err(err) => {
                    scope.warning(&Warning::custom(
                        Some(span),
                        format_args!(
                            "error normalizing depfile path '{}': {err}; ignoring",
                            dep.display()
                        ),
                    ));
                    continue;
                }
            };
            let dep_path = match scope.unresolve_path(&dep_os_path) {
                Ok(dep_path) => dep_path,
                Err(err) => {
                    scope.warning(&Warning::custom(
                        Some(span),
                        format_args!(
                            "error converting depfile path to workspace path '{}': {err}; ignoring",
                            dep.display()
                        ),
                    ));
                    continue;
                }
            };

            match self.manifest.match_build_recipe(&dep_path) {
                Ok(Some(dep_task_match)) => {
                    let dep_task_id =
                        self.add_goal_by_spec(TaskSpec::Recipe(RecipeMatch::Build(dep_task_match)));
                    self.tentative_dependencies.push((task_id, dep_task_id));
                }
                Ok(None) => {
                    let dep_task_id = self.add_goal_by_spec(TaskSpec::CheckExistsRelaxed(
                        Absolute::symbolicate(dep_path),
                    ));
                    self.tentative_dependencies.push((task_id, dep_task_id));
                }
                Err(err) => scope.warning(&Warning::custom(
                    Some(span),
                    format_args!("ambiguous dependency '{dep_path}' in depfile: {err}; ignoring"),
                )),
            }
        }
        Ok(())
    }
}
