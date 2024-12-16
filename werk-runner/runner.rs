use std::{future::Future, marker::PhantomData, pin::Pin, sync::Arc, time::SystemTime};

use futures::channel::oneshot;
use indexmap::{map::Entry, IndexMap};
use parking_lot::Mutex;
use werk_fs::{Path, PathBuf};
use werk_parser::ast;

use crate::{
    compute_stable_hash, depfile::Depfile, eval, eval_run_expr, eval_string_expr, Error, Eval,
    GlobalVar, GlobalVariables, Io, LocalVariables, Outdatedness, OutdatednessTracker,
    PatternMatch, Reason, RecipeMatch, RecipeMatchData, RecipeScope, Recipes, RootScope,
    Scope as _, ShellCommandLine, UsedVariable, Value, Workspace, WorkspaceSettings,
};

pub struct Runner<'a> {
    inner: Arc<Inner>,
    _marker: PhantomData<&'a ()>,
}

struct Inner {
    io: Arc<dyn Io>,
    // Variables from the global scope.
    globals: GlobalVariables,
    recipes: Recipes,
    workspace: Workspace,
    watcher: Arc<dyn Watcher>,
    state: Mutex<State>,
}

pub trait Watcher: Send + Sync {
    /// Build task is about to start.
    fn will_build(
        &self,
        task_id: &TaskId,
        num_steps: usize,
        pre_message: Option<&str>,
        outdatedness: &Outdatedness,
    );

    /// Build task finished (all steps have been completed).
    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<BuildStatus, Error>,
        post_message: Option<&str>,
    );
    /// Shell command is about to be executed.
    fn will_execute(&self, task_id: &TaskId, command: &RunCommand, step: usize, num_steps: usize);
    /// Shell command is finished executing, or failed to start. Note that
    /// `result` will be `Ok` even if the command returned an error, allowing
    /// access to the command's stdout/stderr.
    ///
    /// The runner guarantees that if an `Ok(output)` is passed to this
    /// function,
    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &RunCommand,
        result: &Result<std::process::Output, Error>,
        step: usize,
        num_steps: usize,
        print_success: bool,
    );

    fn message(&self, task_id: Option<&TaskId>, message: &str);
    fn warning(&self, task_id: Option<&TaskId>, message: &str);
}

#[derive(Clone, Default)]
pub struct Settings {
    pub glob: WorkspaceSettings,
    pub dry_run: bool,
}

#[derive(Default)]
struct State {
    tasks: IndexMap<TaskId, TaskState>,
}

struct TaskState {
    status: TaskStatus,
    triggered_by: OwnedDependencyChain,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildStatus {
    /// Target was built, along with the outdatedness. If the outdatedness is
    /// empty, the target was determined to be up-to-date.
    Complete(TaskId, Outdatedness),
    /// Target is a dependency that exists in the filesystem, along with its
    /// last modification time.
    Exists(werk_fs::PathBuf, SystemTime),
}

impl BuildStatus {
    /// Given an output file modification time, return the outdatedness of the
    /// target. If the target is up-to-date, the outdatedness will be empty. If
    /// an output mtime is not available, returns empty outdatedness.
    pub fn into_outdated_reason(self, output_mtime: Option<SystemTime>) -> Option<Reason> {
        match self {
            BuildStatus::Complete(task_id, outdatedness) => {
                if outdatedness.is_outdated() {
                    Some(Reason::Rebuilt(task_id.clone()))
                } else {
                    None
                }
            }
            BuildStatus::Exists(path_buf, system_time) => {
                let Some(output_mtime) = output_mtime else {
                    return None;
                };

                if output_mtime <= system_time {
                    Some(Reason::Modified(path_buf, system_time))
                } else {
                    None
                }
            }
        }
    }
}

enum TaskStatus {
    Built(Result<BuildStatus, Error>),
    Pending(Vec<oneshot::Sender<Result<BuildStatus, Error>>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TaskId {
    name: String,
}

impl TaskId {
    pub fn command(s: impl Into<String>) -> Self {
        let name = s.into();
        debug_assert!(!name.starts_with('/'));
        TaskId { name }
    }

    pub fn build(p: impl Into<PathBuf>) -> Self {
        let path = p.into();
        assert!(path.is_absolute());
        TaskId { name: path.into() }
    }

    #[inline]
    pub fn is_command(&self) -> bool {
        !self.name.starts_with('/')
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn as_path(&self) -> Option<&werk_fs::Path> {
        if self.name.starts_with('/') {
            Some(werk_fs::Path::new_unchecked(&self.name))
        } else {
            None
        }
    }
}

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

enum TaskSpec {
    Recipe(RecipeMatchData),
    CheckExists(werk_fs::PathBuf),
}

enum DepfileSpec {
    /// The depfile is explicitly generated by a recipe.
    Recipe(RecipeMatchData),
    /// The depfile is implicitly generated by the associated command recipe.
    ImplicitlyGenerated(werk_fs::PathBuf),
}

impl TaskSpec {
    pub fn to_task_id(&self) -> TaskId {
        match self {
            TaskSpec::Recipe(recipe_match_data) => recipe_match_data.to_task_id(),
            TaskSpec::CheckExists(path_buf) => TaskId::build(path_buf.clone()),
        }
    }
}

impl<'a> Runner<'a> {
    pub async fn new(
        ast: ast::Root,
        io: Arc<dyn Io>,
        workspace: Workspace,
        watcher: Arc<dyn Watcher>,
    ) -> Result<Self, Error> {
        let mut globals = GlobalVariables::new();
        for (name, value) in &ast.global {
            let doc = &value.comment;

            if let Some(def) = workspace.defines.get(name) {
                globals.insert(
                    name.to_owned(),
                    GlobalVar {
                        value: Eval::using_var(
                            Value::String(def.clone()),
                            UsedVariable::Define(name.clone(), compute_stable_hash(def)),
                        ),
                        comment: doc.clone(),
                    },
                );
                continue;
            }

            // Creating a new scope every time, because it's cheap, and it
            // simplifies things because we don't need a `RootScopeMut` variant.
            let scope = RootScope::new(&globals, &workspace, &*watcher);
            let value = eval(&scope, &*io, value).await?;
            tracing::trace!(
                "global var '{name}' = {:?}, used = {:?}",
                value.value,
                value.used
            );
            globals.insert(
                name.to_owned(),
                GlobalVar {
                    value,
                    comment: doc.clone(),
                },
            );
        }

        // Warn about defines set on the command-line that have no effect.
        for (key, _) in &workspace.defines {
            if !globals.contains_key(key) {
                watcher.warning(None, &format!("Unused define: {key}"));
            }
        }

        let scope = RootScope::new(&globals, &workspace, &*watcher);
        let recipes = Recipes::new(ast, &scope).await?;

        let inner = Inner {
            io,
            recipes,
            globals,
            workspace,
            watcher,
            state: Mutex::new(State::default()),
        };
        Ok(Self {
            inner: Arc::new(inner),
            _marker: PhantomData,
        })
    }

    #[inline]
    pub fn recipes(&self) -> &Recipes {
        &self.inner.recipes
    }

    #[inline]
    pub fn workspace(&self) -> &Workspace {
        &self.inner.workspace
    }

    #[inline]
    pub fn globals(&self) -> &GlobalVariables {
        &self.inner.globals
    }

    pub async fn build_file(&mut self, target: &Path) -> Result<BuildStatus, Error> {
        tracing::debug!("Build: {target}");
        let spec = self.inner.get_build_spec(target)?;
        self.inner.clone().run_task(spec, DepChain::Empty).await
    }

    pub async fn run_command(&mut self, target: &str) -> Result<BuildStatus, Error> {
        tracing::debug!("Run: {target}");
        let spec = self.inner.get_command_spec(target)?;
        self.inner.clone().run_task(spec, DepChain::Empty).await
    }

    pub async fn build_or_run(&mut self, target: &str) -> Result<BuildStatus, Error> {
        tracing::debug!("Build or run: {target}");
        let spec = self.inner.get_build_or_command_spec(target)?;
        self.inner.clone().run_task(spec, DepChain::Empty).await
    }
}

impl Inner {
    fn get_build_spec<'a>(&'a self, target: &Path) -> Result<TaskSpec, Error> {
        let recipe_match = self.recipes.match_build_recipe(target)?;
        Ok(if let Some(recipe_match) = recipe_match {
            TaskSpec::Recipe(recipe_match.into())
        } else {
            TaskSpec::CheckExists(target.absolutize(Path::ROOT)?)
        })
    }

    fn get_depfile_build_spec<'a>(&'a self, target: &Path) -> Result<DepfileSpec, Error> {
        let Some(recipe_match) = self.recipes.match_build_recipe(target)? else {
            return Ok(DepfileSpec::ImplicitlyGenerated(
                target.absolutize(Path::ROOT)?,
            ));
        };
        Ok(DepfileSpec::Recipe(recipe_match.into()))
    }

    fn get_command_spec<'a>(&'a self, target: &str) -> Result<TaskSpec, Error> {
        let recipe_match = self
            .recipes
            .match_command_recipe(target)
            .ok_or_else(|| Error::NoRuleToBuildTarget(target.to_owned()))?;
        Ok(TaskSpec::Recipe(recipe_match.into()))
    }

    fn get_build_or_command_spec<'a>(&'a self, target: &str) -> Result<TaskSpec, Error> {
        let recipe_match = self.recipes.match_recipe_by_name(target)?;
        if let Some(recipe_match) = recipe_match {
            Ok(TaskSpec::Recipe(recipe_match.into()))
        } else if let Ok(path) = Path::new(target) {
            Ok(TaskSpec::CheckExists(path.absolutize(Path::ROOT)?))
        } else {
            Err(Error::NoRuleToBuildTarget(target.to_owned()))
        }
    }

    /// Build the task, coordinating dependencies and rebuilds. The `invoker`
    /// is the chain of dependencies that triggered this build, not including
    /// this task.
    async fn run_task(
        self: &Arc<Self>,
        spec: TaskSpec,
        dep_chain: DepChain<'_>,
    ) -> Result<BuildStatus, Error> {
        let task_id = spec.to_task_id();

        // Perform the circular dependency check regardless of how the task was
        // triggered. Otherwise the check would depend on scheduling, which is
        // nondeterministic.
        if dep_chain.contains(&task_id) {
            let dep_chain = dep_chain.push(&task_id);
            return Err(Error::CircularDependency(dep_chain.collect()));
        }

        enum Scheduling {
            Done(Result<BuildStatus, Error>),
            Pending(oneshot::Receiver<Result<BuildStatus, Error>>),
            BuildNow(TaskSpec),
        }

        fn schedule(this: &Inner, spec: TaskSpec, dep_chain: DepChain<'_>) -> Scheduling {
            match this.state.lock().tasks.entry(spec.to_task_id()) {
                Entry::Occupied(mut entry) => match entry.get_mut().status {
                    TaskStatus::Built(ref result) => Scheduling::Done(result.clone()),
                    TaskStatus::Pending(ref mut waiters) => {
                        let (send, recv) = oneshot::channel();
                        waiters.push(send);
                        Scheduling::Pending(recv)
                    }
                },
                Entry::Vacant(entry) => {
                    entry.insert(TaskState {
                        status: TaskStatus::Pending(Vec::new()),
                        triggered_by: dep_chain.collect(),
                    });
                    Scheduling::BuildNow(spec)
                }
            }
        }

        fn finish_built(this: &Inner, task_id: &TaskId, result: Result<BuildStatus, Error>) {
            // Notify dependents
            let mut lock = this.state.lock();
            let task_state = lock.tasks.get_mut(task_id).expect("task not registered");
            // Set the task status as complete.
            let TaskStatus::Pending(waiters) =
                std::mem::replace(&mut task_state.status, TaskStatus::Built(result.clone()))
            else {
                panic!("Task built multiple times: {task_id}");
            };
            std::mem::drop(lock);

            // Notify dependents that the task completed.
            for waiter in waiters {
                _ = waiter.send(result.clone());
            }
        }

        match schedule(&self, spec, dep_chain) {
            Scheduling::Done(result) => result,
            Scheduling::Pending(receiver) => receiver
                .await
                .map_err(|_| Error::Cancelled(task_id.clone()))?,
            Scheduling::BuildNow(task_spec) => {
                let result = self.rebuild_spec(&task_id, task_spec, dep_chain).await;
                finish_built(&self, &task_id, result.clone());
                result
            }
        }
    }

    async fn check_exists(&self, path: &werk_fs::Path) -> Result<BuildStatus, Error> {
        let Some(entry) = self.workspace.get_project_file(path) else {
            return Err(Error::NoRuleToBuildTarget(path.to_string()));
        };
        let mtime = entry.metadata.mtime;
        tracing::debug!("Check file mtime `{path}`: {mtime:?}");
        Ok(BuildStatus::Exists(path.to_path_buf(), mtime))
    }

    #[tracing::instrument(level = "debug", skip_all, fields(target_file))]
    async fn execute_build_recipe(
        self: &Arc<Self>,
        task_id: &TaskId,
        recipe: &ast::BuildRecipe,
        pattern_match: &PatternMatch<'_>,
        target_file: &werk_fs::Path,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let global_scope = RootScope::new(&self.globals, &self.workspace, &*self.watcher);
        let mut scope = RecipeScope::new(&global_scope, task_id, Some(pattern_match));
        scope.set(
            "out".to_owned(),
            Eval::inherent(Value::String(target_file.to_string())),
        );

        let mut cache = self.workspace.take_build_target_cache(target_file);
        let mut outdatedness = OutdatednessTracker::new(
            &self.workspace,
            cache.as_ref(),
            &pattern_match.pattern.fragments,
            recipe,
        );

        // Evaluate direct dependencies (`out` is available).
        let mut deps = Vec::new();
        if let Some(ref build_deps) = recipe.in_files {
            let build_deps = eval(&scope, &*self.io, build_deps).await?;
            outdatedness.did_use(build_deps.used);
            build_deps.value.try_collect_strings_recursive(|s| {
                let dep = self.get_build_or_command_spec(&s)?;
                deps.push(dep);
                Ok::<_, Error>(())
            })?;
        }

        // Determine the value of the `in` special variable (before evaluating
        // depfile dependencies).
        let in_values = deps
            .iter()
            .filter_map(|dep| match dep {
                TaskSpec::Recipe(RecipeMatchData::Build { target_file, .. }) => {
                    Some(Value::String(target_file.clone().into()))
                }
                TaskSpec::CheckExists(path_buf) => Some(Value::String(path_buf.to_string())),
                _ => None,
            })
            .collect::<Vec<_>>();
        let in_values = if in_values.len() == 1 {
            in_values.into_iter().next().unwrap()
        } else {
            Value::List(in_values)
        };
        tracing::trace!("in = {in_values:?}");
        scope.set(String::from("in"), Eval::inherent(in_values));

        // Check the target's mtime.
        let out_mtime = scope
            .workspace()
            .get_existing_output_file(&*self.io, target_file)?
            .map(|entry| entry.metadata.mtime);

        // Rebuild if the target does not exist.
        match out_mtime {
            Some(mtime) => {
                tracing::debug!("Output exists, mtime: {mtime:?}");
            }
            None => {
                tracing::debug!("Output file missing, target is outdated");
                outdatedness.target_does_not_exist(target_file.to_path_buf());
            }
        }

        let mut check_implicit_depfile_was_generated = None;
        if let Some(ref depfile) = recipe.depfile {
            let depfile_dep_eval = eval_string_expr(&scope, depfile)?;
            // Outdatedness of the `depfile` value itself.
            outdatedness.did_use(depfile_dep_eval.used.clone());

            let depfile_path =
                werk_fs::Path::new(&depfile_dep_eval.value)?.absolutize(werk_fs::Path::ROOT)?;
            let dep = self.get_depfile_build_spec(&depfile_path)?;

            // Make the `depfile` variable available to the recipe body.
            scope.set(String::from("depfile"), depfile_dep_eval.map(Value::String));

            match dep {
                DepfileSpec::Recipe(depfile_recipe_match_data) => {
                    tracing::debug!("Building depfile with recipe: {depfile_recipe_match_data}");
                    // Depfile is explicitly generated by a recipe - build it.
                    let dep_reasons = self
                        .build_dependencies(
                            vec![TaskSpec::Recipe(depfile_recipe_match_data)],
                            dep_chain,
                            out_mtime,
                        )
                        .await?;
                    outdatedness.add_reasons(dep_reasons);
                }
                DepfileSpec::ImplicitlyGenerated(path_buf) => {
                    tracing::debug!("Assuming implicitly generated depfile");
                    check_implicit_depfile_was_generated = Some(path_buf);
                }
            }
            let is_implicit_depfile = check_implicit_depfile_was_generated.is_some();

            if let Some(depfile_entry) = self
                .workspace
                .get_existing_output_file(&*self.io, &depfile_path)?
            {
                // The depfile was generated, either by a build recipe or by a
                // previous run of this recipe. Parse it and add its
                // dependencies!
                tracing::debug!("Parsing depfile: {}", depfile_entry.path.display());
                let depfile_contents = self.io.read_file(&depfile_entry.path).await?;
                let depfile = Depfile::parse(&depfile_contents)?;
                for dep in &depfile.deps {
                    // Translate the filesystem path produced by the compiler into an
                    // abstract path within the workspace. Normally this will be a file
                    // inside the output directory.
                    let abstract_path = self.workspace.unresolve_path(dep)?;
                    tracing::debug!("Discovered depfile dependency: {abstract_path}");
                    deps.push(self.get_build_spec(&abstract_path)?);
                }
            } else {
                if is_implicit_depfile {
                    // The implicit depfile was not generated yet, in this run
                    // or a previous run. Add that as a reason to rebuild the
                    // main recipe. Note that this causes the main recipe to
                    // always be outdated if it fails to generate the depfile!
                    outdatedness.add_reason(Reason::Missing(
                        depfile_path.absolutize(werk_fs::Path::ROOT)?,
                    ));
                } else {
                    // If the depfile is generated by a rule, it is an error if that
                    // rule did not generate the depfile. If it is implicit, it's
                    // fine if it doesn't exist yet.
                    if self.io.is_dry_run() {
                        self.watcher.warning(
                            Some(task_id),
                            "Depfile does not exist, and was not generated, because this is a dry run",
                        );
                    } else {
                        return Err(Error::DepfileNotFound(depfile_path.to_path_buf()));
                    }
                }
            }
        }

        // Build dependencies!
        let dep_reasons = self.build_dependencies(deps, dep_chain, out_mtime).await?;
        outdatedness.add_reasons(dep_reasons);

        // Figure out what to run.
        let mut run_commands = Vec::with_capacity(recipe.command.len());
        for run_expr in recipe.command.iter() {
            let run_command =
                eval_run_expr(&scope, &*self.io, run_expr, self.workspace.force_color).await?;
            outdatedness.did_use(run_command.used);
            run_commands.push(run_command.value);
        }

        let pre_message = if let Some(pre_message) = &recipe.pre_message {
            Some(eval_string_expr(&scope, pre_message)?.value)
        } else {
            None
        };
        let post_message = if let Some(post_message) = &recipe.post_message {
            Some(eval_string_expr(&scope, post_message)?.value)
        } else {
            None
        };

        // Create the parent directory for the target file if it doesn't exist.
        scope
            .workspace()
            .create_output_parent_dirs(&*self.io, target_file)
            .await?;

        let (outdated, new_cache) = outdatedness.finish();
        self.workspace
            .store_build_target_cache(target_file.to_path_buf(), new_cache);

        self.watcher.will_build(
            task_id,
            run_commands.len(),
            pre_message.as_deref(),
            &outdated,
        );

        let result = if outdated.is_outdated() {
            tracing::debug!("Rebuilding");
            self.execute_recipe_commands(task_id, &run_commands, &scope, false)
                .await
                .map(|_| BuildStatus::Complete(task_id.clone(), outdated))
        } else {
            tracing::debug!("Up to date");
            Ok(BuildStatus::Complete(task_id.clone(), outdated))
        };

        // Check if the implicit depfile was actually generated, and emit a warning if not.
        if let Some(ref implicit_depfile_path) = check_implicit_depfile_was_generated {
            if !self.io.is_dry_run() {
                if let Ok(None) | Err(_) = self
                    .workspace
                    .get_existing_output_file(&*self.io, implicit_depfile_path)
                {
                    self.watcher.warning(
                        Some(task_id),
                        &format!(
                            "Depfile was not generated by the recipe: {implicit_depfile_path}"
                        ),
                    );
                }
            }
        }

        self.watcher
            .did_build(task_id, &result, post_message.as_deref());
        result
    }

    async fn execute_command_recipe(
        self: &Arc<Self>,
        task_id: &TaskId,
        recipe: &ast::CommandRecipe,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let global_scope = RootScope::new(&self.globals, &self.workspace, &*self.watcher);
        let scope = RecipeScope::new(&global_scope, task_id, None);

        // Evaluate dependencies (`out` is not available in commands).
        let mut deps = Vec::new();
        if let Some(ref build_deps) = recipe.build {
            let build_deps = eval(&scope, &*self.io, build_deps).await?.value;
            build_deps.try_collect_strings_recursive(|s| {
                let dep = self.get_build_or_command_spec(&s)?;
                deps.push(dep);
                Ok::<_, Error>(())
            })?;
        }

        // Note: We don't care about the status of dependencies.
        self.build_dependencies(deps, dep_chain, None).await?;

        // Figure out what to run.
        // Figure out what to run.
        let mut run_commands = Vec::with_capacity(recipe.command.len());
        for run_expr in recipe.command.iter() {
            let run_command =
                eval_run_expr(&scope, &*self.io, run_expr, self.workspace.force_color).await?;
            run_commands.push(run_command.value);
        }

        let pre_message = if let Some(pre_message) = &recipe.pre_message {
            Some(eval_string_expr(&scope, pre_message)?.value)
        } else {
            None
        };
        let post_message = if let Some(post_message) = &recipe.post_message {
            Some(eval_string_expr(&scope, post_message)?.value)
        } else {
            None
        };

        let outdated = Outdatedness::outdated(Reason::Rebuilt(task_id.clone()));
        self.watcher.will_build(
            task_id,
            run_commands.len(),
            pre_message.as_deref(),
            &outdated,
        );

        let result = self
            .execute_recipe_commands(
                task_id,
                &run_commands,
                &scope,
                recipe.capture.is_some_and(|c| !c),
            )
            .await
            .map(|_| BuildStatus::Complete(task_id.clone(), outdated));

        self.watcher
            .did_build(task_id, &result, post_message.as_deref());
        result
    }

    async fn execute_recipe_commands(
        &self,
        task_id: &TaskId,
        run_commands: &[RunCommand],
        scope: &RecipeScope<'_>,
        print_successful: bool,
    ) -> Result<(), Error> {
        let num_steps = run_commands.len();

        fn default_output() -> std::process::Output {
            std::process::Output {
                status: std::process::ExitStatus::default(),
                stdout: Vec::new(),
                stderr: Vec::new(),
            }
        }

        for (step, run_command) in run_commands.iter().enumerate() {
            self.watcher
                .will_execute(task_id, run_command, step, num_steps);

            let result = match run_command {
                RunCommand::Shell(command_line) => {
                    self.io
                        .run_build_command(command_line, scope.workspace().project_root())
                        .await
                }
                RunCommand::Write(path_buf, vec) => self
                    .io
                    .write_file(&path_buf, vec)
                    .await
                    .map(|_| default_output()),
                RunCommand::Copy(from, to) => {
                    self.io.copy_file(from, to).await.map(|_| default_output())
                }
                RunCommand::Echo(message) => {
                    self.watcher.message(Some(task_id), message);
                    Ok(default_output())
                }
                RunCommand::Delete(path) => {
                    self.io.delete_file(path).await.map(|_| default_output())
                }
            }
            .map_err(Into::into);

            self.watcher.did_execute(
                task_id,
                run_command,
                &result,
                step,
                num_steps,
                print_successful,
            );

            match result {
                Err(err) => {
                    return Err(err);
                }
                Ok(output) => {
                    if !output.status.success() {
                        return Err(Error::CommandFailed(output.status));
                    }
                }
            }
        }

        Ok(())
    }

    fn build_dependencies<'a>(
        self: &'a Arc<Self>,
        mut dependencies: Vec<TaskSpec>,
        dependent: DepChainEntry<'a>,
        output_mtime: Option<SystemTime>,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<Reason>, Error>> + Send + 'a>> {
        if dependencies.len() == 1 {
            // Boxing because of recursion.
            let dependency = dependencies.pop().unwrap();
            return Box::pin(async move {
                self.run_task(dependency, DepChain::Ref(&dependent))
                    .await
                    .map(|status| {
                        status
                            .into_outdated_reason(output_mtime)
                            .into_iter()
                            .collect()
                    })
            });
        } else if !dependencies.is_empty() {
            let parent = dependent.collect();
            let this = self.clone();
            return Box::pin(async move {
                let mut tasks = tokio::task::JoinSet::new();
                for dep in dependencies {
                    let this = this.clone();
                    let parent = parent.clone();
                    tasks.spawn(async move { this.run_task(dep, DepChain::Owned(&parent)).await });
                }

                let mut reasons = Vec::new();
                let mut first_error = None;
                while let Some(status) = tasks.join_next().await {
                    match status.unwrap() {
                        Ok(status) => {
                            if let Some(reason) = status.into_outdated_reason(output_mtime) {
                                reasons.push(reason);
                            }
                        }
                        Err(err) => {
                            // Don't interrupt other tasks if one fails.
                            first_error.get_or_insert(err);
                        }
                    }
                }
                if let Some(first_error) = first_error {
                    Err(first_error)
                } else {
                    Ok(reasons)
                }
            });
        } else {
            return Box::pin(futures::future::ready(Ok(vec![])));
        }
    }

    /// Unconditionally run the task if it is outdated.
    async fn rebuild_spec(
        self: &Arc<Self>,
        task_id: &TaskId,
        spec: TaskSpec,
        dep_chain: DepChain<'_>,
    ) -> Result<BuildStatus, Error> {
        let dep_chain_entry = dep_chain.push(task_id);

        match spec {
            TaskSpec::Recipe(recipe) => {
                let recipe = self.recipes.get_matched(recipe);
                match recipe {
                    RecipeMatch::Command { recipe, .. } => {
                        self.execute_command_recipe(task_id, recipe, dep_chain_entry)
                            .await
                    }
                    RecipeMatch::Build {
                        recipe,
                        pattern_match,
                        target_file,
                        ..
                    } => {
                        self.execute_build_recipe(
                            task_id,
                            recipe,
                            &pattern_match,
                            &target_file,
                            dep_chain_entry,
                        )
                        .await
                    }
                }
            }
            TaskSpec::CheckExists(path) => self.check_exists(&path).await,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RunCommand {
    Shell(ShellCommandLine),
    Write(std::path::PathBuf, Vec<u8>),
    Copy(std::path::PathBuf, std::path::PathBuf),
    Echo(String),
    Delete(std::path::PathBuf),
}

impl std::fmt::Display for RunCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunCommand::Shell(shell_command_line) => std::fmt::Display::fmt(shell_command_line, f),
            RunCommand::Write(path_buf, vec) => {
                write!(f, "write {} ({} bytes)", path_buf.display(), vec.len())
            }
            RunCommand::Copy(from, to) => {
                write!(f, "copy '{}' to '{}'", from.display(), to.display())
            }
            RunCommand::Echo(message) => {
                write!(f, "echo \"{}\"", message.escape_default())
            }
            RunCommand::Delete(path) => {
                write!(f, "delete '{}'", path.display())
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct DepChainEntry<'a> {
    parent: DepChain<'a>,
    this: &'a TaskId,
}

#[derive(Debug, Clone, Copy)]
enum DepChain<'a> {
    Empty,
    Owned(&'a OwnedDependencyChain),
    Ref(&'a DepChainEntry<'a>),
}

impl<'a> DepChain<'a> {
    fn collect(&self) -> OwnedDependencyChain {
        OwnedDependencyChain {
            vec: self.collect_vec(),
        }
    }

    fn collect_vec(&self) -> Vec<TaskId> {
        match self {
            DepChain::Empty => Vec::new(),
            DepChain::Owned(owned) => owned.vec.clone(),
            DepChain::Ref(parent) => parent.collect_vec(),
        }
    }

    pub fn contains(&self, task: &TaskId) -> bool {
        match self {
            DepChain::Empty => false,
            DepChain::Owned(owned) => owned.vec.contains(task),
            DepChain::Ref(parent) => parent.contains(task),
        }
    }

    pub fn push<'b>(self, task: &'b TaskId) -> DepChainEntry<'b>
    where
        'a: 'b,
    {
        DepChainEntry {
            parent: self,
            this: task,
        }
    }
}

impl<'a> DepChainEntry<'a> {
    fn collect(&self) -> OwnedDependencyChain {
        OwnedDependencyChain {
            vec: self.collect_vec(),
        }
    }

    fn collect_vec(&self) -> Vec<TaskId> {
        let mut vec = self.parent.collect_vec();
        vec.push(self.this.clone());
        vec
    }

    fn contains(&self, task_id: &TaskId) -> bool {
        if self.this == task_id {
            true
        } else {
            self.parent.contains(task_id)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnedDependencyChain {
    vec: Vec<TaskId>,
}

impl std::fmt::Display for OwnedDependencyChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, task_id) in self.vec.iter().enumerate() {
            if i > 0 {
                write!(f, " -> ")?;
            }
            write!(f, "{}", task_id)?;
        }
        Ok(())
    }
}
