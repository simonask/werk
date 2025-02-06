use std::{future::Future, sync::Arc, time::SystemTime};

use futures::{channel::oneshot, StreamExt};
use indexmap::{map::Entry, IndexMap};
use parking_lot::Mutex;
use werk_fs::{Absolute, Normalize as _, Path, SymPath};
use werk_util::{Diagnostic, DiagnosticError, Symbol};

use crate::{
    depfile::Depfile,
    eval::{self, Eval},
    ir::{self},
    AmbiguousPatternError, BuildRecipeScope, ChildCaptureOutput, ChildLinesStream, Env, Error,
    Outdatedness, OutdatednessTracker, Reason, RootScope, Scope as _, ShellCommandLine,
    TaskRecipeScope, Value, Workspace, WorkspaceSettings,
};

/// Workspace-wide runner state.
pub(crate) struct RunnerState {
    concurrency_limit: smol::lock::Semaphore,
    tasks: Mutex<IndexMap<TaskId, TaskStatus>>,
}

impl RunnerState {
    pub fn new(jobs: usize) -> Self {
        Self {
            concurrency_limit: smol::lock::Semaphore::new(jobs.max(1)),
            tasks: Mutex::new(IndexMap::default()),
        }
    }
}

pub struct Runner<'a> {
    inner: Arc<Inner<'a>>,
}

struct Inner<'a> {
    workspace: &'a Workspace<'a>,
    executor: smol::Executor<'a>,
}

#[derive(Clone)]
pub struct Settings {
    pub glob: WorkspaceSettings,
    pub dry_run: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildStatus {
    /// Target was built, along with the outdatedness. If the outdatedness is
    /// empty, the target was determined to be up-to-date.
    Complete(TaskId, Outdatedness),
    /// Target is a dependency that exists in the filesystem, along with its
    /// last modification time.
    Exists(Absolute<SymPath>, SystemTime),
}

impl BuildStatus {
    /// Given an output file modification time, return the outdatedness of the
    /// target. If the target is up-to-date, the outdatedness will be empty. If
    /// an output mtime is not available, returns empty outdatedness.
    #[must_use]
    pub fn into_outdated_reason(self, output_mtime: Option<SystemTime>) -> Option<Reason> {
        match self {
            BuildStatus::Complete(task_id, outdatedness) => {
                if outdatedness.is_outdated() {
                    Some(Reason::Rebuilt(task_id))
                } else {
                    None
                }
            }
            BuildStatus::Exists(path_buf, system_time) => {
                let output_mtime = output_mtime?;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TaskId {
    Task(Symbol),
    // TODO: When recipes can build multiple files, this needs to change to some
    // ID that encapsulates the "recipe instance" rather than the path of a
    // single target.
    Build(Absolute<SymPath>),
}

impl TaskId {
    pub fn command(s: impl Into<Symbol>) -> Self {
        let name = s.into();
        debug_assert!(!name.as_str().starts_with('/'));
        TaskId::Task(name)
    }

    pub fn build(p: impl AsRef<Absolute<werk_fs::Path>>) -> Self {
        TaskId::Build(Absolute::symbolicate(p))
    }

    pub fn try_build<P>(p: P) -> Result<Self, P::Error>
    where
        P: TryInto<Absolute<werk_fs::PathBuf>>,
    {
        let path = p.try_into()?;
        Ok(TaskId::build(path))
    }

    #[inline]
    #[must_use]
    pub fn is_command(&self) -> bool {
        matches!(self, TaskId::Task(_))
    }

    #[inline]
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            TaskId::Task(task) => task.as_str(),
            TaskId::Build(build) => build.as_inner().as_str(),
        }
    }

    #[inline]
    #[must_use]
    pub fn as_path(&self) -> Option<&Absolute<werk_fs::Path>> {
        if let TaskId::Build(build) = self {
            Some(build.as_path())
        } else {
            None
        }
    }

    #[inline]
    #[must_use]
    pub fn short_name(&self) -> &'static str {
        match self {
            TaskId::Task(task) => task.as_str(),
            TaskId::Build(path) => {
                let Some((_prefix, filename)) = path
                    .as_inner()
                    .as_str()
                    .rsplit_once(werk_fs::Path::SEPARATOR)
                else {
                    // The path is absolute.
                    unreachable!()
                };
                filename
            }
        }
    }
}

impl std::fmt::Display for TaskId {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

enum TaskSpec<'a> {
    Recipe(ir::RecipeMatch<'a>),
    CheckExists(Absolute<werk_fs::PathBuf>),
    /// Check if the file exists, but don't emit an error if it doesn't. This
    /// applies to dependencies discovered through depfiles, where the depfile
    /// may be outdated (from a previous build).
    ///
    /// If the file does not exist, the task will be considered outdated.
    CheckExistsRelaxed(Absolute<werk_fs::PathBuf>),
}

enum DepfileSpec<'a> {
    /// The depfile is explicitly generated by a recipe.
    Recipe(ir::BuildRecipeMatch<'a>),
    /// The depfile is implicitly generated by the associated command recipe.
    ImplicitlyGenerated(Absolute<werk_fs::PathBuf>),
}

impl TaskSpec<'_> {
    pub fn to_task_id(&self) -> TaskId {
        match self {
            TaskSpec::Recipe(ir::RecipeMatch::Build(build_recipe_match)) => {
                TaskId::build(build_recipe_match.target_file.clone())
            }
            TaskSpec::Recipe(ir::RecipeMatch::Task(command_recipe_match)) => {
                TaskId::command(command_recipe_match.name)
            }
            TaskSpec::CheckExists(path_buf) | TaskSpec::CheckExistsRelaxed(path_buf) => {
                TaskId::build(path_buf.clone().into_boxed_path())
            }
        }
    }
}

impl<'a> Runner<'a> {
    pub fn new(workspace: &'a Workspace) -> Self {
        Self {
            inner: Arc::new(Inner {
                workspace,
                executor: smol::Executor::new(),
            }),
        }
    }

    pub async fn build_file(
        &self,
        target: &Path,
    ) -> Result<BuildStatus, DiagnosticError<'a, Error, &'a Workspace<'a>>> {
        let target = target
            .absolutize(werk_fs::Path::ROOT)
            .map_err(|err| Error::InvalidTargetPath(target.to_string(), err))
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))?;
        tracing::debug!("Build: {target}");
        let spec = self
            .inner
            .get_build_spec(&target)
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))?;
        let inner = self.inner.clone();
        // TODO: Run the executor with multiple threads.
        self.inner
            .executor
            .run(async move { inner.run_task(spec, DepChain::Empty).await })
            .await
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))
    }

    pub async fn run_command(
        &self,
        target: &str,
    ) -> Result<BuildStatus, DiagnosticError<'a, Error, &'a Workspace<'a>>> {
        tracing::debug!("Run: {target}");
        let spec = self
            .inner
            .get_command_spec(target)
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))?;
        let inner = self.inner.clone();
        // TODO: Run the executor with multiple threads.
        self.inner
            .executor
            .run(async move { inner.run_task(spec, DepChain::Empty).await })
            .await
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))
    }

    pub async fn build_or_run(
        &self,
        target: &str,
    ) -> Result<BuildStatus, DiagnosticError<'a, Error, &'a Workspace<'a>>> {
        tracing::debug!("Build or run: {target}");
        let spec = self
            .inner
            .get_build_or_command_spec(target)
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))?;
        let inner = self.inner.clone();
        // TODO: Run the executor with multiple threads.
        self.inner
            .executor
            .run(async move { inner.run_task(spec, DepChain::Empty).await })
            .await
            .map_err(|err| err.into_diagnostic_error(self.inner.workspace))
    }
}

impl<'a> Inner<'a> {
    fn get_build_spec(&self, target: &Absolute<Path>) -> Result<TaskSpec<'a>, Error> {
        let recipe_match = self.workspace.manifest.match_build_recipe(target)?;
        Ok(if let Some(recipe_match) = recipe_match {
            TaskSpec::Recipe(ir::RecipeMatch::Build(recipe_match))
        } else {
            TaskSpec::CheckExists(target.to_owned())
        })
    }

    fn get_build_spec_relaxed(&self, target: &Absolute<Path>) -> Result<TaskSpec<'a>, Error> {
        let recipe_match = self.workspace.manifest.match_build_recipe(target)?;
        Ok(if let Some(recipe_match) = recipe_match {
            TaskSpec::Recipe(ir::RecipeMatch::Build(recipe_match))
        } else {
            TaskSpec::CheckExistsRelaxed(target.to_owned())
        })
    }

    fn get_depfile_build_spec(&self, target: &Absolute<Path>) -> Result<DepfileSpec<'a>, Error> {
        let Some(recipe_match) = self.workspace.manifest.match_build_recipe(target)? else {
            return Ok(DepfileSpec::ImplicitlyGenerated(target.to_owned()));
        };
        Ok(DepfileSpec::Recipe(recipe_match))
    }

    fn get_command_spec(&self, target: &str) -> Result<TaskSpec<'a>, Error> {
        let recipe_match = self
            .workspace
            .manifest
            .match_task_recipe(target)
            .ok_or_else(|| Error::NoRuleToBuildTarget(target.to_owned()))?;
        Ok(TaskSpec::Recipe(ir::RecipeMatch::Task(recipe_match)))
    }

    fn get_build_or_command_spec(&self, target: &str) -> Result<TaskSpec<'a>, Error> {
        let task_recipe_match = self.workspace.manifest.match_task_recipe(target);

        if let Ok(path) = werk_fs::Path::new(target) {
            let path = path
                .absolutize(werk_fs::Path::ROOT)
                .map_err(|err| Error::InvalidTargetPath(path.to_string(), err))?;
            if let Some(build_recipe_match) = self.workspace.manifest.match_build_recipe(&path)? {
                if let Some(task_recipe) = task_recipe_match {
                    return Err(AmbiguousPatternError {
                        pattern1: task_recipe.ast.name.span,
                        pattern2: build_recipe_match.recipe.pattern.span,
                        path: path.to_string(),
                    }
                    .into());
                }

                return Ok(TaskSpec::Recipe(ir::RecipeMatch::Build(build_recipe_match)));
            } else if task_recipe_match.is_none() {
                return Ok(TaskSpec::CheckExists(path.into_owned()));
            }
        }

        match task_recipe_match {
            Some(task_recipe_match) => {
                Ok(TaskSpec::Recipe(ir::RecipeMatch::Task(task_recipe_match)))
            }
            None => Err(Error::NoRuleToBuildTarget(target.to_owned())),
        }
    }

    /// Build the task, coordinating dependencies and rebuilds. The `invoker`
    /// is the chain of dependencies that triggered this build, not including
    /// this task.
    async fn run_task(
        self: Arc<Self>,
        spec: TaskSpec<'a>,
        dep_chain: DepChain<'_>,
    ) -> Result<BuildStatus, Error> {
        enum Scheduling<'a> {
            Done(Result<BuildStatus, Error>),
            Pending(oneshot::Receiver<Result<BuildStatus, Error>>),
            BuildNow(TaskSpec<'a>),
        }

        fn schedule<'a>(this: &RunnerState, spec: TaskSpec<'a>) -> Scheduling<'a> {
            match this.tasks.lock().entry(spec.to_task_id()) {
                Entry::Occupied(mut entry) => match entry.get_mut() {
                    TaskStatus::Built(ref result) => Scheduling::Done(result.clone()),
                    TaskStatus::Pending(ref mut waiters) => {
                        let (send, recv) = oneshot::channel();
                        waiters.push(send);
                        Scheduling::Pending(recv)
                    }
                },
                Entry::Vacant(entry) => {
                    entry.insert(TaskStatus::Pending(Vec::new()));
                    Scheduling::BuildNow(spec)
                }
            }
        }

        fn finish_built(this: &RunnerState, task_id: TaskId, result: &Result<BuildStatus, Error>) {
            // Notify dependents
            let mut tasks = this.tasks.lock();
            let status = tasks.get_mut(&task_id).expect("task not registered");
            // Set the task status as complete.
            let TaskStatus::Pending(waiters) =
                std::mem::replace(status, TaskStatus::Built(result.clone()))
            else {
                panic!("Task built multiple times: {task_id}");
            };
            std::mem::drop(tasks);

            // Notify dependents that the task completed.
            for waiter in waiters {
                _ = waiter.send(result.clone());
            }
        }

        let task_id = spec.to_task_id();

        // Perform the circular dependency check regardless of how the task was
        // triggered. Otherwise the check would depend on scheduling, which is
        // nondeterministic.
        if dep_chain.contains(task_id) {
            let dep_chain = dep_chain.push(task_id);
            return Err(Error::CircularDependency(dep_chain.collect()));
        }

        match schedule(&self.workspace.runner_state, spec) {
            Scheduling::Done(result) => result,
            Scheduling::Pending(receiver) => {
                receiver.await.map_err(|_| Error::Cancelled(task_id))?
            }
            Scheduling::BuildNow(task_spec) => {
                let result = self
                    .clone()
                    .rebuild_spec(task_id, task_spec, dep_chain)
                    .await;
                finish_built(&self.workspace.runner_state, task_id, &result);
                result
            }
        }
    }

    fn check_exists(&self, path: &Absolute<werk_fs::Path>) -> Result<BuildStatus, Error> {
        let Some(entry) = self.workspace.get_project_file(path) else {
            return Err(Error::NoRuleToBuildTarget(path.to_string()));
        };
        let mtime = entry.metadata.mtime;
        tracing::debug!("Check file mtime `{path}`: {mtime:?}");
        Ok(BuildStatus::Exists(Absolute::symbolicate(path), mtime))
    }

    #[tracing::instrument(level = "debug", skip_all, fields(target_file))]
    async fn execute_build_recipe(
        self: &Arc<Self>,
        task_id: TaskId,
        recipe_match: ir::BuildRecipeMatch<'_>,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let global_scope = RootScope::new(self.workspace);
        let mut scope = BuildRecipeScope::new(&global_scope, task_id, &recipe_match);
        scope.set(
            Symbol::new("out"),
            Eval::inherent(Value::String(recipe_match.target_file.to_string())),
        );

        let cache = self
            .workspace
            .take_build_target_cache(&recipe_match.target_file);
        // Check the target's mtime.
        let out_mtime = scope
            .workspace()
            .get_existing_output_file(&recipe_match.target_file)?
            .map(|entry| entry.metadata.mtime);

        let mut outdatedness = OutdatednessTracker::new(
            self.workspace,
            cache.as_ref(),
            recipe_match.recipe,
            out_mtime,
        );

        // Evaluate recipe body (`out` is available and in scope).
        let evaluated = eval::eval_build_recipe_statements(
            &mut scope,
            &recipe_match.recipe.ast.body.statements,
        )?;
        outdatedness.did_use(evaluated.used);
        let evaluated = evaluated.value;

        let mut explicit_dependency_specs = evaluated
            .explicit_dependencies
            .iter()
            .map(|s| self.get_build_or_command_spec(s))
            .collect::<Result<Vec<_>, Error>>()?;

        // Rebuild if the target does not exist.
        if let Some(mtime) = out_mtime {
            tracing::debug!("Output exists, mtime: {mtime:?}");
        } else {
            tracing::debug!("Output file missing, target is outdated");
            outdatedness.missing(Absolute::symbolicate(&recipe_match.target_file));
        }

        let mut check_implicit_depfile_was_generated = None;
        if let Some(depfile) = evaluated.depfile {
            let depfile_path = werk_fs::Path::new(&depfile)
                .and_then(|p| p.absolutize(werk_fs::Path::ROOT))
                .map_err(|err| Error::InvalidTargetPath(depfile.clone(), err))?;
            let dep = self.get_depfile_build_spec(&depfile_path)?;

            // Make the `depfile` variable available to the recipe body.
            scope.set(
                Symbol::from("depfile"),
                Eval::inherent(Value::String(depfile.clone())),
            );

            match dep {
                DepfileSpec::Recipe(depfile_recipe_match_data) => {
                    tracing::debug!(
                        "Building depfile with recipe: {}",
                        depfile_recipe_match_data.recipe.pattern
                    );
                    // Depfile is explicitly generated by a recipe - build it.
                    let dep_reasons = self
                        .clone()
                        .build_dependencies(
                            vec![TaskSpec::Recipe(ir::RecipeMatch::Build(
                                depfile_recipe_match_data,
                            ))],
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

            if let Some(depfile_entry) = self.workspace.get_existing_output_file(&depfile_path)? {
                // The depfile was generated, either by a build recipe or by a
                // previous run of this recipe. Parse it and add its
                // dependencies!
                tracing::debug!("Parsing depfile: {}", depfile_entry.path.display());
                let depfile_contents = self.workspace.io.read_file(&depfile_entry.path)?;
                let depfile = Depfile::parse(&depfile_contents)?;
                for dep in &depfile.deps {
                    // Translate the filesystem path produced by the compiler into an
                    // abstract path within the workspace. Normally this will be a file
                    // inside the output directory.
                    let dep = dep.as_path().normalize()?;
                    let abstract_path = self.workspace.unresolve_path(&dep).map_err(|err| {
                        Error::InvalidPathInDepfile(dep.display().to_string(), err)
                    })?;
                    tracing::debug!("Discovered depfile dependency: {abstract_path}");
                    explicit_dependency_specs.push(self.get_build_spec_relaxed(&abstract_path)?);
                }
            } else if is_implicit_depfile {
                // The implicit depfile was not generated yet, in this run
                // or a previous run. Add that as a reason to rebuild the
                // main recipe. Note that this causes the main recipe to
                // always be outdated if it fails to generate the depfile!
                outdatedness.add_reason(Reason::Missing(Absolute::symbolicate(depfile_path)));
            } else {
                // If the depfile is generated by a rule, it is an error if that
                // rule did not generate the depfile. If it is implicit, it's
                // fine if it doesn't exist yet.
                if self.workspace.io.is_dry_run() {
                    self.workspace.render.warning(
                        Some(task_id),
                        "Depfile does not exist, and was not generated, because this is a dry run",
                    );
                } else {
                    return Err(Error::DepfileNotFound(
                        depfile_path.into_owned().into_inner(),
                    ));
                }
            }
        }

        // Build dependencies!
        let dep_reasons = self
            .build_dependencies(explicit_dependency_specs, dep_chain, out_mtime)
            .await?;
        outdatedness.add_reasons(dep_reasons);

        // Create the parent directory for the target file if it doesn't exist.
        scope
            .workspace()
            .create_output_parent_dirs(&recipe_match.target_file)?;

        let (outdated, new_cache) = outdatedness.finish();
        self.workspace
            .store_build_target_cache(recipe_match.target_file.to_path_buf(), new_cache);

        self.workspace
            .render
            .will_build(task_id, evaluated.commands.len(), &outdated);

        let result = if outdated.is_outdated() {
            tracing::debug!("Rebuilding");
            tracing::trace!("Reasons: {:?}", outdated);
            self.execute_recipe_commands(task_id, evaluated.commands, evaluated.env, true, false)
                .await
                .map(|()| BuildStatus::Complete(task_id, outdated))
        } else {
            tracing::debug!("Up to date");
            Ok(BuildStatus::Complete(task_id, outdated))
        };

        // Check if the implicit depfile was actually generated, and emit a warning if not.
        if let Some(ref implicit_depfile_path) = check_implicit_depfile_was_generated {
            if !self.workspace.io.is_dry_run() {
                if let Ok(None) | Err(_) = self
                    .workspace
                    .get_existing_output_file(implicit_depfile_path)
                {
                    self.workspace.render.warning(
                        Some(task_id),
                        &format!(
                            "Depfile was not generated by the recipe: {implicit_depfile_path}"
                        ),
                    );
                }
            }
        }

        self.workspace.render.did_build(task_id, &result);
        result
    }

    async fn execute_command_recipe(
        self: &Arc<Self>,
        task_id: TaskId,
        recipe: &ir::TaskRecipe<'a>,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let global_scope = RootScope::new(self.workspace);
        let mut scope = TaskRecipeScope::new(&global_scope, task_id);

        // Evaluate dependencies (`out` is not available in commands).

        let evaluated = eval::eval_task_recipe_statements(&mut scope, &recipe.ast.body.statements)?;
        let dependency_specs = evaluated
            .build
            .iter()
            .map(|s| self.get_build_or_command_spec(s))
            .collect::<Result<Vec<_>, _>>()?;

        // Note: We don't care about the status of dependencies.
        self.build_dependencies(dependency_specs, dep_chain, None)
            .await?;

        let outdated = Outdatedness::outdated(Reason::Rebuilt(task_id));
        self.workspace
            .render
            .will_build(task_id, evaluated.commands.len(), &outdated);

        let result = self
            .execute_recipe_commands(task_id, evaluated.commands, evaluated.env, false, true)
            .await
            .map(|()| BuildStatus::Complete(task_id, outdated));

        self.workspace.render.did_build(task_id, &result);
        result
    }

    async fn execute_recipe_commands(
        &self,
        task_id: TaskId,
        run_commands: Vec<RunCommand>,
        mut env: Env,
        silent_by_default: bool,
        forward_stdout: bool,
    ) -> Result<(), Error> {
        let num_steps = run_commands.len();
        if num_steps == 0 {
            return Ok(());
        }

        // Ensure that only the desired number of jobs are running.
        let _limit_concurrency = self
            .workspace
            .runner_state
            .concurrency_limit
            .acquire()
            .await;

        if self.workspace.force_color {
            env.set_force_color();
        } else {
            env.set_no_color();
        }

        let mut silent = silent_by_default;

        if let Some(delay) = self.workspace.artificial_delay {
            smol::Timer::after(delay).await;
        }

        for (step, run_command) in run_commands.into_iter().enumerate() {
            match run_command {
                RunCommand::Shell(command_line) => {
                    self.execute_recipe_run_command(
                        task_id,
                        &command_line,
                        &env,
                        silent,
                        step,
                        num_steps,
                        forward_stdout,
                    )
                    .await?;
                }
                RunCommand::Write(path_buf, vec) => {
                    self.workspace.io.write_file(&path_buf, &vec)?;
                }
                RunCommand::Copy(from, to) => {
                    let Some(src_entry) =
                        self.workspace.get_existing_project_or_output_file(&from)?
                    else {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            "`copy` source file not found in workspace or output directory",
                        )
                        .into());
                    };
                    self.workspace.io.copy_file(&src_entry.path, &to)?;
                }
                RunCommand::Delete(paths) => {
                    self.execute_recipe_delete_command(task_id, &paths, silent)?;
                }
                RunCommand::Info(message) => {
                    self.workspace.render.message(Some(task_id), &message);
                }
                RunCommand::Warn(message) => {
                    self.workspace.render.warning(Some(task_id), &message);
                }
                RunCommand::SetCapture(value) => {
                    silent = value;
                }
                RunCommand::SetEnv(key, value) => {
                    env.env(key, value);
                }
                RunCommand::RemoveEnv(key) => {
                    env.env_remove(key);
                }
            }

            if let Some(delay) = self.workspace.artificial_delay {
                smol::Timer::after(delay).await;
            }
        }

        Ok(())
    }

    #[expect(clippy::too_many_arguments)]
    async fn execute_recipe_run_command(
        &self,
        task_id: TaskId,
        command_line: &ShellCommandLine,
        env: &Env,
        capture: bool,
        step: usize,
        num_steps: usize,
        forward_stdout: bool,
    ) -> Result<(), Error> {
        self.workspace
            .render
            .will_execute(task_id, command_line, step, num_steps);
        let mut child = self.workspace.io.run_recipe_command(
            command_line,
            self.workspace.project_root(),
            env,
            forward_stdout,
        )?;

        // TODO: Avoid this heavy machinery when the renderer isn't
        // interested in the output.
        let mut reader = ChildLinesStream::new(&mut *child, true);
        let result = loop {
            match reader.next().await {
                Some(Err(err)) => break Err(err),
                Some(Ok(output)) => match output {
                    ChildCaptureOutput::Stdout(line) => {
                        self.workspace.render.on_child_process_stdout_line(
                            task_id,
                            command_line,
                            &line,
                        );
                    }
                    ChildCaptureOutput::Stderr(line) => {
                        self.workspace.render.on_child_process_stderr_line(
                            task_id,
                            command_line,
                            &line,
                            capture,
                        );
                    }
                    ChildCaptureOutput::Exit(status) => break Ok(status),
                },
                None => panic!("child process stream ended without an exit status"),
            }
        };

        self.workspace
            .render
            .did_execute(task_id, command_line, &result, step, num_steps);
        let status = result?;
        if !status.success() {
            return Err(Error::CommandFailed(status));
        }
        Ok(())
    }

    fn execute_recipe_delete_command(
        &self,
        task_id: TaskId,
        paths: &[Absolute<std::path::PathBuf>],
        silent: bool,
    ) -> Result<(), Error> {
        for path in paths {
            if self.workspace.is_in_output_directory(path) {
                match self.workspace.io.delete_file(path) {
                    Ok(()) => (),
                    Err(err) => match err.kind() {
                        std::io::ErrorKind::NotFound => {
                            if !silent {
                                self.workspace.render.warning(
                                    Some(task_id),
                                    &format!(
                                        "`delete` ignoring file not found: {}",
                                        path.display()
                                    ),
                                );
                            }
                        }
                        _ => return Err(err.into()),
                    },
                }
            } else if !silent {
                self.workspace.render.warning(
                    Some(task_id),
                    &format!(
                        "cannot `delete` path outside of output directory: {}",
                        path.display()
                    ),
                );
            }
        }

        Ok(())
    }

    async fn build_dependencies(
        self: &Arc<Self>,
        mut dependencies: Vec<TaskSpec<'a>>,
        dependent: DepChainEntry<'_>,
        output_mtime: Option<SystemTime>,
    ) -> Result<Vec<Reason>, Error> {
        // Can't use raw `async fn` because of https://github.com/rust-lang/rust/issues/134101.
        #[expect(clippy::manual_async_fn)]
        fn build_multiple<'a>(
            this: Arc<Inner<'a>>,
            dependencies: Vec<TaskSpec<'a>>,
            dependent: OwnedDependencyChain,
            output_mtime: Option<SystemTime>,
        ) -> impl Future<Output = Result<Vec<Reason>, Error>> + Send + 'a {
            async move {
                let mut tasks = Vec::with_capacity(dependencies.len());
                for dep in dependencies {
                    let parent = dependent.clone();
                    let this2 = this.clone();
                    let task = this
                        .executor
                        .spawn(async move { this2.run_task(dep, DepChain::Owned(&parent)).await });
                    tasks.push(task);
                }

                let mut reasons = Vec::new();
                let mut first_error = None;
                for task in tasks.drain(..) {
                    match task.await {
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
            }
        }

        if dependencies.len() == 1 {
            let dependency = dependencies.pop().unwrap();
            let this = self.clone();
            // Boxing because of recursion.
            Box::pin(async move {
                this.run_task(dependency, DepChain::Ref(&dependent))
                    .await
                    .map(|status| {
                        status
                            .into_outdated_reason(output_mtime)
                            .into_iter()
                            .collect()
                    })
            })
            .await
        } else if !dependencies.is_empty() {
            let parent = dependent.collect();
            let this = self.clone();

            // Boxing for recursion.
            Box::pin(build_multiple(
                this.clone(),
                dependencies,
                parent,
                output_mtime,
            ))
            .await
        } else {
            Ok(vec![])
        }
    }

    /// Unconditionally run the task if it is outdated.
    async fn rebuild_spec(
        self: &Arc<Self>,
        task_id: TaskId,
        spec: TaskSpec<'a>,
        dep_chain: DepChain<'_>,
    ) -> Result<BuildStatus, Error> {
        let dep_chain_entry = dep_chain.push(task_id);

        match spec {
            TaskSpec::Recipe(recipe) => match recipe {
                ir::RecipeMatch::Task(recipe) => {
                    self.execute_command_recipe(task_id, recipe, dep_chain_entry)
                        .await
                }
                ir::RecipeMatch::Build(recipe_match) => {
                    self.execute_build_recipe(task_id, recipe_match, dep_chain_entry)
                        .await
                }
            },
            TaskSpec::CheckExists(path) => self.check_exists(&path),
            TaskSpec::CheckExistsRelaxed(path) => match self.check_exists(&path) {
                Err(Error::NoRuleToBuildTarget(_)) => Ok(BuildStatus::Complete(
                    task_id,
                    Outdatedness::outdated(Reason::Missing(Absolute::symbolicate(&path))),
                )),
                otherwise => otherwise,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RunCommand {
    Shell(ShellCommandLine),
    Write(Absolute<std::path::PathBuf>, Vec<u8>),
    // We don't know yet if the source file is in the workspace or output
    // directory, so we will resolve the path when running it.
    Copy(Absolute<werk_fs::PathBuf>, Absolute<std::path::PathBuf>),
    Info(String),
    Warn(String),
    // Path is always in the output directory. They don't need to exist.
    Delete(Vec<Absolute<std::path::PathBuf>>),
    SetCapture(bool),
    SetEnv(String, String),
    RemoveEnv(String),
}

impl std::fmt::Display for RunCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunCommand::Shell(shell_command_line) => shell_command_line.fmt(f),
            RunCommand::Write(path_buf, vec) => {
                write!(f, "write {} ({} bytes)", path_buf.display(), vec.len())
            }
            RunCommand::Copy(from, to) => {
                write!(f, "copy '{}' to '{}'", from, to.display())
            }
            RunCommand::Info(message) => {
                write!(f, "info \"{}\"", message.escape_default())
            }
            RunCommand::Warn(message) => {
                write!(f, "warn \"{}\"", message.escape_default())
            }
            RunCommand::Delete(paths) => {
                write!(f, "delete ")?;
                if paths.len() == 1 {
                    write!(f, "{}", paths[0].display())
                } else {
                    write!(f, "[")?;
                    for (i, p) in paths.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", p.display())?;
                    }
                    write!(f, "]")
                }
            }
            RunCommand::SetCapture(value) => write!(f, "set_capture = {value}"),
            RunCommand::SetEnv(key, value) => write!(f, "env {key} = {value}"),
            RunCommand::RemoveEnv(key) => write!(f, "env-remove {key}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct DepChainEntry<'a> {
    parent: DepChain<'a>,
    this: TaskId,
}

#[derive(Debug, Clone, Copy)]
enum DepChain<'a> {
    Empty,
    Owned(&'a OwnedDependencyChain),
    Ref(&'a DepChainEntry<'a>),
}

impl<'a> DepChain<'a> {
    fn collect_vec(&self) -> Vec<TaskId> {
        match self {
            DepChain::Empty => Vec::new(),
            DepChain::Owned(owned) => owned.vec.clone(),
            DepChain::Ref(parent) => parent.collect_vec(),
        }
    }

    pub fn contains(&self, task: TaskId) -> bool {
        match self {
            DepChain::Empty => false,
            DepChain::Owned(owned) => owned.vec.contains(&task),
            DepChain::Ref(parent) => parent.contains(task),
        }
    }

    pub fn push<'b>(self, task: TaskId) -> DepChainEntry<'b>
    where
        'a: 'b,
    {
        DepChainEntry {
            parent: self,
            this: task,
        }
    }
}

impl DepChainEntry<'_> {
    fn collect(&self) -> OwnedDependencyChain {
        OwnedDependencyChain {
            vec: self.collect_vec(),
        }
    }

    fn collect_vec(&self) -> Vec<TaskId> {
        let mut vec = self.parent.collect_vec();
        vec.push(self.this);
        vec
    }

    fn contains(&self, task_id: TaskId) -> bool {
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

impl OwnedDependencyChain {
    #[inline]
    #[must_use]
    pub fn into_inner(self) -> Vec<TaskId> {
        self.vec
    }
}

impl std::fmt::Display for OwnedDependencyChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, task_id) in self.vec.iter().enumerate() {
            if i > 0 {
                write!(f, " -> ")?;
            }
            write!(f, "{task_id}")?;
        }
        Ok(())
    }
}
