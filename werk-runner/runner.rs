use std::{future::Future, sync::Arc, time::SystemTime};

use futures::{StreamExt, channel::oneshot, future};
use indexmap::{IndexMap, map::Entry};
use parking_lot::Mutex;
use werk_fs::{Absolute, Normalize as _, Path, SymPath};
use werk_util::{Annotated, AsDiagnostic, DiagnosticSpan, Symbol, cancel};

use crate::{
    AmbiguousPatternError, BuildRecipeScope, ChildCaptureOutput, ChildLinesStream, Env, Error,
    Outdatedness, OutdatednessTracker, Reason, Scope as _, ShellCommandLine, TaskRecipeScope,
    Value, Warning, Workspace, WorkspaceSettings,
    depfile::Depfile,
    eval::{self, Eval},
    ir,
};

mod command;
mod dep_chain;
mod task;

pub use command::*;
pub use dep_chain::*;
pub use task::*;

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
    workspace: &'a Workspace,
    /// Executor for running tasks. All tasks here *must* subscribe to the
    /// cancellation signal, or deadlocks can occur.
    executor: smol::Executor<'a>,
    cancel: cancel::Sender,
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
    /// The target does not exist, and that's fine.
    Ignore(Absolute<SymPath>),
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
            BuildStatus::Ignore(_) => None,
        }
    }
}

impl<'a> Runner<'a> {
    pub fn new(workspace: &'a Workspace) -> Self {
        Self {
            inner: Arc::new(Inner {
                workspace,
                executor: smol::Executor::new(),
                cancel: cancel::Sender::new(),
            }),
        }
    }

    pub async fn build_file(
        &self,
        target: &Path,
    ) -> Result<BuildStatus, Annotated<Error, &'a Workspace>> {
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
    ) -> Result<BuildStatus, Annotated<Error, &'a Workspace>> {
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
    ) -> Result<BuildStatus, Annotated<Error, &'a Workspace>> {
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

    /// Stop long-running child processes and wait for them to finish.
    pub async fn stop(&self, timeout: std::time::Duration) {
        self.inner.cancel.cancel();
        let timer = smol::Timer::after(timeout);

        let wait = self.wait_for_long_running_tasks();
        smol::pin!(wait);

        match future::select(wait, timer).await {
            future::Either::Left(_) => (),
            future::Either::Right(_) => {
                self.inner
                    .workspace
                    .render
                    .warning(None, &Warning::ZombieChild);
            }
        }
    }

    pub async fn wait_for_long_running_tasks(&self) {
        while !self.inner.executor.is_empty() {
            self.inner.executor.tick().await;
        }
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
                        pattern1: task_recipe.span.file.span(task_recipe.ast.name.span),
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
                    TaskStatus::Built(result) => Scheduling::Done(result.clone()),
                    TaskStatus::Pending(waiters) => {
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
                let cancel = self.cancel.receiver();
                let result = self
                    .clone()
                    .rebuild_spec(task_id, task_spec, &cancel, dep_chain)
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

    /// Existence check used for dependencies discovered in depfiles.
    ///
    /// 1. If the file exists in the workspace, use the workspace file's mtime.
    /// 2. Otherwise, if the file exists in the output directory, use its mtime.
    /// 3. When the file neither exists in the filesystem, nor in the output
    ///    directory, nor is there a build recipe to produce it, ignore it.
    fn check_exists_relaxed(&self, path: &Absolute<werk_fs::Path>) -> BuildStatus {
        let mtime = if let Some(entry) = self.workspace.get_project_file(path) {
            Some(entry.metadata.mtime)
        } else if let Ok(Some(entry)) = self.workspace.get_existing_output_file(path) {
            Some(entry.metadata.mtime)
        } else {
            None
        };

        if let Some(mtime) = mtime {
            tracing::debug!("Check file mtime `{path}`: {mtime:?}");
            BuildStatus::Exists(Absolute::symbolicate(path), mtime)
        } else {
            // The dependency could not be found anywhere, so just ignore it.
            tracing::debug!("Depfile dependency not found anywhere, ignoring it: {path}");
            BuildStatus::Ignore(Absolute::symbolicate(path))
        }
    }

    #[tracing::instrument(level = "debug", skip_all, fields(target_file))]
    async fn execute_build_recipe(
        self: &Arc<Self>,
        task_id: TaskId,
        recipe_match: ir::BuildRecipeMatch<'_>,
        cancel: &cancel::Receiver,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let mut scope = BuildRecipeScope::new(self.workspace, task_id, &recipe_match);
        scope.set(
            Symbol::new("out"),
            Eval::inherent(Value::from(recipe_match.target_file.to_string())),
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
            recipe_match.recipe.span.file,
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
                .map_err(|err| Error::InvalidTargetPath(depfile.string.clone(), err))?;
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
                if !self.workspace.io.is_dry_run() {
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
            self.execute_recipe_commands(
                task_id,
                cancel,
                evaluated.commands,
                evaluated.env,
                true,
                false,
            )
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
                        &Warning::DepfileNotGenerated(
                            recipe_match.recipe.pattern.span,
                            self.workspace
                                .get_output_file_path(implicit_depfile_path)
                                .unwrap(),
                        ),
                    );
                }
            }
        }

        self.workspace.render.did_build(task_id, &result);
        result
    }

    async fn execute_task_recipe(
        self: &Arc<Self>,
        task_id: TaskId,
        recipe: &ir::TaskRecipe,
        cancel: &cancel::Receiver,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let mut scope = TaskRecipeScope::new(self.workspace, task_id);

        // Evaluate dependencies (`out` is not available in commands).

        let evaluated = eval::eval_task_recipe_statements(
            &mut scope,
            &recipe.ast.body.statements,
            recipe.span.file,
        )?;
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
            .execute_recipe_commands(
                task_id,
                cancel,
                evaluated.commands,
                evaluated.env,
                false,
                true,
            )
            .await
            .map(|()| BuildStatus::Complete(task_id, outdated));

        self.workspace.render.did_build(task_id, &result);
        result
    }

    async fn execute_recipe_commands(
        &self,
        task_id: TaskId,
        cancel: &cancel::Receiver,
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
                        cancel,
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
                RunCommand::Delete(span, paths) => {
                    self.execute_recipe_delete_command(task_id, &paths, silent, span)?;
                }
                RunCommand::Touch(span, paths) => {
                    self.execute_recipe_touch_command(task_id, &paths, silent, span)?;
                }
                RunCommand::Info(_span, message) => {
                    self.workspace.render.message(Some(task_id), &message);
                }
                RunCommand::Warn(span, message) => {
                    self.workspace
                        .render
                        .warning(Some(task_id), &Warning::WarningExpression(span, message));
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
                future::select(cancel, smol::Timer::after(delay)).await;
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
        cancel: &cancel::Receiver,
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
            let next = reader.next();
            match future::select(cancel, next).await {
                future::Either::Left((_canceled, _)) => {
                    _ = child.kill();
                    _ = reader.wait().await;
                    return Err(Error::Cancelled(task_id));
                }
                future::Either::Right((Some(Err(err)), _)) => break Err(err),
                future::Either::Right((Some(Ok(output)), _)) => match output {
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
                future::Either::Right((None, _)) => {
                    panic!("child process stream ended without an exit status")
                }
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
        span: DiagnosticSpan,
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
                                    &Warning::IgnoringFileNotFound(span, path.clone()),
                                );
                            }
                        }
                        _ => return Err(err.into()),
                    },
                }
            } else if !silent {
                self.workspace.render.warning(
                    Some(task_id),
                    &Warning::IgnoringPathOutsideOutputDirectory(span, path.clone()),
                );
            }
        }

        Ok(())
    }

    fn execute_recipe_touch_command(
        &self,
        task_id: TaskId,
        paths: &[Absolute<std::path::PathBuf>],
        silent: bool,
        span: DiagnosticSpan,
    ) -> Result<(), Error> {
        for path in paths {
            if self.workspace.is_in_output_directory(path) {
                self.workspace.io.touch(path)?;
            } else if !silent {
                self.workspace.render.warning(
                    Some(task_id),
                    &Warning::IgnoringPathOutsideOutputDirectory(span, path.clone()),
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
        cancel: &cancel::Receiver,
        dep_chain: DepChain<'_>,
    ) -> Result<BuildStatus, Error> {
        let dep_chain_entry = dep_chain.push(task_id);

        match spec {
            TaskSpec::Recipe(recipe) => match recipe {
                ir::RecipeMatch::Task(recipe) => {
                    self.execute_task_recipe(task_id, recipe, cancel, dep_chain_entry)
                        .await
                }
                ir::RecipeMatch::Build(recipe_match) => {
                    self.execute_build_recipe(task_id, recipe_match, cancel, dep_chain_entry)
                        .await
                }
            },
            TaskSpec::CheckExists(path) => self.check_exists(&path),
            TaskSpec::CheckExistsRelaxed(path) => Ok(self.check_exists_relaxed(&path)),
        }
    }
}
