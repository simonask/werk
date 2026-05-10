use std::{sync::Arc, time::SystemTime};

use futures::{StreamExt, future, pin_mut};
use parking_lot::Mutex;
use stringleton::Symbol;
use werk_eval::{
    Env, Eval, EvalError, EvaluatedBuildRecipe, EvaluatedTaskRecipe, RunCommand, Scope,
    ShellCommandLine, TaskName, Warning,
};
use werk_fs::{Absolute, PathError, SymPath};
use werk_planner::{EvaluatedTask, PlannerError, RecipeMatch, TaskGraph, TaskId, TaskSpec};
use werk_util::{Annotated, AsDiagnostic, DiagnosticSpan, IoError, broadcast_one, cancel};

use crate::{
    ChildCaptureOutput, ChildLinesStream, Error, Outdatedness, OutdatednessTracker, Reason,
    Workspace,
};

pub struct Runner<'a> {
    inner: Arc<Inner<'a>>,
    state: Mutex<Option<RunState>>,
}

struct RunState {
    status_receivers: Vec<broadcast_one::Receiver<Result<BuildStatus, Error>>>,
}

struct Inner<'a> {
    workspace: &'a Workspace,
    /// Executor for running tasks. All tasks here *must* subscribe to the
    /// cancellation signal, or deadlocks can occur.
    executor: smol::Executor<'a>,
    cancel: cancel::Sender,
    concurrency_limit: smol::lock::Semaphore,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildStatus {
    /// Target was built, along with the outdatedness. If the outdatedness is
    /// empty, the target was determined to be up-to-date.
    Complete(TaskName, Outdatedness),
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

    /// Given an output file modification time, return the outdatedness of the
    /// target. If the target is up-to-date, the outdatedness will be empty. If
    /// an output mtime is not available, returns empty outdatedness.
    #[must_use]
    pub(crate) fn as_outdated_reason(&self, output_mtime: Option<SystemTime>) -> Option<Reason> {
        match self {
            BuildStatus::Complete(task_id, outdatedness) => {
                if outdatedness.is_outdated() {
                    Some(Reason::Rebuilt(*task_id))
                } else {
                    None
                }
            }
            BuildStatus::Exists(path_buf, system_time) => {
                let output_mtime = output_mtime?;

                if output_mtime <= *system_time {
                    Some(Reason::Modified(*path_buf, *system_time))
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
                concurrency_limit: smol::lock::Semaphore::new(workspace.max_concurrent_jobs.max(1)),
            }),
            state: Mutex::new(None),
        }
    }

    pub fn get_status(&self, task_id: TaskId) -> Option<Result<BuildStatus, Error>> {
        self.state.lock().as_ref().and_then(|run_state| {
            run_state
                .status_receivers
                .get(task_id.index())
                .and_then(|receiver| {
                    receiver.try_recv().map(|r| match r {
                        Ok(task_result) => task_result.clone(),
                        Err(broadcast_one::Disconnected) => Err(Error::Cancelled(TaskName::Task(
                            Symbol::new("cancelled task"),
                        ))),
                    })
                })
        })
    }

    pub async fn run(&self, mut task_graph: TaskGraph<'a>) -> Result<(), Annotated<'a, Error>> {
        let tasks = {
            let mut run_state = self.state.lock();
            assert!(
                !run_state.is_some(),
                "Runner is already running; reset it first"
            );
            let mut tasks = Vec::with_capacity(task_graph.num_tasks());
            let mut status_senders = Vec::with_capacity(task_graph.num_tasks());
            let mut status_receivers = Vec::with_capacity(task_graph.num_tasks());

            for _ in 0..task_graph.num_tasks() {
                let (send, recv) = broadcast_one::channel();
                status_senders.push(Some(send));
                status_receivers.push(recv);
            }

            for index in 0..task_graph.num_tasks() {
                let task_id = TaskId::from_index(index);
                let evaluated_task = task_graph.take_task(task_id);
                let task_spec = task_graph.get_task_spec(task_id).clone();
                let result_sender = status_senders[task_id.index()].take().unwrap();
                let wait_for_dependencies = task_graph
                    .get_task_dependencies(task_id)
                    .iter()
                    .map(|dep_id| status_receivers[dep_id.index()].clone())
                    .collect::<Vec<_>>();
                tasks.push(self.inner.executor.spawn(self.inner.clone().run_task(
                    self.inner.cancel.receiver(),
                    task_spec,
                    evaluated_task,
                    wait_for_dependencies,
                    result_sender,
                )));
            }
            *run_state = Some(RunState { status_receivers });
            tasks
        };

        self.inner
            .executor
            .run(async move {
                for task in tasks {
                    task.await?;
                }
                Ok::<(), Error>(())
            })
            .await
            .map_err(|err| err.into_diagnostic_error(&self.inner.workspace.manifest))
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

    pub async fn reset(&self) {
        self.wait_for_long_running_tasks().await;
        *self.state.lock() = None;
    }
}

impl<'a> Inner<'a> {
    /// Build the task, waiting for dependencies and signaling dependents.
    async fn run_task(
        self: Arc<Self>,
        cancel: cancel::Receiver,
        spec: TaskSpec<'a>,
        task: EvaluatedTask,
        wait_for_dependencies: Vec<broadcast_one::Receiver<Result<BuildStatus, Error>>>,
        result_sender: broadcast_one::Sender<Result<BuildStatus, Error>>,
    ) -> Result<(), Error> {
        match task {
            EvaluatedTask::Build(recipe) => {
                match self
                    .execute_build_recipe(spec, recipe, wait_for_dependencies, cancel)
                    .await
                {
                    Ok(status) => {
                        result_sender.send(Ok(status));
                        Ok(())
                    }
                    Err(err) => {
                        result_sender.send(Err(err.clone()));
                        Err(err)
                    }
                }
            }
            EvaluatedTask::Task(recipe) => {
                match self
                    .execute_task_recipe(spec, recipe, wait_for_dependencies, cancel)
                    .await
                {
                    Ok(status) => {
                        result_sender.send(Ok(status));
                        Ok(())
                    }
                    Err(err) => {
                        result_sender.send(Err(err.clone()));
                        Err(err)
                    }
                }
            }
            EvaluatedTask::CheckExists => {
                debug_assert!(wait_for_dependencies.is_empty());
                match spec {
                    TaskSpec::Recipe(_) => unreachable!(),
                    TaskSpec::CheckExists(path) => {
                        let result = self.check_exists(path.as_path())?;
                        result_sender.send(Ok(result.clone()));
                        Ok(())
                    }
                    TaskSpec::CheckExistsRelaxed(path) => {
                        let result = self.check_exists_relaxed(path.as_path());
                        result_sender.send(Ok(result.clone()));
                        Ok(())
                    }
                }
            }
        }
    }

    async fn execute_task_recipe(
        self: Arc<Self>,
        spec: TaskSpec<'a>,
        recipe: EvaluatedTaskRecipe,
        wait_for_dependencies: Vec<broadcast_one::Receiver<Result<BuildStatus, Error>>>,
        cancel: cancel::Receiver,
    ) -> Result<BuildStatus, Error> {
        let TaskSpec::Recipe(RecipeMatch::Task(recipe_match)) = spec else {
            unreachable!()
        };
        let task_name = TaskName::Task(recipe_match.name);

        // Wait for dependencies.
        for dep_status in wait_for_dependencies {
            match dep_status.recv().await {
                Ok(Ok(_)) => {}
                Ok(Err(err)) => {
                    return Err(Error::DependencyFailed(task_name, Arc::new(err.clone())));
                }
                Err(broadcast_one::Disconnected) => return Err(Error::Cancelled(task_name)),
            }
        }

        let outdated = Outdatedness::outdated(Reason::Rebuilt(task_name));
        self.workspace
            .render
            .will_build(task_name, recipe.commands.len(), &outdated);

        let result = if outdated.is_outdated() {
            tracing::debug!("Rebuilding");
            tracing::trace!("Reasons: {:?}", outdated);
            self.execute_recipe_commands(
                task_name,
                cancel,
                recipe.commands,
                recipe.env,
                true,
                false,
            )
            .await
            .map(|()| BuildStatus::Complete(task_name, outdated))
        } else {
            tracing::debug!("Up to date");
            Ok(BuildStatus::Complete(task_name, outdated))
        };

        self.workspace.render.did_build(task_name, &result);
        result
    }

    async fn execute_build_recipe(
        self: Arc<Self>,
        spec: TaskSpec<'a>,
        recipe: Eval<EvaluatedBuildRecipe>,
        wait_for_dependencies: Vec<broadcast_one::Receiver<Result<BuildStatus, Error>>>,
        cancel: cancel::Receiver,
    ) -> Result<BuildStatus, Error> {
        let TaskSpec::Recipe(RecipeMatch::Build(recipe_match)) = spec else {
            unreachable!()
        };
        let task_name = TaskName::Build(recipe_match.target_file);
        let cache = self
            .workspace
            .take_build_target_cache(recipe_match.target_file);
        let out_mtime = match self
            .workspace
            .stat_file_if_exists(recipe_match.target_file.as_path())
        {
            Ok(Some(entry)) => Some(entry.metadata.mtime),
            Ok(None) => None,
            Err(err) if err.error.kind() == std::io::ErrorKind::IsADirectory => {
                return Err(Error::TargetIsADirectory {
                    span: recipe_match.recipe.span,
                    path: recipe_match.target_file.as_path().to_owned().into_inner(),
                });
            }
            Err(err) => return Err(err.into()),
        };

        let mut outdatedness = OutdatednessTracker::new(
            self.workspace,
            cache.as_ref(),
            recipe_match.recipe,
            out_mtime,
        );

        // Include changes to the recipe or config variables as reasons for outdatedness.
        outdatedness.did_use(recipe.used);

        // Rebuild if the target does not exist.
        if let Some(mtime) = out_mtime {
            tracing::debug!("Output exists, mtime: {mtime:?}");
        } else {
            tracing::debug!("Output file missing, target is outdated");
            outdatedness.missing(recipe_match.target_file);
        }

        // Wait for dependencies.
        let mut reasons = Vec::with_capacity(wait_for_dependencies.len());
        for dep_status in wait_for_dependencies {
            match dep_status.recv().await {
                Ok(Ok(dep_status)) => {
                    reasons.extend(dep_status.as_outdated_reason(out_mtime));
                }
                Ok(Err(err)) => {
                    return Err(Error::DependencyFailed(task_name, Arc::new(err.clone())));
                }
                Err(broadcast_one::Disconnected) => return Err(Error::Cancelled(task_name)),
            }
        }

        // Consider this target outdated if any of the dependencies were rebuilt.
        outdatedness.add_reasons(reasons);

        // Create the parent directory for the target file if it doesn't exist.
        self.workspace
            .create_parent_dirs(recipe_match.target_file.as_path())?;

        let (outdated, new_cache) = outdatedness.finish();
        self.workspace
            .store_build_target_cache(recipe_match.target_file, new_cache);

        let evaluated = recipe.value;
        self.workspace
            .render
            .will_build(task_name, evaluated.commands.len(), &outdated);

        let result = if outdated.is_outdated() {
            tracing::debug!("Rebuilding");
            tracing::trace!("Reasons: {:?}", outdated);
            self.execute_recipe_commands(
                task_name,
                cancel,
                evaluated.commands,
                evaluated.env,
                true,
                false,
            )
            .await
            .map(|()| BuildStatus::Complete(task_name, outdated))
        } else {
            tracing::debug!("Up to date");
            Ok(BuildStatus::Complete(task_name, outdated))
        };

        self.workspace.render.did_build(task_name, &result);
        result
    }

    fn check_exists(&self, path: &Absolute<werk_fs::Path>) -> Result<BuildStatus, Error> {
        let Some(entry) = self.workspace.stat_file_if_exists(path)? else {
            return Err(Error::Planner(PlannerError::NoRuleToBuildTarget(
                path.to_string(),
            )));
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
        let mtime = self
            .workspace
            .stat_file_if_exists(path)
            .ok()
            .flatten()
            .map(|e| e.metadata.mtime);

        if let Some(mtime) = mtime {
            tracing::debug!("Check file mtime `{path}`: {mtime:?}");
            BuildStatus::Exists(Absolute::symbolicate(path), mtime)
        } else {
            // The dependency could not be found anywhere, so just ignore it.
            tracing::debug!("Depfile dependency not found, ignoring it: {path}");
            BuildStatus::Ignore(Absolute::symbolicate(path))
        }
    }

    async fn execute_recipe_commands(
        &self,
        task_id: TaskName,
        cancel: cancel::Receiver,
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
        let _limit_concurrency = self.concurrency_limit.acquire().await;

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
                        cancel.clone(),
                    )
                    .await?;
                }
                RunCommand::Spawn(command_line) => {
                    self.execute_recipe_spawn_command(
                        task_id,
                        &command_line,
                        &env,
                        silent,
                        step,
                        num_steps,
                        forward_stdout,
                        cancel.clone(),
                    )?;
                }
                RunCommand::Write(path_buf, vec) => {
                    self.workspace.io.write_file(&path_buf, &vec)?;
                }
                RunCommand::Copy(from, to) => {
                    let src_entry = self.workspace.stat_file(&from)?;
                    self.workspace.io.copy_file(&src_entry.path, &to)?;
                }
                RunCommand::Delete(span, paths) => {
                    self.execute_recipe_delete_command(task_id, &paths, silent, span)?;
                }
                RunCommand::Touch(span, paths) => {
                    self.execute_recipe_touch_command(&paths, span)?;
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
                future::select(cancel.clone(), smol::Timer::after(delay)).await;
            }
        }

        Ok(())
    }

    #[expect(clippy::too_many_arguments)]
    async fn execute_recipe_run_command(
        &self,
        task_id: TaskName,
        command_line: &ShellCommandLine,
        env: &Env,
        capture: bool,
        step: usize,
        num_steps: usize,
        forward_stdout: bool,
        cancel: cancel::Receiver,
    ) -> Result<(), Error> {
        pin_mut!(cancel);

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
            match future::select(cancel.as_mut(), next).await {
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
        let status = result.map_err(|e| IoError::new(&command_line.program, e))?;
        if !status.success() {
            return Err(Error::CommandFailed(status));
        }
        Ok(())
    }

    #[expect(clippy::too_many_arguments)]
    fn execute_recipe_spawn_command(
        &self,
        task_id: TaskName,
        command_line: &ShellCommandLine,
        env: &Env,
        capture: bool,
        step: usize,
        num_steps: usize,
        forward_stdout: bool,
        cancel: cancel::Receiver,
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

        let render = self.workspace.render.clone();
        let command_line = command_line.clone();

        self.executor
            .spawn(async move {
                pin_mut!(cancel);
                let mut reader = ChildLinesStream::new(&mut *child, true);
                let result = loop {
                    let next = reader.next();
                    match future::select(cancel.as_mut(), next).await {
                        future::Either::Left((_canceled, _)) => {
                            render.message(
                                Some(task_id),
                                &format!("Terminating spawned: {command_line}"),
                            );
                            _ = child.kill();
                            _ = reader.wait().await;
                            return;
                        }
                        future::Either::Right((Some(Err(err)), _)) => break Err(err),
                        future::Either::Right((Some(Ok(output)), _)) => match output {
                            ChildCaptureOutput::Stdout(line) => {
                                render.on_child_process_stdout_line(task_id, &command_line, &line);
                            }
                            ChildCaptureOutput::Stderr(line) => {
                                render.on_child_process_stderr_line(
                                    task_id,
                                    &command_line,
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

                render.did_execute(task_id, &command_line, &result, step, num_steps);
            })
            .detach();

        Ok(())
    }

    fn execute_recipe_delete_command(
        &self,
        task_id: TaskName,
        paths: &[Absolute<std::path::PathBuf>],
        silent: bool,
        span: DiagnosticSpan,
    ) -> Result<(), Error> {
        for path in paths {
            self.check_protected(path, span, "delete")?;
        }
        for path in paths {
            match self.workspace.io.delete_file(path) {
                Ok(()) => (),
                Err(err) => match err.error.kind() {
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
        }

        Ok(())
    }

    fn execute_recipe_touch_command(
        &self,
        paths: &[Absolute<std::path::PathBuf>],
        span: DiagnosticSpan,
    ) -> Result<(), Error> {
        for path in paths {
            self.check_protected(path, span, "touch")?;
        }

        for path in paths {
            self.workspace.io.touch(path)?;
        }

        Ok(())
    }

    fn check_protected(
        &self,
        path: &Absolute<std::path::Path>,
        span: DiagnosticSpan,
        operation: &'static str,
    ) -> Result<(), Error> {
        let workspace_path = match self.workspace.unresolve_path(path) {
            Ok(path) => path,
            Err(PathError::UnresolveBeyondRoot(resolved)) => {
                return Err(Error::WriteBeyondRoot {
                    span: Some(span),
                    op: operation,
                    path: resolved,
                });
            }
            Err(err) => return Err(Error::eval(EvalError::Path(span, err))),
        };

        if self.workspace.is_path_protected(&workspace_path) {
            return Err(Error::ProtectedPath {
                span: Some(span),
                op: operation,
                path: path.as_inner().to_owned(),
            });
        }

        Ok(())
    }
}
