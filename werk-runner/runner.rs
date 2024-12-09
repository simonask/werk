use std::{future::Future, marker::PhantomData, pin::Pin, sync::Arc, time::SystemTime};

use ahash::HashSet;
use futures::channel::oneshot;
use indexmap::{map::Entry, IndexMap};
use parking_lot::Mutex;
use werk_fs::{Path, PathBuf};
use werk_parser::ast;

use crate::{
    depfile::Depfile, eval, eval_shell_commands, eval_shell_commands_run_which_and_detect_outdated,
    eval_string_expr, Error, Eval, Io, LocalVariables, Outdatedness, PatternMatch, Reason,
    RecipeMatch, RecipeMatchData, RecipeScope, Recipes, RootScope, Scope as _, ShellCommandLine,
    Value, Workspace, WorkspaceSettings,
};

pub struct Runner<'a> {
    inner: Arc<Inner>,
    _marker: PhantomData<&'a ()>,
}

struct Inner {
    io: Arc<dyn Io>,
    // Variables from the global scope.
    globals: LocalVariables,
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
    fn will_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    );
    /// Shell command is finished executing, or failed to start. Note that
    /// `result` will be `Ok` even if the command returned an error, allowing
    /// access to the command's stdout/stderr.
    ///
    /// The runner guarantees that if an `Ok(output)` is passed to this
    /// function,
    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
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
    pub fn into_output_outdatedness(self, output_mtime: Option<SystemTime>) -> Outdatedness {
        match self {
            BuildStatus::Complete(task_id, outdatedness) => {
                if outdatedness.is_outdated() {
                    Outdatedness::outdated(Reason::Rebuilt(task_id.clone()))
                } else {
                    Outdatedness::unchanged()
                }
            }
            BuildStatus::Exists(path_buf, system_time) => {
                let Some(output_mtime) = output_mtime else {
                    return Outdatedness::unchanged();
                };

                if output_mtime <= system_time {
                    Outdatedness::outdated(Reason::Modified(path_buf, system_time))
                } else {
                    Outdatedness::unchanged()
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
pub enum TaskId {
    Command(String),
    Build(PathBuf),
}

impl TaskId {
    pub fn command(s: impl Into<String>) -> Self {
        TaskId::Command(s.into())
    }

    pub fn build(p: impl Into<PathBuf>) -> Self {
        TaskId::Build(p.into())
    }
}

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TaskId::Command(s) => f.write_str(s),
            TaskId::Build(p) => f.write_str(p.as_str()),
        }
    }
}

enum TaskSpec {
    Recipe(RecipeMatchData),
    CheckExists(werk_fs::PathBuf),
}

impl TaskSpec {
    pub fn to_task_id(&self) -> TaskId {
        match self {
            TaskSpec::Recipe(recipe_match_data) => recipe_match_data.to_task_id(),
            TaskSpec::CheckExists(path_buf) => TaskId::Build(path_buf.clone()),
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
        let mut globals = LocalVariables::new();
        for (name, value) in &ast.global {
            if let Some(def) = workspace.defines.get(name) {
                globals.insert(name.to_owned(), Eval::unchanged(Value::String(def.clone())));
                continue;
            }

            // Creating a new scope every time, because it's cheap, and it
            // simplifies things because we don't need a `RootScopeMut` variant.
            let scope = RootScope::new(&globals, &workspace, &*watcher);
            let value = eval(&scope, &*io, value).await?;
            tracing::trace!(
                "global var '{name}' = {:?}, outdated = {:?}",
                value.value,
                value.outdatedness
            );
            globals.insert(name.to_owned(), value);
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
    pub fn globals(&self) -> &LocalVariables {
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

    async fn parse_depfile_build_specs<'a>(
        &'a self,
        task_id: &TaskId,
        depfile_path: &werk_fs::Path,
    ) -> Result<Vec<TaskSpec>, Error> {
        let Some(dir_entry) = self
            .workspace
            .get_existing_output_file(&*self.io, depfile_path)?
        else {
            if self.io.is_dry_run() {
                self.watcher.warning(
                    Some(task_id),
                    "Depfile does not exist, and was not generated, because this is a dry run",
                );
                return Ok(vec![]);
            } else {
                return Err(Error::DepfileNotFound(depfile_path.to_path_buf()));
            }
        };

        tracing::debug!("Parsing depfile: {}", dir_entry.path.display());
        let depfile_contents = self.io.read_file(&dir_entry.path).await?;
        let depfile = Depfile::parse(&depfile_contents)?;
        let mut specs = Vec::new();
        for dep in &depfile.deps {
            // Translate the filesystem path produced by the compiler into an
            // abstract path within the workspace. Normally this will be a file
            // inside the output directory.
            let abstract_path = self.workspace.unresolve_path(dep)?;
            specs.push(self.get_build_spec(&abstract_path)?);
        }
        Ok(specs)
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

    async fn execute_build_recipe(
        self: &Arc<Self>,
        task_id: &TaskId,
        recipe: &ast::Recipe,
        pattern_match: &PatternMatch<'_>,
        target_file: &werk_fs::Path,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let global_scope = RootScope::new(&self.globals, &self.workspace, &*self.watcher);
        let mut scope = RecipeScope::new(&global_scope, task_id, Some(pattern_match));
        scope.set(
            "out".to_owned(),
            Eval::unchanged(Value::String(target_file.to_string())),
        );

        let mut outdated = Outdatedness::unchanged();

        // Evaluate direct dependencies (`out` is available).
        let mut deps = Vec::new();
        if let Some(ref build_deps) = recipe.in_files {
            let build_deps = eval(&scope, &*self.io, build_deps).await?;
            outdated |= build_deps.outdatedness;
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
        tracing::debug!("in = {in_values:?}");
        scope.set(String::from("in"), Eval::unchanged(in_values));

        let mut depfile_deps = Vec::new();
        let mut depfile_paths = HashSet::default();
        if let Some(ref depfiles) = recipe.depfiles {
            let depfile_deps_eval = eval(&scope, &*self.io, depfiles).await?;
            outdated |= depfile_deps_eval.outdatedness;
            depfile_deps_eval.value.try_collect_strings_recursive(|s| {
                let depfile_path = werk_fs::Path::new(&s)?;
                let dep = self.get_build_spec(depfile_path)?;
                depfile_deps.push(dep);
                depfile_paths.insert(depfile_path.to_path_buf());
                Ok::<_, Error>(())
            })?;
        }

        // Check the target's mtime.
        let out_mtime = scope
            .workspace()
            .get_existing_output_file(&*self.io, target_file)?
            .map(|entry| entry.metadata.mtime);

        // Rebuild if the target does not exist.
        if out_mtime.is_none() {
            outdated.insert(Reason::Missing(target_file.to_path_buf()));
        }

        // Build depfiles, if any.
        outdated |= self
            .build_dependencies(depfile_deps, dep_chain, out_mtime)
            .await?;

        // Parse depfiles and add them to the dependency list.
        for depfile_path in &depfile_paths {
            let depfile_deps = self
                .parse_depfile_build_specs(task_id, depfile_path)
                .await?;
            deps.extend(depfile_deps);
        }

        outdated |= self.build_dependencies(deps, dep_chain, out_mtime).await?;

        // Figure out what to run.
        let mut shell_commands = if let Some(commands) = &recipe.command {
            eval_shell_commands_run_which_and_detect_outdated(
                &scope,
                commands,
                self.workspace.force_color,
            )?
        } else {
            Default::default()
        };
        outdated |= shell_commands.outdatedness;

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

        self.watcher.will_build(
            task_id,
            shell_commands.value.len(),
            pre_message.as_deref(),
            &outdated,
        );

        let result = if outdated.is_outdated() {
            tracing::debug!("Rebuilding: {task_id}");
            self.execute_recipe_commands(task_id, &mut shell_commands.value, &scope, false)
                .await
                .map(|_| BuildStatus::Complete(task_id.clone(), outdated))
        } else {
            tracing::debug!("Up to date: {task_id}");
            Ok(BuildStatus::Complete(task_id.clone(), outdated))
        };

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
        let mut shell_commands = if let Some(commands) = &recipe.command {
            eval_shell_commands(&scope, commands, self.workspace.force_color)?
        } else {
            Vec::new()
        };
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
            shell_commands.len(),
            pre_message.as_deref(),
            &outdated,
        );

        let result = self
            .execute_recipe_commands(
                task_id,
                &mut shell_commands,
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
        shell_commands: &[ShellCommandLine],
        scope: &RecipeScope<'_>,
        print_successful: bool,
    ) -> Result<(), Error> {
        let num_steps = shell_commands.len();
        let mut iter = shell_commands.iter().enumerate();
        loop {
            if let Some((step, shell_command)) = iter.next() {
                self.watcher
                    .will_execute(task_id, shell_command, step, num_steps);

                let result = self
                    .io
                    .run_build_command(&shell_command, scope.workspace().project_root())
                    .await
                    .map_err(Into::into);

                self.watcher.did_execute(
                    task_id,
                    &shell_command,
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
            } else {
                return Ok(());
            }
        }
    }

    fn build_dependencies<'a>(
        self: &'a Arc<Self>,
        mut dependencies: Vec<TaskSpec>,
        dependent: DepChainEntry<'a>,
        output_mtime: Option<SystemTime>,
    ) -> Pin<Box<dyn Future<Output = Result<Outdatedness, Error>> + Send + 'a>> {
        if dependencies.len() == 1 {
            // Boxing because of recursion.
            let dependency = dependencies.pop().unwrap();
            return Box::pin(async move {
                self.run_task(dependency, DepChain::Ref(&dependent))
                    .await
                    .map(|status| status.into_output_outdatedness(output_mtime))
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

                let mut outdated = Outdatedness::unchanged();
                let mut first_error = None;
                while let Some(status) = tasks.join_next().await {
                    match status.unwrap() {
                        Ok(status) => {
                            outdated |= status.into_output_outdatedness(output_mtime);
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
                    Ok(outdated)
                }
            });
        } else {
            return Box::pin(futures::future::ready(Ok(Outdatedness::unchanged())));
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
