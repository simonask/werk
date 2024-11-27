use std::{future::Future, marker::PhantomData, pin::Pin, sync::Arc, time::SystemTime};

use futures::channel::oneshot;
use indexmap::{map::Entry, IndexMap};
use parking_lot::Mutex;
use werk_fs::{Path, PathBuf};
use werk_parser::ast;

use crate::{
    Error, GlobSettings, PatternMatch, Project, RecipeMatch, RecipeMatchData, Recipes,
    ShellCommandLine, ShellCommandLineBuilder, Value,
};

pub struct Runner<'a> {
    inner: Arc<Inner>,
    _marker: PhantomData<&'a ()>,
}

struct Inner {
    project: Project,
    recipes: Recipes,
    watcher: Arc<dyn Watcher>,
    dry_run: bool,
    state: Mutex<State>,
}

pub static NULL_RUN_TRACKER: &'static dyn Watcher = &();

pub trait Watcher: Send + Sync {
    /// Build task is about to start.
    fn will_build(
        &self,
        task_id: &TaskId,
        dry_run: bool,
        num_steps: usize,
        pre_message: Option<&str>,
    );
    /// Build task finished (all steps have been completed).
    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<BuildStatus, Error>,
        dry_run: bool,
        post_message: Option<&str>,
    );
    /// Shell command is about to be executed.
    fn will_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLineBuilder,
        dry_run: bool,
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
        command: &ShellCommandLineBuilder,
        result: &Result<std::process::Output, Error>,
        step: usize,
        num_steps: usize,
    );
    fn echo(&self, task_id: &TaskId, message: &str);
}

impl Watcher for () {
    fn will_build(
        &self,
        _task_id: &TaskId,
        _dry_run: bool,
        _num_steps: usize,
        _pre_message: Option<&str>,
    ) {
    }
    fn did_build(
        &self,
        _task_id: &TaskId,
        _result: &Result<BuildStatus, Error>,
        _dry_run: bool,
        _post_message: Option<&str>,
    ) {
    }
    fn will_execute(
        &self,
        _task_id: &TaskId,
        _command: &ShellCommandLineBuilder,
        _dry_run: bool,
        _step: usize,
        _num_steps: usize,
    ) {
    }
    fn did_execute(
        &self,
        _task_id: &TaskId,
        _command: &ShellCommandLineBuilder,
        _result: &Result<std::process::Output, Error>,
        _step: usize,
        _num_steps: usize,
    ) {
    }
    fn echo(&self, _task_id: &TaskId, _message: &str) {}
}

#[derive(Clone, Default)]
pub struct Settings {
    pub glob: GlobSettings,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuildStatus {
    /// Target was up to date; no build action required.
    Unchanged,
    /// Target was outdated, so build actions were executed. This triggers all
    /// dependents to rebuild.
    Rebuilt,
    /// Target is a file that exists in the filesystem, and its last modification time.
    Exists(SystemTime),
}

impl std::ops::BitOr for BuildStatus {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (BuildStatus::Unchanged, BuildStatus::Unchanged) => BuildStatus::Unchanged,
            (BuildStatus::Unchanged, rhs) => rhs,
            (lhs, BuildStatus::Unchanged) => lhs,
            (BuildStatus::Rebuilt, _) => BuildStatus::Rebuilt,
            (_, BuildStatus::Rebuilt) => BuildStatus::Rebuilt,
            (BuildStatus::Exists(mtime1), BuildStatus::Exists(mtime2)) => {
                BuildStatus::Exists(mtime1.max(mtime2))
            }
        }
    }
}

impl std::ops::BitOrAssign for BuildStatus {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

enum TaskStatus {
    Built(Result<BuildStatus, Error>),
    Pending(Vec<oneshot::Sender<Result<BuildStatus, Error>>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TaskId {
    Command(String),
    Build(PathBuf),
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

    pub fn target_file(&self) -> Option<&werk_fs::Path> {
        match self {
            TaskSpec::Recipe(RecipeMatchData::Build { target_file, .. }) => Some(target_file),
            TaskSpec::CheckExists(path_buf) => Some(path_buf),
            _ => None,
        }
    }
}

impl<'a> Runner<'a> {
    pub fn new(
        project: &'a Project,
        recipes: Recipes,
        watcher: Arc<dyn Watcher>,
        dry_run: bool,
    ) -> Self {
        let inner = Inner {
            project: Project {
                inner: project.inner.clone(),
            },
            recipes,
            watcher,
            state: Mutex::new(State::default()),
            dry_run,
        };
        Self {
            inner: Arc::new(inner),
            _marker: PhantomData,
        }
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
        let Some(mtime) = self.project.project_file_mtime(path)? else {
            return Err(Error::NoRuleToBuildTarget(path.to_string()));
        };
        tracing::debug!("Check file mtime `{path}`: {mtime:?}");
        Ok(BuildStatus::Exists(mtime))
    }

    async fn execute_build_recipe(
        self: &Arc<Self>,
        task_id: &TaskId,
        recipe: &ast::Recipe,
        pattern_match: &PatternMatch<'_>,
        target_file: &werk_fs::Path,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let mut scope = self.recipes.global.child(Some(pattern_match));
        scope.set("out".to_owned(), Value::String(target_file.to_string()));

        // Evaluate dependencies (`out` is available).
        let mut deps = Vec::new();
        if let Some(ref build_deps) = recipe.in_files {
            let build_deps = scope.eval(build_deps, &self.project).await?;
            build_deps.try_collect_strings_recursive(|s| {
                let dep = self.get_build_or_command_spec(&s)?;
                deps.push(dep);
                Ok::<_, Error>(())
            })?;
        }

        // Determine the value of the `in` special variable.
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
        scope.set(String::from("in"), in_values);

        let dep_status = self.build_dependencies(deps, dep_chain).await?;

        // Check whether we need to rebuild the target.
        let out_mtime = self.project.output_file_mtime(target_file)?;
        // Now we know enough to determine the build status.
        let build_status = match (out_mtime, dep_status) {
            // Output file does not exist; rebuild.
            (None, _) => BuildStatus::Rebuilt,
            // Check the modification time of the output file against the
            // mtime of its dependencies.
            (Some(out_mtime), BuildStatus::Exists(max_dep_mtime)) => {
                if out_mtime <= max_dep_mtime {
                    BuildStatus::Rebuilt
                } else {
                    BuildStatus::Unchanged
                }
            }
            // If any dependency was rebuilt, rebuild this.
            (Some(_), BuildStatus::Rebuilt) => BuildStatus::Rebuilt,
            // The file exists and the dependencies are unchanged, so do
            // nothing.
            (Some(_), BuildStatus::Unchanged) => BuildStatus::Unchanged,
        };

        // Figure out what to run.
        let mut shell_commands = if let Some(commands) = &recipe.command {
            scope.eval_shell_commands(commands, &self.project)?
        } else {
            Vec::new()
        };
        let pre_message = if let Some(pre_message) = &recipe.pre_message {
            Some(scope.eval_string_expr(pre_message, &self.project)?)
        } else {
            None
        };
        let post_message = if let Some(post_message) = &recipe.post_message {
            Some(scope.eval_string_expr(post_message, &self.project)?)
        } else {
            None
        };

        // Create the parent directory for the target file if it doesn't exist.
        if !self.dry_run {
            self.project.create_parent_dirs(target_file).await?;
        }

        if let BuildStatus::Rebuilt = build_status {
            self.watcher.will_build(
                task_id,
                self.dry_run,
                shell_commands.len(),
                pre_message.as_deref(),
            );

            let result = self
                .execute_recipe_commands(task_id, &mut shell_commands)
                .await
                .map(|_| build_status);

            self.watcher
                .did_build(task_id, &result, self.dry_run, post_message.as_deref());
            result
        } else {
            tracing::debug!("Up to date: {task_id}");
            self.watcher.will_build(task_id, self.dry_run, 1, None);
            self.watcher
                .did_build(task_id, &Ok(BuildStatus::Unchanged), self.dry_run, None);
            Ok(BuildStatus::Unchanged)
        }
    }

    async fn execute_command_recipe(
        self: &Arc<Self>,
        task_id: &TaskId,
        recipe: &ast::CommandRecipe,
        dep_chain: DepChainEntry<'_>,
    ) -> Result<BuildStatus, Error> {
        let scope = self.recipes.global.child(None);

        // Evaluate dependencies (`out` is not available in commands).
        let mut deps = Vec::new();
        if let Some(ref build_deps) = recipe.build {
            let build_deps = scope.eval(build_deps, &self.project).await?;
            build_deps.try_collect_strings_recursive(|s| {
                let dep = self.get_build_or_command_spec(&s)?;
                deps.push(dep);
                Ok::<_, Error>(())
            })?;
        }

        // Note: We don't care about the status of dependencies.
        self.build_dependencies(deps, dep_chain).await?;

        // Figure out what to run.
        let mut shell_commands = if let Some(commands) = &recipe.command {
            scope.eval_shell_commands(commands, &self.project)?
        } else {
            Vec::new()
        };
        let pre_message = if let Some(pre_message) = &recipe.pre_message {
            Some(scope.eval_string_expr(pre_message, &self.project)?)
        } else {
            None
        };
        let post_message = if let Some(post_message) = &recipe.post_message {
            Some(scope.eval_string_expr(post_message, &self.project)?)
        } else {
            None
        };

        self.watcher.will_build(
            task_id,
            self.dry_run,
            shell_commands.len(),
            pre_message.as_deref(),
        );

        let result = self
            .execute_recipe_commands(task_id, &mut shell_commands)
            .await
            .map(|_| BuildStatus::Rebuilt);

        self.watcher
            .did_build(task_id, &result, self.dry_run, post_message.as_deref());
        result
    }

    async fn execute_recipe_commands(
        &self,
        task_id: &TaskId,
        shell_commands: &mut [ShellCommandLineBuilder],
    ) -> Result<(), Error> {
        if self.dry_run {
            for (step, shell_command) in shell_commands.iter().enumerate() {
                self.watcher
                    .will_execute(task_id, shell_command, true, step, shell_commands.len());
            }
            Ok(())
        } else {
            let num_steps = shell_commands.len();
            let mut iter = shell_commands.iter_mut().enumerate();
            loop {
                if let Some((step, shell_command)) = iter.next() {
                    self.watcher
                        .will_execute(task_id, shell_command, false, step, num_steps);

                    let result = async {
                        let shell_command = shell_command.build(&self.project.inner.which)?;
                        self.project.run(&shell_command).await.map_err(Into::into)
                    }
                    .await;

                    self.watcher
                        .did_execute(task_id, &shell_command, &result, step, num_steps);
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
    }

    fn build_dependencies<'a>(
        self: &'a Arc<Self>,
        mut dependencies: Vec<TaskSpec>,
        dependent: DepChainEntry<'a>,
    ) -> Pin<Box<dyn Future<Output = Result<BuildStatus, Error>> + Send + 'a>> {
        if dependencies.len() == 1 {
            // Boxing because of recursion.
            return Box::pin(async move {
                self.run_task(dependencies.pop().unwrap(), DepChain::Ref(&dependent))
                    .await
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

                let mut dep_status = BuildStatus::Unchanged;
                let mut first_error = None;
                while let Some(status) = tasks.join_next().await {
                    match status.unwrap() {
                        Ok(s) => {
                            dep_status |= s;
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
                    Ok(dep_status)
                }
            });
        } else {
            return Box::pin(futures::future::ready(Ok(BuildStatus::Unchanged)));
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

struct DepChainEntry<'a> {
    parent: DepChain<'a>,
    this: &'a TaskId,
}

#[derive(Clone, Copy)]
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

#[derive(Debug, Clone)]
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
