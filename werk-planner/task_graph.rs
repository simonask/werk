use std::collections::hash_map;

use ahash::{HashMap, HashSet};
use literator::Literator as _;
use tinyvec::TinyVec;
use werk_eval::{Eval, EvaluatedBuildRecipe, EvaluatedTaskRecipe};
use werk_util::ellipsize::{self, ellipsize};

use crate::{CircularDependencyError, TaskId, TaskSpec};

type IdList<Id> = TinyVec<[Id; 4]>;

fn insert_ordered<Id: Ord + Default>(list: &mut IdList<Id>, id: Id) -> usize {
    match list.binary_search(&id) {
        Ok(index) => index,
        Err(index) => {
            list.insert(index, id);
            index
        }
    }
}

#[derive(Default)]
pub struct TaskGraph<'a> {
    tasks: HashMap<TaskSpec<'a>, TaskId>,
    // Indices in these vecs are TaskId.
    task_specs: Vec<TaskSpec<'a>>,
    dependency_lists: Vec<IdList<TaskId>>,
    dependents_lists: Vec<IdList<TaskId>>,
    evaluated: Vec<Option<EvaluatedTask>>,
}

pub enum EvaluatedTask {
    Build(Eval<EvaluatedBuildRecipe>),
    Task(EvaluatedTaskRecipe),
    CheckExists,
}

impl std::fmt::Display for EvaluatedTask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Build(build_recipe) => {
                write!(
                    f,
                    "{}",
                    build_recipe
                        .commands
                        .iter()
                        .suffix_each("<br align=\"left\"/>")
                )
            }
            Self::Task(task_recipe) => {
                write!(
                    f,
                    "{}",
                    task_recipe
                        .commands
                        .iter()
                        .suffix_each("<br align=\"left\"/>")
                )
            }
            Self::CheckExists => write!(f, "check-exists"),
        }
    }
}

impl<'a> TaskGraph<'a> {
    #[must_use]
    pub fn num_tasks(&self) -> usize {
        self.task_specs.len()
    }

    pub fn get_or_insert_task_spec(&mut self, spec: TaskSpec<'a>) -> TaskId {
        match self.tasks.entry(spec) {
            hash_map::Entry::Occupied(occupied_entry) => {
                tracing::debug!("TASK ALREADY IN GRAPH: {:?}", occupied_entry.key());
                *occupied_entry.get()
            }
            hash_map::Entry::Vacant(vacant_entry) => {
                let id = TaskId::from_index(self.task_specs.len());
                self.task_specs.push(vacant_entry.key().clone());
                self.dependency_lists.push(Default::default());
                self.dependents_lists.push(Default::default());
                self.evaluated.push(None);
                vacant_entry.insert(id);
                id
            }
        }
    }

    /// Get all files with no dependencies, meaning that they are input files.
    /// This includes files discovered through depfiles.
    pub fn get_input_files(&self) -> impl Iterator<Item = &werk_fs::Absolute<werk_fs::SymPath>> {
        self.task_specs.iter().filter_map(|spec| match spec {
            TaskSpec::Recipe(_) => None,
            TaskSpec::CheckExists(path) | TaskSpec::CheckExistsRelaxed(path) => Some(path),
        })
    }

    pub fn add_dependency(&mut self, task: TaskId, depends_on: TaskId) {
        let index = task.index();
        insert_ordered(&mut self.dependency_lists[index], depends_on);
        insert_ordered(&mut self.dependents_lists[depends_on.index()], task);
    }

    #[must_use]
    pub fn get_task_spec(&self, task_id: TaskId) -> &TaskSpec<'a> {
        &self.task_specs[task_id.index()]
    }

    #[must_use]
    pub fn get_task_dependencies(&self, task_id: TaskId) -> &[TaskId] {
        &self.dependency_lists[task_id.index()]
    }

    #[must_use]
    pub fn is_evaluated(&self, task_id: TaskId) -> bool {
        self.evaluated[task_id.index()].is_some()
    }

    pub fn take_task(&mut self, task_id: TaskId) -> EvaluatedTask {
        self.evaluated[task_id.index()]
            .take()
            .expect("Task graph is not fully evaluated; this is a planner bug")
    }

    pub(crate) fn set_evaluated(&mut self, task_id: TaskId, evaluated: EvaluatedTask) {
        self.evaluated[task_id.index()] = Some(evaluated);
    }

    pub fn would_cause_circular_dependency(
        &self,
        task_id: TaskId,
        potential_dependency: TaskId,
        scratch: &mut HashSet<TaskId>,
    ) -> bool {
        scratch.clear();
        scratch.insert(potential_dependency);
        self.collect_dependencies_transitively(potential_dependency, scratch);
        scratch.contains(&task_id)
    }

    fn collect_dependencies_transitively(&self, task_id: TaskId, scratch: &mut HashSet<TaskId>) {
        for dependency in self.dependency_lists[task_id.index()].iter().copied() {
            if !scratch.contains(&dependency) {
                scratch.insert(dependency);
                self.collect_dependencies_transitively(dependency, scratch);
            }
        }
    }

    pub fn check_circular_dependencies(&self) -> Result<(), CircularDependencyError> {
        let mut stack = Vec::with_capacity(16);
        let mut checked = HashSet::default();
        for (index, dependencies) in self.dependency_lists.iter().enumerate() {
            let task_id = TaskId::from_index(index);
            stack.push(task_id);
            self.check_circular_dependency(task_id, dependencies, &mut stack, &mut checked)?;
            stack.pop();
        }

        Ok(())
    }

    fn check_circular_dependency(
        &self,
        task_id: TaskId,
        dependencies: &[TaskId],
        stack: &mut Vec<TaskId>,
        checked: &mut HashSet<TaskId>,
    ) -> Result<(), CircularDependencyError> {
        for dependency in dependencies.iter().copied() {
            if checked.contains(&dependency) {
                continue;
            }

            if let Some(index) = stack.iter().position(|id| *id == dependency) {
                stack.push(dependency);
                return Err(CircularDependencyError {
                    chain: stack[index..]
                        .iter()
                        .map(|id| self.get_task_spec(*id).name().to_string())
                        .collect(),
                });
            }

            stack.push(dependency);
            self.check_circular_dependency(
                task_id,
                &self.dependency_lists[dependency.index()],
                stack,
                checked,
            )?;
            stack.pop();
            checked.insert(dependency);
        }

        Ok(())
    }

    pub fn to_dot<W: std::fmt::Write>(
        &self,
        out: &mut W,
        settings: &TaskGraphToDotSettings,
    ) -> std::fmt::Result {
        writeln!(out, "digraph {{")?;
        writeln!(out, "label=\"Werk Plan\"")?;
        writeln!(out, r#"rankdir="TB""#)?;
        writeln!(out, "graph [overlap=false]")?;
        writeln!(
            out,
            r#"node [fontname="Helvetica" labeljust=l nojustify=true]"#
        )?;
        writeln!(out, r#"edge [fontname="Helvetica"]"#)?;

        // Write nodes
        for (index, task) in self.task_specs.iter().enumerate() {
            let commands = match self.evaluated[index].as_ref().unwrap() {
                EvaluatedTask::Build(Eval {
                    value: EvaluatedBuildRecipe { commands, .. },
                    ..
                })
                | EvaluatedTask::Task(EvaluatedTaskRecipe { commands, .. }) => commands
                    .iter()
                    .format_each_with(|item, w| {
                        ellipsize(
                            w,
                            &item.to_string(),
                            ellipsize::StringBreakMode::GraphemeCluster,
                            60,
                        )
                    })
                    .suffix_each("<br align=\"left\"/>")
                    .to_string(),
                EvaluatedTask::CheckExists => {
                    if settings.all_files {
                        String::from("check-exists")
                    } else {
                        continue;
                    }
                }
            };
            let commands = if commands.is_empty() {
                String::from("(no commands)")
            } else {
                commands
            };

            writeln!(
                out,
                "Task{index} [labeljust=l label=<{{{}|<font point-size=\"10\">{commands}</font>}}> shape=record]",
                task.name()
            )?;
        }

        // Write edges
        for (index, dependencies) in self.dependency_lists.iter().enumerate() {
            if !settings.all_files
                && self.evaluated[index]
                    .as_ref()
                    .map_or(false, |t| matches!(t, EvaluatedTask::CheckExists))
            {
                continue;
            }

            for &dep in dependencies {
                if !settings.all_files
                    && self.evaluated[dep.index()]
                        .as_ref()
                        .map_or(false, |dep| matches!(dep, EvaluatedTask::CheckExists))
                {
                    continue;
                }
                writeln!(out, "Task{} -> Task{}", dep.index(), index)?;
            }
        }

        writeln!(out, "}}")
    }
}

#[derive(Debug, Clone, Default)]
pub struct TaskGraphToDotSettings {
    /// When true, includes "check-exists" tasks in the output.
    pub all_files: bool,
}
