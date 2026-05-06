use crate::{TaskId, TaskName};

pub struct TaskGraph {
    task_names: Vec<TaskName>,
}

struct TaskInfo {
    name: TaskName,
    dependencies: Vec<TaskId>,
    depends_on: Vec<TaskId>,
}
