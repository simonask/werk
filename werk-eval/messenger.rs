use crate::{TaskId, Warning};

pub trait Messenger {
    /// Emit a message from the user, typically from the `info` expression in
    /// the manifest.
    fn message(&self, task_id: Option<TaskId>, message: &str);
    /// Emit a warning from the user, typically from the `warn` expression in
    /// the manifest.
    fn warning(&self, task_id: Option<TaskId>, warning: &Warning);
}
