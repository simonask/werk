use crate::{TaskName, Warning};

pub trait Messenger {
    /// Emit a message from the user, typically from the `info` expression in
    /// the manifest.
    fn message(&self, task_id: Option<TaskName>, message: &str);
    /// Emit a warning from the user, typically from the `warn` expression in
    /// the manifest.
    fn warning(&self, task_id: Option<TaskName>, warning: &Warning);
}
