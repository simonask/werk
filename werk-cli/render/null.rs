use werk_eval::{Messenger, TaskName};
use werk_runner::{BuildStatus, Outdatedness, Render};

pub struct NullRender;
impl Messenger for NullRender {
    fn message(&self, _: Option<TaskName>, _: &str) {}

    fn warning(&self, _: Option<TaskName>, _: &werk_eval::Warning) {}
}
impl Render for NullRender {
    fn will_build(&self, _: TaskName, _: usize, _: &Outdatedness) {}

    fn did_build(&self, _: TaskName, _: &Result<BuildStatus, werk_runner::Error>) {}

    fn will_execute(&self, _: TaskName, _: &werk_eval::ShellCommandLine, _: usize, _: usize) {}

    fn did_execute(
        &self,
        _: TaskName,
        _: &werk_eval::ShellCommandLine,
        _: &Result<std::process::ExitStatus, std::io::Error>,
        _: usize,
        _: usize,
    ) {
    }
}
