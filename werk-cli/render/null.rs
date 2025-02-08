use werk_runner::{BuildStatus, Outdatedness, Render, ShellCommandLine, TaskId};

pub struct NullRender;
impl Render for NullRender {
    fn will_build(&self, _: TaskId, _: usize, _: &Outdatedness) {}

    fn did_build(&self, _: TaskId, _: &Result<BuildStatus, werk_runner::Error>) {}

    fn will_execute(&self, _: TaskId, _: &ShellCommandLine, _: usize, _: usize) {}

    fn did_execute(
        &self,
        _: TaskId,
        _: &ShellCommandLine,
        _: &Result<std::process::ExitStatus, std::io::Error>,
        _: usize,
        _: usize,
    ) {
    }

    fn message(&self, _: Option<TaskId>, _: &str) {}

    fn warning(&self, _: Option<TaskId>, _: &str) {}
}
