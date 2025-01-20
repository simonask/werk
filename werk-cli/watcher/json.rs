use super::OutputSettings;

pub struct JsonWatcher {
    settings: OutputSettings,
}

impl JsonWatcher {
    pub fn new(stdout: std::io::Stdout) -> Self {
        todo!()
    }
}

impl werk_runner::Watcher for JsonWatcher {
    fn will_build(
        &self,
        task_id: &werk_runner::TaskId,
        num_steps: usize,
        outdatedness: &werk_runner::Outdatedness,
    ) {
        todo!()
    }

    fn did_build(
        &self,
        task_id: &werk_runner::TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
    ) {
        todo!()
    }

    fn will_execute(
        &self,
        task_id: &werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        todo!()
    }

    fn did_execute(
        &self,
        task_id: &werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        num_steps: usize,
    ) {
        todo!()
    }

    fn message(&self, task_id: Option<&werk_runner::TaskId>, message: &str) {
        todo!()
    }

    fn warning(&self, task_id: Option<&werk_runner::TaskId>, message: &str) {
        todo!()
    }
}
