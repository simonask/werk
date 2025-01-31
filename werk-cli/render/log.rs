use super::OutputSettings;

/// Watcher implementation that logs events to the terminal, using `tracing`.
///
/// Note that logging must be enabled for this to actually do anything.
pub struct LogWatcher {
    settings: OutputSettings,
}

impl LogWatcher {
    pub fn new(settings: OutputSettings) -> Self {
        Self { settings }
    }
}

impl werk_runner::Render for LogWatcher {
    fn will_build(
        &self,
        task_id: werk_runner::TaskId,
        num_steps: usize,
        outdatedness: &werk_runner::Outdatedness,
    ) {
        tracing::info!(
            task_id = %task_id,
            num_steps = num_steps,
            "Will build",
        );
        if self.settings.explain {
            for reason in &outdatedness.reasons {
                tracing::info!(task_id = %task_id, "Reason: {reason}");
            }
        }
    }

    fn did_build(
        &self,
        task_id: werk_runner::TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
    ) {
        match result {
            Ok(ref status) => {
                if let werk_runner::BuildStatus::Complete(task_id, _) = status {
                    tracing::info!(task_id = %task_id, "Success");
                }
            }
            Err(err) => {
                tracing::error!(task_id = %task_id, "Error: {err}");
            }
        }
    }

    fn will_execute(
        &self,
        task_id: werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        step: usize,
        _num_steps: usize,
    ) {
        if self.settings.print_recipe_commands {
            tracing::info!(task_id = %task_id, step = step, "Run: {command}");
        }
    }

    fn did_execute(
        &self,
        task_id: werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        _num_steps: usize,
    ) {
        match status {
            Ok(status) => {
                if status.success() {
                    tracing::info!(task_id = %task_id, step = step, "Success: {command}");
                } else {
                    tracing::error!(task_id = %task_id, step = step, "Failed: {command}");
                }
            }
            Err(err) => {
                tracing::error!(task_id = %task_id, step = step, "Error: {err}");
            }
        }
    }

    fn message(&self, task_id: Option<werk_runner::TaskId>, message: &str) {
        tracing::info!(task_id = ?task_id, "Message: {message}");
    }

    fn warning(&self, task_id: Option<werk_runner::TaskId>, message: &str) {
        tracing::warn!(task_id = ?task_id, "Warning: {message}");
    }
}
