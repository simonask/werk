use werk_util::DiagnosticFileId;

use crate::{BuildStatus, Error, Outdatedness, ShellCommandLine, TaskId, Warning};

pub trait Render: Send + Sync {
    /// Build task is about to start.
    fn will_build(&self, task_id: TaskId, num_steps: usize, outdatedness: &Outdatedness);

    /// Build task finished (all steps have been completed).
    fn did_build(&self, task_id: TaskId, result: &Result<BuildStatus, Error>);
    /// Run command is about to be executed.
    fn will_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    );

    fn on_child_process_stderr_line(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
        quiet: bool,
    ) {
        _ = (task_id, command, line_without_eol, quiet);
    }

    fn on_child_process_stdout_line(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        _ = (task_id, command, line_without_eol);
    }

    /// Run command is finished executing, or failed to start. Note that
    /// `result` will be `Ok` even if the command returned an error, allowing
    /// access to the command's stdout/stderr.
    ///
    /// The runner guarantees that if an `Ok(output)` is passed to this
    /// function,
    fn did_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        num_steps: usize,
    );

    /// Emit a message from the user, typically from the `info` expression in
    /// the manifest.
    fn message(&self, task_id: Option<TaskId>, message: &str);

    /// Emit a warning from the user, typically from the `warn` expression in
    /// the manifest.
    fn warning(&self, task_id: Option<TaskId>, warning: &Warning);

    /// Emit an informational message from the runtime, typically the `werk`
    /// binary wants to tell the user about something that happened.
    ///
    /// For example, this is used by the `--watch` flag to tell the user what's
    /// happening.
    fn runner_message(&self, message: &str) {
        _ = message;
    }

    /// Reset the renderer. This is called between iterations in `--watch` to
    /// reset the render state between runs. For example, if the renderer is
    /// debouncing warnings, this might cause warnings to be emitted again.
    ///
    /// This should also clear the renderer's map of source files.
    fn reset(&self) {}

    /// When the renderer is providing friendly diagnostics to the user, this
    /// informs the renderer that a source file was added. The expectation is
    /// that the renderer keeps a map of source files that it uses when
    /// rendering diagnostics.
    ///
    /// This is called by `Workspace::new()` as source files are discovered.
    fn add_source_file(&self, id: DiagnosticFileId, path: &str, source: &str) {
        _ = (id, path, source);
    }
}
