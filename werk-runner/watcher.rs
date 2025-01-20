use crate::{BuildStatus, Error, Outdatedness, ShellCommandLine, TaskId};

pub trait Watcher: Send + Sync {
    /// Build task is about to start.
    fn will_build(&self, task_id: &TaskId, num_steps: usize, outdatedness: &Outdatedness);

    /// Build task finished (all steps have been completed).
    fn did_build(&self, task_id: &TaskId, result: &Result<BuildStatus, Error>);
    /// Run command is about to be executed.
    fn will_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    );

    fn on_child_process_stdout_line(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
        capture: bool,
    ) {
        _ = (task_id, command, line_without_eol, capture);
    }

    fn on_child_process_stderr_line(
        &self,
        task_id: &TaskId,
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
        task_id: &TaskId,
        command: &ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        num_steps: usize,
    );

    fn message(&self, task_id: Option<&TaskId>, message: &str);
    fn warning(&self, task_id: Option<&TaskId>, message: &str);

    fn write_raw_stdout(&self, bytes: &[u8]) -> std::io::Result<()> {
        _ = bytes;
        Ok(())
    }

    fn write_raw_stderr(&self, bytes: &[u8]) -> std::io::Result<()> {
        _ = bytes;
        Ok(())
    }
}

pub struct WatcherWriter<'a> {
    watcher: &'a dyn Watcher,
}

impl<'a> WatcherWriter<'a> {
    #[inline]
    pub fn new(watcher: &'a dyn Watcher) -> Self {
        Self { watcher }
    }
}

impl std::io::Write for WatcherWriter<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.watcher.write_raw_stdout(buf)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
