use std::fmt::{Display, Write as _};

use owo_colors::OwoColorize;
use werk_runner::BuildStatus;

#[derive(Default)]
pub struct StdioWatcher;

impl werk_runner::Watcher for StdioWatcher {
    fn will_build(&self, _task_id: &werk_runner::TaskId, _dry_run: bool, _num_steps: usize) {}

    fn did_build(
        &self,
        task_id: &werk_runner::TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        _post_message: Option<&str>,
    ) {
        match result {
            Ok(BuildStatus::Unchanged) => {
                anstream::println!("{} {task_id}", Bracketed("Up to date").bright_blue());
            }
            Ok(BuildStatus::Rebuilt) => {
                anstream::println!("{} {task_id}", Bracketed("OK").bright_green());
            }
            Ok(BuildStatus::Exists(_)) => {
                // Print nothing for file existence checks.
            }
            Err(err) => {
                anstream::println!("{} {task_id}\n{err}", Bracketed("FAIL").bright_red());
            }
        }
    }

    fn will_execute(
        &self,
        _task_id: &werk_runner::TaskId,
        command: &werk_runner::ShellCommandLineBuilder,
        _dry_run: bool,
        step: usize,
        num_steps: usize,
    ) {
        anstream::println!("{} {}", Bracketed(Step(step, num_steps)).yellow(), command);
    }

    fn did_execute(
        &self,
        task_id: &werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
    ) {
        match result {
            Ok(output) => {
                if !output.status.success() {
                    anstream::eprintln!(
                        "{} Command failed while building '{task_id}': {command}\nstderr:\n{}",
                        Bracketed(Step(step, num_steps)).red(),
                        String::from_utf8_lossy(&output.stderr)
                    );
                }
            }
            Err(err) => {
                anstream::eprintln!(
                    "{} Error evaluating command while building '{task_id}': {command}\n{err}",
                    Bracketed(Step(step, num_steps)).red(),
                );
            }
        }
    }

    fn echo(&self, task_id: &werk_runner::TaskId, message: &str) {
        anstream::println!("{} {}", Bracketed(task_id).cyan(), message);
    }
}

struct Bracketed<T>(pub T);
impl<T: Display> Display for Bracketed<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        self.0.fmt(f)?;
        f.write_char(']')
    }
}

struct Step(usize, usize);
impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0 + 1, self.1)
    }
}
