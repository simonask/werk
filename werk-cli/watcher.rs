use std::{
    fmt::{Display, Write as _},
    io::Write as _,
    time::{Instant, SystemTime},
};

use indexmap::IndexMap;
use owo_colors::OwoColorize;
use parking_lot::{Mutex, MutexGuard};
use werk_runner::{BuildStatus, ShellCommandLineBuilder, TaskId};

#[derive(Default)]
pub struct StdioWatcher {
    inner: Mutex<Inner>,
}

impl StdioWatcher {
    fn lock(&self) -> Locked {
        Locked {
            inner: self.inner.lock(),
            stdout: std::io::stdout().lock(),
        }
    }
}

#[derive(Default)]
struct Inner {
    current_tasks: IndexMap<TaskId, (usize, usize)>,
    render_buffer: String,
}

struct Locked<'a> {
    inner: MutexGuard<'a, Inner>,
    stdout: std::io::StdoutLock<'a>,
}

impl<'a> Locked<'a> {
    fn clear_current_line(&mut self) {
        crossterm::execute!(
            self.stdout,
            crossterm::cursor::MoveToColumn(0),
            crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine)
        )
        .unwrap();
    }

    fn render(&mut self) {
        let inner = &mut *self.inner;
        let buffer = &mut inner.render_buffer;
        if inner.current_tasks.is_empty() {
            return;
        }
        buffer.clear();
        buffer.push_str("Building: ");
        for (i, (task, (step, num_steps))) in inner.current_tasks.iter().enumerate() {
            if i != 0 {
                write!(buffer, ", ").unwrap();
            }
            if *num_steps > 1 {
                write!(
                    buffer,
                    "{} {}",
                    task,
                    Bracketed(Step(*step, *num_steps)).bright_yellow()
                )
                .unwrap();
            } else {
                write!(buffer, "{}", task).unwrap();
            }
        }
        self.stdout.write_all(buffer.as_bytes()).unwrap();
        self.stdout.flush().unwrap();
    }

    fn will_build(&mut self, task_id: &TaskId, _dry_run: bool, num_steps: usize) {
        self.inner
            .current_tasks
            .insert(task_id.clone(), (0, num_steps));
        self.clear_current_line();
        self.render();
    }

    fn did_build(
        &mut self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        _post_message: Option<&str>,
    ) {
        self.inner.current_tasks.shift_remove(task_id);

        self.clear_current_line();
        match result {
            Ok(BuildStatus::Unchanged) => {
                writeln!(
                    &mut self.stdout,
                    "{} {task_id}",
                    Bracketed("OK").bright_blue()
                )
                .unwrap();
            }
            Ok(BuildStatus::Rebuilt) => {
                writeln!(
                    &mut self.stdout,
                    "{} {task_id}",
                    Bracketed("OK").bright_green()
                )
                .unwrap();
            }
            Ok(BuildStatus::Exists(_)) => {
                // Print nothing for file existence checks.
            }
            Err(err) => {
                writeln!(
                    &mut self.stdout,
                    "{} {task_id}\n{err}",
                    Bracketed("ERROR").bright_red()
                )
                .unwrap();
            }
        }
        self.render();
    }

    fn will_execute(
        &mut self,
        task_id: &TaskId,
        _command: &ShellCommandLineBuilder,
        _dry_run: bool,
        step: usize,
        num_steps: usize,
    ) {
        *self
            .inner
            .current_tasks
            .get_mut(task_id)
            .expect("task not registered") = (step, num_steps);
        self.clear_current_line();
        self.render();
    }

    fn did_execute(
        &mut self,
        task_id: &TaskId,
        command: &ShellCommandLineBuilder,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
    ) {
        match result {
            Ok(output) => {
                if !output.status.success() {
                    self.clear_current_line();
                    _ = writeln!(
                        self.stdout,
                        "{} Command failed while building '{task_id}': {command}\nstderr:\n{}",
                        Bracketed(Step(step, num_steps)).red(),
                        String::from_utf8_lossy(&output.stderr)
                    );
                    self.render();
                }
            }
            Err(err) => {
                self.clear_current_line();
                _ = writeln!(
                    self.stdout,
                    "{} Error evaluating command while building '{task_id}': {command}\n{err}",
                    Bracketed(Step(step, num_steps)).red(),
                );
                self.render();
            }
        }
    }

    fn echo(&mut self, task_id: &TaskId, message: &str) {
        self.clear_current_line();
        _ = writeln!(self.stdout, "{}: {}", task_id.cyan(), message);
        self.render();
    }
}

impl werk_runner::Watcher for StdioWatcher {
    fn will_build(&self, task_id: &TaskId, dry_run: bool, num_steps: usize) {
        self.lock().will_build(task_id, dry_run, num_steps);
    }

    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        post_message: Option<&str>,
    ) {
        self.lock().did_build(task_id, result, post_message);
    }

    fn will_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLineBuilder,
        dry_run: bool,
        step: usize,
        num_steps: usize,
    ) {
        self.lock()
            .will_execute(task_id, command, dry_run, step, num_steps);
    }

    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLineBuilder,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
    ) {
        self.lock()
            .did_execute(task_id, command, result, step, num_steps);
    }

    fn echo(&self, task_id: &TaskId, message: &str) {
        self.lock().echo(task_id, message);
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
