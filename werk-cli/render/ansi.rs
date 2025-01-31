mod progress;
pub mod term_width;

use anstream::stream::IsTerminal;
use indexmap::IndexMap;
use owo_colors::OwoColorize as _;
use parking_lot::Mutex;
use werk_runner::{BuildStatus, Error, Outdatedness, ShellCommandLine, TaskId};

use std::{io::Write, sync::Arc};

use crate::render::Bracketed;

use super::{AutoStream, OutputSettings, Step};

/// A watcher that outputs to the terminal, emitting "destructive" ANSI escape
/// codes that modify the existing terminal (i.e. overwriting the bottom line(s)
/// with current status).
pub struct TerminalRenderer<const LINEAR: bool> {
    inner: Arc<Mutex<Renderer<LINEAR>>>,
    _render_task: Option<smol::Task<()>>,
}

impl<const LINEAR: bool> TerminalRenderer<LINEAR> {
    pub fn new(settings: OutputSettings, stderr: AutoStream<std::io::Stderr>) -> Self {
        let inner = Arc::new(Mutex::new(Renderer {
            stderr,
            state: RenderState {
                current_tasks: IndexMap::new(),
                num_tasks: 0,
                num_completed_tasks: 0,
                progress: if LINEAR {
                    None
                } else {
                    Some(progress::Progress::default())
                },
                settings,
            },
            needs_clear: false,
        }));

        let render_task = if !LINEAR {
            // Spawn a task that automatically updates the terminal with the current
            // status when a long-running task is present.
            //
            // TODO: This task polls continuously when a `--watch` loop is
            // waiting for changes. Consider pausing it when a run is not in
            // progress.
            let renderer = Arc::downgrade(&inner);
            Some(smol::spawn(async move {
                loop {
                    smol::Timer::after(std::time::Duration::from_millis(100)).await;
                    let Some(renderer) = renderer.upgrade() else {
                        return;
                    };
                    Renderer::render_now(&*renderer);
                }
            }))
        } else {
            None
        };

        Self {
            inner,
            _render_task: render_task,
        }
    }
}

struct Renderer<const LINEAR: bool> {
    stderr: AutoStream<std::io::Stderr>,
    state: RenderState,
    needs_clear: bool,
}

impl<const LINEAR: bool> Renderer<LINEAR> {
    /// Render zero or more lines above the status and re-render the status.
    fn render_lines<F>(&mut self, render: F) -> std::io::Result<()>
    where
        F: FnOnce(&mut dyn Write, &mut RenderState) -> std::io::Result<()>,
    {
        if LINEAR {
            render(&mut self.stderr, &mut self.state)
        } else {
            if self.needs_clear {
                self.stderr.write_all(b"\x1B[K")?;
                self.needs_clear = false;
            }
            render(&mut self.stderr, &mut self.state)?;

            if let Some(progress) = self.state.progress.as_mut() {
                let term_width = term_width::stderr_width();
                progress.set_width(term_width);
                progress.set_progress(self.state.num_completed_tasks, self.state.num_tasks);
                progress.render(&mut self.stderr, self.state.current_tasks.iter())?;
                self.needs_clear = true;
            }

            Ok(())
        }
    }

    /// Render zero or more lines above the status and re-render the status.
    fn render_lines_stdout<F>(&mut self, render: F) -> std::io::Result<()>
    where
        F: FnOnce(&mut dyn Write, &mut RenderState) -> std::io::Result<()>,
    {
        let mut stdout = std::io::stdout();
        if LINEAR || !stdout.is_terminal() {
            render(&mut stdout, &mut self.state)
        } else {
            if self.needs_clear {
                self.stderr.write_all(b"\x1B[K")?;
                self.needs_clear = false;
            }
            render(&mut stdout, &mut self.state)?;

            if let Some(progress) = self.state.progress.as_mut() {
                let term_width = term_width::stderr_width();
                progress.set_width(term_width);
                progress.set_progress(self.state.num_completed_tasks, self.state.num_tasks);
                progress.render(&mut self.stderr, self.state.current_tasks.iter())?;
                self.needs_clear = true;
            }

            Ok(())
        }
    }
}

struct RenderState {
    current_tasks: IndexMap<TaskId, TaskStatus>,
    num_tasks: usize,
    num_completed_tasks: usize,
    progress: Option<progress::Progress>,
    settings: OutputSettings,
}

struct TaskStatus {
    pub progress: usize,
    pub num_steps: usize,
    pub captured: Option<Vec<u8>>,
}

impl TaskStatus {
    pub fn new(num_steps: usize) -> Self {
        Self {
            progress: 0,
            num_steps,
            captured: None,
        }
    }
}

impl<const LINEAR: bool> Renderer<LINEAR> {
    pub fn render_now(this: &Mutex<Self>) {
        if !LINEAR {
            let mut this = this.lock();
            _ = this.render_lines(|_, _| Ok(()));
        }
    }
}

impl<const LINEAR: bool> Renderer<LINEAR> {
    pub fn will_build(&mut self, task_id: TaskId, num_steps: usize, outdatedness: &Outdatedness) {
        self.state
            .current_tasks
            .insert(task_id, TaskStatus::new(num_steps));
        self.state.num_tasks += 1;

        _ = self.render_lines(|out, state| {
            if state.settings.explain && outdatedness.is_outdated() {
                if let Some(path) = task_id.as_path() {
                    writeln!(
                        out,
                        "{} rebuilding `{path}`",
                        Bracketed(Step(0, num_steps)).bright_yellow().bold(),
                    )?
                } else {
                    writeln!(
                        out,
                        "{} running task `{}`",
                        Bracketed(Step(0, num_steps)).bright_yellow().bold(),
                        task_id.as_str(),
                    )?
                }

                for reason in &outdatedness.reasons {
                    // Use normal writeln because we already wrote at least one line
                    // (so no overwrite needed).
                    _ = writeln!(out, "  {} {reason}", "Cause:".bright_yellow());
                }
            }

            Ok(())
        });
    }

    fn did_build(&mut self, task_id: TaskId, result: &Result<BuildStatus, Error>) {
        let Some(finished) = self.state.current_tasks.shift_remove(&task_id) else {
            return;
        };

        self.state.num_completed_tasks += 1;

        _ = self.render_lines(|out, state| {
            match result {
                Ok(BuildStatus::Complete(_task_id, outdatedness)) => {
                    if outdatedness.is_outdated() {
                        writeln!(
                            out,
                            "{} {task_id}{}",
                            Bracketed(" ok ").bright_green().bold(),
                            if state.settings.dry_run {
                                " (dry-run)"
                            } else {
                                ""
                            }
                        )?
                    } else if state.settings.print_fresh {
                        writeln!(out, "{} {task_id}", Bracketed(" -- ").bright_blue())?
                    }
                }
                Ok(BuildStatus::Exists(..)) => {
                    // Print nothing for file existence checks.
                }
                Err(err) => {
                    writeln!(
                        out,
                        "{} {task_id}\n{err}",
                        Bracketed("ERROR").bright_red().bold()
                    )?;
                    if let Some(captured) = finished.captured {
                        out.write_all(&captured)?;
                    }
                }
            }
            Ok(())
        });
    }

    fn will_execute(
        &mut self,
        task_id: TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        let Some(status) = self.state.current_tasks.get_mut(&task_id) else {
            return;
        };
        status.progress = step + 1;
        status.num_steps = num_steps;

        // Avoid taking the stdout lock if we aren't actually going to render anything.
        let print_something =
            self.state.settings.dry_run || self.state.settings.print_recipe_commands;

        if print_something {
            _ = self.render_lines(|out, _status| {
                writeln!(
                    out,
                    "{} {task_id}: {}",
                    Bracketed(Step(step + 1, num_steps)).dimmed(),
                    command
                )
            });
        } else if !LINEAR {
            _ = self.render_lines(|_, _| Ok(()));
        }
    }

    fn on_child_process_stderr_line(
        &mut self,
        task_id: TaskId,
        _command: &ShellCommandLine,
        line_without_eol: &[u8],
        quiet: bool,
    ) {
        if (quiet | self.state.settings.quiet) && !self.state.settings.loud {
            // Capture the output for later in case the task fails.
            let Some(status) = self.state.current_tasks.get_mut(&task_id) else {
                return;
            };
            let captured = status.captured.get_or_insert_default();
            captured.extend_from_slice(line_without_eol);
            captured.push(b'\n');
        } else {
            // Print the line immediately.
            _ = self.render_lines(|out, _| {
                out.write_all(line_without_eol)?;
                out.write_all(b"\n")?;
                Ok(())
            });
        }
    }

    fn on_child_process_stdout_line(
        &mut self,
        _task_id: TaskId,
        _command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        // Print the line immediately.
        _ = self.render_lines_stdout(|out, _| {
            out.write_all(line_without_eol)?;
            out.write_all(b"\n")?;
            Ok(())
        });
    }

    fn did_execute(
        &mut self,
        task_id: TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::ExitStatus, std::io::Error>,
        step: usize,
        num_steps: usize,
    ) {
        match result {
            Ok(status) => {
                if !status.success() {
                    _ = self.render_lines(|out, _status| {
                        writeln!(
                            out,
                            "{} Command failed while building '{task_id}': {}",
                            Bracketed(Step(step, num_steps)).bright_red().bold(),
                            command,
                        )
                    });
                }
            }
            Err(err) => {
                _ = self.render_lines(|out, _status| {
                    writeln!(
                        out,
                        "{} Error evaluating command while building '{task_id}': {}\n{err}",
                        Bracketed(Step(step + 1, num_steps)).bright_red().bold(),
                        command,
                    )
                });
            }
        }
    }

    fn message(&mut self, _task_id: Option<TaskId>, message: &str) {
        _ = self
            .render_lines(|out, _status| writeln!(out, "{} {}", "[info]".bright_green(), message));
    }

    fn warning(&mut self, _task_id: Option<TaskId>, message: &str) {
        _ = self
            .render_lines(|out, _status| writeln!(out, "{} {}", "[warn]".bright_yellow(), message));
    }

    fn runner_message(&mut self, message: &str) {
        _ = self.render_lines(|out, _status| {
            writeln!(out, "{} {}", "[werk]".bright_purple().bold(), message)
        });
    }

    fn reset(&mut self) {
        self.state.current_tasks.clear();
        self.state.num_tasks = 0;
        self.state.num_completed_tasks = 0;
    }
}

impl<const LINEAR: bool> werk_runner::Render for TerminalRenderer<LINEAR> {
    fn will_build(&self, task_id: TaskId, num_steps: usize, outdatedness: &Outdatedness) {
        self.inner
            .lock()
            .will_build(task_id, num_steps, outdatedness);
    }

    fn did_build(&self, task_id: TaskId, result: &Result<BuildStatus, Error>) {
        self.inner.lock().did_build(task_id, result);
    }

    fn will_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        self.inner
            .lock()
            .will_execute(task_id, command, step, num_steps);
    }

    fn did_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        num_steps: usize,
    ) {
        self.inner
            .lock()
            .did_execute(task_id, command, status, step, num_steps);
    }

    fn message(&self, task_id: Option<TaskId>, message: &str) {
        self.inner.lock().message(task_id, message)
    }

    fn warning(&self, task_id: Option<TaskId>, message: &str) {
        self.inner.lock().warning(task_id, message)
    }

    fn runner_message(&self, message: &str) {
        self.inner.lock().runner_message(message);
    }

    fn on_child_process_stderr_line(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
        quiet: bool,
    ) {
        self.inner
            .lock()
            .on_child_process_stderr_line(task_id, command, line_without_eol, quiet);
    }

    fn on_child_process_stdout_line(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        self.inner
            .lock()
            .on_child_process_stdout_line(task_id, command, line_without_eol);
    }

    fn reset(&self) {
        self.inner.lock().reset();
    }
}
