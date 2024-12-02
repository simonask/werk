use std::{
    fmt::{Display, Write as _},
    io::Write,
};

use anstream::stream::{AsLockedWrite, IsTerminal};
use indexmap::IndexMap;
use owo_colors::OwoColorize;
use parking_lot::{Mutex, MutexGuard};
use werk_runner::{BuildStatus, ShellCommandLine, TaskId};

use crate::ColorChoice;

#[cfg(not(windows))]
trait ConWrite: Write + AsLockedWrite {}
#[cfg(not(windows))]
impl<S> ConWrite for S where S: Write + AsLockedWrite {}
#[cfg(windows)]
trait ConWrite: Write + AsLockedWrite + anstyle_wincon::WinconStream {}
#[cfg(windows)]
impl<S> ConWrite for S where S: Write + AsLockedWrite + anstyle_wincon::WinconStream {}

/// Similar to `anstream::AutoStream`, but with a predetermined choice.
enum AutoStream<S: ConWrite> {
    Passthrough(S),
    Strip(anstream::StripStream<S>),
    #[cfg(windows)]
    Wincon(anstream::WinconStream<S>),
}

impl<S: ConWrite> AutoStream<S> {
    pub fn new(stream: S, kind: AutoStreamKind) -> Self {
        match kind {
            AutoStreamKind::Ansi => AutoStream::Passthrough(stream),
            AutoStreamKind::Strip => AutoStream::Strip(anstream::StripStream::new(stream)),
            #[cfg(windows)]
            AutoStreamKind::Wincon => AutoStream::Wincon(anstream::WinconStream::new(stream)),
        }
    }

    pub fn advanced_rendering(&self) -> bool {
        match self {
            AutoStream::Passthrough(_) => true,
            AutoStream::Strip(_) => false,
            #[cfg(windows)]
            AutoStream::Wincon(_) => true,
        }
    }
}

impl<S: ConWrite> Write for AutoStream<S> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            AutoStream::Passthrough(inner) => inner.write(buf),
            AutoStream::Strip(inner) => inner.write(buf),
            #[cfg(windows)]
            AutoStream::Wincon(inner) => inner.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            AutoStream::Passthrough(inner) => inner.flush(),
            AutoStream::Strip(inner) => inner.flush(),
            #[cfg(windows)]
            AutoStream::Wincon(inner) => inner.flush(),
        }
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        match self {
            AutoStream::Passthrough(inner) => inner.write_vectored(bufs),
            AutoStream::Strip(inner) => inner.write_vectored(bufs),
            #[cfg(windows)]
            AutoStream::Wincon(inner) => inner.write_vectored(bufs),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self {
            AutoStream::Passthrough(inner) => inner.write_all(buf),
            AutoStream::Strip(inner) => inner.write_all(buf),
            #[cfg(windows)]
            AutoStream::Wincon(inner) => inner.write_all(buf),
        }
    }

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        match self {
            AutoStream::Passthrough(inner) => inner.write_fmt(fmt),
            AutoStream::Strip(inner) => inner.write_fmt(fmt),
            #[cfg(windows)]
            AutoStream::Wincon(inner) => inner.write_fmt(fmt),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum AutoStreamKind {
    Ansi,
    Strip,
    #[cfg(windows)]
    Wincon,
}

impl AutoStreamKind {
    pub fn detect(choice: ColorChoice) -> Self {
        match choice {
            ColorChoice::Auto => {
                let clicolor_force = anstyle_query::clicolor_force();
                let no_color = anstyle_query::no_color();

                if no_color {
                    return AutoStreamKind::Strip;
                }

                let is_terminal = std::io::stdout().is_terminal();

                let ansi = if clicolor_force || is_terminal {
                    anstyle_query::windows::enable_ansi_colors().unwrap_or(true)
                } else {
                    false
                };
                let term_supports_ansi_color = ansi || anstyle_query::term_supports_ansi_color();

                if term_supports_ansi_color {
                    tracing::info!("Terminal supports ANSI color");
                    AutoStreamKind::Ansi
                } else {
                    tracing::info!("Terminal does not support ANSI color");

                    #[cfg(windows)]
                    {
                        if is_terminal {
                            tracing::info!("Falling back to Wincon backend");
                            return AutoStreamKind::Wincon;
                        }
                    }

                    AutoStreamKind::Strip
                }
            }
            ColorChoice::Always => {
                if let Some(false) = anstyle_query::windows::enable_ansi_colors() {
                    tracing::warn!("Failed to enable virtual terminal processing");
                    return AutoStreamKind::Strip;
                } else {
                    return AutoStreamKind::Ansi;
                }
            }
            ColorChoice::Never => AutoStreamKind::Strip,
        }
    }
}

pub struct StdoutWatcher {
    inner: Mutex<Inner>,
    kind: AutoStreamKind,
}

impl StdoutWatcher {
    pub fn new(choice: ColorChoice, print_recipe_commands: bool, dry_run: bool) -> Self {
        #[cfg(windows)]
        {
            anstyle_query::windows::enable_ansi_colors();
        }
        let kind = AutoStreamKind::detect(choice);

        Self {
            inner: Mutex::new(Inner::new(print_recipe_commands, dry_run)),
            kind,
        }
    }

    #[inline]
    pub fn enable_color(&self) -> bool {
        !matches!(self.kind, AutoStreamKind::Strip)
    }

    fn lock(&self) -> StdioLock {
        StdioLock {
            inner: self.inner.lock(),
            stdout: AutoStream::new(std::io::stdout().lock(), self.kind),
        }
    }
}

struct Inner {
    current_tasks: IndexMap<TaskId, (usize, usize)>,
    render_buffer: String,
    print_recipe_commands: bool,
    dry_run: bool,
}

impl Inner {
    pub fn new(print_recipe_commands: bool, dry_run: bool) -> Self {
        Self {
            current_tasks: IndexMap::new(),
            render_buffer: String::with_capacity(1024),
            print_recipe_commands,
            dry_run,
        }
    }
}

struct StdioLock<'a> {
    inner: MutexGuard<'a, Inner>,
    stdout: AutoStream<std::io::StdoutLock<'static>>,
}

impl<'a> StdioLock<'a> {
    fn clear_current_line(&mut self) {
        if self.stdout.advanced_rendering() {
            crossterm::execute!(
                &mut self.stdout,
                crossterm::cursor::MoveToColumn(0),
                crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine)
            )
            .unwrap();
        }
    }

    fn render(&mut self) {
        if self.stdout.advanced_rendering() {
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
        }

        self.stdout.flush().unwrap();
    }

    fn will_build(&mut self, task_id: &TaskId, num_steps: usize, pre_message: Option<&str>) {
        self.inner
            .current_tasks
            .insert(task_id.clone(), (0, num_steps));
        self.clear_current_line();
        if let Some(pre_message) = pre_message {
            _ = writeln!(
                self.stdout,
                "{} {}: {}",
                Bracketed(Step(0, num_steps)).cyan(),
                task_id,
                pre_message
            );
        }
        self.render();
    }

    fn did_build(
        &mut self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        post_message: Option<&str>,
    ) {
        let (_steps, num_steps) = self
            .inner
            .current_tasks
            .shift_remove(task_id)
            .unwrap_or_default();

        self.clear_current_line();
        match result {
            Ok(BuildStatus::Unchanged) => {
                _ = writeln!(
                    &mut self.stdout,
                    "{} {task_id}",
                    Bracketed("--").bright_blue()
                );
            }
            Ok(BuildStatus::Rebuilt) => {
                _ = writeln!(
                    &mut self.stdout,
                    "{} {task_id}{}",
                    Bracketed("OK").bright_green(),
                    if self.inner.dry_run { " (dry-run)" } else { "" }
                );
                if let Some(post_message) = post_message {
                    _ = writeln!(
                        self.stdout,
                        "{} {}: {}",
                        Bracketed(Step(0, num_steps)).cyan(),
                        task_id,
                        post_message
                    );
                }
            }
            Ok(BuildStatus::Exists(_)) => {
                // Print nothing for file existence checks.
            }
            Err(err) => {
                _ = writeln!(
                    &mut self.stdout,
                    "{} {task_id}\n{err}",
                    Bracketed("ERROR").bright_red()
                );
            }
        }
        self.render();
    }

    fn will_execute(
        &mut self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        *self
            .inner
            .current_tasks
            .get_mut(task_id)
            .expect("task not registered") = (step, num_steps);
        self.clear_current_line();
        if self.inner.dry_run || self.inner.print_recipe_commands {
            _ = writeln!(
                self.stdout,
                "{} {task_id}: {command}",
                Bracketed(Step(step, num_steps)).bright_yellow()
            );
        }
        self.render();
    }

    fn did_execute(
        &mut self,
        task_id: &TaskId,
        command: &ShellCommandLine,
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

    fn message(&mut self, task_id: Option<&TaskId>, message: &str) {
        self.clear_current_line();
        if let Some(task_id) = task_id {
            _ = writeln!(self.stdout, "{} {}", Bracketed(task_id).cyan(), message);
        } else {
            _ = writeln!(self.stdout, "{} {}", "[INFO]".cyan(), message);
        }
        self.render();
    }

    fn warning(&mut self, task_id: Option<&TaskId>, message: &str) {
        self.clear_current_line();
        if let Some(task_id) = task_id {
            _ = writeln!(self.stdout, "{} {}", Bracketed(task_id).yellow(), message);
        } else {
            _ = writeln!(self.stdout, "{} {}", "[WARN]".yellow(), message);
        }
        self.render();
    }
}

impl werk_runner::Watcher for StdoutWatcher {
    fn will_build(&self, task_id: &TaskId, num_steps: usize, pre_message: Option<&str>) {
        self.lock().will_build(task_id, num_steps, pre_message);
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
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        self.lock().will_execute(task_id, command, step, num_steps);
    }

    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
    ) {
        self.lock()
            .did_execute(task_id, command, result, step, num_steps);
    }

    fn message(&self, task_id: Option<&TaskId>, message: &str) {
        self.lock().message(task_id, message)
    }

    fn warning(&self, task_id: Option<&TaskId>, message: &str) {
        self.lock().warning(task_id, message)
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
