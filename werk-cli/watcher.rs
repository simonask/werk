use std::{
    fmt::{Display, Write as _},
    io::Write,
};

use anstream::stream::{AsLockedWrite, IsTerminal};
use indexmap::IndexMap;
use owo_colors::OwoColorize;
use parking_lot::{Mutex, MutexGuard};
use werk_runner::{BuildStatus, Outdatedness, RunCommand, ShellCommandLine, TaskId};

use crate::ColorChoice;

#[derive(Clone, Copy, Debug)]
pub struct OutputSettings {
    /// Logging is enabled, so don't try to modify terminal contents in-place.
    pub logging_enabled: bool,
    pub color: ColorChoice,
    pub print_recipe_commands: bool,
    pub print_fresh: bool,
    pub dry_run: bool,
    pub no_capture: bool,
    pub explain: bool,
}

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
        !matches!(self, AutoStream::Strip(_))
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
    settings: OutputSettings,
}

impl StdoutWatcher {
    pub fn new(settings: OutputSettings) -> Self {
        #[cfg(windows)]
        {
            anstyle_query::windows::enable_ansi_colors();
        }
        let kind = AutoStreamKind::detect(settings.color);

        Self {
            inner: Mutex::new(Inner {
                current_tasks: IndexMap::new(),
                num_tasks: 0,
                num_completed_tasks: 0,
                render_buffer: String::with_capacity(1024),
                width: crossterm::terminal::size().map_or(80, |(w, _)| w as usize),
            }),
            settings,
            kind,
        }
    }

    #[inline]
    pub fn enable_color(&self) -> bool {
        !matches!(self.kind, AutoStreamKind::Strip)
    }

    pub fn lock(&self) -> StdioLock {
        StdioLock {
            inner: self.inner.lock(),
            stdout: AutoStream::new(std::io::stdout().lock(), self.kind),
            settings: &self.settings,
        }
    }
}

struct Inner {
    current_tasks: IndexMap<TaskId, (usize, usize)>,
    num_tasks: usize,
    num_completed_tasks: usize,
    render_buffer: String,
    width: usize,
}

pub struct StdioLock<'a> {
    inner: MutexGuard<'a, Inner>,
    stdout: AutoStream<std::io::StdoutLock<'static>>,
    settings: &'a OutputSettings,
}

impl<'a> StdioLock<'a> {
    pub fn start_advanced_rendering(&mut self) {
        if self.stdout.advanced_rendering() {
            crossterm::execute!(
                &mut self.stdout,
                crossterm::cursor::Hide,
                crossterm::terminal::DisableLineWrap
            )
            .unwrap();
        }
    }

    pub fn finish_advanced_rendering(&mut self) {
        if self.stdout.advanced_rendering() {
            crossterm::execute!(
                &mut self.stdout,
                crossterm::cursor::Show,
                crossterm::terminal::EnableLineWrap
            )
            .unwrap();
        }
    }

    fn clear_current_line(&mut self) {
        if self.stdout.advanced_rendering() && !self.settings.logging_enabled {
            crossterm::execute!(
                &mut self.stdout,
                crossterm::cursor::MoveToColumn(0),
                crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine)
            )
            .unwrap();
        }
    }

    fn render(&mut self) {
        if self.stdout.advanced_rendering() && !self.settings.logging_enabled {
            let inner = &mut *self.inner;
            let buffer = &mut inner.render_buffer;
            if inner.current_tasks.is_empty() {
                return;
            }
            buffer.clear();
            _ = write!(
                buffer,
                "{} Building: ",
                Bracketed(Step(inner.num_completed_tasks, inner.num_tasks)).bright_cyan()
            );

            fn num_width(num: usize) -> usize {
                match num.checked_ilog10() {
                    None => 1,
                    Some(n) => n as usize + 1,
                }
            }

            // The width of the "[N/N] Building: " prefix.
            let mut written_width =
                11 + 2 + num_width(inner.num_completed_tasks) + 1 + num_width(inner.num_tasks);
            let num_tasks = inner.current_tasks.len();

            for (i, (task, (step, num_steps))) in inner.current_tasks.iter().enumerate() {
                let is_last = i + 1 == num_tasks;

                let mut available_width = inner.width.saturating_sub(written_width);
                if !is_last {
                    // Make space for ", ..." at the end.
                    available_width = available_width.saturating_sub(5);
                }

                let mut task_width = 0;
                if i != 0 {
                    // Make space for ", " between tasks.
                    task_width += 2;
                }
                // Make space for "[N/N] "
                task_width += 1 + num_width(*step) + 1 + num_width(*num_steps) + 2;

                // Make space for the task name
                task_width += task.as_str().len();

                if task_width > available_width {
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
                    written_width += task_width;
                    continue;
                } else {
                    if i == 0 {
                        buffer.push_str("...");
                    } else if is_last {
                        buffer.push_str(", ...");
                    }
                    break;
                }
            }

            crossterm::queue!(&mut self.stdout, crossterm::terminal::DisableLineWrap).unwrap();
            self.stdout.write_all(buffer.as_bytes()).unwrap();
            crossterm::queue!(&mut self.stdout, crossterm::terminal::EnableLineWrap).unwrap();

            self.stdout.flush().unwrap();
        }
    }

    fn will_build(
        &mut self,
        task_id: &TaskId,
        num_steps: usize,
        pre_message: Option<&str>,
        outdated: &Outdatedness,
    ) {
        self.inner
            .current_tasks
            .insert(task_id.clone(), (0, num_steps));
        self.clear_current_line();

        if self.settings.explain && outdated.is_outdated() {
            if let Some(path) = task_id.as_path() {
                _ = writeln!(
                    self.stdout,
                    "{} rebuilding `{path}`",
                    Bracketed(Step(0, num_steps)).bright_yellow(),
                );
            } else {
                _ = writeln!(
                    self.stdout,
                    "{} running task `{}`",
                    Bracketed(Step(0, num_steps)).bright_yellow(),
                    task_id.as_str(),
                );
            };

            for reason in &outdated.reasons {
                _ = writeln!(self.stdout, "  {} {reason}", "Cause:".yellow());
            }
        }

        if let Some(pre_message) = pre_message {
            _ = writeln!(self.stdout, "{} {}", "[info]".cyan(), pre_message);
        }

        self.render();
    }

    fn did_build(
        &mut self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        post_message: Option<&str>,
    ) {
        self.inner
            .current_tasks
            .shift_remove(task_id)
            .unwrap_or_default();

        self.clear_current_line();
        match result {
            Ok(BuildStatus::Complete(_task_id, outdatedness)) => {
                if outdatedness.is_outdated() {
                    _ = writeln!(
                        &mut self.stdout,
                        "{} {task_id}{}",
                        Bracketed(" ok ").bright_green(),
                        if self.settings.dry_run {
                            " (dry-run)"
                        } else {
                            ""
                        }
                    );
                    if let Some(post_message) = post_message {
                        _ = writeln!(self.stdout, "{} {}", "[info]".cyan(), post_message);
                    }
                } else if self.settings.print_fresh {
                    _ = writeln!(
                        &mut self.stdout,
                        "{} {task_id}",
                        Bracketed(" -- ").bright_blue()
                    );
                }
            }
            Ok(BuildStatus::Exists(..)) => {
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
        command: &RunCommand,
        step: usize,
        num_steps: usize,
    ) {
        *self
            .inner
            .current_tasks
            .get_mut(task_id)
            .expect("task not registered") = (step + 1, num_steps);
        self.clear_current_line();
        if self.settings.dry_run || self.settings.print_recipe_commands {
            _ = writeln!(
                self.stdout,
                "{} {task_id}: {command}",
                Bracketed(Step(step + 1, num_steps)).bright_yellow()
            );
        }
        self.render();
    }

    fn did_execute(
        &mut self,
        task_id: &TaskId,
        command: &RunCommand,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
        print_successful: bool,
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
                } else if print_successful || self.settings.no_capture {
                    self.clear_current_line();
                    _ = writeln!(
                        self.stdout,
                        "{} {task_id}",
                        Bracketed(Step(step + 1, num_steps)).green()
                    );
                    _ = writeln!(
                        self.stdout,
                        "{}",
                        String::from_utf8_lossy(&output.stdout).trim()
                    );
                    self.render();
                }
            }
            Err(err) => {
                self.clear_current_line();
                _ = writeln!(
                    self.stdout,
                    "{} Error evaluating command while building '{task_id}': {command}\n{err}",
                    Bracketed(Step(step + 1, num_steps)).red(),
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
            _ = writeln!(self.stdout, "{} {}", "[info]".cyan(), message);
        }
        self.render();
    }

    fn warning(&mut self, task_id: Option<&TaskId>, message: &str) {
        self.clear_current_line();
        if let Some(task_id) = task_id {
            _ = writeln!(self.stdout, "{} {}", Bracketed(task_id).yellow(), message);
        } else {
            _ = writeln!(self.stdout, "{} {}", "[warn]".yellow(), message);
        }
        self.render();
    }
}

impl werk_runner::Watcher for StdoutWatcher {
    fn will_build(
        &self,
        task_id: &TaskId,
        num_steps: usize,
        pre_message: Option<&str>,
        outdated: &Outdatedness,
    ) {
        self.lock()
            .will_build(task_id, num_steps, pre_message, outdated);
    }

    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
        post_message: Option<&str>,
    ) {
        self.lock().did_build(task_id, result, post_message);
    }

    fn will_execute(&self, task_id: &TaskId, command: &RunCommand, step: usize, num_steps: usize) {
        if let RunCommand::Echo(_) = command {
            return;
        }

        self.lock().will_execute(task_id, command, step, num_steps);
    }

    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &RunCommand,
        result: &Result<std::process::Output, werk_runner::Error>,
        step: usize,
        num_steps: usize,
        print_successful: bool,
    ) {
        if let RunCommand::Echo(_) = command {
            return;
        }

        self.lock()
            .did_execute(task_id, command, result, step, num_steps, print_successful);
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
        write!(f, "{}/{}", self.0, self.1)
    }
}
