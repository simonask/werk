use std::{
    fmt::{Display, Write as _},
    io::Write,
};

use anstream::stream::{AsLockedWrite, IsTerminal};
use crossterm::{
    cursor::MoveToColumn,
    queue,
    style::Print,
    terminal::{Clear, ClearType},
};
use indexmap::IndexMap;
use owo_colors::OwoColorize;
use parking_lot::{Mutex, MutexGuard};
use werk_runner::{BuildStatus, Outdatedness, ShellCommandLine, TaskId};

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
pub trait ConWrite: Write + AsLockedWrite {}
#[cfg(not(windows))]
impl<S> ConWrite for S where S: Write + AsLockedWrite {}
#[cfg(windows)]
pub trait ConWrite: Write + AsLockedWrite + anstyle_wincon::WinconStream {}
#[cfg(windows)]
impl<S> ConWrite for S where S: Write + AsLockedWrite + anstyle_wincon::WinconStream {}

/// Similar to `anstream::AutoStream`, but with a predetermined choice.
pub enum AutoStream<S: ConWrite> {
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
pub enum AutoStreamKind {
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
                spinner_frame: 0,
                last_spinner_tick: std::time::Instant::now(),
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

    pub fn render_force_flush(&self) {
        self.lock().render();
    }
}

struct Inner {
    current_tasks: IndexMap<TaskId, (usize, usize)>,
    num_tasks: usize,
    num_completed_tasks: usize,
    render_buffer: String,
    spinner_frame: u64,
    last_spinner_tick: std::time::Instant,
}

pub struct StdioLock<'a> {
    inner: MutexGuard<'a, Inner>,
    pub stdout: AutoStream<std::io::StdoutLock<'static>>,
    settings: &'a OutputSettings,
}

impl<'a> StdioLock<'a> {
    pub fn start_advanced_rendering(&mut self) {
        if self.stdout.advanced_rendering() {
            queue!(
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

    fn start_line_overwrite(&mut self) {
        if self.stdout.advanced_rendering() && !self.settings.logging_enabled {
            queue!(&mut self.stdout, MoveToColumn(0)).unwrap();
        }
    }

    fn newline_overwrite(&mut self) {
        if self.stdout.advanced_rendering() && !self.settings.logging_enabled {
            _ = queue!(
                &mut self.stdout,
                Clear(ClearType::UntilNewLine),
                Print('\n')
            );
        }
    }

    fn render(&mut self) {
        if self.stdout.advanced_rendering() && !self.settings.logging_enabled {
            let now = std::time::Instant::now();
            if now.duration_since(self.inner.last_spinner_tick)
                > std::time::Duration::from_millis(100)
            {
                self.inner.spinner_frame += 1;
                self.inner.last_spinner_tick = now;
            }

            const SPINNER_CHARS: [char; 10] = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
            let spinner = SPINNER_CHARS[(self.inner.spinner_frame % 10) as usize];

            let inner = &mut *self.inner;
            let buffer = &mut inner.render_buffer;
            if inner.current_tasks.is_empty() {
                return;
            }
            buffer.clear();
            _ = write!(
                buffer,
                "{} {spinner} ",
                Bracketed(Step(inner.num_completed_tasks, inner.num_tasks)).bright_cyan()
            );

            // Write the name of the last task in the map.
            let mut width_written = 20;
            let mut tasks_written = 0;
            let max_width = 100;

            for (index, (id, _)) in inner.current_tasks.iter().enumerate() {
                if width_written > max_width {
                    let num_remaining = inner.current_tasks.len() - (index + 1);
                    if num_remaining > 0 {
                        if tasks_written > 0 {
                            _ = write!(buffer, " + {} more", num_remaining);
                        } else {
                            _ = write!(buffer, "{} recipes", num_remaining);
                        }
                    }
                    break;
                }

                if index != 0 {
                    _ = write!(buffer, ", ");
                    width_written += 2;
                }

                let short_name = id.short_name();
                buffer.push_str(short_name);
                tasks_written += 1;
                // Note: Overaccounts for Unicode characters. Probably fine for now.
                width_written += short_name.len();
            }

            _ = queue!(&mut self.stdout, MoveToColumn(0));
            _ = self.stdout.write_all(buffer.as_bytes()).unwrap();
            _ = queue!(&mut self.stdout, Clear(ClearType::UntilNewLine));
            _ = self.stdout.flush();
        }
    }

    fn will_build(&mut self, task_id: &TaskId, num_steps: usize, outdated: &Outdatedness) {
        self.inner
            .current_tasks
            .insert(task_id.clone(), (0, num_steps));
        self.inner.num_tasks += 1;
        self.start_line_overwrite();

        if self.settings.explain && outdated.is_outdated() {
            if let Some(path) = task_id.as_path() {
                _ = write!(
                    self.stdout,
                    "{} rebuilding `{path}`",
                    Bracketed(Step(0, num_steps)).bright_yellow().bold(),
                );
            } else {
                _ = write!(
                    self.stdout,
                    "{} running task `{}`",
                    Bracketed(Step(0, num_steps)).bright_yellow().bold(),
                    task_id.as_str(),
                );
            };
            self.newline_overwrite();

            for reason in &outdated.reasons {
                // Use normal writeln because we already wrote at least one line
                // (so no overwrite needed).
                _ = writeln!(self.stdout, "  {} {reason}", "Cause:".bright_yellow());
            }
        }

        self.render();
    }

    fn did_build(
        &mut self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
    ) {
        self.inner
            .current_tasks
            .shift_remove(task_id)
            .unwrap_or_default();
        self.inner.num_completed_tasks += 1;

        self.start_line_overwrite();
        match result {
            Ok(BuildStatus::Complete(_task_id, outdatedness)) => {
                if outdatedness.is_outdated() {
                    _ = write!(
                        &mut self.stdout,
                        "{} {task_id}{}",
                        Bracketed(" ok ").bright_green().bold(),
                        if self.settings.dry_run {
                            " (dry-run)"
                        } else {
                            ""
                        }
                    );
                    self.newline_overwrite();
                } else if self.settings.print_fresh {
                    _ = write!(
                        &mut self.stdout,
                        "{} {task_id}",
                        Bracketed(" -- ").bright_blue()
                    );
                    self.newline_overwrite();
                }
            }
            Ok(BuildStatus::Exists(..)) => {
                // Print nothing for file existence checks.
            }
            Err(err) => {
                _ = write!(
                    &mut self.stdout,
                    "{} {task_id}\n{err}",
                    Bracketed("ERROR").bright_red().bold()
                );
                self.newline_overwrite();
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
            .expect("task not registered") = (step + 1, num_steps);
        self.start_line_overwrite();
        if self.settings.dry_run || self.settings.print_recipe_commands {
            _ = write!(
                self.stdout,
                "{} {task_id}: {}",
                Bracketed(Step(step + 1, num_steps)).dimmed(),
                command.display()
            );
            self.newline_overwrite();
        }
        self.render();
    }

    fn on_child_process_stdout_line(
        &mut self,
        _task_id: &TaskId,
        _command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        self.start_line_overwrite();
        _ = self.stdout.write_all(line_without_eol);
        self.newline_overwrite();
        self.render();
    }

    fn on_child_process_stderr_line(
        &mut self,
        _task_id: &TaskId,
        _command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        self.start_line_overwrite();
        _ = self.stdout.write_all(line_without_eol);
        self.newline_overwrite();
        self.render();
    }

    fn did_execute(
        &mut self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::ExitStatus, std::io::Error>,
        step: usize,
        num_steps: usize,
    ) {
        match result {
            Ok(status) => {
                if !status.success() {
                    self.start_line_overwrite();
                    _ = write!(
                        self.stdout,
                        "{} Command failed while building '{task_id}': {}",
                        Bracketed(Step(step, num_steps)).bright_red().bold(),
                        command.display(),
                    );
                    self.newline_overwrite();
                    self.render();
                }
            }
            Err(err) => {
                self.start_line_overwrite();
                _ = write!(
                    self.stdout,
                    "{} Error evaluating command while building '{task_id}': {}\n{err}",
                    Bracketed(Step(step + 1, num_steps)).bright_red().bold(),
                    command.display(),
                );
                self.newline_overwrite();
                self.render();
            }
        }
    }

    fn message(&mut self, _task_id: Option<&TaskId>, message: &str) {
        self.start_line_overwrite();
        _ = write!(self.stdout, "{} {}", "[info]".bright_green(), message);
        self.newline_overwrite();
        self.render();
    }

    fn warning(&mut self, _task_id: Option<&TaskId>, message: &str) {
        self.start_line_overwrite();
        _ = write!(self.stdout, "{} {}", "[warn]".bright_yellow(), message);
        self.newline_overwrite();
        self.render();
    }
}

impl werk_runner::Watcher for StdoutWatcher {
    fn will_build(&self, task_id: &TaskId, num_steps: usize, outdated: &Outdatedness) {
        self.lock().will_build(task_id, num_steps, outdated);
    }

    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
    ) {
        self.lock().did_build(task_id, result);
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

    fn on_child_process_stdout_line(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
        capture: bool,
    ) {
        if !capture || self.settings.no_capture {
            self.lock()
                .on_child_process_stdout_line(task_id, command, line_without_eol);
        }
    }

    fn on_child_process_stderr_line(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        line_without_eol: &[u8],
    ) {
        self.lock()
            .on_child_process_stderr_line(task_id, command, line_without_eol);
    }

    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::ExitStatus, std::io::Error>,
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
        write!(f, "[{}]", self.0)
    }
}

struct Step(usize, usize);
impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}
