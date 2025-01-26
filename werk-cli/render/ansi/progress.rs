use std::{fmt::Write as _, io::Write, time::Instant};

use owo_colors::OwoColorize;
use werk_runner::TaskId;

use crate::render::TtyWidth;

use super::TaskStatus;

pub struct Progress {
    pub style: ProgressStyle,
    frame: u64,
    last_frame_tick: Instant,
    num_tasks: usize,
    progress: usize,
    term_width: TtyWidth,
    buffer: String,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ProgressStyle {
    #[default]
    Spinner,
}

const SPINNER_CHARS: [char; 10] = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

impl Default for Progress {
    fn default() -> Self {
        Self {
            style: ProgressStyle::default(),
            frame: 0,
            last_frame_tick: Instant::now(),
            num_tasks: 0,
            progress: 0,
            term_width: TtyWidth::NoTty,
            buffer: String::with_capacity(1024),
        }
    }
}

impl Progress {
    pub fn set_width(&mut self, term_width: TtyWidth) {
        self.term_width = term_width;
    }

    pub fn set_progress(&mut self, progress: usize, num_tasks: usize) {
        self.progress = progress;
        self.num_tasks = num_tasks;
    }

    pub fn render<'a, W, I>(&mut self, out: &mut W, tasks: I) -> std::io::Result<()>
    where
        W: Write,
        I: IntoIterator<IntoIter: ExactSizeIterator, Item = (&'a TaskId, &'a TaskStatus)>,
    {
        match self.style {
            ProgressStyle::Spinner => self.render_spinner(out, tasks),
        }
    }

    fn render_spinner<'a, W, I>(&mut self, out: &mut W, tasks: I) -> std::io::Result<()>
    where
        W: Write,
        I: IntoIterator<IntoIter: ExactSizeIterator, Item = (&'a TaskId, &'a TaskStatus)>,
    {
        let Some(term_width) = self.term_width.progress_max_width() else {
            // Don't render anything if we don't have a terminal width (not a TTY).
            return Ok(());
        };

        let tasks = tasks.into_iter();
        if tasks.len() == 0 {
            return Ok(());
        }

        let now = std::time::Instant::now();
        if now.duration_since(self.last_frame_tick) > std::time::Duration::from_millis(100) {
            self.frame += 1;
            self.last_frame_tick = now;
        }

        // Render the spinner "responsively", adapting to the terminal width.

        draw_spinner(
            out,
            &mut self.buffer,
            term_width,
            self.frame,
            tasks,
            self.progress,
            self.num_tasks,
        )?;
        out.write_all(b"\r")
    }

    #[cfg(test)]
    fn render_spinner_to_string<'a, I>(&mut self, tasks: I) -> std::io::Result<String>
    where
        I: IntoIterator<IntoIter: ExactSizeIterator, Item = (&'a TaskId, &'a TaskStatus)>,
    {
        let mut buffer = Vec::new();
        let mut stream = crate::render::strip::StripStream::new(&mut buffer);
        self.render_spinner(&mut stream, tasks)?;
        Ok(String::from_utf8(buffer).unwrap())
    }
}

fn draw_spinner<'a, W, I>(
    out: &mut W,
    buffer: &mut String,
    term_width: usize,
    frame: u64,
    mut tasks: I,
    progress: usize,
    num_tasks: usize,
) -> Result<usize, std::io::Error>
where
    W: Write,
    I: ExactSizeIterator<Item = (&'a TaskId, &'a TaskStatus)>,
{
    if term_width < 3 {
        return Ok(0);
    }

    let mut written = 0;

    let spinner = SPINNER_CHARS[(frame % 10) as usize];
    write!(out, "  {spinner}")?;
    written += 3; // Two spaces and the spinner char.

    // Try to write the [n/n] progress indicator.
    buffer.clear();
    _ = write!(buffer, " [{}/{}] ", progress, num_tasks);
    if term_width < written + buffer.len() {
        return Ok(written);
    }
    write!(out, "{}", buffer.bright_cyan())?;
    written += buffer.len();
    buffer.clear();

    let tasks_len = tasks.len();
    if tasks_len <= 1 {
        let Some((task, _)) = tasks.next() else {
            return Ok(written);
        };

        // There is one task left - do not require space for an ellipsis.
        _ = write!(buffer, "{}", task.short_name());
        if term_width < written + buffer.len() {
            return Ok(written);
        }
        write!(out, "{buffer}")?;
        written += buffer.len();
    } else {
        // There is more than one task left, draw them separated by a comma, and
        // end it with an ellipsis when there is no more space.

        for (i, (task, _)) in tasks.enumerate() {
            buffer.clear();
            if i != 0 {
                buffer.push_str(", ");
            }
            _ = write!(buffer, "{}", task.short_name());

            if term_width < written + buffer.len() {
                if i != 0 {
                    // No space for this task, try to write an ellipsis instead.
                    buffer.clear();
                    buffer.push_str(", …");
                    if term_width >= written + 3 {
                        write!(out, "{buffer}")?;
                        written += 3;
                    }
                }
                break;
            } else {
                // There is space for the task name.
                write!(out, "{buffer}")?;
                written += buffer.len();
            }
        }
    }

    Ok(written)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn responsive_spinner_width_one_task() {
        let mut progress = Progress::default();
        progress.set_progress(0, 1);

        let one_task = [(
            &TaskId::try_build("/target_name.o").unwrap(),
            &TaskStatus {
                progress: 0,
                num_steps: 1,
                captured: None,
            },
        )];

        // No TTY
        assert_eq!(progress.render_spinner_to_string(one_task).unwrap(), "");

        progress.set_width(TtyWidth::Known(0));
        assert_eq!(progress.render_spinner_to_string(one_task).unwrap(), "\r");
        progress.set_width(TtyWidth::Known(1));
        assert_eq!(progress.render_spinner_to_string(one_task).unwrap(), "\r");
        progress.set_width(TtyWidth::Known(2));
        assert_eq!(progress.render_spinner_to_string(one_task).unwrap(), "\r");

        progress.set_width(TtyWidth::Known(3));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            "  ⠋\r"
        );
        progress.set_width(TtyWidth::Known(9));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            "  ⠋\r"
        );

        progress.set_width(TtyWidth::Known(10));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            format!("  ⠋ [0/1] \r")
        );
        progress.set_width(TtyWidth::Known(22));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            format!("  ⠋ [0/1] \r")
        );
        progress.set_width(TtyWidth::Known(23));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            format!("  ⠋ [0/1] target_name.o\r")
        );

        progress.set_width(TtyWidth::Known(80));
        assert_eq!(
            progress.render_spinner_to_string(one_task).unwrap(),
            format!("  ⠋ [0/1] target_name.o\r")
        );
    }

    #[test]
    fn responsive_spinner_width_two_tasks() {
        let mut progress = Progress::default();
        progress.set_progress(0, 2);

        let two = [
            (
                &TaskId::try_build("/target1.o").unwrap(),
                &TaskStatus {
                    progress: 0,
                    num_steps: 1,
                    captured: None,
                },
            ),
            (
                &TaskId::try_build("/target2.o").unwrap(),
                &TaskStatus {
                    progress: 0,
                    num_steps: 1,
                    captured: None,
                },
            ),
        ];

        // No TTY
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "");

        progress.set_width(TtyWidth::Known(0));
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "\r");
        progress.set_width(TtyWidth::Known(1));
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "\r");
        progress.set_width(TtyWidth::Known(2));
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "\r");

        progress.set_width(TtyWidth::Known(3));
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "  ⠋\r");
        progress.set_width(TtyWidth::Known(9));
        assert_eq!(progress.render_spinner_to_string(two).unwrap(), "  ⠋\r");

        progress.set_width(TtyWidth::Known(10));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] \r")
        );
        progress.set_width(TtyWidth::Known(18));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] \r")
        );
        progress.set_width(TtyWidth::Known(19));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] target1.o\r")
        );
        progress.set_width(TtyWidth::Known(22));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] target1.o, …\r")
        );
        progress.set_width(TtyWidth::Known(25));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] target1.o, …\r")
        );

        progress.set_width(TtyWidth::Known(30));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] target1.o, target2.o\r")
        );
        progress.set_width(TtyWidth::Known(80));
        assert_eq!(
            progress.render_spinner_to_string(two).unwrap(),
            format!("  ⠋ [0/2] target1.o, target2.o\r")
        );
    }
}
