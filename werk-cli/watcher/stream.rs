use std::io::{IsTerminal, Write};

use crate::ColorChoice;

/// Like `anstream::AutoStream`, but:
///
/// - Does not detect the terminal type on `new()`.
/// - Does not support Wincon output (werk-cli always enables ANSI on Windows,
///   and does not support color output on Windows 7 and below).
pub enum AutoStream<S> {
    Ansi(S, bool),
    Strip(strip::StripStream<S>),
}

impl<S> AutoStream<S> {
    pub fn into_inner(self) -> S {
        match self {
            Self::Ansi(stream, _) => stream,
            Self::Strip(stream) => stream.stream,
        }
    }

    #[inline]
    pub fn supports_nonlinear_output(&self) -> bool {
        match self {
            Self::Ansi(_, is_actual_terminal) => *is_actual_terminal,
            Self::Strip(_) => false,
        }
    }
}

impl<S> AutoStream<S> {
    pub fn new(stream: S, choice: ColorOutputKind) -> Self {
        match choice {
            ColorOutputKind::Never => AutoStream::Strip(strip::StripStream::new(stream)),
            ColorOutputKind::Ansi(terminal) => AutoStream::Ansi(stream, terminal),
        }
    }
}

impl<S: Write> Write for AutoStream<S> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Ansi(s, _) => s.write(buf),
            Self::Strip(s) => s.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Ansi(s, _) => s.flush(),
            Self::Strip(s) => s.flush(),
        }
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        match self {
            Self::Ansi(s, _) => s.write_vectored(bufs),
            Self::Strip(s) => s.write_vectored(bufs),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self {
            Self::Ansi(s, _) => s.write_all(buf),
            Self::Strip(s) => s.write_all(buf),
        }
    }

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        match self {
            Self::Ansi(s, _) => s.write_fmt(fmt),
            Self::Strip(s) => s.write_fmt(fmt),
        }
    }
}

pub mod strip {
    use super::*;

    pub struct StripStream<S> {
        pub(super) stream: S,
        state: anstream::adapter::StripBytes,
    }

    impl<S> StripStream<S> {
        pub fn new(stream: S) -> Self {
            Self {
                stream,
                state: anstream::adapter::StripBytes::default(),
            }
        }
    }

    // See <https://github.com/rust-cli/anstyle/blob/main/crates/anstream/src/strip.rs#L77>
    impl<S: Write> Write for StripStream<S> {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            let initial_state = self.state.clone();

            for printable in self.state.strip_next(buf) {
                let possible = printable.len();
                let written = self.stream.write(printable)?;
                if possible != written {
                    let divergence = &printable[written..];
                    let offset = offset_to(buf, divergence);
                    let consumed = &buf[offset..];
                    self.state = initial_state;
                    self.state.strip_next(consumed).last();
                    return Ok(offset);
                }
            }
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.stream.flush()
        }

        fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
            let buf = bufs
                .iter()
                .find(|b| !b.is_empty())
                .map(|b| &**b)
                .unwrap_or(&[][..]);
            self.write(buf)
        }

        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
            for printable in self.state.strip_next(buf) {
                self.stream.write_all(printable)?;
            }
            Ok(())
        }
    }

    #[inline]
    fn offset_to(total: &[u8], subslice: &[u8]) -> usize {
        let total = total.as_ptr();
        let subslice = subslice.as_ptr();

        debug_assert!(
            total <= subslice,
            "`Offset::offset_to` only accepts slices of `self`"
        );
        subslice as usize - total as usize
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ColorOutputKind {
    /// The terminal does not support any ANSI or Windows Console escape codes,
    /// and they should be stripped from the output.
    Never,
    /// Emit ANSI escape codes for color output, `true` if it is an actual
    /// terminal (i.e. supports nonlinear output).
    Ansi(bool),
}

impl ColorOutputKind {
    #[inline]
    pub fn supports_color(&self) -> bool {
        !matches!(self, Self::Never)
    }

    /// Detect terminal capabilities and choose a color output kind based on the
    /// user choice and the terminal capabilities. On Windows, this tries to
    /// enable ANSI colors when possible, and calls back to legacy Windows
    /// Console APIs when that fails.
    pub fn initialize<S: IsTerminal>(stream: &S, choice: ColorChoice) -> Self {
        match choice {
            ColorChoice::Auto => {
                if anstyle_query::no_color() {
                    // Colors were explicitly disabled by the environment. This
                    // takes priority over any other environment variables.
                    return Self::Never;
                }

                let is_actual_terminal = stream.is_terminal();

                // See <https://docs.rs/anstream/latest/src/anstream/auto.rs.html#187>
                let clicolor = anstyle_query::clicolor();
                let clicolor_enabled = clicolor.unwrap_or(false);
                let clicolor_disabled = !clicolor.unwrap_or(true);
                let clicolor_force = anstyle_query::clicolor_force();
                let is_ci = anstyle_query::is_ci();

                // Try to enable ANSI colors on Windows.
                anstyle_query::windows::enable_ansi_colors();

                if clicolor_force {
                    return Self::Ansi(is_actual_terminal);
                }

                if clicolor_disabled {
                    return Self::Never;
                }

                if is_actual_terminal
                    && (anstyle_query::term_supports_color() || clicolor_enabled || is_ci)
                {
                    Self::Ansi(true)
                } else {
                    Self::Never
                }
            }
            ColorChoice::Always => {
                // Try to enable ANSI colors on Windows.
                anstyle_query::windows::enable_ansi_colors();

                let is_actual_terminal = stream.is_terminal();

                // Note: Explicitly asking for color enables it regardless of
                // the environment variables, as per the recommendations from
                // <https://no-color.org/>.
                Self::Ansi(is_actual_terminal)
            }
            ColorChoice::Never => ColorOutputKind::Never,
        }
    }
}
