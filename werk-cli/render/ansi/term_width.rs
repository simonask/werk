use anstream::stream::IsTerminal;

#[allow(dead_code)]
pub enum TtyWidth {
    NoTty,
    Guess(usize),
    Known(usize),
}

impl TtyWidth {
    pub fn diagnostic_terminal_width(&self) -> Option<usize> {
        match *self {
            TtyWidth::NoTty | TtyWidth::Guess(_) => None,
            TtyWidth::Known(width) => Some(width),
        }
    }

    pub fn progress_max_width(&self) -> Option<usize> {
        match *self {
            TtyWidth::NoTty => None,
            TtyWidth::Guess(width) | TtyWidth::Known(width) => Some(width),
        }
    }
}

#[inline]
pub fn stderr_width() -> TtyWidth {
    if std::io::stderr().is_terminal() {
        imp::stderr_width()
    } else {
        TtyWidth::NoTty
    }
}

#[cfg(unix)]
mod imp {
    use super::TtyWidth;

    // Adapted from Cargo's implementation of tty width detection:
    // <https://github.com/rust-lang/cargo/blob/master/src/cargo/core/shell.rs#L606>
    pub fn stderr_width() -> TtyWidth {
        unsafe {
            let mut winsize: libc::winsize = std::mem::zeroed();
            // The .into() here is needed for FreeBSD which defines TIOCGWINSZ
            // as c_uint but ioctl wants c_ulong.
            if libc::ioctl(libc::STDERR_FILENO, libc::TIOCGWINSZ, &mut winsize) < 0 {
                return TtyWidth::NoTty;
            }
            if winsize.ws_col > 0 {
                TtyWidth::Known(winsize.ws_col as usize)
            } else {
                TtyWidth::NoTty
            }
        }
    }
}

#[cfg(windows)]
mod imp {
    use windows_sys::{
        Win32::{
            Foundation::{CloseHandle, GENERIC_READ, GENERIC_WRITE, INVALID_HANDLE_VALUE},
            Storage::FileSystem::{CreateFileA, FILE_SHARE_READ, FILE_SHARE_WRITE, OPEN_EXISTING},
            System::Console::{
                CONSOLE_SCREEN_BUFFER_INFO, GetConsoleScreenBufferInfo, GetStdHandle,
                STD_ERROR_HANDLE,
            },
        },
        core::PCSTR,
    };

    use super::TtyWidth;

    // Adapted from Cargo's implementation of tty width detection:
    // <https://github.com/rust-lang/cargo/blob/master/src/cargo/core/shell.rs#L647>
    pub fn stderr_width() -> TtyWidth {
        unsafe {
            let stdout = GetStdHandle(STD_ERROR_HANDLE);
            let mut csbi: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
            if GetConsoleScreenBufferInfo(stdout, &mut csbi) != 0 {
                return TtyWidth::Known((csbi.srWindow.Right - csbi.srWindow.Left) as usize);
            }

            // On mintty/msys/cygwin based terminals, the above fails with
            // INVALID_HANDLE_VALUE. Use an alternate method which works
            // in that case as well.
            let h = CreateFileA(
                c"CONOUT$".as_ptr() as PCSTR,
                GENERIC_READ | GENERIC_WRITE,
                FILE_SHARE_READ | FILE_SHARE_WRITE,
                std::ptr::null_mut(),
                OPEN_EXISTING,
                0,
                std::ptr::null_mut(),
            );
            if h == INVALID_HANDLE_VALUE {
                return TtyWidth::NoTty;
            }

            let mut csbi: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
            let rc = GetConsoleScreenBufferInfo(h, &mut csbi);
            CloseHandle(h);
            if rc != 0 {
                let width = (csbi.srWindow.Right - csbi.srWindow.Left) as usize;
                // Unfortunately cygwin/mintty does not set the size of the
                // backing console to match the actual window size. This
                // always reports a size of 80 or 120 (not sure what
                // determines that). Use a conservative max of 60 which should
                // work in most circumstances. ConEmu does some magic to
                // resize the console correctly, but there's no reasonable way
                // to detect which kind of terminal we are running in, or if
                // GetConsoleScreenBufferInfo returns accurate information.
                return TtyWidth::Guess(std::cmp::min(60, width));
            }

            TtyWidth::NoTty
        }
    }
}
