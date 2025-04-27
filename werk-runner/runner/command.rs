use werk_fs::Absolute;
use werk_util::DiagnosticSpan;

use crate::ShellCommandLine;

#[derive(Debug, Clone, PartialEq)]
pub enum RunCommand {
    Shell(ShellCommandLine),
    Write(Absolute<std::path::PathBuf>, Vec<u8>),
    // We don't know yet if the source file is in the workspace or output
    // directory, so we will resolve the path when running it.
    Copy(Absolute<werk_fs::PathBuf>, Absolute<std::path::PathBuf>),
    Info(DiagnosticSpan, String),
    Warn(DiagnosticSpan, String),
    // Path is always in the output directory. They don't need to exist.
    Delete(DiagnosticSpan, Vec<Absolute<std::path::PathBuf>>),
    Touch(DiagnosticSpan, Vec<Absolute<std::path::PathBuf>>),
    SetCapture(bool),
    SetEnv(String, String),
    RemoveEnv(String),
}

impl std::fmt::Display for RunCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunCommand::Shell(shell_command_line) => shell_command_line.fmt(f),
            RunCommand::Write(path_buf, vec) => {
                write!(f, "write {} ({} bytes)", path_buf.display(), vec.len())
            }
            RunCommand::Copy(from, to) => {
                write!(f, "copy '{}' to '{}'", from, to.display())
            }
            RunCommand::Info(_, message) => {
                write!(f, "info \"{}\"", message.escape_default())
            }
            RunCommand::Warn(_, message) => {
                write!(f, "warn \"{}\"", message.escape_default())
            }
            RunCommand::Delete(_, paths) => {
                write!(f, "delete ")?;
                if paths.len() == 1 {
                    write!(f, "{}", paths[0].display())
                } else {
                    write!(f, "[")?;
                    for (i, p) in paths.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", p.display())?;
                    }
                    write!(f, "]")
                }
            }
            RunCommand::Touch(_, paths) => {
                write!(f, "touch ")?;
                if paths.len() == 1 {
                    write!(f, "{}", paths[0].display())
                } else {
                    write!(f, "[")?;
                    for (i, p) in paths.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", p.display())?;
                    }
                    write!(f, "]")
                }
            }
            RunCommand::SetCapture(value) => write!(f, "set_capture = {value}"),
            RunCommand::SetEnv(key, value) => write!(f, "env {key} = {value}"),
            RunCommand::RemoveEnv(key) => write!(f, "env-remove {key}"),
        }
    }
}
