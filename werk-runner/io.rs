use std::path::{Path, PathBuf};

pub use ignore::WalkState;
use werk_eval::{Child, Env, GlobSettings, Metadata, ShellCommandLine};
use werk_fs::{Absolute, Normalize as _};

mod child;
pub use child::*;
use werk_util::IoError;

#[derive(Default)]
pub struct RealSystem(());

impl RealSystem {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl werk_eval::Io for RealSystem {
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
        env: &Env,
        forward_stdout: bool,
    ) -> Result<Box<dyn Child>, IoError> {
        let mut command = smol::process::Command::new(&command_line.program);
        command
            .args(
                command_line
                    .arguments
                    .iter()
                    .filter(|s| !s.trim().is_empty()),
            )
            .stdin(std::process::Stdio::piped())
            // Never capture stdout in recipe commands. By convention, all
            // informational output goes to stderr.
            .stdout(if forward_stdout {
                std::process::Stdio::piped()
            } else {
                std::process::Stdio::null()
            })
            .stderr(std::process::Stdio::piped())
            // All spawned commands always run in the project root.
            .current_dir(working_dir);

        for k in &env.env_remove {
            command.env_remove(k);
        }
        command.envs(&env.env);

        tracing::trace!("spawning {command:?}");
        let child = command
            .spawn()
            .map_err(|err| IoError::new(&command_line.program, err))?;
        Ok(Box::new(ChildProcess(child)))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
        env: &Env,
    ) -> Result<std::process::Output, IoError> {
        let mut command = std::process::Command::new(&*command_line.program);
        command
            .args(
                command_line
                    .arguments
                    .iter()
                    .filter(|s| !s.trim().is_empty()),
            )
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            // All spawned commands always run in the project root.
            .current_dir(working_dir);

        for k in &env.env_remove {
            command.env_remove(k);
        }
        command.envs(&env.env);

        tracing::trace!("spawning {command:?}");
        let child = command
            .spawn()
            .map_err(|err| IoError::new(&command_line.program, err))?;
        let output = child
            .wait_with_output()
            .map_err(|err| IoError::new(&command_line.program, err))?;
        Ok(output)
    }

    fn which(&self, program: &str) -> Result<Absolute<PathBuf>, which::Error> {
        let path = which::which(program)?;
        let normalized = path.normalize().expect("could not normalize program path");
        Ok(normalized)
    }

    fn walk_directory(
        &self,
        path: &Absolute<Path>,
        settings: GlobSettings,
        visit: &(dyn Fn(&Absolute<Path>) + Send + Sync),
    ) -> Result<(), werk_eval::GlobError> {
        struct Builder<'s>(&'s (dyn Fn(&Absolute<Path>) + Send + Sync));
        impl<'s> ignore::ParallelVisitorBuilder<'s> for Builder<'s> {
            fn build(&mut self) -> Box<dyn ignore::ParallelVisitor + 's> {
                Box::new(Visitor(self.0))
            }
        }

        struct Visitor<'s>(&'s (dyn Fn(&Absolute<Path>) + Send + Sync));
        impl ignore::ParallelVisitor for Visitor<'_> {
            fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> WalkState {
                match entry {
                    Ok(entry) => {
                        (self.0)(Absolute::new_ref_unchecked(entry.path()));
                        WalkState::Continue
                    }
                    Err(_) => WalkState::Continue,
                }
            }
        }

        let mut walker = ignore::WalkBuilder::new(path);
        walker
            .git_ignore(settings.git_ignore)
            .git_global(settings.git_ignore_global)
            .git_exclude(settings.git_ignore_exclude)
            .ignore(settings.dot_ignore)
            .parents(settings.git_ignore_from_parents);

        walker.filter_entry(move |entry| !settings.ignore_explicitly.is_match(entry.path()));
        let walker = walker.build_parallel();
        walker.visit(&mut Builder(visit));
        Ok(())
    }

    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, IoError> {
        path.metadata()
            .and_then(Metadata::try_from)
            .map_err(|err| IoError::new(path, err))
    }

    fn read_file(&self, path: &Absolute<Path>) -> Result<Vec<u8>, IoError> {
        std::fs::read(path).map_err(|err| IoError::new(path, err))
    }

    fn write_file(&self, path: &Absolute<Path>, data: &[u8]) -> Result<(), IoError> {
        std::fs::write(path, data).map_err(|err| IoError::new(path, err))
    }

    fn copy_file(&self, from: &Absolute<Path>, to: &Absolute<Path>) -> Result<(), IoError> {
        std::fs::copy(from, to)
            .map(|_| ())
            .map_err(|err| IoError::new(from, err))
    }

    fn delete_file(&self, path: &Absolute<Path>) -> Result<(), IoError> {
        std::fs::remove_file(path).map_err(|err| IoError::new(path, err))
    }

    fn touch(&self, path: &Absolute<Path>) -> Result<(), IoError> {
        let file = std::fs::OpenOptions::new()
            // Ensure that we can write to the file.
            .append(true)
            // Create the file if it does not exist.
            .create(true)
            .open(path)
            .map_err(|err| IoError::new(path, err))?;
        let now = std::time::SystemTime::now();
        file.set_modified(now)
            .map_err(|err| IoError::new(path, err))
    }

    fn create_parent_dirs(&self, path: &Absolute<Path>) -> Result<(), IoError> {
        let parent = path.parent().unwrap();
        let did_exist = parent.is_dir();
        std::fs::create_dir_all(parent).map_err(|err| IoError::new(path, err))?;
        if !did_exist {
            tracing::info!("Created directory: {}", parent.display());
        }
        Ok(())
    }

    fn read_env(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    fn is_dry_run(&self) -> bool {
        false
    }
}
