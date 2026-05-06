use std::path::{Path, PathBuf};

pub use ignore::WalkState;
use parking_lot::Mutex;
use werk_eval::{Child, DirEntry, Env, GlobSettings, Metadata, ShellCommandLine};
use werk_fs::{Absolute, Normalize as _};

mod child;
pub use child::*;

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
    ) -> Result<Box<dyn Child>, std::io::Error> {
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
        let child = command.spawn()?;
        Ok(Box::new(ChildProcess(child)))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<Path>,
        env: &Env,
    ) -> Result<std::process::Output, std::io::Error> {
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
        let child = command.spawn()?;
        let output = child.wait_with_output()?;
        Ok(output)
    }

    fn which(&self, program: &str) -> Result<Absolute<PathBuf>, which::Error> {
        let path = which::which(program)?;
        let normalized = path.normalize().expect("could not normalize program path");
        Ok(normalized)
    }

    fn glob_workspace(
        &self,
        path: &Absolute<Path>,
        settings: &GlobSettings,
    ) -> Result<Vec<DirEntry>, werk_eval::GlobError> {
        struct Builder<'s>(&'s Mutex<Result<Vec<DirEntry>, werk_eval::GlobError>>);
        impl<'s> ignore::ParallelVisitorBuilder<'s> for Builder<'s> {
            fn build(&mut self) -> Box<dyn ignore::ParallelVisitor + 's> {
                Box::new(Visitor(Ok(Vec::new()), self.0))
            }
        }

        struct Visitor<'s>(
            Result<Vec<DirEntry>, werk_eval::GlobError>,
            &'s Mutex<Result<Vec<DirEntry>, werk_eval::GlobError>>,
        );
        impl ignore::ParallelVisitor for Visitor<'_> {
            fn visit(&mut self, entry: Result<ignore::DirEntry, ignore::Error>) -> WalkState {
                let Ok(ref mut entries) = self.0 else {
                    // Already errored.
                    return WalkState::Quit;
                };

                match entry.and_then(TryInto::try_into) {
                    Ok(entry) => {
                        entries.push(entry);
                        WalkState::Continue
                    }
                    Err(err) => {
                        self.0 = Err(err.into());
                        WalkState::Quit
                    }
                }
            }
        }
        impl Drop for Visitor<'_> {
            fn drop(&mut self) {
                let mut results = self.1.lock();
                let Ok(entries) = &mut *results else {
                    // Already errored.
                    return;
                };

                match std::mem::replace(&mut self.0, Ok(Vec::new())) {
                    Ok(new_entries) => entries.extend(new_entries),
                    Err(err) => *results = Err(err),
                }
            }
        }

        let GlobSettings {
            git_ignore,
            git_ignore_global,
            git_ignore_exclude,
            git_ignore_from_parents,
            dot_ignore,
            ignore_explicitly,
        } = settings.clone();

        let mut walker = ignore::WalkBuilder::new(path);
        walker
            .git_ignore(git_ignore)
            .git_global(git_ignore_global)
            .git_exclude(git_ignore_exclude)
            .ignore(dot_ignore)
            .parents(git_ignore_from_parents);

        walker.filter_entry(move |entry| !ignore_explicitly.is_match(entry.path()));

        let walker = walker.build_parallel();

        let results = Mutex::new(Ok(Vec::new()));
        walker.visit(&mut Builder(&results));
        results.into_inner()}

    fn metadata(&self, path: &Absolute<Path>) -> Result<Metadata, std::io::Error> {
        path.metadata()?.try_into()
    }

    fn read_file(&self, path: &Absolute<Path>) -> Result<Vec<u8>, std::io::Error> {
        std::fs::read(path)
    }

    fn write_file(&self, path: &Absolute<Path>, data: &[u8]) -> Result<(), std::io::Error> {
        std::fs::write(path, data)
    }

    fn copy_file(&self, from: &Absolute<Path>, to: &Absolute<Path>) -> Result<(), std::io::Error> {
        std::fs::copy(from, to).map(|_| ())
    }

    fn delete_file(&self, path: &Absolute<Path>) -> Result<(), std::io::Error> {
        std::fs::remove_file(path)
    }

    fn touch(&self, path: &Absolute<Path>) -> Result<(), std::io::Error> {
        let file = std::fs::OpenOptions::new()
            // Ensure that we can write to the file.
            .append(true)
            // Create the file if it does not exist.
            .create(true)
            .open(path)?;
        let now = std::time::SystemTime::now();
        file.set_modified(now)
    }

    fn create_parent_dirs(&self, path: &Absolute<Path>) -> Result<(), std::io::Error> {
        let parent = path.parent().unwrap();
        let did_exist = parent.is_dir();
        std::fs::create_dir_all(parent)?;
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
