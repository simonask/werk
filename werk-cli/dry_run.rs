use std::{future::Future, pin::Pin};

use werk_eval::{Child, Env, ShellCommandLine};
use werk_fs::Absolute;
use werk_util::IoError;

pub struct DryRun(werk_runner::RealSystem);

impl Default for DryRun {
    fn default() -> Self {
        Self::new()
    }
}

impl DryRun {
    pub fn new() -> Self {
        Self(werk_runner::RealSystem::new())
    }
}

#[derive(Default)]
#[expect(clippy::box_collection, clippy::redundant_allocation)]
struct DryRunChild {
    stdin: Option<Pin<Box<Vec<u8>>>>,
    stdout: Option<Pin<Box<&'static [u8]>>>,
    stderr: Option<Pin<Box<&'static [u8]>>>,
}

impl Child for DryRunChild {
    fn stdin(self: Pin<&mut Self>) -> Option<Pin<&mut dyn smol::io::AsyncWrite>> {
        let this = Pin::into_inner(self);
        this.stdin.as_mut().map(|v| v.as_mut() as _)
    }

    fn stderr(self: Pin<&mut Self>) -> Option<Pin<&mut dyn smol::io::AsyncRead>> {
        let this = Pin::into_inner(self);
        this.stderr.as_mut().map(|v| v.as_mut() as _)
    }

    fn take_stdin(&mut self) -> Option<Pin<Box<dyn smol::io::AsyncWrite + Send>>> {
        self.stdin.take().map(|v| v as _)
    }

    fn take_stdout(&mut self) -> Option<Pin<Box<dyn smol::io::AsyncRead + Send>>> {
        self.stdout.take().map(|v| v as _)
    }

    fn take_stderr(&mut self) -> Option<Pin<Box<dyn smol::io::AsyncRead + Send>>> {
        self.stderr.take().map(|v| v as _)
    }

    fn status(
        &mut self,
    ) -> Pin<Box<dyn Future<Output = Result<std::process::ExitStatus, std::io::Error>> + Send>>
    {
        Box::pin(std::future::ready(Ok(std::process::ExitStatus::default())))
    }

    fn kill(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl werk_eval::Io for DryRun {
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        _working_dir: &Absolute<std::path::Path>,
        _env: &Env,
        _forward_stdout: bool,
    ) -> Result<Box<dyn Child>, IoError> {
        tracing::info!("[DRY-RUN] Would run: {}", command_line);
        Ok(Box::new(DryRunChild::default()))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<std::path::Path>,
        env: &Env,
    ) -> Result<std::process::Output, IoError> {
        tracing::warn!(
            "[DRY-MODE] Running executable, despite dry-run mode: {}",
            command_line
        );
        self.0.run_during_eval(command_line, working_dir, env)
    }

    fn which(
        &self,
        command: &str,
    ) -> Result<Absolute<std::path::PathBuf>, werk_runner::WhichError> {
        self.0.which(command)
    }

    fn walk_directory(
        &self,
        path: &Absolute<std::path::Path>,
        settings: werk_eval::GlobSettings,
        visit: &(dyn Fn(&Absolute<std::path::Path>) + Send + Sync),
    ) -> Result<(), werk_eval::GlobError> {
        self.0.walk_directory(path, settings, visit)
    }

    fn metadata(&self, path: &Absolute<std::path::Path>) -> Result<werk_eval::Metadata, IoError> {
        self.0.metadata(path)
    }

    fn read_file(&self, path: &Absolute<std::path::Path>) -> Result<Vec<u8>, IoError> {
        self.0.read_file(path)
    }

    fn write_file(&self, path: &Absolute<std::path::Path>, data: &[u8]) -> Result<(), IoError> {
        tracing::info!(
            "[DRY-RUN] Would write file '{}' ({} bytes)",
            path.display(),
            data.len()
        );
        Ok(())
    }

    fn copy_file(
        &self,
        from: &Absolute<std::path::Path>,
        to: &Absolute<std::path::Path>,
    ) -> Result<(), IoError> {
        tracing::info!(
            "[DRY-RUN] Would copy file '{}' to '{}'",
            from.display(),
            to.display()
        );
        Ok(())
    }

    fn delete_file(&self, path: &Absolute<std::path::Path>) -> Result<(), IoError> {
        tracing::info!("[DRY-RUN] Would delete file '{}'", path.display());
        Ok(())
    }

    fn touch(&self, path: &Absolute<std::path::Path>) -> Result<(), IoError> {
        tracing::info!("[DRY-RUN] Would touch file '{}'", path.display());
        Ok(())
    }

    fn create_parent_dirs(&self, path: &Absolute<std::path::Path>) -> Result<(), IoError> {
        tracing::info!(
            "[DRY-RUN] Would create parent directories for '{}'",
            path.display()
        );
        Ok(())
    }

    fn read_env(&self, name: &str) -> Option<String> {
        self.0.read_env(name)
    }

    fn is_dry_run(&self) -> bool {
        true
    }
}
