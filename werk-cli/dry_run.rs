use std::{future::Future, pin::Pin};

use werk_fs::Absolute;
use werk_runner::{Child, DirEntry, Error, ShellCommandLine};

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

    fn stdout(self: Pin<&mut Self>) -> Option<Pin<&mut dyn smol::io::AsyncRead>> {
        let this = Pin::into_inner(self);
        this.stdout.as_mut().map(|v| v.as_mut() as _)
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
}

impl werk_runner::Io for DryRun {
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        _working_dir: &Absolute<std::path::Path>,
    ) -> std::io::Result<Box<dyn Child>> {
        tracing::info!("[DRY-RUN] Would run: {}", command_line.display());
        Ok(Box::new(DryRunChild::default()))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        working_dir: &Absolute<std::path::Path>,
    ) -> Result<std::process::Output, std::io::Error> {
        tracing::warn!(
            "[DRY-MODE] Running executable, despite dry-run mode: {}",
            command_line.display()
        );
        self.0.run_during_eval(command_line, working_dir)
    }

    fn which(
        &self,
        command: &str,
    ) -> Result<Absolute<std::path::PathBuf>, werk_runner::WhichError> {
        self.0.which(command)
    }

    fn glob_workspace(
        &self,
        path: &Absolute<std::path::Path>,
        settings: &werk_runner::GlobSettings,
    ) -> Result<Vec<DirEntry>, Error> {
        self.0.glob_workspace(path, settings)
    }

    fn metadata(&self, path: &Absolute<std::path::Path>) -> Result<werk_runner::Metadata, Error> {
        self.0.metadata(path)
    }

    fn read_file(&self, path: &Absolute<std::path::Path>) -> Result<Vec<u8>, std::io::Error> {
        self.0.read_file(path)
    }

    fn write_file(
        &self,
        path: &Absolute<std::path::Path>,
        data: &[u8],
    ) -> Result<(), std::io::Error> {
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
    ) -> Result<(), std::io::Error> {
        tracing::info!(
            "[DRY-RUN] Would copy file '{}' to '{}'",
            from.display(),
            to.display()
        );
        Ok(())
    }

    fn delete_file(&self, path: &Absolute<std::path::Path>) -> Result<(), std::io::Error> {
        tracing::info!("[DRY-RUN] Would delete file '{}'", path.display());
        Ok(())
    }

    fn create_parent_dirs(&self, path: &Absolute<std::path::Path>) -> Result<(), std::io::Error> {
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
