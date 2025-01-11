use werk_fs::Absolute;
use werk_runner::{DirEntry, Error, PinBoxFut, ShellCommandLine};

pub struct DryRun(werk_runner::RealSystem);

impl DryRun {
    pub fn new() -> Self {
        Self(werk_runner::RealSystem::new())
    }
}

impl werk_runner::Io for DryRun {
    fn run_recipe_command<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        _working_dir: &'a Absolute<std::path::Path>,
    ) -> PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        tracing::info!("[DRY-RUN] Would run: {}", command_line.display());
        Box::pin(std::future::ready(Ok(std::process::Output {
            status: std::process::ExitStatus::default(),
            stdout: Vec::new(),
            stderr: Vec::new(),
        })))
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
