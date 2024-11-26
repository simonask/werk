use std::{sync::Arc, time::SystemTime};

use parking_lot::Mutex;
use werk_fs::PathError;

use crate::{Error, Globber, Settings, ShellCommandLine, Value, WhichCache, WhichError, Workspace};

pub struct Project {
    pub(crate) inner: Arc<ProjectInner>,
}

pub(crate) struct ProjectInner {
    pub workspace: Workspace,
    pub globber: Mutex<Globber>,
    pub which: WhichCache,
    pub settings: Settings,
}

impl Project {
    pub fn new(
        project_root: std::path::PathBuf,
        out_dir: std::path::PathBuf,
        settings: Settings,
    ) -> Result<Self, Error> {
        let workspace = Workspace::new(project_root, out_dir, settings.glob.clone())?;
        let inner = Arc::new(ProjectInner {
            workspace,
            globber: Mutex::new(Globber::new()),
            which: WhichCache::default(),
            settings,
        });
        Ok(Self { inner })
    }

    pub fn which(&self, command: &str) -> Result<Value, WhichError> {
        self.inner
            .which
            .which(command)
            .map(|p| p.display().to_string())
            .map(Value::String)
    }

    pub fn glob(&self, pattern: &str) -> Result<Vec<Value>, globset::Error> {
        let mut globber = self.inner.globber.lock();
        globber.glob(pattern, &self.inner.workspace).map(|paths| {
            paths
                .into_iter()
                .map(|p| Value::String(p.to_string()))
                .collect()
        })
    }

    pub async fn run(
        &self,
        command_line: &ShellCommandLine,
    ) -> Result<std::process::Output, std::io::Error> {
        tracing::debug!("Run: {}", command_line);
        let mut command = tokio::process::Command::new(&command_line.program);
        command
            .args(
                command_line
                    .arguments
                    .iter()
                    .filter(|s| !s.trim().is_empty()),
            )
            .envs(&command_line.env)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            // All spawned commands always run in the project root.
            .current_dir(&self.inner.workspace.project_root);

        for k in &command_line.env_remove {
            command.env_remove(k);
        }

        tracing::trace!("spawning {command:?}");
        let child = command.spawn()?;
        let output = child.wait_with_output().await?;
        Ok(output)
    }

    pub fn resolve_path(&self, path: &werk_fs::Path) -> Result<std::path::PathBuf, PathError> {
        let abs_path;
        let path = if !path.is_absolute() {
            abs_path = path.absolutize(werk_fs::Path::ROOT)?;
            &abs_path
        } else {
            path
        };

        if self.inner.workspace.contains(path)? {
            Ok(path.resolve(&self.inner.workspace.project_root)?)
        } else {
            Ok(path.resolve(&self.inner.workspace.out_dir)?)
        }
    }

    pub fn file_mtime(&self, path: &werk_fs::Path) -> Result<Option<SystemTime>, Error> {
        if self.inner.workspace.contains(path)? {
            self.project_file_mtime(path)
        } else {
            self.output_file_mtime(path)
        }
    }

    pub fn project_file_mtime(&self, path: &werk_fs::Path) -> Result<Option<SystemTime>, Error> {
        let path = path.resolve(&self.inner.workspace.project_root)?;
        self.fs_mtime(&path)
    }

    pub fn output_file_mtime(&self, path: &werk_fs::Path) -> Result<Option<SystemTime>, Error> {
        let path = path.resolve(&self.inner.workspace.out_dir)?;
        self.fs_mtime(&path)
    }

    fn fs_mtime(&self, path: &std::path::Path) -> Result<Option<SystemTime>, Error> {
        match path.metadata() {
            Err(err) => {
                if err.kind() == std::io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(err.into())
                }
            }
            Ok(metadata) => Ok(Some(metadata.modified()?)),
        }
    }

    pub async fn create_parent_dirs(&self, path: &werk_fs::Path) -> Result<(), Error> {
        let fs_path = path.resolve(&self.inner.workspace.out_dir)?;
        let parent = fs_path.parent().unwrap();
        let did_exist = parent.is_dir();
        tokio::fs::create_dir_all(&parent).await?;
        if !did_exist {
            tracing::info!("Created directory: {}", parent.display());
        }
        Ok(())
    }
}
