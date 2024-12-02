use std::collections::hash_map;

use ahash::HashMap;
use parking_lot::Mutex;
use werk_runner::{
    BuildStatus, DirEntry, Error, Metadata, ShellCommandLine, TaskId, WhichError, WorkspaceSettings,
};

#[derive(Default)]
pub struct MockWatcher {
    pub log: Mutex<Vec<MockWatcherEvent>>,
}

#[derive(Debug, PartialEq)]
pub enum MockWatcherEvent {
    WillBuild(TaskId, usize, Option<String>),
    DidBuild(TaskId, Result<BuildStatus, Error>, Option<String>),
    WillExecute(TaskId, ShellCommandLine, usize, usize),
    DidExecute(
        TaskId,
        ShellCommandLine,
        Result<std::process::Output, Error>,
        usize,
        usize,
    ),
    Message(Option<TaskId>, String),
    Warning(Option<TaskId>, String),
}

impl MockWatcher {
    pub fn did_see(&self, event: &MockWatcherEvent) -> bool {
        self.log.lock().iter().any(|e| e == event)
    }
}

impl werk_runner::Watcher for MockWatcher {
    fn will_build(&self, task_id: &TaskId, num_steps: usize, pre_message: Option<&str>) {
        self.log.lock().push(MockWatcherEvent::WillBuild(
            task_id.clone(),
            num_steps,
            pre_message.map(|s| s.to_string()),
        ));
    }

    fn did_build(
        &self,
        task_id: &TaskId,
        result: &Result<BuildStatus, Error>,
        post_message: Option<&str>,
    ) {
        self.log.lock().push(MockWatcherEvent::DidBuild(
            task_id.clone(),
            result.clone(),
            post_message.map(|s| s.to_string()),
        ));
    }

    fn will_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        self.log.lock().push(MockWatcherEvent::WillExecute(
            task_id.clone(),
            command.clone(),
            step,
            num_steps,
        ));
    }

    fn did_execute(
        &self,
        task_id: &TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::Output, Error>,
        step: usize,
        num_steps: usize,
    ) {
        self.log.lock().push(MockWatcherEvent::DidExecute(
            task_id.clone(),
            command.clone(),
            result.clone(),
            step,
            num_steps,
        ));
    }

    fn message(&self, task_id: Option<&TaskId>, message: &str) {
        self.log.lock().push(MockWatcherEvent::Message(
            task_id.cloned(),
            message.to_string(),
        ));
    }

    fn warning(&self, task_id: Option<&TaskId>, message: &str) {
        self.log.lock().push(MockWatcherEvent::Warning(
            task_id.cloned(),
            message.to_string(),
        ));
    }
}

#[derive(Default)]
pub struct MockIo {
    pub filesystem: Mutex<HashMap<std::path::PathBuf, (Metadata, Vec<u8>)>>,
    pub which: HashMap<String, std::path::PathBuf>,
    pub programs: Mutex<
        HashMap<
            std::path::PathBuf,
            Box<
                dyn FnMut(&ShellCommandLine) -> Result<std::process::Output, std::io::Error> + Send,
            >,
        >,
    >,
    pub env: HashMap<String, String>,
    pub oplog: Mutex<Vec<MockIoOp>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MockIoOp {
    RunDuringBuild(ShellCommandLine),
    RunDuringEval(ShellCommandLine),
    Which(String),
    ReadFile(std::path::PathBuf),
    WriteFile(std::path::PathBuf),
    CreateParentDirs(std::path::PathBuf),
    ReadEnv(String),
}

impl MockIo {
    pub fn with_filesystem<I, K>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = (K, (Metadata, Vec<u8>))>,
        K: Into<std::path::PathBuf>,
    {
        let mut filesystem = self.filesystem.lock();
        filesystem.extend(iter.into_iter().map(|(k, v)| (k.into(), v)));
        std::mem::drop(filesystem);
        self
    }

    pub fn with_programs<I>(mut self, iter: I) -> Self
    where
        I: IntoIterator<
            Item = (
                String,
                Box<
                    dyn FnMut(&ShellCommandLine) -> Result<std::process::Output, std::io::Error>
                        + Send,
                >,
            ),
        >,
    {
        let mut programs = self.programs.lock();
        for (program, fun) in iter {
            let path = std::path::Path::new("/").join(&program);
            self.which.insert(program, path.clone());
            programs.insert(path, fun);
        }
        std::mem::drop(programs);

        self
    }

    pub fn with_program(
        mut self,
        program: impl Into<String>,
        path: impl Into<std::path::PathBuf>,
        fun: impl FnMut(&ShellCommandLine) -> Result<std::process::Output, std::io::Error>
            + Send
            + 'static,
    ) -> Self {
        let program = program.into();
        let path = path.into();
        self.which.insert(program.clone(), path.clone());
        self.programs.lock().insert(path, Box::new(fun));
        self
    }

    pub fn with_envs<I, K, V>(mut self, iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        self.env
            .extend(iter.into_iter().map(|(k, v)| (k.into(), v.into())));
        self
    }

    pub fn did_run_during_build(&self, command_line: &ShellCommandLine) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::RunDuringBuild(c) if c == command_line))
    }

    pub fn did_run_during_eval(&self, command_line: &ShellCommandLine) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::RunDuringEval(c) if c == command_line))
    }

    pub fn did_which(&self, command: &str) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::Which(c) if c == command))
    }

    pub fn did_read_file(&self, path: &std::path::Path) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::ReadFile(p) if p == path))
    }

    pub fn did_write_file(&self, path: &std::path::Path) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::WriteFile(p) if p == path))
    }

    pub fn did_create_parent_dirs(&self, path: &std::path::Path) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::CreateParentDirs(p) if p == path))
    }

    pub fn did_read_env(&self, name: &str) -> bool {
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::ReadEnv(n) if n == name))
    }
}

impl werk_runner::Io for MockIo {
    fn run_build_command<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        _working_dir: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringBuild(command_line.clone()));

        Box::pin(async {
            let mut programs = self.programs.lock();
            let Some(program) = programs.get_mut(std::path::Path::new(&command_line.program))
            else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "program not found",
                ));
            };
            program(command_line)
        })
    }

    fn run_during_eval<'a>(
        &'a self,
        command_line: &'a ShellCommandLine,
        _working_dir: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringEval(command_line.clone()));

        Box::pin(async {
            let mut programs = self.programs.lock();
            let Some(program) = programs.get_mut(std::path::Path::new(&command_line.program))
            else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "program not found",
                ));
            };
            program(command_line)
        })
    }

    fn which(&self, command: &str) -> Result<std::path::PathBuf, WhichError> {
        self.oplog.lock().push(MockIoOp::Which(command.to_string()));

        self.which
            .get(command)
            .cloned()
            .ok_or(WhichError::CannotFindBinaryPath)
    }

    fn walk_directory<'a>(
        &'a self,
        path: &'a std::path::Path,
        settings: &'a WorkspaceSettings,
        ignore_subdirs: &'a [&std::path::Path],
    ) -> Result<werk_runner::BoxIter<'a, Result<DirEntry, Error>>, Error> {
        let vec = self
            .filesystem
            .lock()
            .iter()
            .filter_map(|(entry_path, (metadata, _))| {
                if entry_path.starts_with(path) {
                    if settings.ignore.iter().any(|ignore| ignore == path)
                        || ignore_subdirs.iter().any(|ignore| *ignore == path)
                    {
                        return None;
                    }
                    Some(Ok(DirEntry {
                        path: entry_path.clone(),
                        metadata: metadata.clone(),
                    }))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        Ok(Box::new(vec.into_iter()))
    }

    fn metadata(&self, path: &std::path::Path) -> Result<Metadata, Error> {
        self.filesystem
            .lock()
            .get(path)
            .map(|(metadata, _)| metadata.clone())
            .ok_or(std::io::Error::new(std::io::ErrorKind::NotFound, "file not found").into())
    }

    fn read_file<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<Vec<u8>, std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::ReadFile(path.to_path_buf()));

        Box::pin(async {
            self.filesystem
                .lock()
                .get(path)
                .map(|(_, data)| data.clone())
                .ok_or(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "file not found",
                ))
        })
    }

    fn write_file<'a>(
        &'a self,
        path: &'a std::path::Path,
        data: &'a [u8],
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::WriteFile(path.to_path_buf()));

        Box::pin(async {
            let mut lock = self.filesystem.lock();
            let Some(file) = lock.get_mut(path) else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "file not found",
                ));
            };
            file.1.clear();
            file.1.extend_from_slice(data);
            Ok(())
        })
    }

    fn create_parent_dirs<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<(), Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::CreateParentDirs(path.to_path_buf()));

        Box::pin(async {
            let mut lock = self.filesystem.lock();
            let mut current = path.parent();
            while let Some(p) = current {
                match lock.entry(p.to_path_buf()) {
                    hash_map::Entry::Occupied(occupied_entry) => {
                        assert!(occupied_entry.get().0.is_dir());
                        break;
                    }
                    hash_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert((
                            Metadata {
                                mtime: std::time::SystemTime::now(),
                                is_file: false,
                                is_symlink: false,
                            },
                            Vec::new(),
                        ));
                        current = p.parent();
                    }
                }
            }
            Ok(())
        })
    }

    fn read_env(&self, name: &str) -> Option<String> {
        self.oplog.lock().push(MockIoOp::ReadEnv(name.to_string()));
        self.env.get(name).cloned()
    }
}
