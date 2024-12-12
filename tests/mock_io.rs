use core::panic;
use std::{
    collections::{hash_map, HashMap},
    sync::atomic::AtomicU64,
    time::SystemTime,
};

use parking_lot::Mutex;
use werk_runner::{
    globset, BuildStatus, DirEntry, Error, GlobSettings, Metadata, Outdatedness, RunCommand,
    ShellCommandLine, TaskId, WhichError,
};

#[inline]
pub fn make_mtime(secs_since_epoch: u64) -> std::time::SystemTime {
    std::time::UNIX_EPOCH + std::time::Duration::from_secs(secs_since_epoch)
}

#[inline]
pub fn default_mtime() -> std::time::SystemTime {
    make_mtime(0)
}

#[derive(Default)]
pub struct MockWatcher {
    pub log: Mutex<Vec<MockWatcherEvent>>,
}

#[derive(Debug, PartialEq)]
pub enum MockWatcherEvent {
    WillBuild(TaskId, usize, Option<String>, Outdatedness),
    DidBuild(TaskId, Result<BuildStatus, Error>, Option<String>),
    WillExecute(TaskId, RunCommand, usize, usize),
    DidExecute(
        TaskId,
        RunCommand,
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
    fn will_build(
        &self,
        task_id: &TaskId,
        num_steps: usize,
        pre_message: Option<&str>,
        outdatedness: &Outdatedness,
    ) {
        self.log.lock().push(MockWatcherEvent::WillBuild(
            task_id.clone(),
            num_steps,
            pre_message.map(|s| s.to_string()),
            outdatedness.clone(),
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

    fn will_execute(&self, task_id: &TaskId, command: &RunCommand, step: usize, num_steps: usize) {
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
        command: &RunCommand,
        result: &Result<std::process::Output, Error>,
        step: usize,
        num_steps: usize,
        _print_successful: bool,
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

pub type MockDir = HashMap<String, MockDirEntry>;
pub type ProgramResult = std::io::Result<std::process::Output>;

pub enum MockDirEntry {
    File(Metadata, Vec<u8>),
    Dir(MockDir),
}

#[derive(Default)]
pub struct MockIo {
    pub filesystem: Mutex<MockDir>,
    pub which: Mutex<HashMap<String, std::path::PathBuf>>,
    pub programs: Mutex<
        HashMap<
            std::path::PathBuf,
            Box<dyn FnMut(&ShellCommandLine, &mut MockDir) -> ProgramResult + Send>,
        >,
    >,
    pub env: Mutex<HashMap<String, String>>,
    pub oplog: Mutex<Vec<MockIoOp>>,
    pub now: AtomicU64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MockIoOp {
    RunDuringBuild(ShellCommandLine),
    RunDuringEval(ShellCommandLine),
    Which(String),
    ReadFile(std::path::PathBuf),
    WriteFile(std::path::PathBuf),
    CopyFile(std::path::PathBuf, std::path::PathBuf),
    DeleteFile(std::path::PathBuf),
    CreateParentDirs(std::path::PathBuf),
    ReadEnv(String),
}

fn create_parent_dirs(fs: &mut MockDir, path: &std::path::Path) -> std::io::Result<()> {
    let Some(parent) = path.parent() else {
        return Ok(());
    };

    fn create_dirs(fs: &mut MockDir, path: &std::path::Path) -> std::io::Result<()> {
        let mut iter = path.components();

        let (Some(here), rest) = (iter.next(), iter.as_path()) else {
            return Ok(());
        };

        match here {
            std::path::Component::Prefix(_)
            | std::path::Component::RootDir
            | std::path::Component::CurDir => create_dirs(fs, rest),
            std::path::Component::ParentDir => panic!("parent component"),
            std::path::Component::Normal(name) => {
                let name = name.to_string_lossy().into_owned();

                match fs.entry(name) {
                    hash_map::Entry::Occupied(occupied_entry) => {
                        let MockDirEntry::Dir(subdir) = occupied_entry.into_mut() else {
                            panic!("subdir is a file");
                        };
                        create_dirs(subdir, rest)
                    }
                    hash_map::Entry::Vacant(vacant_entry) => {
                        let MockDirEntry::Dir(subdir) =
                            vacant_entry.insert(MockDirEntry::Dir(HashMap::new()))
                        else {
                            unreachable!()
                        };
                        create_dirs(subdir, rest)
                    }
                }
            }
        }
    }

    create_dirs(fs, parent)
}

pub fn insert_fs(
    fs: &mut MockDir,
    path: &std::path::Path,
    (metadata, vec): (Metadata, Vec<u8>),
) -> std::io::Result<()> {
    let mut iter = path.components();

    let (Some(here), rest) = (iter.next(), iter.as_path()) else {
        panic!("empty path");
    };

    match here {
        std::path::Component::Prefix(_)
        | std::path::Component::RootDir
        | std::path::Component::CurDir => insert_fs(fs, rest, (metadata, vec)),
        std::path::Component::ParentDir => panic!("parent component"),
        std::path::Component::Normal(name) => {
            let name = name.to_string_lossy().into_owned();

            match fs.entry(name) {
                hash_map::Entry::Occupied(occupied_entry) => {
                    if rest.as_os_str().is_empty() {
                        match occupied_entry.into_mut() {
                            MockDirEntry::File(ref mut m, ref mut v) => {
                                *m = metadata;
                                *v = vec;
                                Ok(())
                            }
                            MockDirEntry::Dir(_) => {
                                panic!("file is a dir")
                            }
                        }
                    } else {
                        match occupied_entry.into_mut() {
                            MockDirEntry::Dir(subdir) => insert_fs(subdir, rest, (metadata, vec)),
                            MockDirEntry::File(..) => panic!("dir is a file"),
                        }
                    }
                }
                hash_map::Entry::Vacant(vacant_entry) => {
                    if rest.as_os_str().is_empty() {
                        vacant_entry.insert(MockDirEntry::File(metadata, vec));
                        Ok(())
                    } else {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            "parent directory not found",
                        ));
                    }
                }
            }
        }
    }
}

pub fn remove_fs(fs: &mut MockDir, path: &std::path::Path) -> std::io::Result<()> {
    let mut iter = path.components();

    let (Some(here), rest) = (iter.next(), iter.as_path()) else {
        panic!("empty path");
    };

    match here {
        std::path::Component::Prefix(_)
        | std::path::Component::RootDir
        | std::path::Component::CurDir => remove_fs(fs, rest),
        std::path::Component::ParentDir => panic!("parent component"),
        std::path::Component::Normal(name) => {
            let name = name.to_string_lossy().into_owned();

            match fs.entry(name) {
                hash_map::Entry::Occupied(occupied_entry) => {
                    if rest.as_os_str().is_empty() {
                        occupied_entry.remove();
                        Ok(())
                    } else {
                        match occupied_entry.into_mut() {
                            MockDirEntry::Dir(subdir) => remove_fs(subdir, rest),
                            MockDirEntry::File(..) => panic!("dir is a file"),
                        }
                    }
                }
                hash_map::Entry::Vacant(_) => Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "parent directory not found",
                )),
            }
        }
    }
}

fn read_fs<'a>(fs: &'a MockDir, path: &std::path::Path) -> std::io::Result<(DirEntry, &'a [u8])> {
    fn read_fs<'a>(
        fs: &'a MockDir,
        path: &std::path::Path,
        whole_path: &std::path::Path,
    ) -> std::io::Result<(DirEntry, &'a [u8])> {
        let mut iter = path.components();

        let (Some(here), rest) = (iter.next(), iter.as_path()) else {
            panic!("empty path")
        };

        match here {
            std::path::Component::Prefix(_)
            | std::path::Component::RootDir
            | std::path::Component::CurDir => read_fs(fs, rest, whole_path),
            std::path::Component::ParentDir => panic!("parent component"),
            std::path::Component::Normal(name) => {
                let Some(entry) = fs.get(name.to_string_lossy().as_ref()) else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        "file not found",
                    ));
                };

                if rest.as_os_str().is_empty() {
                    match entry {
                        MockDirEntry::File(metadata, data) => Ok((
                            DirEntry {
                                path: whole_path.to_path_buf(),
                                metadata: *metadata,
                            },
                            data,
                        )),
                        MockDirEntry::Dir(_) => Ok((
                            DirEntry {
                                path: whole_path.to_path_buf(),
                                metadata: Metadata {
                                    mtime: default_mtime(),
                                    is_file: false,
                                    is_symlink: false,
                                },
                            },
                            &[],
                        )),
                    }
                } else {
                    match entry {
                        MockDirEntry::File(..) => panic!("subdir is a file"),
                        MockDirEntry::Dir(subdir) => read_fs(subdir, rest, whole_path),
                    }
                }
            }
        }
    }

    read_fs(fs, path, path)
}

fn copy_fs(fs: &mut MockDir, from: &std::path::Path, to: &std::path::Path) -> std::io::Result<()> {
    let (entry, data) = read_fs(fs, from)?;
    let data = data.to_vec();
    insert_fs(fs, to, (entry.metadata, data))
}

pub fn contains_file(fs: &MockDir, path: &std::path::Path) -> bool {
    read_fs(fs, path).is_ok()
}

impl MockIo {
    pub fn with_filesystem<I, K>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = (K, (Metadata, Vec<u8>))>,
        K: Into<std::path::PathBuf>,
    {
        let mut filesystem = self.filesystem.lock();
        for (path, (metadata, data)) in iter {
            let path = path.into();
            create_parent_dirs(&mut *filesystem, &path).unwrap();
            insert_fs(&mut *filesystem, &path, (metadata, data)).unwrap();
        }
        std::mem::drop(filesystem);
        self
    }

    pub fn with_programs<I>(self, iter: I) -> Self
    where
        I: IntoIterator<
            Item = (
                String,
                Box<dyn FnMut(&ShellCommandLine, &mut MockDir) -> ProgramResult + Send>,
            ),
        >,
    {
        let mut programs = self.programs.lock();
        for (program, fun) in iter {
            let path = std::path::Path::new("/").join(&program);
            self.which.lock().insert(program, path.clone());
            programs.insert(path, fun);
        }
        std::mem::drop(programs);

        self
    }

    pub fn with_program(
        self,
        program: impl Into<String>,
        path: impl Into<std::path::PathBuf>,
        fun: impl FnMut(&ShellCommandLine, &mut MockDir) -> ProgramResult + Send + 'static,
    ) -> Self {
        self.set_program(program, path, fun);
        self
    }

    pub fn with_envs<I, K, V>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        self.env
            .lock()
            .extend(iter.into_iter().map(|(k, v)| (k.into(), v.into())));
        self
    }

    pub fn set_env(&self, name: impl Into<String>, value: impl Into<String>) {
        self.env.lock().insert(name.into(), value.into());
    }

    pub fn set_program(
        &self,
        program: impl Into<String>,
        path: impl Into<std::path::PathBuf>,
        fun: impl FnMut(&ShellCommandLine, &mut MockDir) -> ProgramResult + Send + 'static,
    ) {
        let program = program.into();
        let path = path.into();
        self.which.lock().insert(program.clone(), path.clone());
        self.programs.lock().insert(path, Box::new(fun));
    }

    pub fn set_file(
        &self,
        path: impl AsRef<std::path::Path>,
        contents: impl AsRef<[u8]>,
    ) -> std::io::Result<()> {
        let mut fs = self.filesystem.lock();
        insert_fs(
            &mut *fs,
            path.as_ref(),
            (
                Metadata {
                    mtime: self.now(),
                    is_file: true,
                    is_symlink: false,
                },
                Vec::from(contents.as_ref()),
            ),
        )
    }

    pub fn delete_file(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<()> {
        let mut fs = self.filesystem.lock();
        remove_fs(&mut *fs, path.as_ref())
    }

    pub fn clear_oplog(&self) {
        self.oplog.lock().clear();
    }

    pub fn now(&self) -> SystemTime {
        make_mtime(self.now.load(std::sync::atomic::Ordering::SeqCst))
    }

    pub fn tick(&self) {
        self.now.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
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
            let mut fs = self.filesystem.lock();
            program(command_line, &mut *fs)
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
            let mut fs = self.filesystem.lock();
            program(command_line, &mut *fs)
        })
    }

    fn which(&self, command: &str) -> Result<std::path::PathBuf, WhichError> {
        self.oplog.lock().push(MockIoOp::Which(command.to_string()));

        println!("which: {command}");

        self.which
            .lock()
            .get(command)
            .cloned()
            .ok_or(WhichError::CannotFindBinaryPath)
    }

    fn glob_workspace<'a>(
        &'a self,
        path: &'a std::path::Path,
        settings: &'a GlobSettings,
    ) -> werk_runner::PinBoxFut<'a, Result<Vec<DirEntry>, Error>> {
        let fs = self.filesystem.lock();

        fn glob(
            path: &std::path::Path,
            dir: &MockDir,
            results: &mut Vec<DirEntry>,
            ignore_explicitly: &globset::GlobSet,
        ) -> Result<(), Error> {
            for (name, entry) in dir {
                let entry_path = path.join(name);
                match entry {
                    MockDirEntry::File(metadata, _) => {
                        if !ignore_explicitly.is_match(&entry_path) {
                            results.push(DirEntry {
                                path: entry_path,
                                metadata: metadata.clone(),
                            });
                        }
                    }
                    MockDirEntry::Dir(subdir) => {
                        glob(&entry_path, subdir, results, ignore_explicitly)?;
                    }
                }
            }

            Ok(())
        }

        let mut results = Vec::new();
        let result = glob(path, &*fs, &mut results, &settings.ignore_explicitly);

        Box::pin(std::future::ready(result.map(move |_| results)))
    }

    fn metadata(&self, path: &std::path::Path) -> Result<Metadata, Error> {
        let fs = self.filesystem.lock();
        read_fs(&*fs, path)
            .map(|(entry, _)| entry.metadata)
            .map_err(Into::into)
    }

    fn read_file<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<Vec<u8>, std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::ReadFile(path.to_path_buf()));
        let fut = async move {
            let fs = self.filesystem.lock();
            let (entry, data) = read_fs(&*fs, path)?;
            if !entry.metadata.is_file {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::IsADirectory,
                    "is a directory",
                ));
            }
            let data = Vec::from(data);
            Ok(data)
        };

        Box::pin(fut)
    }

    fn write_file<'a>(
        &'a self,
        path: &'a std::path::Path,
        data: &'a [u8],
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        let path = path.to_path_buf();
        self.oplog.lock().push(MockIoOp::WriteFile(path.clone()));

        let fut = async move {
            let mut fs = self.filesystem.lock();
            insert_fs(
                &mut *fs,
                &path,
                (
                    Metadata {
                        mtime: self.now(),
                        is_file: true,
                        is_symlink: false,
                    },
                    Vec::from(data),
                ),
            )
        };

        Box::pin(fut)
    }

    fn copy_file<'a>(
        &'a self,
        from: &'a std::path::Path,
        to: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        let from = from.to_path_buf();
        let to = to.to_path_buf();
        self.oplog
            .lock()
            .push(MockIoOp::CopyFile(from.clone(), to.clone()));

        let fut = async move {
            let mut fs = self.filesystem.lock();
            copy_fs(&mut *fs, &from, &to)
        };
        Box::pin(fut)
    }

    fn delete_file<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        let path = path.to_path_buf();
        self.oplog.lock().push(MockIoOp::DeleteFile(path.clone()));

        let fut = async move {
            let mut fs = self.filesystem.lock();
            remove_fs(&mut *fs, &path)
        };

        Box::pin(fut)
    }

    fn create_parent_dirs<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::CreateParentDirs(path.to_path_buf()));

        let fut = async move {
            let mut fs = self.filesystem.lock();
            create_parent_dirs(&mut *fs, path)
        };

        Box::pin(fut)
    }

    fn read_env(&self, name: &str) -> Option<String> {
        self.oplog.lock().push(MockIoOp::ReadEnv(name.to_string()));
        self.env.lock().get(name).cloned()
    }

    fn is_dry_run(&self) -> bool {
        false
    }
}
