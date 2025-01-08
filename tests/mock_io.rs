use core::panic;
use std::{
    collections::{hash_map, HashMap},
    ffi::OsString,
    sync::atomic::AtomicU64,
    time::SystemTime,
};

use parking_lot::Mutex;
use werk_fs::Absolute;
use werk_runner::{
    globset, BuildStatus, DirEntry, Error, GlobSettings, Metadata, Outdatedness, ShellCommandLine,
    TaskId, WhichError, WorkspaceSettings,
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
    WillBuild(TaskId, usize, Outdatedness),
    DidBuild(TaskId, Result<BuildStatus, Error>),
    WillExecute(TaskId, ShellCommandLine, usize, usize),
    DidExecute(
        TaskId,
        ShellCommandLine,
        Result<std::process::Output, ()>,
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
    fn will_build(&self, task_id: &TaskId, num_steps: usize, outdatedness: &Outdatedness) {
        self.log.lock().push(MockWatcherEvent::WillBuild(
            task_id.clone(),
            num_steps,
            outdatedness.clone(),
        ));
    }

    fn did_build(&self, task_id: &TaskId, result: &Result<BuildStatus, Error>) {
        self.log
            .lock()
            .push(MockWatcherEvent::DidBuild(task_id.clone(), result.clone()));
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
        result: &Result<std::process::Output, std::io::Error>,
        step: usize,
        num_steps: usize,
        _print_successful: bool,
    ) {
        self.log.lock().push(MockWatcherEvent::DidExecute(
            task_id.clone(),
            command.clone(),
            result.as_ref().map_err(|_| ()).cloned(),
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

pub type MockDir = HashMap<OsString, MockDirEntry>;
pub type ProgramResult = std::io::Result<std::process::Output>;

#[derive(Debug)]
pub enum MockDirEntry {
    File(Metadata, Vec<u8>),
    Dir(MockDir),
}

#[derive(Default)]
pub struct MockIo {
    pub filesystem: Mutex<MockDir>,
    pub which: Mutex<HashMap<String, Absolute<std::path::PathBuf>>>,
    pub programs: Mutex<
        HashMap<
            Absolute<std::path::PathBuf>,
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
    ReadFile(Absolute<std::path::PathBuf>),
    WriteFile(Absolute<std::path::PathBuf>),
    CopyFile(Absolute<std::path::PathBuf>, Absolute<std::path::PathBuf>),
    DeleteFile(Absolute<std::path::PathBuf>),
    CreateParentDirs(Absolute<std::path::PathBuf>),
    ReadEnv(String),
}

fn create_dirs(fs: &mut MockDir, path: &Absolute<std::path::Path>) -> std::io::Result<()> {
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
            std::path::Component::Normal(name) => match fs.entry(name.to_owned()) {
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
            },
        }
    }

    create_dirs(fs, path)
}

fn create_parent_dirs(fs: &mut MockDir, path: &Absolute<std::path::Path>) -> std::io::Result<()> {
    let Some(parent) = path.parent() else {
        return Ok(());
    };
    create_dirs(fs, Absolute::new_ref_unchecked(parent))
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
            if name.is_empty() {
                return insert_fs(fs, rest, (metadata, vec));
            }

            match fs.entry(name.to_owned()) {
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
            if name.is_empty() {
                return remove_fs(fs, rest);
            }

            match fs.entry(name.to_owned()) {
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

fn get_fs<'a>(fs: &'a MockDir, path: &Absolute<std::path::Path>) -> Option<&'a MockDirEntry> {
    fn get_fs_entry<'a>(fs: &'a MockDir, path: &std::path::Path) -> Option<&'a MockDirEntry> {
        let mut iter = path.components();

        let (Some(here), rest) = (iter.next(), iter.as_path()) else {
            return None;
        };

        match here {
            std::path::Component::Prefix(_)
            | std::path::Component::RootDir
            | std::path::Component::CurDir => get_fs_entry(fs, rest),
            std::path::Component::ParentDir => panic!("parent component"),
            std::path::Component::Normal(name) => {
                if name.is_empty() {
                    return get_fs_entry(fs, rest);
                }

                let entry = fs.get(name)?;
                if rest.as_os_str().is_empty() {
                    return Some(entry);
                }
                match fs.get(name)? {
                    MockDirEntry::File(..) => {
                        // Path below file.
                        None
                    }
                    MockDirEntry::Dir(subdir) => get_fs_entry(subdir, rest),
                }
            }
        }
    }

    get_fs_entry(fs, path)
}

fn read_fs<'a>(
    fs: &'a MockDir,
    path: &Absolute<std::path::Path>,
) -> std::io::Result<(DirEntry, &'a [u8])> {
    let entry = get_fs(fs, path).ok_or(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        "file not found",
    ))?;
    if let MockDirEntry::File(ref metadata, ref data) = entry {
        Ok((
            DirEntry {
                path: path.to_path_buf(),
                metadata: metadata.clone(),
            },
            data,
        ))
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::IsADirectory,
            "is a directory",
        ))
    }
}

fn copy_fs(
    fs: &mut MockDir,
    from: &Absolute<std::path::Path>,
    to: &Absolute<std::path::Path>,
) -> std::io::Result<()> {
    let (entry, data) = read_fs(fs, from)?;
    let data = data.to_vec();
    insert_fs(fs, to, (entry.metadata, data))
}

pub fn contains_file(fs: &MockDir, path: &std::path::Path) -> bool {
    let path2;
    let path = if !path.is_absolute() {
        path2 = std::path::PathBuf::from("/").join(path);
        &path2
    } else {
        path
    };
    read_fs(fs, Absolute::new_ref_unchecked(path)).is_ok()
}

impl MockIo {
    pub fn with_default_workspace_dir(self) -> Self {
        let mut fs = self.filesystem.lock();
        create_dirs(&mut *fs, test_workspace_dir()).unwrap();
        std::mem::drop(fs);
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
            let path = Absolute::new_unchecked(std::path::Path::new("/").join(&program));
            self.which.lock().insert(program, path.clone());
            programs.insert(path, fun);
        }
        std::mem::drop(programs);

        self
    }

    pub fn with_program(
        self,
        program: impl Into<String>,
        path: Absolute<std::path::PathBuf>,
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
        path: Absolute<std::path::PathBuf>,
        fun: impl FnMut(&ShellCommandLine, &mut MockDir) -> ProgramResult + Send + 'static,
    ) {
        let program = program.into();
        self.which.lock().insert(program.clone(), path.clone());
        self.programs.lock().insert(path, Box::new(fun));
    }

    pub fn with_workspace_files<I, K>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = (K, (Metadata, Vec<u8>))>,
        K: Into<String>,
    {
        let mut filesystem = self.filesystem.lock();
        for (path, (metadata, data)) in iter {
            let path = path.into();
            let path = workspace_file(&path);
            tracing::trace!("inserting workspace file: {}", path.display());
            create_parent_dirs(&mut *filesystem, path.as_deref()).unwrap();
            insert_fs(&mut *filesystem, &path, (metadata, data)).unwrap();
        }
        std::mem::drop(filesystem);
        self
    }

    pub fn set_workspace_file(
        &self,
        path: &str,
        contents: impl AsRef<[u8]>,
    ) -> std::io::Result<()> {
        let mut fs = self.filesystem.lock();
        let path = workspace_file(path);
        create_parent_dirs(&mut *fs, path.as_deref()).unwrap();
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

    pub fn did_read_file(&self, path: &str) -> bool {
        let workspace_file = workspace_file(path);
        let output_file = output_file(path);
        self.oplog.lock().iter().any(
            |op| matches!(op, MockIoOp::ReadFile(p) if *p == workspace_file || *p == output_file),
        )
    }

    pub fn did_write_file(&self, path: &str) -> bool {
        let path = output_file(path);
        self.oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::WriteFile(p) if *p == path))
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
        _working_dir: &'a Absolute<std::path::Path>,
    ) -> werk_runner::PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        tracing::trace!("run during build: {}", command_line.display());
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringBuild(command_line.clone()));

        Box::pin(async {
            let mut programs = self.programs.lock();
            let Some(program) = programs.get_mut(command_line.program.as_deref()) else {
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
        _working_dir: &'a Absolute<std::path::Path>,
    ) -> werk_runner::PinBoxFut<'a, Result<std::process::Output, std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringEval(command_line.clone()));

        Box::pin(async {
            let mut programs = self.programs.lock();
            let Some(program) = programs.get_mut(&*command_line.program) else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "program not found",
                ));
            };
            let mut fs = self.filesystem.lock();
            program(command_line, &mut *fs)
        })
    }

    fn which(&self, command: &str) -> Result<Absolute<std::path::PathBuf>, WhichError> {
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
        path: &'a Absolute<std::path::Path>,
        settings: &'a GlobSettings,
    ) -> werk_runner::PinBoxFut<'a, Result<Vec<DirEntry>, Error>> {
        tracing::trace!("glob workspace: {}", path.display());
        let fs = self.filesystem.lock();

        let MockDirEntry::Dir(workspace) =
            get_fs(&*fs, path).expect("workspace path does not exist")
        else {
            panic!("workspace path is a file");
        };

        fn glob(
            path: &std::path::Path,
            dir: &MockDir,
            results: &mut Vec<DirEntry>,
            ignore_explicitly: &globset::GlobSet,
        ) -> Result<(), Error> {
            for (name, entry) in dir {
                let entry_path = Absolute::new_unchecked(path.join(name));
                match entry {
                    MockDirEntry::File(metadata, _) => {
                        if !ignore_explicitly.is_match(&*entry_path) {
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
        let result = glob(path, workspace, &mut results, &settings.ignore_explicitly);

        Box::pin(std::future::ready(result.map(move |_| results)))
    }

    fn metadata(&self, path: &Absolute<std::path::Path>) -> Result<Metadata, Error> {
        let fs = self.filesystem.lock();
        read_fs(&*fs, path)
            .map(|(entry, _)| entry.metadata)
            .map_err(Into::into)
    }

    fn read_file<'a>(
        &'a self,
        path: &'a Absolute<std::path::Path>,
    ) -> werk_runner::PinBoxFut<'a, Result<Vec<u8>, std::io::Error>> {
        self.oplog.lock().push(MockIoOp::ReadFile(path.to_owned()));
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
        path: &'a Absolute<std::path::Path>,
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
        from: &'a Absolute<std::path::Path>,
        to: &'a Absolute<std::path::Path>,
    ) -> werk_runner::PinBoxFut<'a, Result<(), std::io::Error>> {
        self.oplog
            .lock()
            .push(MockIoOp::CopyFile(from.to_path_buf(), to.to_path_buf()));

        let fut = async move {
            let mut fs = self.filesystem.lock();
            copy_fs(&mut *fs, from, to)
        };
        Box::pin(fut)
    }

    fn delete_file<'a>(
        &'a self,
        path: &'a Absolute<std::path::Path>,
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
        path: &'a Absolute<std::path::Path>,
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

pub fn test_workspace_dir() -> &'static Absolute<std::path::Path> {
    if cfg!(windows) {
        Absolute::new_ref_unchecked(std::path::Path::new("c:\\workspace"))
    } else {
        Absolute::new_ref_unchecked(std::path::Path::new("/workspace"))
    }
}

pub fn output_dir() -> Absolute<std::path::PathBuf> {
    Absolute::new_unchecked(test_workspace_dir().join("target"))
}

pub fn output_file(filename: &str) -> Absolute<std::path::PathBuf> {
    Absolute::new_unchecked(output_dir().join(filename))
}

pub fn workspace_file(filename: &str) -> Absolute<std::path::PathBuf> {
    Absolute::new_unchecked(test_workspace_dir().join(filename))
}

pub fn workspace_file_str(filename: &str) -> String {
    test_workspace_dir()
        .join(filename)
        .to_string_lossy()
        .into_owned()
}

pub fn test_workspace_settings() -> WorkspaceSettings {
    let mut settings = WorkspaceSettings::new(output_dir());
    let ignore_pattern = output_file("**");
    settings.ignore_explicitly(
        globset::GlobSet::builder()
            .add(ignore_pattern.to_string_lossy().parse().unwrap())
            .build()
            .unwrap(),
    );
    settings
}

pub fn program_path(program: &str) -> Absolute<std::path::PathBuf> {
    if cfg!(windows) {
        Absolute::new_unchecked(std::path::Path::new("c:\\bin").join(program))
    } else {
        Absolute::new_unchecked(std::path::Path::new("/bin").join(program))
    }
}
