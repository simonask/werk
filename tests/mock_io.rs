use core::panic;
use std::{
    collections::{hash_map, HashMap},
    ffi::{OsStr, OsString},
    pin::Pin,
    sync::{atomic::AtomicU64, Arc, OnceLock},
    time::SystemTime,
};

use parking_lot::Mutex;
use werk_fs::Absolute;
use werk_parser::parser::{Offset, Span};
use werk_runner::{
    globset, BuildStatus, DirEntry, Env, Error, GlobSettings, Io, Metadata, Outdatedness,
    ShellCommandLine, TaskId, WhichError, WorkspaceSettings,
};
use werk_util::{Diagnostic as _, DiagnosticError, DiagnosticSource};
use winnow::stream::Offset as _;

#[inline]
#[must_use]
pub fn make_mtime(secs_since_epoch: u64) -> std::time::SystemTime {
    std::time::UNIX_EPOCH + std::time::Duration::from_secs(secs_since_epoch)
}

#[inline]
#[must_use]
pub fn default_mtime() -> std::time::SystemTime {
    make_mtime(0)
}

#[inline]
#[must_use]
pub fn empty_program_output() -> std::process::Output {
    std::process::Output {
        status: std::process::ExitStatus::default(),
        stdout: Vec::new(),
        stderr: Vec::new(),
    }
}

pub struct TestBuilder<'a> {
    /// Note: Path in the mocked filesystem, not the actual filesystem.
    /// `canonicalize()` won't work etc.
    pub workspace_dir: Absolute<std::path::PathBuf>,
    pub output_dir: Absolute<std::path::PathBuf>,
    pub defines: Vec<(String, String)>,
    pub default_filesystem: bool,
    pub create_workspace_dir: bool,
    pub werkfile: &'a str,
    pub werkfile_path: &'a std::path::Path,
}

pub fn native_path<I: IntoIterator<Item: AsRef<OsStr>>>(
    components: I,
) -> Absolute<std::path::PathBuf> {
    let mut path = if cfg!(windows) {
        std::path::PathBuf::from("c:\\")
    } else {
        std::path::PathBuf::from("/")
    };
    for c in components.into_iter() {
        path.push(c.as_ref());
    }
    Absolute::new(path).expect("native path contains relative components")
}

impl Default for TestBuilder<'_> {
    fn default() -> Self {
        Self {
            workspace_dir: native_path(&["workspace"]),
            output_dir: native_path(&["workspace", "output"]),
            defines: Vec::new(),
            default_filesystem: true,
            create_workspace_dir: true,
            werkfile: "",
            werkfile_path: std::path::Path::new("INPUT"),
        }
    }
}

impl<'a> TestBuilder<'a> {
    pub fn workspace_dir(&mut self, path: &[&str]) -> &mut Self {
        self.workspace_dir = native_path(path);
        self
    }

    pub fn output_dir(&mut self, path: &[&str]) -> &mut Self {
        self.output_dir = native_path(path);
        self
    }

    pub fn define(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.defines.push((key.into(), value.into()));
        self
    }

    pub fn werkfile(&mut self, werkfile: &'a str) -> &mut Self {
        self.werkfile = werkfile;
        self
    }

    pub fn build(&self) -> Result<Test<'a>, werk_parser::Error> {
        let ast = werk_parser::parse_werk(self.werkfile_path, self.werkfile)?;

        let io = MockIo::default();
        io.set_env("PROFILE", "debug");
        if self.default_filesystem {
            create_dirs(&mut io.filesystem.lock(), &self.workspace_dir).unwrap();
            create_dirs(&mut io.filesystem.lock(), &self.output_dir).unwrap();

            io.set_program("clang", program_path("clang"), |_cmd, _fs, _env| {
                Ok(empty_program_output())
            });
            io.set_program("write", program_path("write"), move |cmdline, fs, _env| {
                let contents = cmdline.arguments[0].as_str();
                let file = std::path::Path::new(cmdline.arguments[1].as_str());
                tracing::trace!("write {}", file.display());
                insert_fs(
                    fs,
                    file,
                    (
                        Metadata {
                            mtime: make_mtime(1),
                            is_file: true,
                            is_symlink: false,
                        },
                        contents.as_bytes().into(),
                    ),
                )
                .unwrap();
                Ok(empty_program_output())
            });
            io.set_program(
                "write-env",
                program_path("write-env"),
                |cmdline, fs, env| {
                    let varname = std::ffi::OsString::from(cmdline.arguments[0].as_str());
                    let file = std::path::Path::new(cmdline.arguments[1].as_str());
                    tracing::trace!("write-env {}", file.display());

                    let contents = env
                        .get(&varname)
                        .cloned()
                        .or_else(|| std::env::var_os(&varname))
                        .unwrap_or_default();
                    let contents: String = contents.into_string().unwrap();
                    insert_fs(
                        fs,
                        file,
                        (
                            Metadata {
                                mtime: make_mtime(1),
                                is_file: true,
                                is_symlink: false,
                            },
                            contents.as_bytes().into(),
                        ),
                    )
                    .unwrap();

                    Ok(empty_program_output())
                },
            );
        }

        if self.create_workspace_dir {
            io.create_parent_dirs(&self.workspace_dir).unwrap()
        }
        Ok(Test {
            io: Arc::new(io),
            render: Arc::new(MockRender::default()),
            workspace_dir: self.workspace_dir.clone(),
            output_dir: self.output_dir.clone(),
            ast,
            source: self.werkfile,
            pragma_check_files: vec![],
        })
    }
}

pub struct Test<'a> {
    pub io: Arc<MockIo>,
    pub render: Arc<MockRender>,
    pub workspace_dir: Absolute<std::path::PathBuf>,
    pub output_dir: Absolute<std::path::PathBuf>,
    pub ast: werk_parser::Document<'a>,
    pub source: &'a str,
    pragma_check_files: Vec<(Span, String, Vec<u8>)>,
}

impl<'a> Test<'a> {
    pub fn new(
        source: &'a str,
    ) -> Result<Self, werk_util::DiagnosticError<'a, werk_parser::Error, DiagnosticSource<'a>>>
    {
        let mut test = TestBuilder::default()
            .werkfile(source)
            .build()
            .map_err(|err| {
                err.into_diagnostic_error(DiagnosticSource::new(
                    std::path::Path::new("input"),
                    source,
                ))
            })?;
        test.reload_test_pragmas();
        Ok(test)
    }

    pub fn reload(
        &mut self,
        source: &'a str,
    ) -> Result<(), werk_util::DiagnosticError<'a, werk_parser::Error, DiagnosticSource<'a>>> {
        self.ast = werk_parser::parse_werk(self.ast.origin, source).map_err(|err| {
            err.into_diagnostic_error(DiagnosticSource::new(std::path::Path::new("input"), source))
        })?;
        self.source = source;
        self.reload_test_pragmas();
        Ok(())
    }

    fn reload_test_pragmas(&mut self) {
        self.pragma_check_files.clear();

        // Interpret pragmas in the trailing comment of the werkfile.
        let trailing_whitespace = self.ast.get_whitespace(self.ast.root.ws_trailing);
        let trailing_whitespace_lines = trailing_whitespace.trim().lines();
        let trailing_ws_span_start = if self.ast.root.ws_trailing.0.is_ignored() {
            0
        } else {
            self.ast.root.ws_trailing.0.start.0 as usize
        };

        let regexes = regexes();
        {
            let mut fs = self.io.filesystem.lock();
            for line in trailing_whitespace_lines {
                let offset = line.offset_from(&trailing_whitespace);
                let span = Span {
                    start: Offset((trailing_ws_span_start + offset) as u32),
                    end: Offset((trailing_ws_span_start + offset + line.len()) as u32),
                };

                if let Some(captures) = regexes.file.captures(line) {
                    let filename = captures.get(1).unwrap().as_str();
                    let content = captures.get(2).unwrap().as_str();
                    let path = self.workspace_path(filename.split('/'));
                    insert_fs(
                        &mut fs,
                        &path,
                        (
                            Metadata {
                                mtime: default_mtime(),
                                is_file: true,
                                is_symlink: false,
                            },
                            content.as_bytes().to_owned(),
                        ),
                    )
                    .unwrap();
                } else if let Some(captures) = regexes.dir.captures(line) {
                    let dirname = captures.get(1).unwrap().as_str();
                    let path = self.workspace_path(dirname.split('/'));
                    create_dirs(&mut fs, &path).unwrap();
                } else if let Some(captures) = regexes.assert_file.captures(line) {
                    let filename = captures.get(1).unwrap().as_str();
                    let content = captures.get(2).unwrap().as_str();
                    self.pragma_check_files.push((
                        span,
                        filename.to_owned(),
                        content.as_bytes().to_owned(),
                    ));
                } else if let Some(captures) = regexes.env.captures(line) {
                    let key = captures.get(1).unwrap().as_str();
                    let value = captures.get(2).unwrap().as_str();
                    self.io.set_env(key, value);
                }
            }
        }
    }

    pub fn run_pragma_tests(&self) -> Result<(), werk_runner::EvalError> {
        let fs = self.io.filesystem.lock();
        for (span, filename, expected) in &self.pragma_check_files {
            let out_file = self.output_path(filename.split('/'));
            let (_entry, actual) = read_fs(&fs, &out_file).unwrap();
            if actual != expected {
                return Err(werk_runner::EvalError::AssertCustomFailed(
                    *span,
                    format!("contents of output file `{filename}` do not match\nexpected: {expected:?}\n  actual: {actual:?}"),
                ));
            }
        }

        Ok(())
    }

    pub fn create_workspace<'b>(
        &'b self,
        defines: &[(&str, &str)],
    ) -> Result<
        werk_runner::Workspace<'b>,
        DiagnosticError<'b, werk_runner::Error, &'b werk_parser::Document<'b>>,
    > {
        let mut settings = WorkspaceSettings::new(self.output_dir.clone());

        // Normally this would be covered by `.gitignore`, but we don't have that,
        // so just use a manual ignore pattern.
        let ignore_pattern = self.output_dir.join("**").unwrap();
        settings.ignore_explicitly(
            globset::GlobSet::builder()
                .add(ignore_pattern.to_string_lossy().parse().unwrap())
                .build()
                .unwrap(),
        );

        for (key, value) in defines {
            settings.define(*key, *value);
        }

        werk_runner::Workspace::new_with_diagnostics(
            &self.ast,
            &*self.io,
            &*self.render,
            self.workspace_dir.clone(),
            &settings,
        )
    }

    pub fn workspace_path(
        &self,
        path: impl IntoIterator<Item: AsRef<OsStr>>,
    ) -> Absolute<std::path::PathBuf> {
        let mut workspace_path = self.workspace_dir.clone();
        for c in path {
            workspace_path
                .push(std::path::Component::Normal(c.as_ref()))
                .unwrap();
        }
        workspace_path
    }

    pub fn workspace_path_str(&self, path: impl IntoIterator<Item: AsRef<OsStr>>) -> String {
        self.workspace_path(path)
            .into_inner()
            .to_string_lossy()
            .into_owned()
    }

    pub fn output_path(
        &self,
        path: impl IntoIterator<Item: AsRef<OsStr>>,
    ) -> Absolute<std::path::PathBuf> {
        let mut output_path = self.output_dir.clone();
        for c in path {
            output_path
                .push(std::path::Component::Normal(c.as_ref()))
                .unwrap();
        }
        output_path
    }

    pub fn output_path_str(&self, path: impl IntoIterator<Item: AsRef<OsStr>>) -> String {
        self.output_path(path)
            .into_inner()
            .to_string_lossy()
            .into_owned()
    }

    pub fn set_workspace_file(
        &self,
        path: &[&str],
        contents: impl AsRef<[u8]>,
    ) -> std::io::Result<()> {
        let mut fs = self.io.filesystem.lock();
        let path = self.workspace_path(path);
        create_parent_dirs(&mut fs, &path).unwrap();
        insert_fs(
            &mut fs,
            path.as_ref(),
            (
                Metadata {
                    mtime: self.io.now(),
                    is_file: true,
                    is_symlink: false,
                },
                Vec::from(contents.as_ref()),
            ),
        )
    }

    pub fn set_output_file(
        &self,
        path: &[&str],
        contents: impl AsRef<[u8]>,
    ) -> std::io::Result<()> {
        let mut fs = self.io.filesystem.lock();
        let path = self.output_path(path);
        create_parent_dirs(&mut fs, &path).unwrap();
        insert_fs(
            &mut fs,
            path.as_ref(),
            (
                Metadata {
                    mtime: self.io.now(),
                    is_file: true,
                    is_symlink: false,
                },
                Vec::from(contents.as_ref()),
            ),
        )
    }

    pub fn set_workspace_dir(&self, path: &[&str]) -> std::io::Result<()> {
        let mut fs = self.io.filesystem.lock();
        let path = self.workspace_path(path);
        create_dirs(&mut fs, &path)
    }

    pub fn did_run_during_build(&self, command_line: &ShellCommandLine) -> bool {
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::RunDuringBuild(c) if c == command_line))
    }

    pub fn did_run_during_eval(&self, command_line: &ShellCommandLine) -> bool {
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::RunDuringEval(c) if c == command_line))
    }

    pub fn did_which(&self, command: &str) -> bool {
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::Which(c) if c == command))
    }

    pub fn did_read_workspace_file(&self, path: &[&str]) -> bool {
        let workspace_file = self.workspace_path(path);
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::ReadFile(p) if *p == workspace_file))
    }

    pub fn did_read_output_file(&self, path: &[&str]) -> bool {
        let workspace_file = self.output_path(path);
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::ReadFile(p) if *p == workspace_file))
    }

    pub fn did_write_output_file(&self, path: &[&str]) -> bool {
        let path = self.output_path(path);
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::WriteFile(p) if *p == path))
    }

    pub fn did_copy_file(
        &self,
        src: impl AsRef<Absolute<std::path::Path>>,
        dst: impl AsRef<Absolute<std::path::Path>>,
    ) -> bool {
        let src = src.as_ref().to_path_buf();
        let dst = dst.as_ref().to_path_buf();
        self
            .io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::CopyFile(op_src, op_dst) if *op_src == src && *op_dst == dst))
    }

    pub fn did_create_parent_dirs(&self, path: &std::path::Path) -> bool {
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::CreateParentDirs(p) if p == path))
    }

    pub fn did_read_env(&self, name: &str) -> bool {
        self.io
            .oplog
            .lock()
            .iter()
            .any(|op| matches!(op, MockIoOp::ReadEnv(n) if n == name))
    }
}

#[derive(Default)]
pub struct MockRender {
    pub log: Mutex<Vec<MockRenderEvent>>,
}

#[derive(Debug, PartialEq)]
pub enum MockRenderEvent {
    WillBuild(TaskId, usize, Outdatedness),
    DidBuild(TaskId, Result<BuildStatus, Error>),
    WillExecute(TaskId, ShellCommandLine, usize, usize),
    DidExecute(
        TaskId,
        ShellCommandLine,
        Result<std::process::ExitStatus, ()>,
        usize,
        usize,
    ),
    Message(Option<TaskId>, String),
    Warning(Option<TaskId>, String),
}

impl MockRender {
    pub fn did_see(&self, event: &MockRenderEvent) -> bool {
        self.log.lock().iter().any(|e| e == event)
    }
}

impl werk_runner::Render for MockRender {
    fn will_build(&self, task_id: TaskId, num_steps: usize, outdatedness: &Outdatedness) {
        self.log.lock().push(MockRenderEvent::WillBuild(
            task_id,
            num_steps,
            outdatedness.clone(),
        ));
    }

    fn did_build(&self, task_id: TaskId, result: &Result<BuildStatus, Error>) {
        self.log
            .lock()
            .push(MockRenderEvent::DidBuild(task_id, result.clone()));
    }

    fn will_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        self.log.lock().push(MockRenderEvent::WillExecute(
            task_id,
            command.clone(),
            step,
            num_steps,
        ));
    }

    fn did_execute(
        &self,
        task_id: TaskId,
        command: &ShellCommandLine,
        result: &Result<std::process::ExitStatus, std::io::Error>,
        step: usize,
        num_steps: usize,
    ) {
        self.log.lock().push(MockRenderEvent::DidExecute(
            task_id,
            command.clone(),
            result.as_ref().map_err(|_| ()).cloned(),
            step,
            num_steps,
        ));
    }

    fn message(&self, task_id: Option<TaskId>, message: &str) {
        tracing::trace!(
            "info({}) {message}",
            task_id.map(|t| t.to_string()).unwrap_or_default()
        );
        self.log
            .lock()
            .push(MockRenderEvent::Message(task_id, message.to_string()));
    }

    fn warning(&self, task_id: Option<TaskId>, message: &str) {
        self.log
            .lock()
            .push(MockRenderEvent::Warning(task_id, message.to_string()));
    }
}

pub type MockDir = HashMap<OsString, MockDirEntry>;
pub type ProgramResult = std::io::Result<std::process::Output>;

#[derive(Debug)]
pub enum MockDirEntry {
    File(Metadata, Vec<u8>),
    Dir(MockDir),
}

pub type Program = Box<dyn FnMut(&ShellCommandLine, &mut MockDir, &Env) -> ProgramResult + Send>;

#[derive(Default)]
pub struct MockIo {
    pub filesystem: Mutex<MockDir>,
    pub which: Mutex<HashMap<String, Absolute<std::path::PathBuf>>>,
    pub programs: Mutex<HashMap<Absolute<std::path::PathBuf>, Program>>,
    pub env: Mutex<Env>,
    pub oplog: Mutex<Vec<MockIoOp>>,
    pub now: AtomicU64,
}

impl MockIo {
    pub fn initialize_default_env(&mut self) {
        self.with_envs([("PROFILE", "debug")]);
    }
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
                        Err(std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            "parent directory not found (insert)",
                        ))
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
                    "parent directory not found (remove)",
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

pub fn read_fs<'a>(
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
                metadata: *metadata,
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

#[must_use]
pub fn contains_file(fs: &MockDir, path: &Absolute<std::path::Path>) -> bool {
    read_fs(fs, path).is_ok()
}

pub fn contains_dir(fs: &MockDir, path: &Absolute<std::path::Path>) -> bool {
    let entry = get_fs(fs, path);
    matches!(entry, Some(MockDirEntry::Dir(_)))
}

impl MockIo {
    pub fn with_default_workspace_dir(&self) -> &Self {
        let mut fs = self.filesystem.lock();
        create_dirs(&mut fs, test_workspace_dir()).unwrap();
        std::mem::drop(fs);
        self
    }

    pub fn with_programs<I>(&self, iter: I) -> &Self
    where
        I: IntoIterator<Item = (String, Program)>,
    {
        let mut programs = self.programs.lock();
        for (program, fun) in iter {
            let path = native_path([&program]);
            self.which.lock().insert(program, path.clone());
            programs.insert(path, fun);
        }
        std::mem::drop(programs);

        self
    }

    pub fn with_envs<I, K, V>(&self, iter: I) -> &Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<OsString>,
        V: Into<OsString>,
    {
        self.env
            .lock()
            .envs(iter.into_iter().map(|(k, v)| (k.into(), v.into())));
        self
    }

    pub fn set_env(&self, name: impl AsRef<OsStr>, value: impl AsRef<OsStr>) {
        self.env.lock().env(name, value);
    }

    pub fn set_program(
        &self,
        program: impl Into<String>,
        path: Absolute<std::path::PathBuf>,
        fun: impl FnMut(&ShellCommandLine, &mut MockDir, &Env) -> ProgramResult + Send + 'static,
    ) -> &Self {
        let program = program.into();
        self.which.lock().insert(program.clone(), path.clone());
        self.programs.lock().insert(path, Box::new(fun));
        self
    }

    pub fn remove_program(&self, program: &str) {
        let Some(path) = self.which.lock().remove(program) else {
            return;
        };
        self.programs.lock().remove(&path);
    }

    #[must_use]
    pub fn with_program_removed(self, program: &str) -> Self {
        self.remove_program(program);
        self
    }

    pub fn delete_file(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<()> {
        let mut fs = self.filesystem.lock();
        remove_fs(&mut fs, path.as_ref())
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

    pub fn contains_file(&self, path: impl AsRef<Absolute<std::path::Path>>) -> bool {
        contains_file(&self.filesystem.lock(), path.as_ref())
    }

    pub fn contains_dir(&self, path: impl AsRef<Absolute<std::path::Path>>) -> bool {
        contains_dir(&self.filesystem.lock(), path.as_ref())
    }
}

struct MockChild {
    stdout: Option<Pin<Box<futures::io::Cursor<Vec<u8>>>>>,
    stderr: Option<Pin<Box<futures::io::Cursor<Vec<u8>>>>>,
    status: Option<Pin<Box<futures::future::Ready<std::io::Result<std::process::ExitStatus>>>>>,
}

impl werk_runner::Child for MockChild {
    fn stdin(
        self: std::pin::Pin<&mut Self>,
    ) -> Option<std::pin::Pin<&mut dyn futures::AsyncWrite>> {
        None
    }

    fn stderr(
        self: std::pin::Pin<&mut Self>,
    ) -> Option<std::pin::Pin<&mut dyn futures::AsyncRead>> {
        let this = Pin::into_inner(self);
        this.stderr.as_mut().map(|v| v.as_mut() as _)
    }

    fn take_stdin(&mut self) -> Option<std::pin::Pin<Box<dyn futures::AsyncWrite + Send>>> {
        None
    }

    fn take_stdout(&mut self) -> Option<std::pin::Pin<Box<dyn futures::AsyncRead + Send>>> {
        self.stdout.take().map(|v| v as _)
    }

    fn take_stderr(&mut self) -> Option<std::pin::Pin<Box<dyn futures::AsyncRead + Send>>> {
        self.stderr.take().map(|v| v as _)
    }

    fn status(
        &mut self,
    ) -> std::pin::Pin<
        Box<
            dyn std::future::Future<Output = Result<std::process::ExitStatus, std::io::Error>>
                + Send,
        >,
    > {
        self.status.take().unwrap()
    }
}

impl werk_runner::Io for MockIo {
    fn run_recipe_command(
        &self,
        command_line: &ShellCommandLine,
        _working_dir: &Absolute<std::path::Path>,
        env: &Env,
        forward_stdout: bool,
    ) -> std::io::Result<Box<dyn werk_runner::Child>> {
        tracing::trace!("run during build: {}", command_line);
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringBuild(command_line.clone()));

        let mut programs = self.programs.lock();
        let Some(program) = programs.get_mut(&command_line.program) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "program not found",
            ));
        };
        let mut fs = self.filesystem.lock();
        let mut global_env = self.env.lock().clone();
        global_env.merge_from(env);
        let std::process::Output {
            status,
            stderr,
            stdout,
        } = program(command_line, &mut fs, &global_env)?;

        Ok(Box::new(MockChild {
            stderr: Some(Box::pin(futures::io::Cursor::new(stderr))),
            stdout: if forward_stdout {
                Some(Box::pin(futures::io::Cursor::new(stdout)))
            } else {
                None
            },
            status: Some(Box::pin(futures::future::ready(Ok(status)))),
        }))
    }

    fn run_during_eval(
        &self,
        command_line: &ShellCommandLine,
        _working_dir: &Absolute<std::path::Path>,
        command_env: &werk_runner::Env,
    ) -> Result<std::process::Output, std::io::Error> {
        self.oplog
            .lock()
            .push(MockIoOp::RunDuringEval(command_line.clone()));

        let mut programs = self.programs.lock();
        let Some(program) = programs.get_mut(&*command_line.program) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "program not found",
            ));
        };
        let mut fs = self.filesystem.lock();
        let mut env = self.env.lock().clone();
        env.merge_from(command_env);
        program(command_line, &mut fs, &env)
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

    fn glob_workspace(
        &self,
        path: &Absolute<std::path::Path>,
        settings: &GlobSettings,
    ) -> Result<Vec<DirEntry>, Error> {
        fn glob(
            path: &Absolute<std::path::Path>,
            dir: &MockDir,
            results: &mut Vec<DirEntry>,
            ignore_explicitly: &globset::GlobSet,
        ) -> Result<(), Error> {
            for (name, entry) in dir {
                let entry_path = path.join(name).unwrap();
                match entry {
                    MockDirEntry::File(metadata, _) => {
                        if !ignore_explicitly.is_match(&*entry_path) {
                            results.push(DirEntry {
                                path: entry_path,
                                metadata: *metadata,
                            });
                        }
                    }
                    MockDirEntry::Dir(subdir) => {
                        if !ignore_explicitly.is_match(&*entry_path) {
                            results.push(DirEntry {
                                path: entry_path.clone(),
                                metadata: Metadata {
                                    mtime: SystemTime::UNIX_EPOCH,
                                    is_file: false,
                                    is_symlink: false,
                                },
                            });
                        }
                        glob(&entry_path, subdir, results, ignore_explicitly)?;
                    }
                }
            }

            Ok(())
        }

        tracing::trace!("glob workspace: {}", path.display());
        let fs = self.filesystem.lock();

        let MockDirEntry::Dir(workspace) =
            get_fs(&fs, path).expect("workspace path does not exist")
        else {
            panic!("workspace path is a file");
        };

        let mut results = Vec::new();
        let result = glob(path, workspace, &mut results, &settings.ignore_explicitly);

        result.map(move |()| results)
    }

    fn metadata(&self, path: &Absolute<std::path::Path>) -> Result<Metadata, Error> {
        let fs = self.filesystem.lock();
        read_fs(&fs, path)
            .map(|(entry, _)| entry.metadata)
            .map_err(Into::into)
    }

    fn read_file(&self, path: &Absolute<std::path::Path>) -> Result<Vec<u8>, std::io::Error> {
        self.oplog.lock().push(MockIoOp::ReadFile(path.to_owned()));
        let fs = self.filesystem.lock();
        let (entry, data) = read_fs(&fs, path)?;
        if !entry.metadata.is_file {
            return Err(std::io::Error::new(
                std::io::ErrorKind::IsADirectory,
                "is a directory",
            ));
        }
        let data = Vec::from(data);
        Ok(data)
    }

    fn write_file(
        &self,
        path: &Absolute<std::path::Path>,
        data: &[u8],
    ) -> Result<(), std::io::Error> {
        let path = path.to_path_buf();
        self.oplog.lock().push(MockIoOp::WriteFile(path.clone()));

        let mut fs = self.filesystem.lock();
        insert_fs(
            &mut fs,
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
    }

    fn copy_file(
        &self,
        from: &Absolute<std::path::Path>,
        to: &Absolute<std::path::Path>,
    ) -> Result<(), std::io::Error> {
        self.oplog
            .lock()
            .push(MockIoOp::CopyFile(from.to_path_buf(), to.to_path_buf()));

        let mut fs = self.filesystem.lock();
        copy_fs(&mut fs, from, to)
    }

    fn delete_file(&self, path: &Absolute<std::path::Path>) -> Result<(), std::io::Error> {
        let path = path.to_path_buf();
        self.oplog.lock().push(MockIoOp::DeleteFile(path.clone()));

        let mut fs = self.filesystem.lock();
        remove_fs(&mut fs, &path)
    }

    fn create_parent_dirs(&self, path: &Absolute<std::path::Path>) -> Result<(), std::io::Error> {
        self.oplog
            .lock()
            .push(MockIoOp::CreateParentDirs(path.to_path_buf()));

        let mut fs = self.filesystem.lock();
        create_parent_dirs(&mut fs, path)
    }

    fn read_env(&self, name: &str) -> Option<String> {
        self.oplog.lock().push(MockIoOp::ReadEnv(name.to_string()));
        self.env
            .lock()
            .get(OsStr::new(name))
            .map(|s| s.clone().into_string().unwrap())
    }

    fn is_dry_run(&self) -> bool {
        false
    }
}

#[must_use]
fn test_workspace_dir() -> &'static Absolute<std::path::Path> {
    if cfg!(windows) {
        Absolute::new_ref(std::path::Path::new("c:\\workspace")).unwrap()
    } else {
        Absolute::new_ref(std::path::Path::new("/workspace")).unwrap()
    }
}

#[must_use]
pub fn program_path(program: &str) -> Absolute<std::path::PathBuf> {
    if cfg!(windows) {
        Absolute::new_ref(std::path::Path::new("c:\\bin"))
            .unwrap()
            .join(program)
            .unwrap()
    } else {
        Absolute::new_ref(std::path::Path::new("/bin"))
            .unwrap()
            .join(program)
            .unwrap()
    }
}

struct PragmaRegexes {
    pub file: regex::Regex,
    pub dir: regex::Regex,
    pub assert_file: regex::Regex,
    pub env: regex::Regex,
}

impl Default for PragmaRegexes {
    fn default() -> Self {
        Self {
            file: regex::Regex::new(r"^#\!file (.*)=(.*)$").unwrap(),
            dir: regex::Regex::new(r"^#\!dir (.*)$").unwrap(),
            assert_file: regex::Regex::new(r"^#\!assert-file (.*)=(.*)$").unwrap(),
            env: regex::Regex::new(r"^#\!env (.*)=(.*)$").unwrap(),
        }
    }
}

fn regexes() -> &'static PragmaRegexes {
    static REGEXES: OnceLock<PragmaRegexes> = OnceLock::new();
    REGEXES.get_or_init(PragmaRegexes::default)
}
