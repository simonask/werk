mod validate;

use std::{
    borrow::{Borrow, Cow},
    fmt::{Debug, Display},
    ops::Deref,
    str::FromStr,
};

use crate::{Absolute, Absolutize, IsAbsolute};

/// Virtual path in a werkspace.
///
/// Similar to OS paths, but much more limited. This is in order to maximize
/// portability, so that the same path can be unambiguously resolved to a
/// filesystem path on any OS.
///
/// - All path components are valid UTF-8.
/// - Path components are separated by forward slash `/`.
/// - The following characters are disallowed, because they are disallowed in
///   regular Windows paths: `<>:"/\|?*` (superset of the disallowed paths on
///   Unices).
/// - Characters outside of the printable range are not allowed (including the
///   NUL byte).
/// - Windows reserved filenames cannot be used as directory names or filename
///   stems on any system: `CON`, `PRN`, `AUX`, `NUL`, `COM0`-`COM9`,
///   `LPT0`-`LPT9`. These are reserved even true when they have an extension,
///   and Windows also considers some superscript digits as equivalent to their
///   normal representations, e.g. `COM¹` is also reserved. Read more:
///   <https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file>
/// - Paths cannot end with a separator.
/// - Path components cannot begin or end with any whitespace character. It
///   follows from the previous rule that full paths also cannot begin or end
///   with a whitespace character.
/// - Path components cannot end with a dot `.`.
/// - Paths are case-sensitive.
/// - Empty paths are not allowed.
/// - Absolute paths are always relative to the werkspace root.
/// - Any prefix or suffix of whole path components are always also valid paths.
///   I.e., it is not possible to build an invalid path from the components of a
///   valid path.
///
/// `Path` can always be losslessly converted to `std::path::Path` with full
/// fidelity, and it is guaranteed that a roundtrip conversion produces the same
/// result. However, there are valid `std::path::Path`s that cannot be converted
/// to `Path`, such as a path on Linux containing a reserved Windows path
/// component.
#[repr(transparent)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    path: str,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[serde(transparent)] // TODO: Validate deserialized paths.
pub struct PathBuf {
    path: String,
}

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum PathError {
    #[error("empty path or path component")]
    Empty,
    #[error("path ends with a separator")]
    EndsWithSeparator,
    #[error("path component starts with whitespace")]
    StartsWithWhitespace,
    #[error("path component ends with whitespace")]
    EndsWithWhitespace,
    #[error("path extension contains a separator")]
    SeparatorInExtension,
    #[error("path component ends with a dot")]
    EndsWithDot,
    #[error("path would contain too many parents (would go above the root)")]
    TooManyParents,
    #[error("path is outside the workspace")]
    UnresolveBeyondRoot,
    #[error("illegal character in path: {0}")]
    IllegalChar(char),
    #[error("illegal filename stem (reserved on Windows): {0}")]
    ReservedStem(&'static &'static str),
    #[error("invalid UTF-8 byte: {0}")]
    InvalidUtf8(u8),
    #[error("cannot resolve a relative path to an OS path from a relative working directory: {0}")]
    ResolveRelative(PathBuf),
    #[error("path is not absolute")]
    NotAbsolute,
}

impl Path {
    pub const SEPARATOR: char = '/';
    pub const ROOT: &'static Absolute<Path> = Absolute::new_ref_unchecked(Self::new_unchecked("/"));
    pub const CURRENT: &'static Self = Self::new_unchecked(".");
    pub const PARENT: &'static Self = Self::new_unchecked("..");

    #[inline(always)]
    #[must_use]
    pub const fn new_unchecked(path: &str) -> &Self {
        unsafe {
            // SAFETY: #[repr(transparent)]
            &*(std::ptr::from_ref::<str>(path) as *const Self)
        }
    }

    pub fn new(path: &str) -> Result<&Self, PathError> {
        Self::validate(path)?;
        Ok(Self::new_unchecked(path))
    }

    pub fn from_utf8(path: &[u8]) -> Result<&Self, PathError> {
        match std::str::from_utf8(path) {
            Ok(s) => {
                Self::validate(s)?;
                Ok(Self::new_unchecked(s))
            }
            Err(err) => Err(PathError::InvalidUtf8(path[err.valid_up_to()])),
        }
    }

    /// Create a path from an OS path.
    ///
    /// Note: This is not a conversion, but a validation. If `path` is an
    /// absolute path, this returns an absolute path referencing the project
    /// root rather than the filesystem root.
    ///
    /// Furthermore, if `path` uses Windows path separators, validation will
    /// fail, because backslashes are not allowed in `Path`s.
    ///
    /// The purpose of this function is for roundtrips with
    /// [`Path::as_os_path()`].
    ///
    /// For converting between [`Path`] and [`std::path::Path`], use
    /// [`Path::resolve()`] and [`Path::unresolve()`].
    pub fn from_os_path(path: &std::path::Path) -> Result<&Self, PathError> {
        let path = path.to_str().ok_or(PathError::InvalidUtf8(0))?;
        Self::new(path)
    }

    /// Same as [`Path::from_os_path()`], but does not validate the path (except
    /// that it must be valid UTF-8).
    pub fn from_os_path_unchecked(path: &std::path::Path) -> Result<&Self, PathError> {
        let path = path.to_str().ok_or(PathError::InvalidUtf8(0))?;
        Ok(Self::new_unchecked(path))
    }

    #[inline(always)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.path.len()
    }

    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        // Paths are never empty.
        false
    }

    #[inline]
    pub fn validate(path: &str) -> Result<(), PathError> {
        validate::validate(path)
    }

    #[inline]
    #[must_use]
    pub fn is_root(&self) -> bool {
        self == Self::ROOT
    }

    #[inline]
    #[must_use]
    pub fn is_absolute(&self) -> bool {
        self.path.starts_with(Self::SEPARATOR)
    }

    #[inline]
    #[must_use]
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }

    #[inline]
    #[must_use]
    pub fn parent(&self) -> Option<&Path> {
        if self.is_root() {
            return None;
        }

        let (parent, _tail) = self.path.rsplit_once(Self::SEPARATOR)?;
        debug_assert!(!parent.is_empty());
        Some(Self::new_unchecked(parent))
    }

    #[must_use]
    pub fn is_parent_of(&self, other: &Path) -> bool {
        if self.is_absolute() != other.is_absolute() {
            return false;
        }

        let mut self_components = self.components();
        let mut other_components = other.components();

        loop {
            match (self_components.next(), other_components.next()) {
                // self is shorter than other.
                (None, Some(_)) => return true,
                // self == other; path is a parent of itself.
                (None, None) => return true,
                // self is longer than other.
                (Some(_), None) => return false,
                (Some(lhs), Some(rhs)) => {
                    if lhs != rhs {
                        return false;
                    }
                }
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn ancestors(&self) -> Ancestors {
        Ancestors { path: Some(self) }
    }

    #[inline]
    #[must_use]
    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf {
            path: self.path.to_owned(),
        }
    }

    #[inline]
    #[must_use]
    pub fn into_path_buf(self: Box<Self>) -> PathBuf {
        unsafe {
            // SAFETY: #[repr(transparent)]
            let boxed_str = Box::from_raw(Box::into_raw(self) as *mut str);
            PathBuf {
                path: boxed_str.into_string(),
            }
        }
    }

    #[inline]
    pub fn join(&self, rhs: impl AsPath) -> PathBuf {
        self.try_join(rhs).expect("invalid path component")
    }

    pub fn try_join(&self, rhs: impl AsPath) -> Result<PathBuf, PathError> {
        let mut buf = self.to_path_buf();
        buf.try_push(rhs)?;
        Ok(buf)
    }

    #[inline(always)]
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.path
    }

    /// View this path as an OS path.
    ///
    /// Caution: For absolute [`Path`]s, this produces an absolute path
    /// referencing the filesystem root, which is likely not what you want when
    /// accessing files. The use cases for this function is when reasoning about
    /// the path in some abstract way, e.g. globbing.
    #[inline(always)]
    #[must_use]
    pub fn as_os_path(&self) -> &std::path::Path {
        std::path::Path::new(&self.path)
    }

    pub fn from_components<I>(iter: I) -> Result<PathBuf, PathError>
    where
        I: IntoIterator,
        I::Item: AsPath,
    {
        let mut buf = PathBuf::new(String::new())?;
        buf.extend(iter);
        Ok(buf)
    }

    /// Resolve all `.` and `..` components in the path. If the path is
    /// relative, resolve it relative to `base`. `base` must be an absolute
    /// path, and can be e.g. `Path::ROOT`.
    pub fn absolutize<'a>(
        &'a self,
        base: &Absolute<Path>,
    ) -> Result<Cow<'a, Absolute<Path>>, PathError> {
        if self.is_absolute() {
            return Ok(Cow::Borrowed(Absolute::new_ref_unchecked(self)));
        }

        // Components excluding the root.
        let mut components = Vec::<&Path>::with_capacity(16);
        let mut strcap = 0;

        if !self.is_absolute() {
            for component in base.components() {
                match component {
                    Component::Root | Component::Current => {}
                    Component::Parent => match components.pop() {
                        None => return Err(PathError::TooManyParents),
                        Some(component) => {
                            strcap -= component.len();
                        }
                    },
                    Component::Component(path) => {
                        strcap += path.len();
                        components.push(path);
                    }
                }
            }
        }

        for component in self.components() {
            match component {
                Component::Root | Component::Current => {}
                Component::Parent => match components.pop() {
                    None => return Err(PathError::TooManyParents),
                    Some(component) => {
                        strcap -= component.len();
                    }
                },
                Component::Component(path) => {
                    strcap += path.len();
                    components.push(path);
                }
            }
        }

        if components.is_empty() {
            return Ok(Cow::Borrowed(Self::ROOT));
        }

        let mut buf = String::with_capacity(strcap + components.len());
        for component in components {
            assert!(!component.path.is_empty());
            buf.push(Path::SEPARATOR);
            buf.push_str(component.as_ref());
        }
        Ok(Cow::Owned(Absolute::new_unchecked(PathBuf::new_unchecked(
            buf,
        ))))
    }

    /// Build a filesystem path from an abstract relative to `root`.
    ///
    /// This does not access the filesystem.
    pub fn resolve(
        &self,
        working_dir: &Absolute<Path>,
        root: &Absolute<std::path::Path>,
    ) -> Result<Absolute<std::path::PathBuf>, PathError> {
        if !working_dir.is_absolute() {
            return Err(PathError::ResolveRelative(working_dir.to_path_buf()));
        }
        let in_buf;
        let path = if self.is_absolute() {
            self
        } else {
            in_buf = working_dir.join(self);
            &*in_buf
        };
        debug_assert!(path.is_absolute());

        let mut buf = root.to_path_buf().into_inner();
        for component in path.components() {
            match component {
                Component::Root => {}
                Component::Current => {}
                Component::Parent => {
                    if !buf.pop() {
                        return Err(PathError::TooManyParents);
                    }
                }
                Component::Component(s) => {
                    buf.push(s.as_str());
                }
            }
        }

        Ok(Absolute::new_unchecked(buf))
    }

    /// Get the workspace-relative path from an OS path. If `path` is absolute,
    /// it will be resolved relative to `root`. Otherwise, this function returns
    /// a relative `PathBuf`.
    ///
    /// If `path` is absolute, and does not have `root` as a prefix, an error
    /// will be returned.
    ///
    /// `root` must be an absolute path.
    pub fn unresolve(
        path: &std::path::Path,
        root: &Absolute<std::path::Path>,
    ) -> Result<Absolute<PathBuf>, PathError> {
        // Resolve `..` and `.` so strip_prefix is reliable.
        fn resolve_cur_and_parent(
            input: &std::path::Path,
        ) -> Result<std::path::PathBuf, PathError> {
            let mut abs_buf = std::path::PathBuf::new();
            for component in input.components() {
                match component {
                    std::path::Component::Prefix(_)
                    | std::path::Component::RootDir
                    | std::path::Component::Normal(_) => abs_buf.push(component.as_os_str()),
                    std::path::Component::CurDir => {}
                    std::path::Component::ParentDir => {
                        if !abs_buf.pop() {
                            return Err(PathError::TooManyParents);
                        }
                    }
                }
            }
            Ok(abs_buf)
        }

        let root = resolve_cur_and_parent(root)?;
        let absolute_path = root.join(path);
        let absolute_path = resolve_cur_and_parent(&absolute_path)?;
        let relative_to_root = absolute_path
            .strip_prefix(root)
            .map_err(|_| PathError::UnresolveBeyondRoot)?;

        let mut buf: PathBuf = Path::ROOT.to_owned().into_inner();
        for component in relative_to_root.components() {
            match component {
                std::path::Component::Prefix(_) => return Err(PathError::UnresolveBeyondRoot),
                std::path::Component::RootDir => {}
                std::path::Component::CurDir => unreachable!(". in rerooted path"),
                std::path::Component::ParentDir => unreachable!(".. in rerooted path"),
                std::path::Component::Normal(s) => {
                    let s = Path::from_utf8(s.as_encoded_bytes())?;
                    buf.push(s);
                }
            }
        }

        Ok(Absolute::new_unchecked(buf))
    }

    #[inline]
    #[must_use]
    pub fn components(&self) -> Components {
        Components { path: Some(self) }
    }

    #[inline]
    #[must_use]
    pub fn into_boxed_str(self: Box<Self>) -> Box<str> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            Box::from_raw(Box::into_raw(self) as *mut str)
        }
    }

    #[inline]
    #[must_use]
    pub fn from_boxed_str_unchecked(s: Box<str>) -> Box<Self> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            Box::from_raw(Box::into_raw(s) as *mut Self)
        }
    }
}

impl IsAbsolute for Path {
    #[inline(always)]
    fn is_absolute(&self) -> bool {
        Path::is_absolute(self)
    }
}

impl IsAbsolute for PathBuf {
    #[inline(always)]
    fn is_absolute(&self) -> bool {
        Path::is_absolute(self)
    }
}

impl Absolutize for Path {
    type Err = PathError;

    #[inline(always)]
    fn absolutize<'a>(
        &'a self,
        base: &Absolute<Self>,
    ) -> Result<Cow<'a, Absolute<Self>>, Self::Err> {
        Path::absolutize(self, base)
    }
}

impl Clone for Box<Path> {
    #[inline(always)]
    #[expect(clippy::borrowed_box)]
    fn clone(&self) -> Self {
        let s: &Box<str> = unsafe {
            // SAFETY: #[repr(transparent)]
            &*std::ptr::from_ref::<Box<Path>>(self).cast::<Box<str>>()
        };
        Path::from_boxed_str_unchecked(s.clone())
    }
}

impl From<&Path> for Box<Path> {
    #[inline(always)]
    fn from(path: &Path) -> Self {
        let s: Box<str> = path.path.into();
        Path::from_boxed_str_unchecked(s)
    }
}

impl From<PathBuf> for Box<Path> {
    #[inline(always)]
    fn from(path: PathBuf) -> Self {
        path.into_boxed_path()
    }
}

/// Classified path component.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Component<'a> {
    /// Initial `/`
    Root,
    /// `.`
    Current,
    /// `..`
    Parent,
    /// A path component (no separators)
    Component(&'a Path),
}

impl Component<'_> {
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        match self {
            Component::Root => 1,
            Component::Current => 1,
            Component::Parent => 2,
            Component::Component(path) => path.len(),
        }
    }

    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        // Path components are never empty.
        false
    }
}

impl AsRef<str> for Component<'_> {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        match self {
            Component::Root => "/",
            Component::Current => ".",
            Component::Parent => "..",
            Component::Component(path) => path.as_ref(),
        }
    }
}

pub struct Components<'a> {
    // Note: First character of this path is the separator iff the path is absolute.
    path: Option<&'a Path>,
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let path = self.path?;
        match path.path.split_once(Path::SEPARATOR) {
            None => {
                self.path = None;
                Some(Component::Component(path))
            }
            Some(("", "")) => {
                self.path = None;
                Some(Component::Root)
            }
            Some(("", tail)) => {
                debug_assert!(!tail.is_empty());
                self.path = Some(Path::new_unchecked(tail));
                Some(Component::Root)
            }
            Some((first, tail)) => {
                debug_assert!(!tail.is_empty());
                self.path = Some(Path::new_unchecked(tail));
                match first {
                    "." => Some(Component::Current),
                    ".." => Some(Component::Parent),
                    component => Some(Component::Component(Path::new_unchecked(component))),
                }
            }
        }
    }
}

pub struct Ancestors<'a> {
    path: Option<&'a Path>,
}

impl<'a> Iterator for Ancestors<'a> {
    type Item = &'a Path;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.path {
            self.path = current.parent();
            Some(current)
        } else {
            None
        }
    }
}

impl PathBuf {
    #[inline]
    pub fn new(path: String) -> Result<Self, PathError> {
        Path::validate(&path)?;
        Ok(Self::new_unchecked(path))
    }

    #[inline]
    #[must_use]
    pub fn new_unchecked(path: String) -> Self {
        PathBuf { path }
    }

    pub fn from_utf8(path: Vec<u8>) -> Result<Self, PathError> {
        match String::from_utf8(path) {
            Ok(path) => {
                Path::validate(&path)?;
                Ok(PathBuf { path })
            }
            Err(err) => {
                let utf8err = err.utf8_error();
                let wrong_byte = err.as_bytes()[utf8err.valid_up_to()];
                Err(PathError::InvalidUtf8(wrong_byte))
            }
        }
    }

    /// Extends `self` with `path`.
    ///
    /// If `path` is absolute, it replaces the current path.
    pub fn push(&mut self, path: impl AsPath) -> &mut Self {
        self.try_push(path).expect("invalid path component")
    }

    pub fn try_push(&mut self, path: impl AsPath) -> Result<&mut Self, PathError> {
        let path = path.as_path()?;
        if path.is_absolute() {
            self.path.clear();
        } else if self != Path::ROOT {
            self.path.push(Path::SEPARATOR);
        }
        self.path.push_str(&path.path);
        Ok(self)
    }

    pub fn pop(&mut self) -> bool {
        if self.is_root() {
            return false;
        }

        if let Some(last_sep) = self.path.rfind(Path::SEPARATOR) {
            self.path.truncate(last_sep);
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn into_boxed_path(self) -> Box<Path> {
        let boxed_str = self.path.into_boxed_str();
        unsafe {
            // SAFETY: #[repr(transparent)]
            Box::from_raw(Box::into_raw(boxed_str) as *mut Path)
        }
    }
}

impl FromStr for PathBuf {
    type Err = PathError;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Path::validate(s)?;
        Ok(PathBuf { path: s.to_owned() })
    }
}

impl<'a> TryFrom<&'a str> for &'a Path {
    type Error = PathError;

    #[inline(always)]
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Path::new(value)
    }
}

impl TryFrom<&str> for PathBuf {
    type Error = PathError;

    #[inline(always)]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        PathBuf::new(value.to_owned())
    }
}

impl From<&Path> for PathBuf {
    #[inline(always)]
    fn from(path: &Path) -> Self {
        path.to_path_buf()
    }
}

/// Fallible conversion to `Path`.
pub trait AsPath {
    fn as_path(&self) -> Result<&Path, PathError>;
}

impl AsPath for Path {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Ok(self)
    }
}

impl AsPath for PathBuf {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Ok(self)
    }
}

impl AsPath for &Path {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Ok(*self)
    }
}

impl AsPath for &PathBuf {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Ok(*self)
    }
}

impl AsPath for str {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Path::new(self)
    }
}

impl AsPath for String {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Path::new(self)
    }
}

impl AsPath for &str {
    #[inline(always)]
    fn as_path(&self) -> Result<&Path, PathError> {
        Path::new(self)
    }
}

impl AsPath for Component<'_> {
    #[inline]
    fn as_path(&self) -> Result<&Path, PathError> {
        Ok(match self {
            Component::Root => Path::ROOT,
            Component::Current => Path::CURRENT,
            Component::Parent => Path::PARENT,
            Component::Component(path) => path,
        })
    }
}

impl<P> Extend<P> for PathBuf
where
    P: AsPath,
{
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = P>,
    {
        for path in iter {
            self.push(path);
        }
    }
}

impl AsRef<str> for Path {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        &self.path
    }
}

impl AsRef<str> for PathBuf {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        &self.path
    }
}

impl AsRef<Path> for Path {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for PathBuf {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl Borrow<Path> for PathBuf {
    #[inline(always)]
    fn borrow(&self) -> &Path {
        self
    }
}

impl Deref for PathBuf {
    type Target = Path;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        Path::new_unchecked(&self.path)
    }
}

impl Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <str as Debug>::fmt(&self.path, f)
    }
}

impl Debug for PathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <String as Debug>::fmt(&self.path, f)
    }
}

impl Display for Path {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <str as Display>::fmt(&self.path, f)
    }
}

impl Display for PathBuf {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <String as Display>::fmt(&self.path, f)
    }
}

impl PartialEq<Path> for PathBuf {
    #[inline(always)]
    fn eq(&self, other: &Path) -> bool {
        self.path == other.path
    }
}

impl PartialEq<&Path> for PathBuf {
    #[inline(always)]
    fn eq(&self, other: &&Path) -> bool {
        self.path == other.path
    }
}

impl PartialEq<PathBuf> for Path {
    #[inline(always)]
    fn eq(&self, other: &PathBuf) -> bool {
        self.path == other.path
    }
}

impl PartialEq<&PathBuf> for Path {
    #[inline(always)]
    fn eq(&self, other: &&PathBuf) -> bool {
        self.path == other.path
    }
}

impl From<PathBuf> for String {
    #[inline(always)]
    fn from(path: PathBuf) -> Self {
        path.path
    }
}

impl From<PathBuf> for Box<str> {
    #[inline(always)]
    fn from(path: PathBuf) -> Self {
        path.path.into_boxed_str()
    }
}
