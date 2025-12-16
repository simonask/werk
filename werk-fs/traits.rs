use std::{borrow::Cow, ops::Deref};

use crate::{Absolute, TooManyParents};

/// Path normalization. Normalization is similar to canonicalization, except it
/// does not access the filesystem. It does not resolve symlinks, and it does
/// not convert Windows paths to their "verbatim" representation, but it _does_
/// convert relative paths to absolute paths (using the current working
/// directory for OS paths).
pub trait Normalize<'a> {
    type Normalized;
    type Err;

    /// True if the path is absolute and does not contain any relative path
    /// components `..` or `.`. It may still contain symlinks.
    fn is_normalized(&self) -> bool;

    /// Normalize the path by resolving relative path components, producing an
    /// absolute path with no `..` or `.` components. Symlinks are not resolved.
    ///
    /// This does does not access the filesystem, but it may access the process
    /// environment to get the current working directory.
    fn normalize(self) -> Result<Self::Normalized, Self::Err>;
}

pub trait Join<T> {
    type Joined;
    type Err;

    fn join(&self, other: T) -> Result<Self::Joined, Self::Err>;
}

impl<T: AsRef<std::path::Path>> Join<T> for std::path::Path {
    type Joined = std::path::PathBuf;
    type Err = std::convert::Infallible;

    #[inline]
    fn join(&self, other: T) -> Result<Self::Joined, Self::Err> {
        Ok(std::path::Path::join(self, other))
    }
}

pub trait Push<T> {
    type Err;
    fn push(&mut self, other: T) -> Result<(), Self::Err>;
}

pub trait IntoBoxedUnsized<Unsized: ?Sized> {
    fn into_boxed_unsized(self) -> Box<Unsized>;
}

impl IntoBoxedUnsized<crate::Path> for crate::PathBuf {
    #[inline]
    fn into_boxed_unsized(self) -> Box<crate::Path> {
        self.into_boxed_path()
    }
}

impl<'a> Normalize<'a> for &'a std::path::Path {
    type Normalized = Cow<'a, Absolute<std::path::Path>>;
    type Err = std::io::Error;

    #[inline]
    fn is_normalized(&self) -> bool {
        self.has_root()
            && !self.components().any(|c| {
                matches!(
                    c,
                    std::path::Component::CurDir | std::path::Component::ParentDir
                )
            })
    }

    fn normalize(self) -> Result<Cow<'a, Absolute<std::path::Path>>, Self::Err> {
        if self.is_normalized() {
            return Ok(Cow::Borrowed(Absolute::new_ref_unchecked(self)));
        }

        let mut buf = if self.is_absolute() {
            std::path::PathBuf::new()
        } else {
            std::env::current_dir()?
        };
        buf.reserve(self.as_os_str().len());

        for c in self.components() {
            match c {
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    if !buf.pop() {
                        return Err(std::io::Error::other(
                            TooManyParents,
                        ));
                    }
                }
                _ => buf.push(c),
            }
        }

        Ok(Cow::Owned(Absolute::new_unchecked(buf)))
    }
}

impl Normalize<'_> for std::path::PathBuf {
    type Normalized = Absolute<Self>;
    type Err = std::io::Error;

    #[inline]
    fn is_normalized(&self) -> bool {
        self.as_path().is_normalized()
    }

    #[inline]
    fn normalize(self) -> Result<Absolute<Self>, Self::Err> {
        if self.is_normalized() {
            return Ok(Absolute::new_unchecked(self));
        }

        self.as_path().normalize().map(Cow::into_owned)
    }
}

macro_rules! push_via_std {
    ($t:ty, $arg:ty) => {
        impl Push<$arg> for $t {
            type Err = std::convert::Infallible;

            #[inline]
            fn push(&mut self, other: $arg) -> Result<(), Self::Err> {
                std::path::PathBuf::push(self, other);
                Ok(())
            }
        }
    };
}

push_via_std!(std::path::PathBuf, &std::path::Path);
push_via_std!(std::path::PathBuf, std::path::PathBuf);
push_via_std!(std::path::PathBuf, &std::path::PathBuf);
push_via_std!(std::path::PathBuf, &str);

impl<'a> Normalize<'a> for &'a crate::Path {
    type Normalized = Cow<'a, Absolute<crate::Path>>;
    type Err = TooManyParents;

    fn is_normalized(&self) -> bool {
        self.is_absolute()
            && !self
                .components()
                .any(|c| matches!(c, crate::Component::Current | crate::Component::Parent))
    }

    fn normalize(self) -> Result<Self::Normalized, Self::Err> {
        if self.is_normalized() {
            return Ok(Cow::Borrowed(Absolute::new_ref_unchecked(self)));
        }

        let mut buf = crate::Path::ROOT.to_owned();

        for c in self.components() {
            buf.push(c)?;
        }

        Ok(Cow::Owned(buf))
    }
}

impl Normalize<'_> for crate::PathBuf {
    type Normalized = Absolute<crate::PathBuf>;
    type Err = TooManyParents;

    fn is_normalized(&self) -> bool {
        self.deref().is_normalized()
    }

    fn normalize(self) -> Result<Self::Normalized, Self::Err> {
        if self.is_normalized() {
            return Ok(Absolute::new_unchecked(self));
        }

        Normalize::normalize(&*self).map(Cow::into_owned)
    }
}

pub trait Parent {
    type Parent: ?Sized;
    fn parent(&self) -> Option<&Self::Parent>;
}

impl Parent for std::path::Path {
    type Parent = Self;

    #[inline]
    fn parent(&self) -> Option<&Self::Parent> {
        self.parent()
    }
}

impl Parent for std::path::PathBuf {
    type Parent = std::path::Path;

    #[inline]
    fn parent(&self) -> Option<&Self::Parent> {
        self.as_path().parent()
    }
}
