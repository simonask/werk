use std::{borrow::Borrow, ops::Deref};

use crate::{
    traits::{IntoBoxedUnsized, Join, Normalize, Parent, Push},
    AsPath, TooManyParents,
};

/// Toll-free wrapper marking that a path is normalized.
///
/// Works with both `werk_fs` paths and `std` paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Absolute<P: ?Sized> {
    path: P,
}

impl<P: serde::Serialize> serde::Serialize for Absolute<P> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.path.serialize(serializer)
    }
}

impl<'de, P: serde::Deserialize<'de> + Normalize<'de>> serde::Deserialize<'de> for Absolute<P> {
    fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Absolute<P>, D::Error> {
        let path = P::deserialize(de)?;
        if !path.is_normalized() {
            return Err(serde::de::Error::custom(
                "deserialized path is not normalied",
            ));
        }
        Ok(Absolute::new_unchecked(path))
    }
}

impl<P: Sized> Absolute<P> {
    #[must_use]
    pub const fn new_unchecked(path: P) -> Self {
        Self { path }
    }

    pub fn into_inner(self) -> P {
        self.path
    }
}

impl<P: ?Sized> Absolute<P> {
    #[must_use]
    pub fn as_inner(&self) -> &P {
        &self.path
    }
}

impl<P: for<'a> Normalize<'a> + ?Sized> Absolute<P> {
    /// Check if `path` is already normalized, and return it if it is.
    pub fn new(path: P) -> Result<Self, P>
    where
        P: Sized,
    {
        if path.is_normalized() {
            Ok(Self { path })
        } else {
            Err(path)
        }
    }
}

impl<'a, P: ?Sized> Absolute<P>
where
    &'a P: Normalize<'a> + 'a,
{
    /// Check if `path` is already normalized, and return it if it is.
    pub fn new_ref(path: &'a P) -> Result<&'a Self, &'a P> {
        if path.is_normalized() {
            Ok(Self::new_ref_unchecked(path))
        } else {
            Err(path)
        }
    }
}

impl<P: ?Sized> Absolute<P> {
    pub(crate) const fn new_ref_unchecked(path: &P) -> &Self {
        unsafe {
            // SAFETY: #[repr(transparent)]
            &*(std::ptr::from_ref(path) as *const Self)
        }
    }
}

impl<P> Normalize<'_> for Absolute<P> {
    type Normalized = Self;
    type Err = std::convert::Infallible;

    fn is_normalized(&self) -> bool {
        true
    }

    fn normalize(self) -> Result<Self, Self::Err> {
        Ok(self)
    }
}

impl<'a, P> Normalize<'a> for &'a Absolute<P> {
    type Normalized = &'a Absolute<P>;
    type Err = std::convert::Infallible;

    fn is_normalized(&self) -> bool {
        true
    }

    fn normalize(self) -> Result<&'a Absolute<P>, Self::Err> {
        Ok(self)
    }
}

impl Push<crate::Component<'_>> for Absolute<crate::PathBuf> {
    type Err = TooManyParents;

    #[inline]
    fn push(&mut self, other: crate::Component<'_>) -> Result<(), Self::Err> {
        match other {
            crate::Component::Root => {
                crate::Path::ROOT.clone_into(self);
            }
            crate::Component::Current => {}
            crate::Component::Parent => {
                if !self.path.pop() {
                    return Err(TooManyParents);
                }
            }
            crate::Component::Component(path) => {
                self.path.push_raw_component(path.as_str());
            }
        }

        Ok(())
    }
}

impl Push<std::path::Component<'_>> for Absolute<std::path::PathBuf> {
    type Err = TooManyParents;

    fn push(&mut self, other: std::path::Component<'_>) -> Result<(), Self::Err> {
        match other {
            std::path::Component::Prefix(_) | std::path::Component::RootDir => {
                self.path.push(other);
            }
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if !self.path.pop() {
                    return Err(TooManyParents);
                }
            }
            std::path::Component::Normal(os_str) => self.path.push(os_str),
        }

        Ok(())
    }
}

impl<P: Parent + ?Sized> Parent for Absolute<P> {
    type Parent = Absolute<P::Parent>;

    fn parent(&self) -> Option<&Self::Parent> {
        let parent = self.path.parent()?;
        Some(Absolute::new_ref_unchecked(parent))
    }
}

impl<P: Parent + ?Sized> Absolute<P> {
    // Overload deref.
    pub fn parent(&self) -> Option<&Absolute<P::Parent>> {
        Parent::parent(self)
    }
}

impl<P> Absolute<P> {
    pub fn into_boxed_path<T>(self) -> Box<Absolute<T>>
    where
        P: IntoBoxedUnsized<T>,
        T: ?Sized,
    {
        let path = self.path.into_boxed_unsized();
        unsafe {
            // SAFETY: #[repr(transparent)]
            Box::from_raw(Box::into_raw(path) as *mut Absolute<T>)
        }
    }
}

impl<P: ?Sized> Absolute<P> {
    pub fn join<T>(&self, other: T) -> Result<<Self as Join<T>>::Joined, <Self as Join<T>>::Err>
    where
        Self: Join<T>,
    {
        Join::join(self, other)
    }

    pub fn push<T>(&mut self, other: T) -> Result<(), <Self as Push<T>>::Err>
    where
        Self: Push<T>,
    {
        Push::push(self, other)
    }

    pub fn to_path_buf(&self) -> Absolute<P::Owned>
    where
        P: ToOwned,
    {
        Absolute::new_unchecked(self.path.to_owned())
    }
}

impl<T: AsRef<std::path::Path>> Join<T> for Absolute<std::path::Path> {
    type Joined = Absolute<std::path::PathBuf>;
    type Err = TooManyParents;

    fn join(&self, other: T) -> Result<Self::Joined, Self::Err> {
        let mut buf = self.to_path_buf();
        for c in other.as_ref().components() {
            buf.push(c)?;
        }
        Ok(buf)
    }
}

impl<P: AsPath> AsPath for Absolute<P> {
    fn as_path(&self) -> Result<&crate::Path, crate::PathError> {
        self.path.as_path()
    }
}

impl<P: ?Sized> Absolute<P> {
    #[expect(clippy::borrowed_box)]
    fn as_inner_box(self: &Box<Self>) -> &Box<P> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            let path: &Box<P> = &*(std::ptr::from_ref(self).cast());
            path
        }
    }

    fn from_inner_box_unchecked(path: Box<P>) -> Box<Self> {
        unsafe {
            let path: *mut P = Box::into_raw(path);
            let path: *mut Self = path as _;
            // SAFETY: #[repr(transparent)]
            Box::from_raw(path)
        }
    }
}

impl Clone for Box<Absolute<crate::Path>> {
    #[inline]
    fn clone(&self) -> Self {
        Absolute::from_inner_box_unchecked(self.as_inner_box().clone())
    }
}

impl Clone for Box<Absolute<std::path::Path>> {
    #[inline]
    fn clone(&self) -> Self {
        Absolute::from_inner_box_unchecked(self.as_inner_box().clone())
    }
}

impl<P: ?Sized> Borrow<P> for Absolute<P> {
    #[inline]
    fn borrow(&self) -> &P {
        &self.path
    }
}

impl Borrow<Absolute<std::path::Path>> for Absolute<std::path::PathBuf> {
    #[inline]
    fn borrow(&self) -> &Absolute<std::path::Path> {
        Absolute::new_ref_unchecked(self.path.as_path())
    }
}

impl ToOwned for Absolute<std::path::Path> {
    type Owned = Absolute<std::path::PathBuf>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        Absolute::new_unchecked(self.path.to_owned())
    }
}

impl Borrow<Absolute<crate::Path>> for Absolute<crate::PathBuf> {
    #[inline]
    fn borrow(&self) -> &Absolute<crate::Path> {
        Absolute::new_ref_unchecked(self.path.as_ref())
    }
}

impl ToOwned for Absolute<crate::Path> {
    type Owned = Absolute<crate::PathBuf>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        Absolute::new_unchecked(self.path.to_path_buf())
    }
}

impl From<Absolute<crate::PathBuf>> for Box<Absolute<crate::Path>> {
    #[inline]
    fn from(value: Absolute<crate::PathBuf>) -> Self {
        Absolute::from_inner_box_unchecked(value.path.into_boxed_path())
    }
}

impl<P: PartialEq + ?Sized> PartialEq<P> for Absolute<P> {
    #[inline]
    fn eq(&self, other: &P) -> bool {
        self.path == *other
    }
}

impl PartialEq<Absolute<std::path::Path>> for std::path::Path {
    #[inline]
    fn eq(&self, other: &Absolute<std::path::Path>) -> bool {
        *self == other.path
    }
}

impl PartialEq<std::path::Path> for Absolute<std::path::PathBuf> {
    #[inline]
    fn eq(&self, other: &std::path::Path) -> bool {
        self.path == *other
    }
}

impl PartialEq<Absolute<std::path::Path>> for std::path::PathBuf {
    #[inline]
    fn eq(&self, other: &Absolute<std::path::Path>) -> bool {
        *self == other.path
    }
}

impl PartialEq<Absolute<std::path::PathBuf>> for std::path::Path {
    #[inline]
    fn eq(&self, other: &Absolute<std::path::PathBuf>) -> bool {
        *self == other.path
    }
}

impl PartialEq<Absolute<std::path::PathBuf>> for std::path::PathBuf {
    #[inline]
    fn eq(&self, other: &Absolute<std::path::PathBuf>) -> bool {
        *self == other.path
    }
}

impl<P: PartialOrd + ?Sized> PartialOrd<P> for Absolute<P> {
    #[inline]
    fn partial_cmp(&self, other: &P) -> Option<std::cmp::Ordering> {
        self.path.partial_cmp(other)
    }
}

impl PartialEq<Absolute<crate::Path>> for crate::Path {
    #[inline]
    fn eq(&self, other: &Absolute<crate::Path>) -> bool {
        *self == other.path
    }
}

impl PartialEq<Absolute<crate::Path>> for crate::PathBuf {
    #[inline]
    fn eq(&self, other: &Absolute<crate::Path>) -> bool {
        *self == other.path
    }
}

impl PartialEq<Absolute<crate::PathBuf>> for crate::Path {
    #[inline]
    fn eq(&self, other: &Absolute<crate::PathBuf>) -> bool {
        *self == other.path
    }
}

impl PartialEq<Absolute<crate::PathBuf>> for crate::PathBuf {
    #[inline]
    fn eq(&self, other: &Absolute<crate::PathBuf>) -> bool {
        *self == other.path
    }
}

impl<P: std::fmt::Display + ?Sized> std::fmt::Display for Absolute<P> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.path.fmt(f)
    }
}

impl Absolute<crate::Path> {
    /// Build a filesystem path from an abstract relative to `root`.
    ///
    /// This does not access the filesystem.
    #[must_use]
    pub fn resolve(&self, root: &Absolute<std::path::Path>) -> Absolute<std::path::PathBuf> {
        if self.is_root() {
            return root.to_owned();
        }

        let mut buf = root.to_owned();

        for c in self.components() {
            match c {
                crate::Component::Root => {}
                crate::Component::Current | crate::Component::Parent => {
                    unreachable!("path is not normalized!")
                }
                crate::Component::Component(path) => buf.path.push(path.as_str()),
            }
        }

        buf
    }
}

impl Absolute<std::path::Path> {
    /// Get the workspace-relative path from an OS path by removing the prefix `root` from `self`.
    pub fn unresolve(
        &self,
        root: &Absolute<std::path::Path>,
    ) -> Result<Absolute<crate::PathBuf>, crate::PathError> {
        let Ok(tail) = self.path.strip_prefix(root) else {
            return Err(crate::PathError::UnresolveBeyondRoot);
        };

        let mut buf = crate::Path::ROOT.to_owned();

        for c in tail.components() {
            match c {
                std::path::Component::Prefix(_) | std::path::Component::RootDir => {
                    unreachable!("strip_prefix did not return a relative path")
                }
                std::path::Component::CurDir | std::path::Component::ParentDir => {
                    unreachable!("path is not normalized!")
                }
                std::path::Component::Normal(os_str) => {
                    buf.path.try_push(os_str)?;
                }
            }
        }

        Ok(buf)
    }
}

impl Absolute<std::path::PathBuf> {
    #[inline]
    pub fn current_dir() -> Result<Self, std::io::Error> {
        let current_dir = std::env::current_dir()?;
        debug_assert!(current_dir.is_absolute());
        debug_assert!(current_dir.is_normalized());
        Ok(Self::new_unchecked(current_dir))
    }
}

impl Deref for Absolute<crate::Path> {
    type Target = crate::Path;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl Deref for Absolute<crate::PathBuf> {
    type Target = Absolute<crate::Path>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        Absolute::new_ref_unchecked(&self.path)
    }
}

impl Deref for Absolute<std::path::Path> {
    type Target = std::path::Path;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl Deref for Absolute<std::path::PathBuf> {
    type Target = Absolute<std::path::Path>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        Absolute::new_ref_unchecked(&self.path)
    }
}

impl<P: ?Sized> AsRef<Absolute<P>> for Absolute<P> {
    fn as_ref(&self) -> &Absolute<P> {
        self
    }
}

impl AsRef<std::ffi::OsStr> for Absolute<std::path::Path> {
    #[inline]
    fn as_ref(&self) -> &std::ffi::OsStr {
        self.path.as_ref()
    }
}

impl AsRef<std::ffi::OsStr> for Absolute<std::path::PathBuf> {
    #[inline]
    fn as_ref(&self) -> &std::ffi::OsStr {
        self.path.as_ref()
    }
}

impl AsRef<std::path::Path> for Absolute<std::path::Path> {
    #[inline]
    fn as_ref(&self) -> &std::path::Path {
        &self.path
    }
}

impl AsRef<Absolute<std::path::Path>> for Absolute<std::path::PathBuf> {
    #[inline]
    fn as_ref(&self) -> &Absolute<std::path::Path> {
        self
    }
}

impl AsRef<std::path::Path> for Absolute<std::path::PathBuf> {
    #[inline]
    fn as_ref(&self) -> &std::path::Path {
        &self.path
    }
}

impl AsRef<Absolute<crate::Path>> for Absolute<crate::PathBuf> {
    #[inline]
    fn as_ref(&self) -> &Absolute<crate::Path> {
        self
    }
}

impl Absolute<crate::PathBuf> {
    #[inline]
    #[must_use]
    pub fn into_boxed_str(self) -> Box<str> {
        self.path.into_boxed_unsized().into_boxed_str()
    }
}

impl TryFrom<&str> for Absolute<crate::PathBuf> {
    type Error = crate::PathError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let path = crate::Path::new(value)?;
        path.normalize().map(std::borrow::Cow::into_owned)
    }
}
