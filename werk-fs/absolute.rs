use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
};

pub trait IsAbsolute {
    fn is_absolute(&self) -> bool;
    #[inline]
    fn is_relative(&self) -> bool {
        !self.is_absolute()
    }
}

pub trait Absolutize: IsAbsolute {
    type Err;

    fn absolutize<'a>(
        &'a self,
        base: &Absolute<Self>,
    ) -> Result<Cow<'a, Absolute<Self>>, Self::Err>
    where
        Absolute<Self>: ToOwned;
}

impl IsAbsolute for std::path::Path {
    #[inline]
    fn is_absolute(&self) -> bool {
        std::path::Path::is_absolute(self)
    }

    #[inline]
    fn is_relative(&self) -> bool {
        std::path::Path::is_relative(self)
    }
}

impl Absolutize for std::path::Path {
    type Err = std::io::Error;

    #[inline]
    fn absolutize<'a>(
        &'a self,
        base: &Absolute<Self>,
    ) -> Result<Cow<'a, Absolute<Self>>, Self::Err> {
        if self.is_absolute() {
            Ok(Cow::Borrowed(Absolute::new_ref_unchecked(self)))
        } else {
            let joined = base.join(self);
            Ok(Cow::Owned(Absolute::new_unchecked(std::path::absolute(
                joined,
            )?)))
        }
    }
}

pub trait IntoBoxedPath {
    type Unsized: ?Sized;
    fn into_boxed_path(self) -> Box<Self::Unsized>;
}

impl IntoBoxedPath for std::path::PathBuf {
    type Unsized = std::path::Path;

    #[inline]
    fn into_boxed_path(self) -> Box<Self::Unsized> {
        self.into_boxed_path()
    }
}

impl IntoBoxedPath for crate::PathBuf {
    type Unsized = crate::Path;

    #[inline]
    fn into_boxed_path(self) -> Box<Self::Unsized> {
        self.into_boxed_path()
    }
}

pub trait IntoBoxedStr {
    fn into_boxed_str(self: Box<Self>) -> Box<str>;
}

impl IntoBoxedStr for crate::Path {
    #[inline]
    fn into_boxed_str(self: Box<Self>) -> Box<str> {
        crate::Path::into_boxed_str(self)
    }
}

/// Toll-free wrapper marking that a path is absolute.
///
/// Works with both `werk_fs` paths and `std` paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Absolute<P: ?Sized> {
    path: P,
}

impl<P: Sized> Absolute<P> {
    pub fn try_new(path: P) -> Result<Self, P>
    where
        P: IsAbsolute,
    {
        if path.is_absolute() {
            Ok(Absolute::new_unchecked(path))
        } else {
            Err(path)
        }
    }

    pub fn into_inner(self) -> P {
        self.path
    }

    pub fn into_boxed_path(self) -> Box<Absolute<P::Unsized>>
    where
        P: IntoBoxedPath,
    {
        unsafe {
            // SAFETY: #[repr(transparent)]
            let boxed = self.path.into_boxed_path();
            Box::from_raw(Box::into_raw(boxed) as *mut _)
        }
    }
}

impl<P: ?Sized> Absolute<P> {
    pub fn into_boxed_str(self: Box<Self>) -> Box<str>
    where
        P: IntoBoxedStr,
    {
        self.into_inner_box().into_boxed_str()
    }

    pub fn try_new_ref(path: &P) -> Option<&Absolute<P>>
    where
        P: IsAbsolute,
    {
        if path.is_absolute() {
            Some(Absolute::new_ref_unchecked(path))
        } else {
            None
        }
    }

    pub const fn new_ref_unchecked(path: &P) -> &Self {
        unsafe {
            // SAFETY: #[repr(transparent)]
            &*(path as *const P as *const Absolute<P>)
        }
    }

    pub const fn new_unchecked(path: P) -> Self
    where
        P: Sized,
    {
        Absolute { path }
    }

    pub fn to_owned(&self) -> Absolute<P::Owned>
    where
        P: ToOwned,
    {
        Absolute::new_unchecked(self.path.to_owned())
    }

    pub fn borrow<T: ?Sized>(&self) -> &Absolute<T>
    where
        P: Borrow<T>,
    {
        Absolute::new_ref_unchecked(self.path.borrow())
    }

    pub fn to_path_buf(&self) -> Absolute<P::Owned>
    where
        Absolute<P>: ToOwned<Owned = Absolute<P::Owned>>,
        P: ToOwned,
    {
        self.to_owned()
    }

    pub fn as_path(&self) -> &Absolute<P::Target>
    where
        P: Deref,
    {
        self.as_deref()
    }

    pub fn as_deref(&self) -> &Absolute<P::Target>
    where
        P: Deref,
    {
        Absolute::new_ref_unchecked(self.path.deref())
    }

    fn as_inner_box(self: &Box<Self>) -> &Box<P> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            &*(self as *const Box<Self> as *const Box<P>)
        }
    }

    fn into_inner_box(self: Box<Self>) -> Box<P> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            let raw = Box::from_raw(Box::into_raw(self) as *mut Absolute<P>);
            Box::from_raw(Box::into_raw(raw) as *mut P)
        }
    }

    fn from_inner_box_unchecked(this: Box<P>) -> Box<Self> {
        unsafe {
            // SAFETY: #[repr(transparent)]
            let raw = Box::from_raw(Box::into_raw(this) as *mut P);
            Box::from_raw(Box::into_raw(raw) as *mut Absolute<P>)
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

impl<P: ?Sized> Deref for Absolute<P> {
    type Target = P;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl<P: ?Sized> Borrow<P> for Absolute<P> {
    #[inline]
    fn borrow(&self) -> &P {
        &self.path
    }
}

impl<P: ?Sized> AsRef<P> for Absolute<P> {
    #[inline]
    fn as_ref(&self) -> &P {
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
        value.into_boxed_path()
    }
}

impl TryFrom<&str> for Absolute<crate::PathBuf> {
    type Error = crate::PathError;

    #[inline]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        crate::PathBuf::try_from(value)
            .and_then(|path| Absolute::try_new(path).map_err(|_| crate::PathError::NotAbsolute))
    }
}

impl AsRef<std::path::Path> for Absolute<std::path::PathBuf> {
    #[inline]
    fn as_ref(&self) -> &std::path::Path {
        self.path.as_ref()
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
