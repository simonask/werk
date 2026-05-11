use std::sync::Arc;

/// Clonable `std::io::Error`
#[derive(Clone)]
pub struct IoError {
    pub path: std::path::PathBuf,
    pub error: Arc<std::io::Error>,
}

impl IoError {
    pub fn new(path: impl AsRef<std::path::Path>, error: std::io::Error) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
            error: Arc::new(error),
        }
    }
}

impl std::fmt::Debug for IoError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.error, f)
    }
}

impl std::fmt::Display for IoError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.path.display(), self.error)
    }
}

impl PartialEq for IoError {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
            && (Arc::ptr_eq(&self.error, &other.error) || self.error.kind() == other.error.kind())
    }
}

impl std::error::Error for IoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.error.source()
    }
}
