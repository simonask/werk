use std::fmt::Display;

use ahash::HashMap;
use parking_lot::Mutex;

#[derive(Clone, Debug, thiserror::Error)]
pub struct WhichError {
    pub program: String,
    pub error: which::Error,
}

impl Display for WhichError {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "which `{}`: {}", self.program, self.error)
    }
}

#[derive(Default)]
pub struct WhichCache {
    cache: Mutex<HashMap<String, Result<std::path::PathBuf, WhichError>>>,
}

impl WhichCache {
    pub fn which(&self, command: &str) -> Result<std::path::PathBuf, WhichError> {
        let mut lock = self.cache.lock();
        if let Some(result) = lock.get(command) {
            Ok(result.clone()?)
        } else {
            let result = which::which(command).map_err(|e| WhichError {
                program: command.to_owned(),
                error: e,
            });
            lock.insert(command.to_owned(), result.clone());
            if let Ok(ref path) = result {
                tracing::info!("which `{command}`: {}", path.display());
            }
            Ok(result?)
        }
    }
}
