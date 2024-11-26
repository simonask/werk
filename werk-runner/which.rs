use ahash::HashMap;
use parking_lot::Mutex;

use crate::Error;

#[derive(Default)]
pub struct WhichCache {
    cache: Mutex<HashMap<String, Result<std::path::PathBuf, which::Error>>>,
}

impl WhichCache {
    pub fn which(&self, command: &str) -> Result<std::path::PathBuf, Error> {
        let mut lock = self.cache.lock();
        if let Some(result) = lock.get(command) {
            Ok(result.clone()?)
        } else {
            let result = which::which(command);
            lock.insert(command.to_owned(), result.clone());
            if let Ok(ref path) = result {
                tracing::info!("which `{command}`: {}", path.display());
            }
            Ok(result?)
        }
    }
}
