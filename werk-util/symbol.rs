use std::borrow::Borrow;

use hashbrown::{HashMap, hash_map::EntryRef};
use parking_lot::{Mutex, MutexGuard};

#[derive(Clone, Copy, Eq)]
pub struct Symbol(&'static &'static str);

impl PartialEq for Symbol {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl std::hash::Hash for Symbol {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0.as_ptr(), state);
    }
}

impl PartialOrd for Symbol {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0.as_ptr() as usize).cmp(&(other.0.as_ptr() as usize))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SymStr(&'static &'static str);
impl From<&str> for SymStr {
    #[inline]
    fn from(s: &str) -> SymStr {
        let string: &'static str = s.to_string().leak();
        let leaked: &'static &'static str = Box::leak(Box::new(string));
        Self(leaked)
    }
}
impl Borrow<str> for SymStr {
    #[inline]
    fn borrow(&self) -> &str {
        self.0
    }
}

struct Registry {
    by_name: HashMap<SymStr, (), ahash::RandomState>,
}
static REGISTRY: Mutex<Registry> = Mutex::new(Registry {
    by_name: HashMap::with_hasher(ahash::RandomState::with_seeds(
        0xb944_112d_2c57_0bb2,
        0xa9e3_3d0c_afb0_6253,
        0x9038_b72c_8d04_99f6,
        0x3715_c24b_7e23_6bf4,
    )),
});

impl Symbol {
    #[inline]
    #[must_use]
    pub fn new(string: &str) -> Symbol {
        SymbolRegistryLock::lock().insert(string)
    }

    #[inline]
    #[must_use]
    pub fn get(string: &str) -> Option<Symbol> {
        SymbolRegistryLock::lock().get(string)
    }

    #[inline]
    #[must_use]
    pub fn as_str(self) -> &'static str {
        self.0
    }
}

impl std::fmt::Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

impl std::fmt::Display for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

impl AsRef<str> for Symbol {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl From<&str> for Symbol {
    #[inline]
    fn from(string: &str) -> Symbol {
        Symbol::new(string)
    }
}

impl PartialEq<str> for Symbol {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<Symbol> for str {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<&str> for Symbol {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<Symbol> for &str {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        *self == other.as_str()
    }
}

/// Bulk-registration of symbols.
pub struct SymbolRegistryLock {
    lock: MutexGuard<'static, Registry>,
}

impl SymbolRegistryLock {
    #[inline]
    #[must_use]
    pub fn lock() -> Self {
        Self {
            lock: REGISTRY.lock(),
        }
    }

    #[inline]
    #[must_use]
    pub fn get(&self, string: &str) -> Option<Symbol> {
        let (key, ()) = self.lock.by_name.get_key_value(string)?;
        Some(Symbol(key.0))
    }

    #[inline]
    #[must_use]
    pub fn get_str(&self, sym: Symbol) -> &'static str {
        sym.0
    }

    #[inline]
    #[must_use]
    pub fn insert(&mut self, string: &str) -> Symbol {
        let registry = &mut *self.lock;
        match registry.by_name.entry_ref(string) {
            EntryRef::Occupied(occupied_entry) => Symbol(occupied_entry.key().0),
            EntryRef::Vacant(vacant_entry_ref) => {
                // String is leaked here via `SymStr::from`.
                let occupied = vacant_entry_ref.insert_entry(());
                Symbol(occupied.key().0)
            }
        }
    }
}

impl serde::Serialize for Symbol {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Symbol {
    fn deserialize<D>(deserializer: D) -> Result<Symbol, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let string = <std::borrow::Cow<'de, str>>::deserialize(deserializer)?;
        Ok(Symbol::new(&string))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let i = 123;

        // Two different heap pointers.
        let a = format!("hello {i}");
        let b = format!("hello {i}");

        let a = Symbol::new(&a);
        let b = Symbol::new(&b);

        assert_eq!(a, b);
        assert_eq!(a.0.as_ptr(), b.0.as_ptr());
    }
}
