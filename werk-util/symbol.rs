use std::{borrow::Borrow, num::NonZero};

use hashbrown::{hash_map::EntryRef, HashMap};
use parking_lot::{Mutex, MutexGuard};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(NonZero<u32>);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SymStr(&'static str);
impl From<&str> for SymStr {
    #[inline]
    fn from(s: &str) -> SymStr {
        Self(String::leak(s.to_string()))
    }
}
impl Borrow<str> for SymStr {
    #[inline]
    fn borrow(&self) -> &str {
        self.0
    }
}

struct Registry {
    by_name: HashMap<SymStr, Symbol, ahash::RandomState>,
    by_index: Vec<SymStr>,
}
static REGISTRY: Mutex<Registry> = Mutex::new(Registry {
    by_name: HashMap::with_hasher(ahash::RandomState::with_seeds(
        0xb944_112d_2c57_0bb2,
        0xa9e3_3d0c_afb0_6253,
        0x9038_b72c_8d04_99f6,
        0x3715_c24b_7e23_6bf4,
    )),
    by_index: Vec::new(),
});

impl Symbol {
    #[expect(clippy::cast_possible_truncation)]
    #[must_use]
    pub(crate) const unsafe fn from_index_unchecked(index: usize) -> Symbol {
        unsafe {
            let index = index.unchecked_add(1);
            let index = index as u32;
            Symbol(NonZero::new_unchecked(index))
        }
    }

    #[must_use]
    pub(crate) const fn from_index(index: usize) -> Symbol {
        const SYM_MAX: usize = u32::MAX as usize - 1;
        assert!(index <= SYM_MAX, "too many interned strings");
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    #[must_use]
    pub(crate) const fn index(self) -> usize {
        self.0.get() as usize - 1
    }

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
        SymbolRegistryLock::lock().get_str(self)
    }
}

impl std::fmt::Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::fmt::Display for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
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
        self.lock.by_name.get(string).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_str(&self, sym: Symbol) -> &'static str {
        let index = sym.index();
        self.lock.by_index[index].0
    }

    #[inline]
    #[must_use]
    pub fn insert(&mut self, string: &str) -> Symbol {
        let registry = &mut *self.lock;
        match registry.by_name.entry_ref(string) {
            EntryRef::Occupied(occupied_entry) => *occupied_entry.get(),
            EntryRef::Vacant(vacant_entry_ref) => {
                let index = registry.by_index.len();
                let symbol = Symbol::from_index(index);
                // String is leaked here via `SymStr::from`.
                let occupied = vacant_entry_ref.insert_entry(symbol);
                registry.by_index.push(*occupied.key());
                symbol
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
