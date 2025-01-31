use std::{
    collections::BTreeSet,
    ops::{BitOr, BitOrAssign},
};

use werk_fs::{Absolute, SymPath};
use werk_util::Symbol;

use crate::cache::Hash128;

/// The external variables used when evaluating an expression.
#[derive(Debug, Clone, Default)]
pub struct Used {
    // TODO: Use some kind of interning to avoid all these string allocations
    // and comparisons.
    pub vars: BTreeSet<UsedVariable>,
}

impl Used {
    pub const NONE: Self = Self {
        vars: BTreeSet::new(),
    };

    #[inline]
    #[must_use]
    pub const fn none() -> Self {
        Self {
            vars: BTreeSet::new(),
        }
    }

    #[inline]
    pub fn insert(&mut self, var: UsedVariable) {
        self.vars.insert(var);
    }

    #[inline]
    pub fn iter(&self) -> std::collections::btree_set::Iter<'_, UsedVariable> {
        self.vars.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UsedVariable {
    Glob(Symbol, Hash128),
    Which(Symbol, Hash128),
    Env(Symbol, Hash128),
    Define(Symbol, Hash128),
    /// Used a global variable. The hash is the hash of the expression (not the
    /// value itself).
    Global(Symbol, Hash128),
    WorkspaceFile(Absolute<SymPath>, std::time::SystemTime),
}

impl FromIterator<UsedVariable> for Used {
    fn from_iter<I: IntoIterator<Item = UsedVariable>>(iter: I) -> Self {
        Used {
            vars: BTreeSet::from_iter(iter),
        }
    }
}

impl<'a> IntoIterator for &'a Used {
    type IntoIter = std::collections::btree_set::Iter<'a, UsedVariable>;
    type Item = &'a UsedVariable;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl IntoIterator for Used {
    type IntoIter = std::collections::btree_set::IntoIter<UsedVariable>;
    type Item = UsedVariable;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.vars.into_iter()
    }
}

impl BitOrAssign for Used {
    fn bitor_assign(&mut self, rhs: Self) {
        if self.vars.is_empty() {
            self.vars = rhs.vars;
            return;
        }

        self.vars.extend(rhs.vars);
    }
}

impl BitOrAssign<&Used> for Used {
    fn bitor_assign(&mut self, rhs: &Used) {
        if self.vars.is_empty() {
            self.vars.clone_from(&rhs.vars);
            return;
        }

        self.vars.extend(rhs.vars.iter().cloned());
    }
}

impl BitOr for Used {
    type Output = Used;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        if self.vars.is_empty() {
            return rhs;
        }

        self |= rhs;
        self
    }
}

impl BitOr<&Used> for Used {
    type Output = Used;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        if self.vars.is_empty() {
            return rhs.clone();
        }

        self |= rhs;
        self
    }
}
