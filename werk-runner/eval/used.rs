use std::{
    collections::BTreeSet,
    ops::{BitOr, BitOrAssign},
};

use crate::cache::Hash128;

/// The external variables used when evaluating an expression.
#[derive(Debug, Clone, Default)]
pub struct Used {
    // TODO: Use some kind of interning to avoid all these string allocations
    // and comparisons.
    pub vars: BTreeSet<UsedVariable>,
}

impl Used {
    #[inline]
    pub fn none() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, var: UsedVariable) {
        self.vars.insert(var);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UsedVariable {
    Glob(String, Hash128),
    Which(String, Hash128),
    Env(String, Hash128),
    Define(String, Hash128),
}

impl FromIterator<UsedVariable> for Used {
    fn from_iter<I: IntoIterator<Item = UsedVariable>>(iter: I) -> Self {
        Used {
            vars: BTreeSet::from_iter(iter),
        }
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
            self.vars = rhs.vars.clone();
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
