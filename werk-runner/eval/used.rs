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
    pub vars: SmallSet,
}

#[derive(Clone, Default, Debug)]
pub enum SmallSet {
    #[default]
    Zero,
    One(UsedVariable),
    // Using B-tree set because we're computing a lot of intersections.
    Set(BTreeSet<UsedVariable>),
}

impl SmallSet {
    #[must_use]
    pub const fn new() -> Self {
        Self::Zero
    }

    #[inline]
    pub fn merge(&mut self, other: &SmallSet) {
        match other {
            SmallSet::Zero => (),
            SmallSet::One(rhs) => match self {
                SmallSet::Zero => *self = SmallSet::One(*rhs),
                SmallSet::One(lhs) => {
                    *self = SmallSet::Set(BTreeSet::from_iter([*lhs, *rhs]));
                }
                SmallSet::Set(lhs) => {
                    lhs.insert(*rhs);
                }
            },
            SmallSet::Set(rhs) => {
                let mut set = rhs.clone();
                match self {
                    SmallSet::Zero => (),
                    SmallSet::One(lhs) => {
                        set.insert(*lhs);
                    }
                    SmallSet::Set(lhs) => set.extend(lhs.iter().copied()),
                }
                *self = SmallSet::Set(set);
            }
        }
    }

    #[inline]
    pub fn merge_by_value(&mut self, other: SmallSet) {
        match other {
            SmallSet::Zero => (),
            SmallSet::One(rhs) => match self {
                SmallSet::Zero => *self = SmallSet::One(rhs),
                SmallSet::One(lhs) => {
                    *self = SmallSet::Set(BTreeSet::from_iter([*lhs, rhs]));
                }
                SmallSet::Set(lhs) => {
                    lhs.insert(rhs);
                }
            },
            SmallSet::Set(mut rhs) => match self {
                SmallSet::Zero => {
                    *self = SmallSet::Set(rhs);
                }
                SmallSet::One(lhs) => {
                    rhs.insert(*lhs);
                    *self = SmallSet::Set(rhs);
                }
                SmallSet::Set(lhs) => {
                    lhs.extend(rhs.iter().copied());
                }
            },
        }
    }

    #[inline]
    pub fn insert(&mut self, var: UsedVariable) {
        match self {
            SmallSet::Zero => *self = SmallSet::One(var),
            SmallSet::One(one) => {
                *self = SmallSet::Set(BTreeSet::from_iter([*one, var]));
            }
            SmallSet::Set(set) => {
                set.insert(var);
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        matches!(self, SmallSet::Zero)
    }

    #[inline]
    #[must_use]
    pub fn iter(&self) -> SmallSetIter<'_> {
        match self {
            SmallSet::Zero => SmallSetIter::Zero,
            SmallSet::One(used_variable) => SmallSetIter::One(Some(*used_variable)),
            SmallSet::Set(btree_set) => SmallSetIter::Set(btree_set.iter().copied()),
        }
    }
}

pub enum SmallSetIter<'a> {
    Zero,
    One(Option<UsedVariable>),
    Set(std::iter::Copied<std::collections::btree_set::Iter<'a, UsedVariable>>),
}

impl Iterator for SmallSetIter<'_> {
    type Item = UsedVariable;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SmallSetIter::Zero => None,
            SmallSetIter::One(used_variable) => used_variable.take(),
            SmallSetIter::Set(iter) => iter.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl ExactSizeIterator for SmallSetIter<'_> {
    #[inline]
    fn len(&self) -> usize {
        match self {
            SmallSetIter::Zero => 0,
            SmallSetIter::One(used_variable) => used_variable.is_some().into(),
            SmallSetIter::Set(iter) => iter.len(),
        }
    }
}

pub enum SmallSetIntoIter {
    Zero,
    One(Option<UsedVariable>),
    Set(std::collections::btree_set::IntoIter<UsedVariable>),
}

impl Iterator for SmallSetIntoIter {
    type Item = UsedVariable;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SmallSetIntoIter::Zero => None,
            SmallSetIntoIter::One(used_variable) => used_variable.take(),
            SmallSetIntoIter::Set(iter) => iter.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl ExactSizeIterator for SmallSetIntoIter {
    #[inline]
    fn len(&self) -> usize {
        match self {
            SmallSetIntoIter::Zero => 0,
            SmallSetIntoIter::One(used_variable) => used_variable.is_some().into(),
            SmallSetIntoIter::Set(iter) => iter.len(),
        }
    }
}

impl<'a> IntoIterator for &'a SmallSet {
    type IntoIter = SmallSetIter<'a>;
    type Item = UsedVariable;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl IntoIterator for SmallSet {
    type IntoIter = SmallSetIntoIter;
    type Item = UsedVariable;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            SmallSet::Zero => SmallSetIntoIter::Zero,
            SmallSet::One(used_variable) => SmallSetIntoIter::One(Some(used_variable)),
            SmallSet::Set(btree_set) => SmallSetIntoIter::Set(btree_set.into_iter()),
        }
    }
}

impl Extend<UsedVariable> for SmallSet {
    fn extend<T: IntoIterator<Item = UsedVariable>>(&mut self, iter: T) {
        if let SmallSet::Set(set) = self {
            set.extend(iter);
        } else {
            for var in iter {
                self.insert(var);
            }
        }
    }
}

impl FromIterator<UsedVariable> for SmallSet {
    fn from_iter<T: IntoIterator<Item = UsedVariable>>(iter: T) -> Self {
        let mut set = Self::default();
        set.extend(iter);
        set
    }
}

impl Used {
    pub const NONE: Self = Self::none();

    #[inline]
    #[must_use]
    pub const fn none() -> Self {
        Self {
            vars: SmallSet::Zero,
        }
    }

    #[inline]
    pub fn insert(&mut self, var: UsedVariable) {
        self.vars.insert(var);
    }

    #[inline]
    #[must_use]
    pub fn iter(&self) -> SmallSetIter<'_> {
        self.vars.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UsedVariable {
    Glob(Symbol, Hash128),
    Which(Symbol, Hash128),
    Env(Symbol, Hash128),
    Define(Symbol, Hash128),
    /// Used a global variable. The hash is the hash of the expression AST (not
    /// the value itself).
    Global(Symbol, Hash128),
    WorkspaceFile(Absolute<SymPath>, std::time::SystemTime),
}

impl FromIterator<UsedVariable> for Used {
    fn from_iter<I: IntoIterator<Item = UsedVariable>>(iter: I) -> Self {
        Used {
            vars: SmallSet::from_iter(iter),
        }
    }
}

impl<'a> IntoIterator for &'a Used {
    type IntoIter = SmallSetIter<'a>;
    type Item = UsedVariable;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl IntoIterator for Used {
    type IntoIter = SmallSetIntoIter;
    type Item = UsedVariable;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.vars.into_iter()
    }
}

impl BitOrAssign for Used {
    fn bitor_assign(&mut self, rhs: Self) {
        self.vars.merge_by_value(rhs.vars);
    }
}

impl BitOrAssign<&Used> for Used {
    fn bitor_assign(&mut self, rhs: &Used) {
        self.vars.merge(&rhs.vars);
    }
}

impl BitOr for Used {
    type Output = Used;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        self.vars.merge_by_value(rhs.vars);
        self
    }
}

impl BitOr<&Used> for Used {
    type Output = Used;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        self.vars.merge(&rhs.vars);
        self
    }
}
