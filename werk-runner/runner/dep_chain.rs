use werk_eval::TaskName;

#[derive(Debug, Clone, Copy)]
pub struct DepChainEntry<'a> {
    parent: DepChain<'a>,
    this: TaskName,
}

#[derive(Debug, Clone, Copy)]
pub enum DepChain<'a> {
    Empty,
    Owned(&'a OwnedDependencyChain),
    Ref(&'a DepChainEntry<'a>),
}

impl<'a> DepChain<'a> {
    fn collect_vec(&self) -> Vec<TaskName> {
        match self {
            DepChain::Empty => Vec::new(),
            DepChain::Owned(owned) => owned.vec.clone(),
            DepChain::Ref(parent) => parent.collect_vec(),
        }
    }

    #[must_use]
    pub fn contains(&self, task: TaskName) -> bool {
        match self {
            DepChain::Empty => false,
            DepChain::Owned(owned) => owned.vec.contains(&task),
            DepChain::Ref(parent) => parent.contains(task),
        }
    }

    #[must_use]
    pub fn push<'b>(self, task: TaskName) -> DepChainEntry<'b>
    where
        'a: 'b,
    {
        DepChainEntry {
            parent: self,
            this: task,
        }
    }
}

impl DepChainEntry<'_> {
    #[must_use]
    pub fn collect(&self) -> OwnedDependencyChain {
        OwnedDependencyChain {
            vec: self.collect_vec(),
        }
    }

    #[must_use]
    pub fn collect_vec(&self) -> Vec<TaskName> {
        let mut vec = self.parent.collect_vec();
        vec.push(self.this);
        vec
    }

    #[must_use]
    pub fn contains(&self, task_id: TaskName) -> bool {
        if self.this == task_id {
            true
        } else {
            self.parent.contains(task_id)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnedDependencyChain {
    vec: Vec<TaskName>,
}

impl OwnedDependencyChain {
    #[inline]
    #[must_use]
    pub fn into_inner(self) -> Vec<TaskName> {
        self.vec
    }
}

impl std::fmt::Display for OwnedDependencyChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, task_id) in self.vec.iter().enumerate() {
            if i > 0 {
                write!(f, " -> ")?;
            }
            write!(f, "{task_id}")?;
        }
        Ok(())
    }
}
