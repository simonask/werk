use std::num::NonZero;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TaskId {
    /// Index plus one.
    id: NonZero<u32>,
}

impl TaskId {
    #[must_use]
    pub fn index(self) -> usize {
        self.id.get() as usize - 1
    }

    #[must_use]
    pub fn from_index(index: usize) -> Self {
        Self {
            id: NonZero::new((index + 1) as u32).unwrap(),
        }
    }
}
