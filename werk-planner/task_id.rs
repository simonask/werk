use std::num::NonZero;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TaskId {
    /// Index plus one.
    id: NonZero<u32>,
}

impl TaskId {
    pub fn index(self) -> usize {
        self.id.get() as usize - 1
    }

    pub fn from_index(index: usize) -> Self {
        Self {
            id: NonZero::new((index + 1) as u32).unwrap(),
        }
    }
}
