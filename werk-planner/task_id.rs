#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TaskId {
    /// Index plus one.
    index: u32,
}

impl TaskId {
    #[must_use]
    pub fn index(self) -> usize {
        self.index as usize
    }

    #[must_use]
    pub fn from_index(index: usize) -> Self {
        Self {
            index: index as u32,
        }
    }
}
