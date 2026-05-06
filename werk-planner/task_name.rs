use stringleton::Symbol;
use werk_fs::{Absolute, SymPath};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TaskName {
    Task(Symbol),
    // TODO: When recipes can build multiple files, this needs to change to some
    // ID that encapsulates the "recipe instance" rather than the path of a
    // single target.
    Build(Absolute<SymPath>),
}
