#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    fragments: Vec<PatternFragment>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatternFragment {
    Literal(String),
    Placeholder(String),
    OneOf(Vec<String>),
}
