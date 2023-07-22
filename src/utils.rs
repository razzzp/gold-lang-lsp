
pub trait IRange {
    fn get_range(&self) -> Range;
}

#[derive(Debug,Clone, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position
}

#[derive(Debug,Clone,PartialEq, Default)]
pub struct Position {
    pub line: usize,
    pub character: usize
}