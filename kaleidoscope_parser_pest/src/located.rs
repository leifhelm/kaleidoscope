use std::ops::Range;

pub trait Located {
    fn new(range: Range<usize>) -> Self;
    fn position(&self) -> &Range<usize>;
}

#[derive(Debug, PartialEq)]
pub struct Position {
    range: Range<usize>,
}

impl Located for Position {
    fn new(range: Range<usize>) -> Self {
        Position { range }
    }

    fn position(&self) -> &Range<usize> {
        &self.range
    }
}

impl Located for () {
    fn new(_: Range<usize>) -> Self {}

    /// panics
    fn position(&self) -> &Range<usize> {
        unimplemented!()
    }
}

pub trait LocatedInput {
    fn position(&self) -> usize;
}
