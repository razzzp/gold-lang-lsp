

pub trait IRange {
    fn get_range(&self) -> Range;
}

#[derive(Debug,Clone,PartialEq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position
}
impl Range{
    pub fn as_lsp_type_range(&self) -> lsp_types::Range{
        lsp_types::Range {start: self.start.as_lsp_type_pos(), end: self.end.as_lsp_type_pos()}
    }
}

#[derive(Debug,Clone,PartialEq, Default)]
pub struct Position {
    pub line: usize,
    pub character: usize
}
impl Position{
    pub fn as_lsp_type_pos(&self) -> lsp_types::Position{
        lsp_types::Position { line: self.line as u32, character: self.character as u32 }
    }
}

pub fn get_start_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().start.clone();
}

pub fn get_end_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().end.clone();
}

pub fn create_new_range<T: IRange>(first_item: &T, second_item: &T) -> Range{
    return Range{
        start: first_item.get_range().start.clone(),
        end: second_item.get_range().end.clone()
    }
}

#[cfg(test)]
pub mod test_utils{
    use crate::ast::IAstNode;

    pub fn cast_and_unwrap<'a, T: 'static>(node: &'a Box<dyn IAstNode>) -> &'a T{
        return node.as_ref().as_any().downcast_ref::<T>().unwrap();
    }
}