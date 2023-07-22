use crate::utils::{Position, Range, IRange};

use super::GoldParserError;

pub fn prepend_msg_to_error<'a>(s: &str, mut error: GoldParserError<'a>) -> GoldParserError<'a>{
    error.msg.insert_str(0, s);
    return error;
}

pub fn get_start_pos<T>(item: &T) -> Position
where T: IRange {
    return item.get_range().start.clone();
}

pub fn get_end_pos<T>(item: &T) -> Position
where T: IRange {
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