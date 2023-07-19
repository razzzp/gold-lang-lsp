use super::ParserError;

pub fn prepend_msg_to_error<'a>(s: &str, mut error: ParserError<'a>) -> ParserError<'a>{
    error.msg.insert_str(0, s);
    return error;
}

#[cfg(test)]
pub mod test_utils{
    use crate::ast::IAstNode;

    pub fn cast_and_unwrap<'a, T: 'static>(node: &'a Box<dyn IAstNode>) -> &'a T{
        return node.as_ref().as_any().downcast_ref::<T>().unwrap();
    }
}