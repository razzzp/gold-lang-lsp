use crate::utils::{Position, Range, IRange};

use super::GoldParserError;

pub fn prepend_msg_to_error<'a>(s: &str, mut error: GoldParserError<'a>) -> GoldParserError<'a>{
    error.msg.insert_str(0, s);
    return error;
}
