use super::ParserError;

pub fn prepend_msg_to_error<'a>(s: &str, mut error: ParserError<'a>) -> ParserError<'a>{
    error.msg.insert_str(0, s);
    return error;
}