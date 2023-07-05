use super::MyError;

pub fn prepend_msg_to_error<'a>(s: &str, mut error: MyError<'a>) -> MyError<'a>{
    error.msg.insert_str(0, s);
    return error;
}