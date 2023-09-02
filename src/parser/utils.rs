use crate::{
    ast::IAstNode,
    lexer::tokens::{Token, TokenType},
    utils::{IRange, Position, Range},
};

use super::{GoldParserError, GoldDocumentError};

pub fn prepend_msg_to_error<'a>(s: &str, mut error: GoldParserError<'a>) -> GoldParserError<'a> {
    error.msg.insert_str(0, s);
    return error;
}

pub fn parse_separated_list<'a, T: IAstNode + ?Sized>(
    input: &'a [Token],
    parser: impl Fn(&[Token]) -> Result<(&[Token], Box<T>), GoldParserError>,
    separator: TokenType,
) -> Result<(&'a [Token], Vec<Box<T>>), GoldParserError<'a>> {
    let mut identifiers = Vec::<Box<T>>::new();
    // match first identifier
    let r = _parse_seperated_list_recursive(input, &parser, &separator, &mut identifiers);
    match r {
        Ok(r) => return Ok((r, identifiers)),
        Err(e) => return Err(e),
    };
}

fn _parse_seperated_list_recursive<'a, 'b, T: IAstNode + ?Sized>(
    input: &'a [Token],
    parser: &'b impl Fn(&[Token]) -> Result<(&[Token], Box<T>), GoldParserError>,
    sep: &'b TokenType,
    result: &'b mut Vec<Box<T>>,
) -> Result<&'a [Token], GoldParserError<'a>> {
    // match first identifier
    let next = match parser(input) {
        Ok((r, n)) => {
            result.push(n);
            r
        }
        Err(e) => {
            return Err(GoldParserError {
                input: e.input,
                msg: e.msg,
            })
        }
    };
    let next = match exp_token(sep.clone())(next) {
        Ok((r, _)) => r,
        Err(e) => return Ok(e.input),
    };
    return _parse_seperated_list_recursive(next, parser, sep, result);
}

pub fn parse_separated_list_token<'a>(
    input: &'a [Token],
    item: TokenType,
    separator: TokenType,
) -> Result<(&'a [Token], Vec<Token>), GoldParserError<'a>> {
    let mut identifiers = Vec::<Token>::new();
    // match first identifier
    let r = _parse_seperated_list_token_recursive(input, &item, &separator, &mut identifiers);
    match r {
        Ok(r) => return Ok((r, identifiers)),
        Err(e) => return Err(e),
    };
}

fn _parse_seperated_list_token_recursive<'a, 'b>(
    input: &'a [Token],
    item: &'b TokenType,
    sep: &'b TokenType,
    result: &'b mut Vec<Token>,
) -> Result<&'a [Token], GoldParserError<'a>> {
    // match first identifier
    let mut next = match exp_token(item.clone())(input) {
        Ok((r, t)) => {
            result.push(t);
            r
        }
        Err(e) => {
            return Err(GoldParserError {
                input: e.input,
                msg: e.msg,
            })
        }
    };
    next = match exp_token(sep.clone())(next) {
        Ok((r, _)) => r,
        Err(e) => return Ok(e.input),
    };
    return _parse_seperated_list_token_recursive(next, item, sep, result);
}

/// Returns parser that expects the given token
pub fn exp_token(
    token_type: TokenType,
) -> impl Fn(&[Token]) -> Result<(&[Token], Token), GoldParserError> {
    move |input: &[Token]| -> Result<(&[Token], Token), GoldParserError> {
        let mut it = input.iter();
        loop{
            match it.next() {
                Some(t) if t.token_type == token_type => return Ok((it.as_slice(), t.clone())),
                Some(t) => {
                    if t.token_type == TokenType::Comment{
                        // ignore comments
                        continue;
                    }
                    return Err(GoldParserError {
                        input: input,
                        msg: String::from(format!(
                            "Unexpected {:?} token found",
                            t.token_type
                        )),
                    })
                },
                None => return Err(GoldParserError {
                    input: input,
                    msg: String::from(format!("Unexpected EOF")),
                }),
            }
        }
    }
}

/// Wraps the parser so that it doesn't throw error
pub fn opt_token(
    token_type: TokenType,
) -> impl Fn(&[Token]) -> Result<(&[Token], Option<Token>), GoldParserError> {
    move |input: &[Token]| -> Result<(&[Token], Option<Token>), GoldParserError> {
        let mut it = input.iter();
        match it.next() {
            Some(t) if t.token_type == token_type => Ok((it.as_slice(), Some(t.clone()))),
            _ => Ok((input, None)),
        }
    }
}

pub fn take_until(
    token_types: &[TokenType],
) -> impl Fn(&[Token]) -> Result<(&[Token], &[Token], Option<Token>), GoldParserError> + '_ {
    move |input: &[Token]| -> Result<(&[Token], &[Token], Option<Token>), GoldParserError> {
        let mut it = input.iter();
        let mut count: usize = 0;

        let mut next = it.next();
        let mut end_method_token: Option<Token> = None;
        while next.is_some() {
            let next_token = next.unwrap();
            count += 1;
            if token_types
                .iter()
                .any(|token_type| next_token.token_type == token_type.clone())
            {
                end_method_token = Some(next_token.clone());
                break;
            }
            next = it.next()
        }
        count = if count == 0 { count } else { count - 1 };
        return Ok((it.as_slice(), &input[0..count], end_method_token));
    }
}

/// Wraps the parser so that it doesn't throw error
pub fn opt_parse<T>(
    parser: impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError>,
) -> impl Fn(&[Token]) -> Result<(&[Token], Option<T>), GoldParserError> {
    move |input: &[Token]| -> Result<(&[Token], Option<T>), GoldParserError> {
        match parser(input) {
            Ok((r, n)) => return Ok((r, Some(n))),
            _ => (),
        };

        return Ok((input, None));
    }
}

/// Returns parser which parses with one of the provided parsers.
/// Parser returns the first successful parse.
/// If unable to parse, will return the error of the parser which was able
/// to parse the most
pub fn alt_parse<'a, T>(
    list_of_parsers: &'a [impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError>],
) -> impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError> + 'a {
    move |input: &[Token]| -> Result<(&[Token], T), GoldParserError> {
        let mut most_matched: Option<GoldParserError> = None;
        for parser in list_of_parsers {
            let r = parser(input);
            match r {
                Ok(r) => return Ok(r),
                Err(e) => {
                    // update most matched
                    if most_matched.is_some() {
                        if most_matched.as_ref().unwrap().input.len() > e.input.len() {
                            most_matched = Some(e);
                        }
                    } else {
                        most_matched = Some(e);
                    }
                }
            }
        }
        return Err(most_matched.unwrap());
    }
}

/// Returns parser which parses with the given sequence of parsers.
pub fn seq_parse<T>(
    list_of_parsers: &[impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError>],
) -> impl Fn(&[Token]) -> Result<(&[Token], Vec<T>), GoldParserError> + '_ {
    move |input: &[Token]| -> Result<(&[Token], Vec<T>), GoldParserError> {
        let mut i = 0;
        let mut next = input;
        let mut nodes = Vec::<T>::new();
        while i < list_of_parsers.len() {
            next = match list_of_parsers[i](next) {
                Ok(r) => {
                    nodes.push(r.1);
                    r.0
                }
                Err(e) => {
                    return Err(GoldParserError {
                        input: e.input,
                        msg: e.msg,
                    })
                }
            };
            i += 1;
        }
        return Ok((next, nodes));
    }
}

pub fn create_closure<T>(
    func: impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError>,
) -> impl Fn(&[Token]) -> Result<(&[Token], T), GoldParserError> {
    move |input: &[Token]| -> Result<(&[Token], T), GoldParserError> { func(input) }
}

/// parses using the parser until the stop parser matches
pub fn parse_until<'a, T: IAstNode + ?Sized>(
    input: &'a [Token],
    stop_parser: impl Fn(&[Token]) -> Result<(&[Token], Token), GoldParserError>,
    parser: impl Fn(&[Token]) -> Result<(&[Token], (Box<T>, Vec<GoldDocumentError>)), GoldParserError>,
) -> (
    &'a [Token],
    Vec<Box<T>>,
    Vec<GoldDocumentError>,
    Option<Token>,
) {
    let mut result: Vec<Box<T>> = Vec::new();
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {
            break;
        }
        // check if it sees a delimiting token
        next = match stop_parser(next) {
            Ok((next, t)) => {
                return (next, result, errors, Some(t));
            }
            Err(_) => {
                // parse statements and adds to current block
                let new_next = match parser(next) {
                    Ok((next, (new_statement_node, errs))) => {
                        result.push(new_statement_node);
                        errors.extend(errs.into_iter());
                        next
                    }
                    Err(e) => {
                        let error_at = match e.input.first() {
                            Some(t) => t.get_range(),
                            None => Default::default(),
                        };
                        errors.push(GoldDocumentError {
                            range: error_at,
                            msg: e.msg,
                        });
                        let mut new_it = e.input.iter();
                        // if iterator has not been moved, move by one, to prevent
                        //  infinite loop
                        if e.input.len() == next.len() {
                            new_it.next();
                        }
                        new_it.as_slice()
                    }
                };
                new_next
            }
        };
    }
    // end token not found
    return (next, result, errors, None);
}

/// parses using the parser until the stop parser matches
pub fn parse_until_strict<'a, T: IAstNode + ?Sized>(
    input: &'a [Token],
    stop_parser: impl Fn(&[Token]) -> Result<(&[Token], Token), GoldParserError>,
    parser: impl Fn(&[Token]) -> Result<(&[Token], Box<T>), GoldParserError>,
) -> Result<(
    &'a [Token],
    Vec<Box<T>>,
    Option<Token>,
), GoldParserError> {
    let mut result: Vec<Box<T>> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {
            break;
        }
        // check if it sees a delimiting token
        next = match stop_parser(next) {
            Ok((next, t)) => {
                return Ok((next, result, Some(t)));
            }
            Err(_) => {
                // parse statements and adds to current block
                let (new_next, new_node) = parser(next)?;
                result.push(new_node);
                new_next
            }
        };
    }
    // end token not found
    return Ok((next, result, None));
}

/// parses using the parser until it doesn't match
pub fn parse_until_no_match<'a, T: IAstNode + ?Sized>(
    input: &'a [Token],
    parser: impl Fn(&[Token]) -> Result<(&[Token], Box<T>, Vec<GoldDocumentError>), GoldParserError>,
) -> (&'a [Token], Vec<Box<T>>, Vec<GoldDocumentError>) {
    let mut result: Vec<Box<T>> = Vec::new();
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {
            break;
        }

        // parse statements and adds to current block
        next = match parser(next) {
            Ok((next, new_statement_node, errs)) => {
                result.push(new_statement_node);
                errors.extend(errs.into_iter());
                next
            }
            Err(e) => {
                // not matching anymore, return
                return (next, result, errors);
            }
        };
    }

    return (next, result, errors);
}

pub fn parse_repeat<'a>(
    input: &'a [Token],
    parser: impl Fn(&[Token],)
        -> Result<(&[Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>,
) -> (&'a [Token], Vec<Box<dyn IAstNode>>, Vec<GoldDocumentError>) {
    let mut result: Vec<Box<dyn IAstNode>> = Vec::new();
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {
            break;
        }
        // parse statements and adds to current block
        next = match parser(next) {
            Ok((next, (new_statement_node, errs))) => {
                result.push(new_statement_node);
                errors.extend(errs.into_iter());
                next
            }
            Err(e) => {
                let error_at = match e.input.first() {
                    Some(t) => t.get_range(),
                    None => Default::default(),
                };
                errors.push(GoldDocumentError {
                    range: error_at,
                    msg: e.msg,
                });
                let mut new_it = e.input.iter();
                // if iterator has not been moved, move by one, to prevent
                //  infinite loop
                if e.input.len() == next.len() {
                    new_it.next();
                }
                new_it.as_slice()
            }
        };
    }
    // end token not found
    return (next, result, errors);
}
