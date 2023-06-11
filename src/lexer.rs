use std::{str::Chars, iter::{Peekable, Enumerate}};

#[derive(Debug)]
enum TokenType {
    // declarations
    Class,
    Func,
    EndFunc,
    Proc,
    EndProc,

    // control
    If,
    EndIf,

    // operators
    OBracket,
    CBracket,
    OSqrBracket,
    CSqrBracket,
    OCurBracket,
    CCurBracket,
    Plus,
    Hypen,
    Assign,
    Equality,
    LessThan,
    GreaterThan,

    // others
    Identifier
}

#[derive(Debug)]
pub struct Token {
    pos: usize,
    token_type: TokenType,
    value: Option<String>
}

pub fn lex(buf: &String) -> Result<Vec<Token>, &'static str> {
    let mut chars =buf.chars().enumerate().peekable();
    let mut result = Vec::<Token>::new();
    loop {
        let cur_char =  skip_whitespace(&mut chars);
        if cur_char.is_none() { break };

        let cur_token: Option<Token>; 
        cur_token = match cur_char.unwrap() {
            'a'..='z' | 'A'..='Z' | '_' => read_keyword_or_identifier(&mut chars),
            _ => read_operator(&mut chars),
        };
        if cur_token.is_some(){
            result.push(cur_token.unwrap());
        }
    }
    return Ok(result);
}

fn skip_whitespace(buf: &mut Peekable<Enumerate<Chars>>) -> Option<char> {
    let mut next_char : Option<char> = None;
    while next_char.is_none() {
        next_char = match buf.peek() {
            Some((_ , ' ' | '\n' | '\r')) => {buf.next(); None},
            Some((_, c))=> Some(*c),
            _=> {break;}
        }
    }
    return next_char;
}

fn read_keyword_or_identifier(buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let mut identifier_name = String::new();
    let identifier_pos = buf.peek().unwrap().0;
    
    loop {
        let next = buf.peek();
        if next.is_none() {break;};

        match next.unwrap().1{
            'a'..='z' | 'A'..='Z' | '_' => {identifier_name.push(buf.next().unwrap().1)},
            _ => break
        }
    }
    return Some(Token {
        pos: identifier_pos,
        token_type:TokenType::Identifier,
        value: Some(identifier_name)});
}

fn read_operator(buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let next = buf.next().unwrap();
    let pos = next.0;
    let token: Option<Token> = match next.1 {
        '(' => Some(create_token(pos, TokenType::OBracket, None)),
        ')' => Some(create_token(pos, TokenType::CBracket, None)),
        '[' => Some(create_token(pos, TokenType::OSqrBracket, None)),
        ']' => Some(create_token(pos, TokenType::CSqrBracket, None)),
        '{' => Some(create_token(pos, TokenType::OCurBracket, None)),
        '}' => Some(create_token(pos, TokenType::CCurBracket, None)),
        '<' => read_double_char_op(next.1, pos, buf),
        '>' => read_double_char_op(next.1, pos, buf),
        '+' => read_double_char_op(next.1, pos, buf),
        '-' => read_double_char_op(next.1, pos, buf),
        '=' => read_double_char_op(next.1, pos, buf),
        _=> None
    };
    return token;
}

fn read_double_char_op(first_op: char, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let next = buf.peek();
    if next.is_none() {return None};

    let mut is_double_op = true;
    let mut result: Option<Token> = None;
    if first_op == '='{
        result = match next.unwrap().1 {
            '=' => Some(create_token(pos, TokenType::Equality, None)),
            _ => {is_double_op = false; None}
        };
    } else {
        is_double_op = false;
    }
    if is_double_op {buf.next();};

    return result;
}

fn create_double_op_token(first_op: char, second_op: char, pos: usize) -> Option<Token>{
    if first_op == '=' && second_op == '=' {
        return Some(create_token(pos, TokenType::Equality, None));
    } else {
        return None;
    }
}

fn create_token(pos: usize, token_type: TokenType, value: Option<String>) -> Token{
    return Token {
        pos,
        token_type,
        value 
    };
}




