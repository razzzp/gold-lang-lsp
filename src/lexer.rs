use std::{str::Chars, iter::{Peekable, Enumerate}};

#[derive(Debug, PartialEq, Eq)]
enum TokenType {
    // types
    Absolute,
    Abstract,
    Array,
    Char,
    Const,
    CString,
    Decimal,
    External,
    Final,
    Forward,

    // control
    If,
    EndIf,
    Begin,
    Break,
    Catch,
    Continue,
    DownTo,
    Else,
    ElseIf,
    End,
    For,
    ForEach,
    EndFor,
    Class,
    EndClass,
    Func,
    EndFunc,
    Proc,
    EndProc,
    EndLoop,
    EndRecord,
    EndSwitch,
    EndTry,
    EndWhen,
    EndWhile,
    Exit,
    Finally,

    // operators
    OBracket,
    CBracket,
    OSqrBracket,
    CSqrBracket,
    OCurBracket,
    CCurBracket,
    Plus,
    Minus,
    Equals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Multiply,
    Divide,
    Modulus,
    StringConcat,
    LeftShift,
    RightShift,
    NotEquals,
    Increment,
    IncrementAssign,
    Decrement,
    DecrementAssign,
    DeepAssign,
    TypeAssign,
    Dot,
    AddressOf,
    And,
    BAnd,
    BNot,
    BOr,
    BXor,
    In,

    // intrinsics
    MethodName,
    ModuleName,
    Move,
    Chr,
    Concat,
    Dispose,
    First,

    // oql
    AllVersionsOf,
    Descending,
    Distinct,
    From,
    Group,

    // others
    SingleQuote,
    DoubleQuotes,
    SemiColon,
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
            'a'..='z' | 'A'..='Z' | '_' => read_word(&mut chars),
            _ => read_symbol(&mut chars),
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

fn read_word(buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let mut word = String::new();
    let pos = buf.peek().unwrap().0;
    
    loop {
        let next = buf.peek();
        if next.is_none() {break;};

        match next.unwrap().1{
            'a'..='z' | 'A'..='Z' | '_' => {word.push(buf.next().unwrap().1)},
            _ => break
        }
    }
    return Some(create_word_token(pos, word));
}

fn create_word_token(pos: usize, word : String) -> Token {
    match word.to_uppercase().as_str() {
        "CLASS" => create_token(pos, TokenType::Class, Some(word)),
        "FUNC" | "FUNCTION" => create_token(pos, TokenType::Func, Some(word)),
        "ENDFUNC" => create_token(pos, TokenType::EndFunc, Some(word)),
        "PROC" | "PROCEDURE" => create_token(pos, TokenType::Proc, Some(word)),
        "ENDPROC" => create_token(pos, TokenType::EndProc, Some(word)),
        _ => create_token(pos, TokenType::Identifier, Some(word))
    }
}

fn read_symbol(buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let next = buf.next().unwrap();
    let pos = next.0;
    let token: Option<Token> = match next.1 {
        '(' => Some(create_token(pos, TokenType::OBracket, None)),
        ')' => Some(create_token(pos, TokenType::CBracket, None)),
        '[' => Some(create_token(pos, TokenType::OSqrBracket, None)),
        ']' => Some(create_token(pos, TokenType::CSqrBracket, None)),
        '{' => Some(create_token(pos, TokenType::OCurBracket, None)),
        '}' => Some(create_token(pos, TokenType::CCurBracket, None)),
        '*' => Some(create_token(pos, TokenType::Multiply, None)),
        '/' => Some(create_token(pos, TokenType::Divide, None)),
        '%' => Some(create_token(pos, TokenType::Modulus, None)),
        '@' => Some(create_token(pos, TokenType::AddressOf, None)),
        '.' => Some(create_token(pos, TokenType::Dot, None)),
        '=' => Some(create_token(pos, TokenType::Equals, None)),
        '<' => read_double_char_op(next.1, pos, buf),
        '>' => read_double_char_op(next.1, pos, buf),
        '+' => read_double_char_op(next.1, pos, buf),
        '-' => read_double_char_op(next.1, pos, buf),
        ':' => read_double_char_op(next.1, pos, buf),
        '&' => read_double_char_op(next.1, pos, buf),
        _=> None
    };
    return token;
}

fn read_double_char_op(first_op: char, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
    let next = buf.peek();

    let mut is_double_op = true;
    let mut result: Option<Token> = None;
    if  first_op == '<'{
        result = match next {
            Some((_,'<')) => Some(create_token(pos, TokenType::LeftShift, None)),
            Some((_,'=')) => Some(create_token(pos, TokenType::LessThanOrEqual, None)),
            Some((_,'>')) => Some(create_token(pos, TokenType::NotEquals, None)),
            _ => {is_double_op = false; Some(create_token(pos, TokenType::LessThan, None))}
        };
    } else if  first_op == '>'{
        result = match next {
            Some((_,'>')) => Some(create_token(pos, TokenType::RightShift, None)),
            Some((_,'=')) => Some(create_token(pos, TokenType::GreaterThanOrEqual, None)),
            _ => {is_double_op = false; Some(create_token(pos, TokenType::GreaterThan, None))}
        };
    } else if  first_op == '&'{
        result = match next {
            Some((_,'&')) => Some(create_token(pos, TokenType::StringConcat, None)),
            _ => {is_double_op = false; None}
        };
    } else if  first_op == '+'{
        result = match next{
            Some((_,'+')) => Some(create_token(pos, TokenType::Increment, None)),
            Some((_,'=')) => Some(create_token(pos, TokenType::IncrementAssign, None)),
            _ => {is_double_op = false; Some(create_token(pos, TokenType::Plus, None))}
        };
    } else if  first_op == '-'{
        result = match next {
            Some((_,'-')) => Some(create_token(pos, TokenType::Decrement, None)),
            Some((_,'=')) => Some(create_token(pos, TokenType::DecrementAssign, None)),
            _ => {is_double_op = false; Some(create_token(pos, TokenType::Minus, None))}
        };
    } else if  first_op == ':'{
        result = match next {
            Some((_,'=')) => Some(create_token(pos, TokenType::DeepAssign, None)),
            _ => {is_double_op = false; Some(create_token(pos, TokenType::TypeAssign, None))}
        };
    } else {
        is_double_op = false;
    }
    if is_double_op {buf.next();};

    return result;
}

fn create_token(pos: usize, token_type: TokenType, value: Option<String>) -> Token{
    return Token {
        pos,
        token_type,
        value 
    };
}

#[cfg(test)]
mod test {
    use std::{iter::{Enumerate, Peekable}, str::Chars};

    use crate::lexer::{TokenType, lex, read_symbol};

    fn create_buffer(val : &String) -> Peekable<Enumerate<Chars>> {
        return val.chars().enumerate().peekable();
    }

    #[test]
    fn test_read_operator_single(){
        let input = String::from("+");
        let mut buf = create_buffer(&input);
        let result = read_symbol(&mut buf);
        let token = result.unwrap();

        assert_eq!(token.pos, 0);
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.value, None);
    }

    #[test]
    fn test_lex_operators(){
        let input = String::from(
            "* / % + - && << >> < <= > >=
            = <> @ . ++ += -- -= :=");
        let result = lex(&input);
        let token = result.unwrap();

        assert_eq!(token.len(), 21);
        assert_eq!(token[0].token_type, TokenType::Multiply);
        assert_eq!(token[1].token_type, TokenType::Divide);
        assert_eq!(token[2].token_type, TokenType::Modulus);
        assert_eq!(token[3].token_type, TokenType::Plus);
        assert_eq!(token[4].token_type, TokenType::Minus);
        assert_eq!(token[5].token_type, TokenType::StringConcat);
        assert_eq!(token[6].token_type, TokenType::LeftShift);
        assert_eq!(token[7].token_type, TokenType::RightShift);
        assert_eq!(token[8].token_type, TokenType::LessThan);
        assert_eq!(token[9].token_type, TokenType::LessThanOrEqual);
        assert_eq!(token[10].token_type, TokenType::GreaterThan);
        assert_eq!(token[11].token_type, TokenType::GreaterThanOrEqual);
        assert_eq!(token[12].token_type, TokenType::Equals);
        assert_eq!(token[13].token_type, TokenType::NotEquals);
        assert_eq!(token[14].token_type, TokenType::AddressOf);
        assert_eq!(token[15].token_type, TokenType::Dot);
        assert_eq!(token[16].token_type, TokenType::Increment);
        assert_eq!(token[17].token_type, TokenType::IncrementAssign);
        assert_eq!(token[18].token_type, TokenType::Decrement);
        assert_eq!(token[19].token_type, TokenType::DecrementAssign);
        assert_eq!(token[20].token_type, TokenType::DeepAssign);
    }
}


