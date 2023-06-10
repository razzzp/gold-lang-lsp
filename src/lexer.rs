use std::str::Chars;

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
    Equal,
    Equality,
    LessThan,
    GreaterThan,

    // others
    Identifier
}

pub struct Token {
    token_type: TokenType 
}

pub fn lex(buf: &String) -> Result<Vec<Token>, &'static str> {
    let mut chars = buf.chars();
    let mut result = Vec::<Token>::new();
    let mut cur_pos: usize = 0;
    let len = buf.len();
    let mut cur_char : char;
    loop {
        cur_pos = skip_whitespace(cur_pos, &mut chars);
        cur_char = match chars.nth(cur_pos) {
            Some(c) => c,
            _ => break
        };
        let cur_token = match cur_char {
            'a'..='z' | 'A'..='Z' | '_' => read_keyword_or_identifier(cur_pos, &mut chars).1
        };
        result.push(cur_token);
    }
    return Ok(result);
}

fn skip_whitespace(pos: usize, buf: &mut Chars) -> usize {
    let mut next_pos = pos;
    loop {
        match buf.nth(pos) {
            Some(' ' | '\n' | '\r') => next_pos += 1,
            _ => break
        }
    }
    return next_pos;
}

fn read_keyword_or_identifier(pos: usize, buf: &mut Chars) -> (usize, Token) {

}



