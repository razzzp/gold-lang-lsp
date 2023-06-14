use std::{str::Chars, iter::{Peekable, Enumerate}};

use self::tokens::{Token, TokenType};

pub mod tokens;

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
        "ABSOLUTE" => create_token(pos, TokenType::Absolute, Some(word)),
        "ABSTRACT" => create_token(pos, TokenType::Abstract, Some(word)),
        "ARRAY" => create_token(pos, TokenType::Array, Some(word)),
        "CHAR" => create_token(pos, TokenType::Char, Some(word)),
        "CONST" => create_token(pos, TokenType::Const, Some(word)),
        "CSTRING" => create_token(pos, TokenType::CString, Some(word)),
        "DECIMAL" => create_token(pos, TokenType::Decimal, Some(word)),
        "EXTERNAL" => create_token(pos, TokenType::External, Some(word)),
        "FINAL" => create_token(pos, TokenType::Final, Some(word)),
        "FORWARD" => create_token(pos, TokenType::Forward, Some(word)),
        "INOUT" => create_token(pos, TokenType::InOut, Some(word)),
        "INT" => create_token(pos, TokenType::Int, Some(word)),
        "INVERSE" => create_token(pos, TokenType::Inverse, Some(word)),
        "LISTOF" => create_token(pos, TokenType::ListOf, Some(word)),
        "MEMORY" => create_token(pos, TokenType::Memory, Some(word)),
        "NUM" => create_token(pos, TokenType::Num, Some(word)),
        "OF" => create_token(pos, TokenType::Of, Some(word)),
        "OVERRIDE" => create_token(pos, TokenType::Override, Some(word)),
        "PRIVATE" => create_token(pos, TokenType::Private, Some(word)),
        "PROTECTED" => create_token(pos, TokenType::Protected, Some(word)),
        "RECORD" => create_token(pos, TokenType::Record, Some(word)),
        "REFTO" => create_token(pos, TokenType::RefTo, Some(word)),
        "REIMPLEM" => create_token(pos, TokenType::ReImplem, Some(word)),
        "STRING" => create_token(pos, TokenType::String, Some(word)),
        "TYPE" => create_token(pos, TokenType::Type, Some(word)),
        "VAR" => create_token(pos, TokenType::Var, Some(word)),
        "VERSIONED" => create_token(pos, TokenType::Versioned, Some(word)),
        "IF" => create_token(pos, TokenType::If, Some(word)),
        "ENDIF" => create_token(pos, TokenType::EndIf, Some(word)),
        "BEGIN" => create_token(pos, TokenType::Begin, Some(word)),
        "BREAK" => create_token(pos, TokenType::Break, Some(word)),
        "CATCH" => create_token(pos, TokenType::Catch, Some(word)),
        "CONTINUE" => create_token(pos, TokenType::Continue, Some(word)),
        "DOWNTO" => create_token(pos, TokenType::DownTo, Some(word)),
        "ELSE" => create_token(pos, TokenType::Else, Some(word)),
        "ELSEIF" => create_token(pos, TokenType::ElseIf, Some(word)),
        "END" => create_token(pos, TokenType::End, Some(word)),
        "FOR" => create_token(pos, TokenType::For, Some(word)),
        "FOREACH" => create_token(pos, TokenType::ForEach, Some(word)),
        "ENDFOR" => create_token(pos, TokenType::EndFor, Some(word)),
        "ENDCLASS" => create_token(pos, TokenType::EndClass, Some(word)),
        "ENDLOOP" => create_token(pos, TokenType::EndLoop, Some(word)),
        "ENDRECORD" => create_token(pos, TokenType::EndRecord, Some(word)),
        "ENDSWITCH" => create_token(pos, TokenType::EndSwitch, Some(word)),
        "ENDTRY" => create_token(pos, TokenType::EndTry, Some(word)),
        "ENDWHEN" => create_token(pos, TokenType::EndWhen, Some(word)),
        "ENDWHILE" => create_token(pos, TokenType::EndWhile, Some(word)),
        "EXIT" => create_token(pos, TokenType::Exit, Some(word)),
        "FINALLY" => create_token(pos, TokenType::Finally, Some(word)),
        "LOOP" => create_token(pos, TokenType::Loop, Some(word)),
        "NATIVERECORD" => create_token(pos, TokenType::NativeRecord, Some(word)),
        "ENDNATIVERECORD" => create_token(pos, TokenType::EndNativeRecord, Some(word)),
        "REPEAT" => create_token(pos, TokenType::Repeat, Some(word)),
        "RETURN" => create_token(pos, TokenType::Return, Some(word)),
        "STEP" => create_token(pos, TokenType::Step, Some(word)),
        "SWITCH" => create_token(pos, TokenType::Switch, Some(word)),
        "THROW" => create_token(pos, TokenType::Throw, Some(word)),
        "TO" => create_token(pos, TokenType::To, Some(word)),
        "TRY" => create_token(pos, TokenType::Try, Some(word)),
        "UNTIL" => create_token(pos, TokenType::Until, Some(word)),
        "WHEN" => create_token(pos, TokenType::When, Some(word)),
        "WHILE" => create_token(pos, TokenType::While, Some(word)),
        "AND" => create_token(pos, TokenType::And, Some(word)),
        "BAND" => create_token(pos, TokenType::BAnd, Some(word)),
        "BNOT" => create_token(pos, TokenType::BNot, Some(word)),
        "BOR" => create_token(pos, TokenType::BOr, Some(word)),
        "BXOR" => create_token(pos, TokenType::BXor, Some(word)),
        "IN" => create_token(pos, TokenType::In, Some(word)),
        "LIKE" => create_token(pos, TokenType::Like, Some(word)),
        "NOT" => create_token(pos, TokenType::Not, Some(word)),
        "OR" => create_token(pos, TokenType::Or, Some(word)),
        "XOR" => create_token(pos, TokenType::Xor, Some(word)),
        "_METHODNAME" => create_token(pos, TokenType::MethodName, Some(word)),
        "_MODULENAME" => create_token(pos, TokenType::ModuleName, Some(word)),
        "_MOVE" => create_token(pos, TokenType::Move, Some(word)),
        "CHR" => create_token(pos, TokenType::Chr, Some(word)),
        "CONCAT" => create_token(pos, TokenType::Concat, Some(word)),
        "DISPOSE" => create_token(pos, TokenType::Dispose, Some(word)),
        "FIRST" => create_token(pos, TokenType::First, Some(word)),
        "LAST" => create_token(pos, TokenType::Last, Some(word)),
        "INHERITED" => create_token(pos, TokenType::Inherited, Some(word)),
        "INSTANCEOF" => create_token(pos, TokenType::InstanceOf, Some(word)),
        "LENGTH" => create_token(pos, TokenType::Length, Some(word)),
        "MEMBER" => create_token(pos, TokenType::Member, Some(word)),
        "METAMODELENTITY" => create_token(pos, TokenType::MetaModelEntity, Some(word)),
        "NEW" => create_token(pos, TokenType::New, Some(word)),
        "NIL" => create_token(pos, TokenType::Nil, Some(word)),
        "ORD" => create_token(pos, TokenType::Ord, Some(word)),
        "PASS" => create_token(pos, TokenType::Pass, Some(word)),
        "PRED" => create_token(pos, TokenType::Pred, Some(word)),
        "SCENARIO" => create_token(pos, TokenType::Scenario, Some(word)),
        "SIZEOF" => create_token(pos, TokenType::SizeOf, Some(word)),
        "SUCC" => create_token(pos, TokenType::Succ, Some(word)),
        "UPCASE" => create_token(pos, TokenType::Upcase, Some(word)),
        "USES" => create_token(pos, TokenType::Uses, Some(word)),
        "USING" => create_token(pos, TokenType::Using, Some(word)),
        "WRITE" => create_token(pos, TokenType::Write, Some(word)),
        "WRITELN" => create_token(pos, TokenType::WriteLn, Some(word)),
        "ALLVERSIONSOF" => create_token(pos, TokenType::AllVersionsOf, Some(word)),
        "DESCENDING" => create_token(pos, TokenType::Descending, Some(word)),
        "DISTINCT" => create_token(pos, TokenType::Distinct, Some(word)),
        "FROM" => create_token(pos, TokenType::From, Some(word)),
        "OQL" => create_token(pos, TokenType::OQL, Some(word)),
        "FETCH" => create_token(pos, TokenType::Fetch, Some(word)),
        "SELECT" => create_token(pos, TokenType::Select, Some(word)),
        "OQLCLASSID" => create_token(pos, TokenType::OQLClassId, Some(word)),
        "OQLCOUNT" => create_token(pos, TokenType::OQLCount, Some(word)),
        "OQLMAX" => create_token(pos, TokenType::OQLMax, Some(word)),
        "OQLMIN" => create_token(pos, TokenType::OQLMin, Some(word)),
        "OQLSUM" => create_token(pos, TokenType::OQLSum, Some(word)),
        "OQLUPDATEDATE" => create_token(pos, TokenType::OQLUpdateDate, Some(word)),
        "OQLUPDATETIME" => create_token(pos, TokenType::OQLUpdateTime, Some(word)),
        "ORDER" => create_token(pos, TokenType::Order, Some(word)),
        "BY" => create_token(pos, TokenType::By, Some(word)),
        "PHANTOMSTOO" => create_token(pos, TokenType::PhantomsToo, Some(word)),
        "WHERE" => create_token(pos, TokenType::Where, Some(word)),
        "SELF" => create_token(pos, TokenType::TSelf, Some(word)),
        "_RESULT" => create_token(pos, TokenType::Result, Some(word)),
        "MODULE" => create_token(pos, TokenType::Module, Some(word)),
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
        '\'' => Some(create_token(pos, TokenType::SingleQuote, None)),
        '\"' => Some(create_token(pos, TokenType::DoubleQuotes, None)),
        '<' => read_double_char_op(next.1, pos, buf),
        '>' => read_double_char_op(next.1, pos, buf),
        '+' => read_double_char_op(next.1, pos, buf),
        '-' => read_double_char_op(next.1, pos, buf),
        ':' => read_double_char_op(next.1, pos, buf),
        '&' => read_double_char_op(next.1, pos, buf),
        ';' => Some(read_until_newline(pos, buf)),
        _=> None
    };
    return token;
}

fn read_until_newline(pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token{
    let mut comment = String::new();
    loop {
        match buf.next() {
            Some((_,'\n' | '\r')) | None => break,
            Some((_,c)) => comment.push(c)
        }
    }
    return create_token(pos, TokenType::Comment, Some(comment));
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
    use std::{iter::{Enumerate, Peekable}, str::Chars, fs::File, io::Read};

    use crate::lexer::{TokenType, lex, read_symbol};

    fn create_buffer(val : &String) -> Peekable<Enumerate<Chars>> {
        return val.chars().enumerate().peekable();
    }

    #[test]
    fn test_symbol_plus(){
        let input = String::from("+");
        let mut buf = create_buffer(&input);
        let result = read_symbol(&mut buf);
        let token = result.unwrap();

        assert_eq!(token.pos, 0);
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.value, None);
    }

    #[test]
    fn test_lex_symbols(){
        let input = String::from(
            "* / % + - && << >> < <= > >=
            = <> @ . ++ += -- -= := \' \"");
        let result = lex(&input);
        let token = result.unwrap();

        assert_eq!(token.len(), 23);
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
        assert_eq!(token[21].token_type, TokenType::SingleQuote);
        assert_eq!(token[22].token_type, TokenType::DoubleQuotes);
    }

    #[test]
    fn test_words(){
        let  mut f = File::open("./test_inputs/words.txt").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        let tokens = lex(&file_contents).unwrap();
        // println!("{:#?}", tokens[60].token_type);
        assert_eq!(tokens.len(), 125);
        assert_eq!(tokens[0].token_type, TokenType::MethodName);
        assert_eq!(tokens[60].token_type, TokenType::Like);
        assert_eq!(tokens[124].token_type, TokenType::Module);
    }

    #[test]
    fn test_comment(){
        let input = String::from(
            "not a comment ; a comment\n
            ; entire line is a comment wow \n");
        let result = lex(&input);
        let token = result.unwrap();
        println!("{:#?}", token);
        assert_eq!(token.len(), 5);
        assert_eq!(token[0].token_type, TokenType::Not);
        assert_eq!(token[3].token_type, TokenType::Comment);
        assert_eq!(token[4].token_type, TokenType::Comment);
    }
}


