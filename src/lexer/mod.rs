use std::{str::Chars, iter::{Peekable, Enumerate}, ops::Add};

use self::tokens::{Token, TokenType, Position};

pub mod tokens;

pub struct Lexer {
    line_pos: Vec<usize>
}

impl Lexer{
    pub fn new() -> Lexer{
        return Lexer{line_pos:Vec::<usize>::new()};
    }

    pub fn lex(&mut self, buf: &String) -> Result<Vec<Token>, &'static str> {
        let mut chars =buf.chars().enumerate().peekable();
        let mut result = Vec::<Token>::new();
        loop {
            let cur_char =  self.skip_whitespace(&mut chars);
            if cur_char.is_none() { break };
    
            let cur_token: Option<Token>; 
            cur_token = match cur_char.unwrap() {
                'a'..='z' | 'A'..='Z' | '_' => self.read_word(&mut chars),
                _ => self.read_symbol(&mut chars),
            };
            if cur_token.is_some(){
                result.push(cur_token.unwrap());
            }
        }
        return Ok(result);
    }
    
    fn skip_whitespace(&mut self, buf: &mut Peekable<Enumerate<Chars>>) -> Option<char> {
        // skips all whitespace and peeks the next non-whitespace char
        let mut next_char : Option<char> = None;
        while next_char.is_none() {
            next_char = match buf.peek() {
                // if space discard and proceed
                Some((_ , ' ' )) => {buf.next(); None},
                Some((pos , '\n')) => {
                    // push newline pos to list
                    self.line_pos.push(*pos);
                    buf.next(); 
                    None
                },
                Some((pos , '\r')) => {
                    // push newline pos to list
                    self.line_pos.push(*pos);
                    buf.next();
                    // check next char \n, if it is discard that too
                    match buf.peek() {
                        Some((_, '\n')) => {buf.next();}
                        _ => (),
                    }
                    None
                },
                // not whitepace, break and return char
                Some((_, c))=> Some(*c),
                _=> {break;}
            }
        }
        return next_char;
    }
    
    fn read_word(&self, buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
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
        return Some(self.create_word_token(pos, word));
    }

    fn read_symbol(&mut self, buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
        let next = buf.next().unwrap();
        let pos = next.0;
        let token: Option<Token> = match next.1 {
            '(' => Some(self.create_token(pos, TokenType::OBracket, Some(next.1.to_string()))),
            ')' => Some(self.create_token(pos, TokenType::CBracket, Some(next.1.to_string()))),
            '[' => Some(self.create_token(pos, TokenType::OSqrBracket, Some(next.1.to_string()))),
            ']' => Some(self.create_token(pos, TokenType::CSqrBracket, Some(next.1.to_string()))),
            '{' => Some(self.create_token(pos, TokenType::OCurBracket, Some(next.1.to_string()))),
            '}' => Some(self.create_token(pos, TokenType::CCurBracket, Some(next.1.to_string()))),
            '*' => Some(self.create_token(pos, TokenType::Multiply, Some(next.1.to_string()))),
            '/' => Some(self.create_token(pos, TokenType::Divide, Some(next.1.to_string()))),
            '%' => Some(self.create_token(pos, TokenType::Modulus, Some(next.1.to_string()))),
            '@' => Some(self.create_token(pos, TokenType::AddressOf, Some(next.1.to_string()))),
            '.' => Some(self.create_token(pos, TokenType::Dot, Some(next.1.to_string()))),
            '=' => Some(self.create_token(pos, TokenType::Equals, Some(next.1.to_string()))),
            '\'' => Some(self.read_string_constant(pos, buf)),
            '\"' => Some(self.read_string_constant_doublequotes(pos, buf)),
            ',' => Some(self.create_token(pos, TokenType::Comma, Some(next.1.to_string()))),
            '<' => self.read_double_char_op(next.1, pos, buf),
            '>' => self.read_double_char_op(next.1, pos, buf),
            '+' => self.read_double_char_op(next.1, pos, buf),
            '-' => self.read_double_char_op(next.1, pos, buf),
            ':' => self.read_double_char_op(next.1, pos, buf),
            '&' => self.read_double_char_op(next.1, pos, buf),
            ';' => Some(self.read_until_newline(pos, buf)),
            _=> None
        };
        return token;
    }

    fn read_string_constant(&mut self, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token {
        let mut value = String::new();
        loop{
            match buf.next(){
                Some((_ , '\'')) => {
                    match buf.peek(){
                        // double '' means escaped quotes, append and skip second quote
                        Some((_, '\'')) => {buf.next();value.push('\'')},
                        _ => break
                    }
                },
                Some((_ , c)) => value.push(c),
                None => break
            }
        }
        return self.create_token(pos, TokenType::StringConstant, Some(value))
    }

    fn read_string_constant_doublequotes(&mut self, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token {
        let mut value = String::new();
        loop{
            match buf.next(){
                Some((_ , '"')) => break,
                Some((_ , c)) => value.push(c),
                None => break
            }
        }
        return self.create_token(pos, TokenType::StringConstant, Some(value))
    }
    
    fn read_until_newline(&mut self, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token{
        let mut comment = String::new();
        loop {
            match buf.peek() {
                Some((_ , '\n'|'\r')) => {
                    // let skip_whitespace handle it
                    break;
                },
                Some((_,_)) => comment.push(buf.next().unwrap().1),
                _ => break
            }
        }
        return self.create_token(pos, TokenType::Comment, Some(comment));
    }
    
    fn read_double_char_op(&self, first_op: char, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Option<Token> {
        let next = buf.peek();
    
        let mut is_double_op = true;
        let mut result: Option<Token> = None;
        if  first_op == '<'{
            result = match next {
                Some((_,'<')) => Some(self.create_token(pos, TokenType::LeftShift, Some("<<".to_string()))),
                Some((_,'=')) => Some(self.create_token(pos, TokenType::LessThanOrEqual, Some("<=".to_string()))),
                Some((_,'>')) => Some(self.create_token(pos, TokenType::NotEquals, Some("<>".to_string()))),
                _ => {is_double_op = false; Some(self.create_token(pos, TokenType::LessThan, Some("<".to_string())))}
            };
        } else if  first_op == '>'{
            result = match next {
                Some((_,'>')) => Some(self.create_token(pos, TokenType::RightShift, Some(">>".to_string()))),
                Some((_,'=')) => Some(self.create_token(pos, TokenType::GreaterThanOrEqual, Some(">=".to_string()))),
                _ => {is_double_op = false; Some(self.create_token(pos, TokenType::GreaterThan, Some(">".to_string())))}
            };
        } else if  first_op == '&'{
            result = match next {
                Some((_,'&')) => Some(self.create_token(pos, TokenType::StringConcat, Some("&&".to_string()))),
                _ => {is_double_op = false; None}
            };
        } else if  first_op == '+'{
            result = match next{
                Some((_,'+')) => Some(self.create_token(pos, TokenType::Increment, Some("++".to_string()))),
                Some((_,'=')) => Some(self.create_token(pos, TokenType::IncrementAssign, Some("+=".to_string()))),
                _ => {is_double_op = false; Some(self.create_token(pos, TokenType::Plus, Some("+".to_string())))}
            };
        } else if  first_op == '-'{
            result = match next {
                Some((_,'-')) => Some(self.create_token(pos, TokenType::Decrement, Some("--".to_string()))),
                Some((_,'=')) => Some(self.create_token(pos, TokenType::DecrementAssign, Some("-=".to_string()))),
                _ => {is_double_op = false; Some(self.create_token(pos, TokenType::Minus, Some("-".to_string())))}
            };
        } else if  first_op == ':'{
            result = match next {
                Some((_,'=')) => Some(self.create_token(pos, TokenType::DeepAssign, Some(":=".to_string()))),
                _ => {is_double_op = false; Some(self.create_token(pos, TokenType::Colon, Some(":".to_string())))}
            };
        } else {
            is_double_op = false;
        }
        if is_double_op {buf.next();};
    
        return result;
    }

    fn create_word_token(&self, pos: usize, word : String) -> Token {
        match word.to_uppercase().as_str() {
            "CLASS" =>self.create_token(pos, TokenType::Class, Some(word)),
            "FUNC" | "FUNCTION" =>self.create_token(pos, TokenType::Func, Some(word)),
            "ENDFUNC" =>self.create_token(pos, TokenType::EndFunc, Some(word)),
            "PROC" | "PROCEDURE" =>self.create_token(pos, TokenType::Proc, Some(word)),
            "ENDPROC" =>self.create_token(pos, TokenType::EndProc, Some(word)),
            "ABSOLUTE" =>self.create_token(pos, TokenType::Absolute, Some(word)),
            "ABSTRACT" =>self.create_token(pos, TokenType::Abstract, Some(word)),
            "ARRAY" =>self.create_token(pos, TokenType::Array, Some(word)),
            "CHAR" =>self.create_token(pos, TokenType::Char, Some(word)),
            "CONST" =>self.create_token(pos, TokenType::Const, Some(word)),
            "CSTRING" =>self.create_token(pos, TokenType::CString, Some(word)),
            "TEXT" =>self.create_token(pos, TokenType::Text, Some(word)),
            "DECIMAL" =>self.create_token(pos, TokenType::Decimal, Some(word)),
            "EXTERNAL" =>self.create_token(pos, TokenType::External, Some(word)),
            "FINAL" =>self.create_token(pos, TokenType::Final, Some(word)),
            "FORWARD" =>self.create_token(pos, TokenType::Forward, Some(word)),
            "INOUT" =>self.create_token(pos, TokenType::InOut, Some(word)),
            "INT" =>self.create_token(pos, TokenType::Int, Some(word)),
            "INT1" =>self.create_token(pos, TokenType::Int1, Some(word)),
            "INT2" =>self.create_token(pos, TokenType::Int2, Some(word)),
            "INT4" =>self.create_token(pos, TokenType::Int4, Some(word)),
            "INT8" =>self.create_token(pos, TokenType::Int8, Some(word)),
            "BOOLEAN" =>self.create_token(pos, TokenType::Int8, Some(word)),
            "INVERSE" =>self.create_token(pos, TokenType::Inverse, Some(word)),
            "LISTOF" =>self.create_token(pos, TokenType::ListOf, Some(word)),
            "MEMORY" =>self.create_token(pos, TokenType::Memory, Some(word)),
            "NUM" =>self.create_token(pos, TokenType::Num, Some(word)),
            "NUM4" =>self.create_token(pos, TokenType::Num4, Some(word)),
            "NUM8" =>self.create_token(pos, TokenType::Num8, Some(word)),
            "NUM10" =>self.create_token(pos, TokenType::Num10, Some(word)),
            "OF" =>self.create_token(pos, TokenType::Of, Some(word)),
            "OVERRIDE" =>self.create_token(pos, TokenType::Override, Some(word)),
            "PRIVATE" =>self.create_token(pos, TokenType::Private, Some(word)),
            "PROTECTED" =>self.create_token(pos, TokenType::Protected, Some(word)),
            "RECORD" =>self.create_token(pos, TokenType::Record, Some(word)),
            "REFTO" =>self.create_token(pos, TokenType::RefTo, Some(word)),
            "REIMPLEM" =>self.create_token(pos, TokenType::ReImplem, Some(word)),
            "STRING" =>self.create_token(pos, TokenType::String, Some(word)),
            "TYPE" =>self.create_token(pos, TokenType::Type, Some(word)),
            "VAR" =>self.create_token(pos, TokenType::Var, Some(word)),
            "VERSIONED" =>self.create_token(pos, TokenType::Versioned, Some(word)),
            "IF" =>self.create_token(pos, TokenType::If, Some(word)),
            "ENDIF" =>self.create_token(pos, TokenType::EndIf, Some(word)),
            "BEGIN" =>self.create_token(pos, TokenType::Begin, Some(word)),
            "BREAK" =>self.create_token(pos, TokenType::Break, Some(word)),
            "CATCH" =>self.create_token(pos, TokenType::Catch, Some(word)),
            "CONTINUE" =>self.create_token(pos, TokenType::Continue, Some(word)),
            "DOWNTO" =>self.create_token(pos, TokenType::DownTo, Some(word)),
            "ELSE" =>self.create_token(pos, TokenType::Else, Some(word)),
            "ELSEIF" =>self.create_token(pos, TokenType::ElseIf, Some(word)),
            "END" =>self.create_token(pos, TokenType::End, Some(word)),
            "FOR" =>self.create_token(pos, TokenType::For, Some(word)),
            "FOREACH" =>self.create_token(pos, TokenType::ForEach, Some(word)),
            "ENDFOR" =>self.create_token(pos, TokenType::EndFor, Some(word)),
            "ENDCLASS" =>self.create_token(pos, TokenType::EndClass, Some(word)),
            "ENDLOOP" =>self.create_token(pos, TokenType::EndLoop, Some(word)),
            "ENDRECORD" =>self.create_token(pos, TokenType::EndRecord, Some(word)),
            "ENDSWITCH" =>self.create_token(pos, TokenType::EndSwitch, Some(word)),
            "ENDTRY" =>self.create_token(pos, TokenType::EndTry, Some(word)),
            "ENDWHEN" =>self.create_token(pos, TokenType::EndWhen, Some(word)),
            "ENDWHILE" =>self.create_token(pos, TokenType::EndWhile, Some(word)),
            "EXIT" =>self.create_token(pos, TokenType::Exit, Some(word)),
            "FINALLY" =>self.create_token(pos, TokenType::Finally, Some(word)),
            "LOOP" =>self.create_token(pos, TokenType::Loop, Some(word)),
            "NATIVERECORD" =>self.create_token(pos, TokenType::NativeRecord, Some(word)),
            "ENDNATIVERECORD" =>self.create_token(pos, TokenType::EndNativeRecord, Some(word)),
            "REPEAT" =>self.create_token(pos, TokenType::Repeat, Some(word)),
            "RETURN" =>self.create_token(pos, TokenType::Return, Some(word)),
            "STEP" =>self.create_token(pos, TokenType::Step, Some(word)),
            "SWITCH" =>self.create_token(pos, TokenType::Switch, Some(word)),
            "THROW" =>self.create_token(pos, TokenType::Throw, Some(word)),
            "TO" =>self.create_token(pos, TokenType::To, Some(word)),
            "TRY" =>self.create_token(pos, TokenType::Try, Some(word)),
            "UNTIL" =>self.create_token(pos, TokenType::Until, Some(word)),
            "WHEN" =>self.create_token(pos, TokenType::When, Some(word)),
            "WHILE" =>self.create_token(pos, TokenType::While, Some(word)),
            "AND" =>self.create_token(pos, TokenType::And, Some(word)),
            "BAND" =>self.create_token(pos, TokenType::BAnd, Some(word)),
            "BNOT" =>self.create_token(pos, TokenType::BNot, Some(word)),
            "BOR" =>self.create_token(pos, TokenType::BOr, Some(word)),
            "BXOR" =>self.create_token(pos, TokenType::BXor, Some(word)),
            "IN" =>self.create_token(pos, TokenType::In, Some(word)),
            "LIKE" =>self.create_token(pos, TokenType::Like, Some(word)),
            "NOT" =>self.create_token(pos, TokenType::Not, Some(word)),
            "OR" =>self.create_token(pos, TokenType::Or, Some(word)),
            "XOR" =>self.create_token(pos, TokenType::Xor, Some(word)),
            "_METHODNAME" =>self.create_token(pos, TokenType::MethodName, Some(word)),
            "_MODULENAME" =>self.create_token(pos, TokenType::ModuleName, Some(word)),
            "_MOVE" =>self.create_token(pos, TokenType::Move, Some(word)),
            "CHR" =>self.create_token(pos, TokenType::Chr, Some(word)),
            "CONCAT" =>self.create_token(pos, TokenType::Concat, Some(word)),
            "DISPOSE" =>self.create_token(pos, TokenType::Dispose, Some(word)),
            "FIRST" =>self.create_token(pos, TokenType::First, Some(word)),
            "LAST" =>self.create_token(pos, TokenType::Last, Some(word)),
            "INHERITED" =>self.create_token(pos, TokenType::Inherited, Some(word)),
            "INSTANCEOF" =>self.create_token(pos, TokenType::InstanceOf, Some(word)),
            "LENGTH" =>self.create_token(pos, TokenType::Length, Some(word)),
            "MEMBER" =>self.create_token(pos, TokenType::Member, Some(word)),
            "METAMODELENTITY" =>self.create_token(pos, TokenType::MetaModelEntity, Some(word)),
            "NEW" =>self.create_token(pos, TokenType::New, Some(word)),
            "NIL" =>self.create_token(pos, TokenType::Nil, Some(word)),
            "ORD" =>self.create_token(pos, TokenType::Ord, Some(word)),
            "PASS" =>self.create_token(pos, TokenType::Pass, Some(word)),
            "PRED" =>self.create_token(pos, TokenType::Pred, Some(word)),
            "SCENARIO" =>self.create_token(pos, TokenType::Scenario, Some(word)),
            "SIZEOF" =>self.create_token(pos, TokenType::SizeOf, Some(word)),
            "SUCC" =>self.create_token(pos, TokenType::Succ, Some(word)),
            "UPCASE" =>self.create_token(pos, TokenType::Upcase, Some(word)),
            "USES" =>self.create_token(pos, TokenType::Uses, Some(word)),
            "USING" =>self.create_token(pos, TokenType::Using, Some(word)),
            "WRITE" =>self.create_token(pos, TokenType::Write, Some(word)),
            "WRITELN" =>self.create_token(pos, TokenType::WriteLn, Some(word)),
            "ALLVERSIONSOF" =>self.create_token(pos, TokenType::AllVersionsOf, Some(word)),
            "DESCENDING" =>self.create_token(pos, TokenType::Descending, Some(word)),
            "DISTINCT" =>self.create_token(pos, TokenType::Distinct, Some(word)),
            "FROM" =>self.create_token(pos, TokenType::From, Some(word)),
            "OQL" =>self.create_token(pos, TokenType::OQL, Some(word)),
            "FETCH" =>self.create_token(pos, TokenType::Fetch, Some(word)),
            "SELECT" =>self.create_token(pos, TokenType::Select, Some(word)),
            "OQLCLASSID" =>self.create_token(pos, TokenType::OQLClassId, Some(word)),
            "OQLCOUNT" =>self.create_token(pos, TokenType::OQLCount, Some(word)),
            "OQLMAX" =>self.create_token(pos, TokenType::OQLMax, Some(word)),
            "OQLMIN" =>self.create_token(pos, TokenType::OQLMin, Some(word)),
            "OQLSUM" =>self.create_token(pos, TokenType::OQLSum, Some(word)),
            "OQLUPDATEDATE" =>self.create_token(pos, TokenType::OQLUpdateDate, Some(word)),
            "OQLUPDATETIME" =>self.create_token(pos, TokenType::OQLUpdateTime, Some(word)),
            "ORDER" =>self.create_token(pos, TokenType::Order, Some(word)),
            "BY" =>self.create_token(pos, TokenType::By, Some(word)),
            "PHANTOMSTOO" =>self.create_token(pos, TokenType::PhantomsToo, Some(word)),
            "WHERE" =>self.create_token(pos, TokenType::Where, Some(word)),
            "SELF" =>self.create_token(pos, TokenType::TSelf, Some(word)),
            "_RESULT" =>self.create_token(pos, TokenType::Result, Some(word)),
            "MODULE" =>self.create_token(pos, TokenType::Module, Some(word)),
            _ =>self.create_token(pos, TokenType::Identifier, Some(word))
        }
    }
    
    
    fn create_token(&self, pos: usize, token_type: TokenType, value: Option<String>) -> Token{
        // plus 1 for zero based offset
        let last_line_pos = if self.line_pos.last().is_none() {0} else {self.line_pos.last().unwrap().to_owned()+1};
        return Token {
            raw_pos: pos,
            pos: Position {
                line: self.line_pos.len(),
                character: pos - last_line_pos
            },
            token_type,
            value 
        };
    }
    
}




#[cfg(test)]
mod test {
    use std::{iter::{Enumerate, Peekable}, str::Chars, fs::File, io::Read};

    use crate::lexer::{TokenType, Lexer, tokens::Position};

    fn create_buffer(val : &String) -> Peekable<Enumerate<Chars>> {
        return val.chars().enumerate().peekable();
    }

    #[test]
    fn test_symbol_plus(){
        let mut lexer = Lexer::new();
        let input = String::from("+");
        let mut buf = create_buffer(&input);
        let result = lexer.read_symbol(&mut buf);
        let token = result.unwrap();

        assert_eq!(token.raw_pos, 0);
        assert_eq!(token.pos.line, 0);
        assert_eq!(token.pos.character, 0);
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.value.unwrap().as_str(), "+");
    }

    #[test]
    fn test_lex_symbols(){
        let mut lexer = Lexer::new();
        let input = String::from(
            "* / % + - && << >> < <= > >=
= <> @ . ++ += -- -= :=");
        let result = lexer.lex(&input);
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
        assert_eq!(token[12].raw_pos, 29);
        assert_eq!(token[12].pos.line, 1);
        assert_eq!(token[12].pos.character, 0);

        assert_eq!(token[13].token_type, TokenType::NotEquals);
        assert_eq!(token[14].token_type, TokenType::AddressOf);
        assert_eq!(token[15].token_type, TokenType::Dot);
        assert_eq!(token[16].token_type, TokenType::Increment);
        assert_eq!(token[17].token_type, TokenType::IncrementAssign);
        assert_eq!(token[18].token_type, TokenType::Decrement);
        assert_eq!(token[19].token_type, TokenType::DecrementAssign);
        assert_eq!(token[20].token_type, TokenType::DeepAssign);
    }

    #[test]
    fn test_words(){
        let mut lexer = Lexer::new();
        let  mut f = File::open("./test_inputs/words.txt").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        let tokens = lexer.lex(&file_contents).unwrap();
        // println!("{:#?}", tokens[60].token_type);
        assert_eq!(tokens.len(), 125);
        assert_eq!(tokens[0].token_type, TokenType::MethodName);
        assert_eq!(tokens[60].token_type, TokenType::Like);
        assert_eq!(tokens[124].token_type, TokenType::Module);
    }

    #[test]
    fn test_comment(){
        let mut lexer = Lexer::new();
        let input = String::from(
            "not a comment ; a comment\n
            ; entire line is a comment wow \n");
        let result = lexer.lex(&input);
        let token = result.unwrap();
        println!("{:#?}", token);
        assert_eq!(token.len(), 5);
        assert_eq!(token[0].token_type, TokenType::Not);
        assert_eq!(token[3].token_type, TokenType::Comment);
        assert_eq!(token[4].token_type, TokenType::Comment);
    }

    #[test]
    fn test_string_constants(){
        let mut lexer = Lexer::new();
        let input = String::from(
            "'first string constant'    'b'\n \"double quote of newline\"\n");
        let result = lexer.lex(&input);
        let token = result.unwrap();
        println!("{:#?}", token);
        assert_eq!(token.len(), 3);
        assert_eq!(token[0].token_type, TokenType::StringConstant);
        // first
        assert_eq!(token[0].raw_pos, 0);
        assert_eq!(token[0].pos, Position {line:0,character:0});
        assert_eq!(token[0].value.as_ref().unwrap().as_str(), "first string constant");
        // second
        assert_eq!(token[1].raw_pos, 27);
        assert_eq!(token[1].pos, Position {line:0,character:27});
        assert_eq!(token[1].value.as_ref().unwrap().as_str(), "b");
        // third
        assert_eq!(token[2].raw_pos, 32);
        assert_eq!(token[2].pos, Position {line:1,character:1});
        assert_eq!(token[2].value.as_ref().unwrap().as_str(), "double quote of newline");
    }
}


