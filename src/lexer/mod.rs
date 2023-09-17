
use std::{str::Chars, iter::{Peekable, Enumerate}};

use self::tokens::{Token, TokenType};
use crate::utils::{Position, Range};

pub mod tokens;

#[derive(Debug)]
pub struct GoldLexer {
    line_pos: Vec<usize>,
    errors : Vec<GoldLexerError>
}

#[derive(Debug, Clone)]
pub struct GoldLexerError {
    pub range: Range,
    pub msg: String,
}

impl GoldLexer{
    pub fn new() -> GoldLexer{
        return GoldLexer{
            line_pos:Vec::<usize>::new(),
            errors: Vec::<GoldLexerError>::new()
        };
    }

    pub fn lex(&mut self, buf: &String) -> (Vec<Token>, Vec<GoldLexerError>) {
        let mut chars =buf.chars().enumerate().peekable();
        let mut result = Vec::<Token>::new();
        loop {
            let cur_char =  self.skip_whitespace(&mut chars);
            if cur_char.is_none() { break };
    
            let cur_token = match cur_char.unwrap() {
                'a'..='z' | 'A'..='Z' | '_' => self.read_word(&mut chars),
                '0'..='9' => self.read_number(&mut chars),
                _ => self.read_symbol(&mut chars),
            };
            match cur_token{
                Ok(token) =>result.push(token),
                Err(error) => self.errors.push(error)
            }
        }
        return (result, self.errors.clone());
    }
    
    fn skip_whitespace(&mut self, buf: &mut Peekable<Enumerate<Chars>>) -> Option<char> {
        // skips all whitespace and peeks the next non-whitespace char
        let mut next_char : Option<char> = None;
        while next_char.is_none() {
            next_char = match buf.peek() {
                // if space or tab discard and proceed
                Some((_ , ' ' | '\t')) => {buf.next(); None},
                Some((pos , '\n')) => {
                    // push newline pos to list
                    self.line_pos.push(*pos);
                    buf.next(); 
                    None
                },
                Some((_ , '\r')) => {
                    // skip \r
                    buf.next();
                    // check next char \n, if it is discard that too
                    match buf.peek() {
                        Some((pos, '\n')) => {
                            self.line_pos.push(*pos);
                            buf.next();
                        }
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
    
    fn read_word(&self, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let mut word = String::new();
        let pos = buf.peek().unwrap().0;
        
        loop {
            let next = buf.peek();
            if next.is_none() {break;};
    
            match next.unwrap().1{
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {word.push(buf.next().unwrap().1)},
                _ => break
            }
        }
        return Ok(self.create_word_token(pos, word));
    }

    fn read_number(&self, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let mut number = String::new();
        let pos = buf.peek().unwrap().0;
        
        loop {
            let next = buf.peek();
            if next.is_none() {break;};
    
            match next.unwrap().1{
                // match until symbol or space, validate later
                '0'..='9' | '.' | 'a'..='z' | 'A'..='Z' => {
                    number.push(buf.next().unwrap().1)
                },
                _ => break
            }
        }
        return Ok(self.create_token(pos, TokenType::NumericLiteral, Some(number)));
    }

    fn read_symbol(&mut self, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let next = buf.next().unwrap();
        let pos = next.0;
        let token: Result<Token, GoldLexerError> = match next.1 {
            '(' => Ok(self.create_token(pos, TokenType::OBracket, Some(next.1.to_string()))),
            ')' => Ok(self.create_token(pos, TokenType::CBracket, Some(next.1.to_string()))),
            '[' => Ok(self.create_token(pos, TokenType::OSqrBracket, Some(next.1.to_string()))),
            ']' => Ok(self.create_token(pos, TokenType::CSqrBracket, Some(next.1.to_string()))),
            '{' => Ok(self.create_token(pos, TokenType::OCurBracket, Some(next.1.to_string()))),
            '}' => Ok(self.create_token(pos, TokenType::CCurBracket, Some(next.1.to_string()))),
            '*' => Ok(self.create_token(pos, TokenType::Asterisk, Some(next.1.to_string()))),
            '/' => Ok(self.create_token(pos, TokenType::Divide, Some(next.1.to_string()))),
            '%' => Ok(self.create_token(pos, TokenType::Modulus, Some(next.1.to_string()))),
            '@' => Ok(self.create_token(pos, TokenType::AddressOf, Some(next.1.to_string()))),
            '.' => Ok(self.create_token(pos, TokenType::Dot, Some(next.1.to_string()))),
            '=' => Ok(self.create_token(pos, TokenType::Equals, Some(next.1.to_string()))),
            '\'' => Ok(self.read_string_constant(pos, buf)),
            '\"' => Ok(self.read_string_constant_doublequotes(pos, buf)),
            ',' => Ok(self.create_token(pos, TokenType::Comma, Some(next.1.to_string()))),
            '<' => self.read_double_char_op(next.1, pos, buf),
            '>' => self.read_double_char_op(next.1, pos, buf),
            '+' => self.read_double_char_op(next.1, pos, buf),
            '-' => self.read_double_char_op(next.1, pos, buf),
            ':' => self.read_double_char_op(next.1, pos, buf),
            '&' => self.read_double_char_op(next.1, pos, buf),
            ';' => Ok(self.read_comment(pos, buf)),
            '#' => Ok(self.read_int_char_literal(pos, buf)),
            _=> Err(GoldLexerError { range: self.create_range(pos, 1), msg: format!("Unknown first symbol: {}", next.1) })
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
        return self.create_token(pos, TokenType::StringLiteral, Some(value))
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
        return self.create_token(pos, TokenType::StringLiteral, Some(value))
    }
    
    fn read_comment(&mut self, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token{
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
    
    fn read_double_char_op(&self, first_op: char, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let next = buf.peek();
    
        let mut is_double_op = true;
        let result: Result<Token, GoldLexerError>;
        if  first_op == '<'{
            result = match next {
                Some((_,'<')) => Ok(self.create_token(pos, TokenType::LeftShift, Some("<<".to_string()))),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::LessThanOrEqual, Some("<=".to_string()))),
                Some((_,'>')) => Ok(self.create_token(pos, TokenType::NotEquals, Some("<>".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::LessThan, Some("<".to_string())))}
            };
        } else if  first_op == '>'{
            result = match next {
                Some((_,'>')) => Ok(self.create_token(pos, TokenType::RightShift, Some(">>".to_string()))),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::GreaterThanOrEqual, Some(">=".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::GreaterThan, Some(">".to_string())))}
            };
        } else if  first_op == '&'{
            result = match next {
                Some((_,'&')) => Ok(self.create_token(pos, TokenType::StringConcat, Some("&&".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::StringConcat2, Some("&".to_string())))}
            };
        } else if  first_op == '+'{
            result = match next{
                Some((_,'+')) => Ok(self.create_token(pos, TokenType::Increment, Some("++".to_string()))),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::IncrementAssign, Some("+=".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Plus, Some("+".to_string())))}
            };
        } else if  first_op == '-'{
            result = match next {
                Some((_,'-')) => Ok(self.create_token(pos, TokenType::Decrement, Some("--".to_string()))),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::DecrementAssign, Some("-=".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Minus, Some("-".to_string())))}
            };
        } else if  first_op == ':'{
            result = match next {
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::DeepAssign, Some(":=".to_string()))),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Colon, Some(":".to_string())))}
            };
        } else {
            result = Err(GoldLexerError { range: self.create_range(pos, 1), msg: format!("Unknown first symbol: {}", first_op) });
        }
        if is_double_op {buf.next();};
    
        return result;
    }

    fn read_int_char_literal(&mut self, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Token{
        let mut int_char = String::new();
        int_char.push('#');
        loop {
            match buf.peek() {
                Some((_ , '0'..='1')) => {
                    int_char.push(buf.next().unwrap().1);
                },
                Some((_, ' '| '\n'| '\r'| '\t')) => {
                    break
                },
                _ => break,
            }
        }
        let token_type = if int_char.len() == 1 {TokenType::Pound} else {TokenType::StringLiteral};
        return self.create_token(pos, token_type, Some(int_char));
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
            // remove tokens not used crucial to grammar, to simplify parsing
            // "_METHODNAME" =>self.create_token(pos, TokenType::MethodName, Some(word)),
            // "_MODULENAME" =>self.create_token(pos, TokenType::ModuleName, Some(word)),
            // "_MOVE" =>self.create_token(pos, TokenType::Move, Some(word)),
            // "CHR" =>self.create_token(pos, TokenType::Chr, Some(word)),
            // "CONCAT" =>self.create_token(pos, TokenType::Concat, Some(word)),
            // "DISPOSE" =>self.create_token(pos, TokenType::Dispose, Some(word)),
            // "FIRST" =>self.create_token(pos, TokenType::First, Some(word)),
            // "LAST" =>self.create_token(pos, TokenType::Last, Some(word)),
            "INHERITED" =>self.create_token(pos, TokenType::Inherited, Some(word)),
            "INSTANCEOF" =>self.create_token(pos, TokenType::InstanceOf, Some(word)),
            // "LENGTH" =>self.create_token(pos, TokenType::Length, Some(word)),
            // "MEMBER" =>self.create_token(pos, TokenType::Member, Some(word)),
            // "METAMODELENTITY" =>self.create_token(pos, TokenType::MetaModelEntity, Some(word)),
            // "NEW" =>self.create_token(pos, TokenType::New, Some(word)),
            "NIL" =>self.create_token(pos, TokenType::Nil, Some(word)),
            // "ORD" =>self.create_token(pos, TokenType::Ord, Some(word)),
            // "PASS" =>self.create_token(pos, TokenType::Pass, Some(word)),
            // "PRED" =>self.create_token(pos, TokenType::Pred, Some(word)),
            // "SCENARIO" =>self.create_token(pos, TokenType::Scenario, Some(word)),
            // "SIZEOF" =>self.create_token(pos, TokenType::SizeOf, Some(word)),
            // "SUCC" =>self.create_token(pos, TokenType::Succ, Some(word)),
            // "UPCASE" =>self.create_token(pos, TokenType::Upcase, Some(word)),
            "USES" =>self.create_token(pos, TokenType::Uses, Some(word)),
            // "WRITE" =>self.create_token(pos, TokenType::Write, Some(word)),
            // "WRITELN" =>self.create_token(pos, TokenType::WriteLn, Some(word)),
            "OQL" =>self.create_token(pos, TokenType::OQL, Some(word)),
            "TOP" =>self.create_token(pos, TokenType::Using, Some(word)),
            "DISTINCT" =>self.create_token(pos, TokenType::Distinct, Some(word)),
            "FROM" =>self.create_token(pos, TokenType::From, Some(word)),
            "USING" =>self.create_token(pos, TokenType::Using, Some(word)),
            "WHERE" =>self.create_token(pos, TokenType::Where, Some(word)),
            "FETCH" =>self.create_token(pos, TokenType::Fetch, Some(word)),
            "INTO" =>self.create_token(pos, TokenType::Into, Some(word)),
            "DESCENDING" =>self.create_token(pos, TokenType::Descending, Some(word)),
            "CONDITIONAL" =>self.create_token(pos, TokenType::Conditional, Some(word)),
            "ALLVERSIONSOF" =>self.create_token(pos, TokenType::AllVersionsOf, Some(word)),
            "PHANTOMSTOO" =>self.create_token(pos, TokenType::PhantomsToo, Some(word)),
            "SELECT" =>self.create_token(pos, TokenType::Select, Some(word)),
            // "OQLCLASSID" =>self.create_token(pos, TokenType::OQLClassId, Some(word)),
            // "OQLCOUNT" =>self.create_token(pos, TokenType::OQLCount, Some(word)),
            // "OQLMAX" =>self.create_token(pos, TokenType::OQLMax, Some(word)),
            // "OQLMIN" =>self.create_token(pos, TokenType::OQLMin, Some(word)),
            // "OQLSUM" =>self.create_token(pos, TokenType::OQLSum, Some(word)),
            // "OQLUPDATEDATE" =>self.create_token(pos, TokenType::OQLUpdateDate, Some(word)),
            // "OQLUPDATETIME" =>self.create_token(pos, TokenType::OQLUpdateTime, Some(word)),
            "ORDER" =>self.create_token(pos, TokenType::Order, Some(word)),
            "BY" =>self.create_token(pos, TokenType::By, Some(word)),

            "SELF" =>self.create_token(pos, TokenType::TSelf, Some(word)),
            "_RESULT" =>self.create_token(pos, TokenType::Result, Some(word)),
            "MODULE" =>self.create_token(pos, TokenType::Module, Some(word)),
            "MULTILANG" => self.create_token(pos, TokenType::MultiLang, Some(word)),
            "TRUE" => self.create_token(pos, TokenType::BooleanTrue, Some(word)),
            "FALSE" => self.create_token(pos, TokenType::BooleanFalse, Some(word)),
            "SEQUENCE" => self.create_token(pos, TokenType::Sequence, Some(word)),
            _ =>self.create_token(pos, TokenType::Identifier, Some(word))
        }
    }
    
    
    fn create_token(&self, pos: usize, token_type: TokenType, value: Option<String>) -> Token{
        // plus 1 for zero based offset
        let range = self.create_range(pos, value.as_ref().unwrap_or(&"".to_string()).len());
        return Token {
            raw_pos: pos,
            pos: range.start.clone(),
            range: range,
            token_type,
            value 
        };
    }

    /*creates a range from a raw position and length */
    fn create_range(&self, raw_pos: usize, length: usize) -> Range{
        let last_line_pos = if self.line_pos.last().is_none() {0} else {self.line_pos.last().unwrap().to_owned()+1};
        let start_pos = Position {
            line: self.line_pos.len(),
            // minus one because char right after newline should be 0
            character: raw_pos - last_line_pos
        };
        let end_pos = Position {
            line: start_pos.line,
            character: start_pos.character + length
        };
        return Range { start: start_pos, end: end_pos}
    }
    
}




#[cfg(test)]
mod test {
    use std::{iter::{Enumerate, Peekable}, str::Chars, fs::File, io::Read};

    use crate::lexer::{TokenType, GoldLexer};
    use crate::utils::Position;

    fn create_buffer(val : &String) -> Peekable<Enumerate<Chars>> {
        return val.chars().enumerate().peekable();
    }

    #[test]
    fn test_symbol_plus(){
        let mut lexer = GoldLexer::new();
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
        let mut lexer = GoldLexer::new();
        let input = String::from(
            "* / % + - && << >> < <= > >=
= <> @ . ++ += -- -= := #100 & #");
        let (tokens, errors) = lexer.lex(&input);
        assert_eq!(errors.len(), 0);
        assert_eq!(tokens.len(), 24);
        assert_eq!(tokens[0].token_type, TokenType::Asterisk);
        assert_eq!(tokens[1].token_type, TokenType::Divide);
        assert_eq!(tokens[2].token_type, TokenType::Modulus);
        assert_eq!(tokens[3].token_type, TokenType::Plus);
        assert_eq!(tokens[4].token_type, TokenType::Minus);
        assert_eq!(tokens[5].token_type, TokenType::StringConcat);
        assert_eq!(tokens[6].token_type, TokenType::LeftShift);
        assert_eq!(tokens[7].token_type, TokenType::RightShift);
        assert_eq!(tokens[8].token_type, TokenType::LessThan);
        assert_eq!(tokens[9].token_type, TokenType::LessThanOrEqual);
        assert_eq!(tokens[10].token_type, TokenType::GreaterThan);
        assert_eq!(tokens[11].token_type, TokenType::GreaterThanOrEqual);

        assert_eq!(tokens[12].token_type, TokenType::Equals);
        assert_eq!(tokens[12].raw_pos, 29);
        assert_eq!(tokens[12].pos.line, 1);
        assert_eq!(tokens[12].pos.character, 0);

        assert_eq!(tokens[13].token_type, TokenType::NotEquals);
        assert_eq!(tokens[14].token_type, TokenType::AddressOf);
        assert_eq!(tokens[15].token_type, TokenType::Dot);
        assert_eq!(tokens[16].token_type, TokenType::Increment);
        assert_eq!(tokens[17].token_type, TokenType::IncrementAssign);
        assert_eq!(tokens[18].token_type, TokenType::Decrement);
        assert_eq!(tokens[19].token_type, TokenType::DecrementAssign);
        assert_eq!(tokens[20].token_type, TokenType::DeepAssign);
        assert_eq!(tokens[21].token_type, TokenType::StringLiteral);
        assert_eq!(tokens[22].token_type, TokenType::StringConcat2);
        assert_eq!(tokens[23].token_type, TokenType::Pound);
    }

    #[test]
    fn test_words(){
        let mut lexer = GoldLexer::new();
        let  mut f = File::open("./test/words.txt").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        let tokens = lexer.lex(&file_contents).0;
        // println!("{:#?}", tokens[60].token_type);
        assert_eq!(tokens.len(), 125);
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[60].token_type, TokenType::Like);
        assert_eq!(tokens[124].token_type, TokenType::Module);
    }

    #[test]
    fn test_comment(){
        let mut lexer = GoldLexer::new();
        let input = String::from(
            "not a comment ; a comment\n
            ; entire line is a comment wow \n");
        let result = lexer.lex(&input);
        let token = result.0;
        // println!("{:#?}", token);
        assert_eq!(token.len(), 5);
        assert_eq!(token[0].token_type, TokenType::Not);
        assert_eq!(token[3].token_type, TokenType::Comment);
        assert_eq!(token[4].token_type, TokenType::Comment);
    }

    #[test]
    fn test_string_constants(){
        let mut lexer = GoldLexer::new();
        let input = String::from(
            "'first string constant'    'b'\n \"double quote of newline\"\n");
        let result = lexer.lex(&input);
        let token = result.0;
        // println!("{:#?}", token);
        assert_eq!(token.len(), 3);
        // first
        assert_eq!(token[0].token_type, TokenType::StringLiteral);
        assert_eq!(token[0].raw_pos, 0);
        assert_eq!(token[0].pos, Position {line:0,character:0});
        assert_eq!(token[0].value.as_ref().unwrap().as_str(), "first string constant");
        // second
        assert_eq!(token[1].token_type, TokenType::StringLiteral);
        assert_eq!(token[1].raw_pos, 27);
        assert_eq!(token[1].pos, Position {line:0,character:27});
        assert_eq!(token[1].value.as_ref().unwrap().as_str(), "b");
        // third
        assert_eq!(token[2].token_type, TokenType::StringLiteral);
        assert_eq!(token[2].raw_pos, 32);
        assert_eq!(token[2].pos, Position {line:1,character:1});
        assert_eq!(token[2].value.as_ref().unwrap().as_str(), "double quote of newline");
    }

    #[test]
    fn test_numeric_constants(){
        let mut lexer = GoldLexer::new();
        let input = String::from(
            "10 12.55 10000\n 77.1234134141412424\n");
        let result = lexer.lex(&input);
        let token = result.0;
        // println!("{:#?}", token);
        assert_eq!(token.len(), 4);    
        // first
        assert_eq!(token[0].token_type, TokenType::NumericLiteral);
        assert_eq!(token[0].raw_pos, 0);
        assert_eq!(token[0].pos, Position {line:0,character:0});
        assert_eq!(token[0].value.as_ref().unwrap().as_str(), "10");
        // second
        assert_eq!(token[1].token_type, TokenType::NumericLiteral);
        assert_eq!(token[1].raw_pos, 3);
        assert_eq!(token[1].pos, Position {line:0,character:3});
        assert_eq!(token[1].value.as_ref().unwrap().as_str(), "12.55");
        // third
        assert_eq!(token[2].token_type, TokenType::NumericLiteral);
        assert_eq!(token[2].raw_pos, 9);
        assert_eq!(token[2].pos, Position {line:0,character:9});
        assert_eq!(token[2].value.as_ref().unwrap().as_str(), "10000");
        // fourth
        assert_eq!(token[3].token_type, TokenType::NumericLiteral);
        assert_eq!(token[3].raw_pos, 16);
        assert_eq!(token[3].pos, Position {line:1,character:1});
        assert_eq!(token[3].value.as_ref().unwrap().as_str(), "77.1234134141412424");
    }
}


