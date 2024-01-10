
use std::{str::Chars, iter::{Peekable, Enumerate}, sync::Arc};

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
        return Ok(self.create_token(pos, TokenType::NumericLiteral, number));
    }

    fn read_symbol(&mut self, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let next = buf.next().unwrap();
        let pos = next.0;
        let token: Result<Token, GoldLexerError> = match next.1 {
            '(' => Ok(self.create_token(pos, TokenType::OBracket, next.1.to_string())),
            ')' => Ok(self.create_token(pos, TokenType::CBracket, next.1.to_string())),
            '[' => Ok(self.create_token(pos, TokenType::OSqrBracket, next.1.to_string())),
            ']' => Ok(self.create_token(pos, TokenType::CSqrBracket, next.1.to_string())),
            '{' => Ok(self.create_token(pos, TokenType::OCurBracket, next.1.to_string())),
            '}' => Ok(self.create_token(pos, TokenType::CCurBracket, next.1.to_string())),
            '*' => Ok(self.create_token(pos, TokenType::Asterisk, next.1.to_string())),
            '/' => Ok(self.create_token(pos, TokenType::Divide, next.1.to_string())),
            '%' => Ok(self.create_token(pos, TokenType::Modulus, next.1.to_string())),
            '@' => Ok(self.create_token(pos, TokenType::AddressOf, next.1.to_string())),
            '.' => Ok(self.create_token(pos, TokenType::Dot, next.1.to_string())),
            '=' => Ok(self.create_token(pos, TokenType::Equals, next.1.to_string())),
            '\'' => Ok(self.read_string_constant(pos, buf)),
            '\"' => Ok(self.read_string_constant_doublequotes(pos, buf)),
            ',' => Ok(self.create_token(pos, TokenType::Comma, next.1.to_string())),
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
        return self.create_token(pos, TokenType::StringLiteral, value)
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
        return self.create_token(pos, TokenType::StringLiteral, value)
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
        return self.create_token(pos, TokenType::Comment, comment);
    }
    
    fn read_double_char_op(&self, first_op: char, pos: usize, buf: &mut Peekable<Enumerate<Chars>>) -> Result<Token, GoldLexerError> {
        let next = buf.peek();
    
        let mut is_double_op = true;
        let result: Result<Token, GoldLexerError>;
        if  first_op == '<'{
            result = match next {
                Some((_,'<')) => Ok(self.create_token(pos, TokenType::LeftShift, "<<".to_string())),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::LessThanOrEqual, "<=".to_string())),
                Some((_,'>')) => Ok(self.create_token(pos, TokenType::NotEquals, "<>".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::LessThan, "<".to_string()))}
            };
        } else if  first_op == '>'{
            result = match next {
                Some((_,'>')) => Ok(self.create_token(pos, TokenType::RightShift, ">>".to_string())),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::GreaterThanOrEqual, ">=".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::GreaterThan, ">".to_string()))}
            };
        } else if  first_op == '&'{
            result = match next {
                Some((_,'&')) => Ok(self.create_token(pos, TokenType::StringConcat, "&&".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::StringConcat2, "&".to_string()))}
            };
        } else if  first_op == '+'{
            result = match next{
                Some((_,'+')) => Ok(self.create_token(pos, TokenType::Increment, "++".to_string())),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::IncrementAssign, "+=".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Plus, "+".to_string()))}
            };
        } else if  first_op == '-'{
            result = match next {
                Some((_,'-')) => Ok(self.create_token(pos, TokenType::Decrement, "--".to_string())),
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::DecrementAssign, "-=".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Minus, "-".to_string()))}
            };
        } else if  first_op == ':'{
            result = match next {
                Some((_,'=')) => Ok(self.create_token(pos, TokenType::DeepAssign, ":=".to_string())),
                _ => {is_double_op = false; Ok(self.create_token(pos, TokenType::Colon, ":".to_string()))}
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
                Some((_ , '0'..='9')) => {
                    int_char.push(buf.next().unwrap().1);
                },
                Some((_, ' '| '\n'| '\r'| '\t')) => {
                    break
                },
                _ => break,
            }
        }
        let token_type = if int_char.len() == 1 {TokenType::Pound} else {TokenType::StringLiteral};
        return self.create_token(pos, token_type, int_char);
    }

    fn create_word_token(&self, pos: usize, word : String) -> Token {
        match word.to_uppercase().as_str() {
            "CLASS" =>self.create_token(pos, TokenType::Class, word),
            "FUNC" | "FUNCTION" =>self.create_token(pos, TokenType::Func, word),
            "ENDFUNC" =>self.create_token(pos, TokenType::EndFunc, word),
            "PROC" | "PROCEDURE" =>self.create_token(pos, TokenType::Proc, word),
            "ENDPROC" =>self.create_token(pos, TokenType::EndProc, word),
            "ABSOLUTE" =>self.create_token(pos, TokenType::Absolute, word),
            "ABSTRACT" =>self.create_token(pos, TokenType::Abstract, word),
            "ARRAY" =>self.create_token(pos, TokenType::Array, word),
            "CONST" =>self.create_token(pos, TokenType::Const, word),
            "EXTERNAL" =>self.create_token(pos, TokenType::External, word),
            "FINAL" =>self.create_token(pos, TokenType::Final, word),
            "FORWARD" =>self.create_token(pos, TokenType::Forward, word),
            "INOUT" =>self.create_token(pos, TokenType::InOut, word),
            // types
            // "DECIMAL" =>self.create_token(pos, TokenType::Decimal, word),
            // "CHAR" =>self.create_token(pos, TokenType::Char, word),
            // "CSTRING" =>self.create_token(pos, TokenType::CString, word),
            // "TEXT" =>self.create_token(pos, TokenType::Text, word),
            // "INT" =>self.create_token(pos, TokenType::Int, word),
            // "INT1" =>self.create_token(pos, TokenType::Int1, word),
            // "INT2" =>self.create_token(pos, TokenType::Int2, word),
            // "INT4" =>self.create_token(pos, TokenType::Int4, word),
            // "INT8" =>self.create_token(pos, TokenType::Int8, word),
            // "BOOLEAN" =>self.create_token(pos, TokenType::Int8, word),
            // "NUM" =>self.create_token(pos, TokenType::Num, word),
            // "NUM4" =>self.create_token(pos, TokenType::Num4, word),
            // "NUM8" =>self.create_token(pos, TokenType::Num8, word),
            // "NUM10" =>self.create_token(pos, TokenType::Num10, word),
            // "STRING" =>self.create_token(pos, TokenType::String, word),

            "INVERSE" =>self.create_token(pos, TokenType::Inverse, word),
            "LISTOF" =>self.create_token(pos, TokenType::ListOf, word),
            "MEMORY" =>self.create_token(pos, TokenType::Memory, word),
            "OF" =>self.create_token(pos, TokenType::Of, word),
            "OVERRIDE" =>self.create_token(pos, TokenType::Override, word),
            "PRIVATE" =>self.create_token(pos, TokenType::Private, word),
            "PROTECTED" =>self.create_token(pos, TokenType::Protected, word),
            "RECORD" =>self.create_token(pos, TokenType::Record, word),
            "REFTO" =>self.create_token(pos, TokenType::RefTo, word),
            "REIMPLEM" =>self.create_token(pos, TokenType::ReImplem, word),
            "TYPE" =>self.create_token(pos, TokenType::Type, word),
            "VAR" =>self.create_token(pos, TokenType::Var, word),
            "VERSIONED" =>self.create_token(pos, TokenType::Versioned, word),
            "IF" =>self.create_token(pos, TokenType::If, word),
            "ENDIF" =>self.create_token(pos, TokenType::EndIf, word),
            "BEGIN" =>self.create_token(pos, TokenType::Begin, word),
            "BREAK" =>self.create_token(pos, TokenType::Break, word),
            "CATCH" =>self.create_token(pos, TokenType::Catch, word),
            "CONTINUE" =>self.create_token(pos, TokenType::Continue, word),
            "DOWNTO" =>self.create_token(pos, TokenType::DownTo, word),
            "ELSE" =>self.create_token(pos, TokenType::Else, word),
            "ELSEIF" =>self.create_token(pos, TokenType::ElseIf, word),
            "END" =>self.create_token(pos, TokenType::End, word),
            "FOR" =>self.create_token(pos, TokenType::For, word),
            "FOREACH" =>self.create_token(pos, TokenType::ForEach, word),
            "ENDFOR" =>self.create_token(pos, TokenType::EndFor, word),
            "ENDCLASS" =>self.create_token(pos, TokenType::EndClass, word),
            "ENDLOOP" =>self.create_token(pos, TokenType::EndLoop, word),
            "ENDRECORD" =>self.create_token(pos, TokenType::EndRecord, word),
            "ENDSWITCH" =>self.create_token(pos, TokenType::EndSwitch, word),
            "ENDTRY" =>self.create_token(pos, TokenType::EndTry, word),
            "ENDWHEN" =>self.create_token(pos, TokenType::EndWhen, word),
            "ENDWHILE" =>self.create_token(pos, TokenType::EndWhile, word),
            "EXIT" =>self.create_token(pos, TokenType::Exit, word),
            "FINALLY" =>self.create_token(pos, TokenType::Finally, word),
            "LOOP" =>self.create_token(pos, TokenType::Loop, word),
            "NATIVERECORD" =>self.create_token(pos, TokenType::NativeRecord, word),
            "ENDNATIVERECORD" =>self.create_token(pos, TokenType::EndNativeRecord, word),
            "REPEAT" =>self.create_token(pos, TokenType::Repeat, word),
            "RETURN" =>self.create_token(pos, TokenType::Return, word),
            "STEP" =>self.create_token(pos, TokenType::Step, word),
            "SWITCH" =>self.create_token(pos, TokenType::Switch, word),
            // "THROW" =>self.create_token(pos, TokenType::Throw, word),
            "TO" =>self.create_token(pos, TokenType::To, word),
            "TRY" =>self.create_token(pos, TokenType::Try, word),
            "UNTIL" =>self.create_token(pos, TokenType::Until, word),
            "WHEN" =>self.create_token(pos, TokenType::When, word),
            "WHILE" =>self.create_token(pos, TokenType::While, word),
            "AND" =>self.create_token(pos, TokenType::And, word),
            "BAND" =>self.create_token(pos, TokenType::BAnd, word),
            "BNOT" =>self.create_token(pos, TokenType::BNot, word),
            "BOR" =>self.create_token(pos, TokenType::BOr, word),
            "BXOR" =>self.create_token(pos, TokenType::BXor, word),
            "IN" =>self.create_token(pos, TokenType::In, word),
            "LIKE" =>self.create_token(pos, TokenType::Like, word),
            "NOT" =>self.create_token(pos, TokenType::Not, word),
            "OR" =>self.create_token(pos, TokenType::Or, word),
            "XOR" =>self.create_token(pos, TokenType::Xor, word),
            // remove tokens not used crucial to grammar, to simplify parsing
            // "_METHODNAME" =>self.create_token(pos, TokenType::MethodName, word),
            // "_MODULENAME" =>self.create_token(pos, TokenType::ModuleName, word),
            // "_MOVE" =>self.create_token(pos, TokenType::Move, word),
            // "CHR" =>self.create_token(pos, TokenType::Chr, word),
            // "CONCAT" =>self.create_token(pos, TokenType::Concat, word),
            // "DISPOSE" =>self.create_token(pos, TokenType::Dispose, word),
            // "FIRST" =>self.create_token(pos, TokenType::First, word),
            // "LAST" =>self.create_token(pos, TokenType::Last, word),
            "INHERITED" =>self.create_token(pos, TokenType::Inherited, word),
            "INSTANCEOF" =>self.create_token(pos, TokenType::InstanceOf, word),
            // "LENGTH" =>self.create_token(pos, TokenType::Length, word),
            // "MEMBER" =>self.create_token(pos, TokenType::Member, word),
            // "METAMODELENTITY" =>self.create_token(pos, TokenType::MetaModelEntity, word),
            // "NEW" =>self.create_token(pos, TokenType::New, word),
            "NIL" =>self.create_token(pos, TokenType::Nil, word),
            // "ORD" =>self.create_token(pos, TokenType::Ord, word),
            // "PASS" =>self.create_token(pos, TokenType::Pass, word),
            // "PRED" =>self.create_token(pos, TokenType::Pred, word),
            // "SCENARIO" =>self.create_token(pos, TokenType::Scenario, word),
            // "SIZEOF" =>self.create_token(pos, TokenType::SizeOf, word),
            // "SUCC" =>self.create_token(pos, TokenType::Succ, word),
            // "UPCASE" =>self.create_token(pos, TokenType::Upcase, word),
            "USES" =>self.create_token(pos, TokenType::Uses, word),
            // "WRITE" =>self.create_token(pos, TokenType::Write, word),
            // "WRITELN" =>self.create_token(pos, TokenType::WriteLn, word),
            "OQL" =>self.create_token(pos, TokenType::OQL, word),
            "TOP" =>self.create_token(pos, TokenType::Top, word),
            "DISTINCT" =>self.create_token(pos, TokenType::Distinct, word),
            "FROM" =>self.create_token(pos, TokenType::From, word),
            "USING" =>self.create_token(pos, TokenType::Using, word),
            "WHERE" =>self.create_token(pos, TokenType::Where, word),
            "FETCH" =>self.create_token(pos, TokenType::Fetch, word),
            "INTO" =>self.create_token(pos, TokenType::Into, word),
            "DESCENDING" =>self.create_token(pos, TokenType::Descending, word),
            "CONDITIONAL" =>self.create_token(pos, TokenType::Conditional, word),
            "ALLVERSIONSOF" =>self.create_token(pos, TokenType::AllVersionsOf, word),
            "PHANTOMSTOO" =>self.create_token(pos, TokenType::PhantomsToo, word),
            "SELECT" =>self.create_token(pos, TokenType::Select, word),
            // "OQLCLASSID" =>self.create_token(pos, TokenType::OQLClassId, word),
            // "OQLCOUNT" =>self.create_token(pos, TokenType::OQLCount, word),
            // "OQLMAX" =>self.create_token(pos, TokenType::OQLMax, word),
            // "OQLMIN" =>self.create_token(pos, TokenType::OQLMin, word),
            // "OQLSUM" =>self.create_token(pos, TokenType::OQLSum, word),
            // "OQLUPDATEDATE" =>self.create_token(pos, TokenType::OQLUpdateDate, word),
            // "OQLUPDATETIME" =>self.create_token(pos, TokenType::OQLUpdateTime, word),
            "ORDER" =>self.create_token(pos, TokenType::Order, word),
            "BY" =>self.create_token(pos, TokenType::By, word),

            // "SELF" =>self.create_token(pos, TokenType::TSelf, word),
            // "_RESULT" =>self.create_token(pos, TokenType::Result, word),
            "MODULE" =>self.create_token(pos, TokenType::Module, word),
            "MULTILANG" => self.create_token(pos, TokenType::MultiLang, word),
            "TRUE" => self.create_token(pos, TokenType::BooleanTrue, word),
            "FALSE" => self.create_token(pos, TokenType::BooleanFalse, word),
            "SEQUENCE" => self.create_token(pos, TokenType::Sequence, word),
            _ =>self.create_token(pos, TokenType::Identifier, word)
        }
    }
    
    
    fn create_token(&self, pos: usize, token_type: TokenType, value: String) -> Token{
        // plus 1 for zero based offset
        let range = self.create_range(pos, value.len());
        return Token {
            raw_pos: pos,
            range: range,
            token_type,
            value: Arc::from(value)
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
    use std::{iter::{Enumerate, Peekable}, str::Chars, fs::File, io::Read, path::PathBuf, ops::Deref};

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
        assert_eq!(token.get_pos().line, 0);
        assert_eq!(token.get_pos().character, 0);
        assert_eq!(token.token_type, TokenType::Plus);
        assert_eq!(token.value.deref(), "+");
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
        assert_eq!(tokens[12].get_pos().line, 1);
        assert_eq!(tokens[12].get_pos().character, 0);

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
        assert_eq!(token[0].get_pos(), Position {line:0,character:0});
        assert_eq!(token[0].value.deref(), "first string constant");
        // second
        assert_eq!(token[1].token_type, TokenType::StringLiteral);
        assert_eq!(token[1].raw_pos, 27);
        assert_eq!(token[1].get_pos(), Position {line:0,character:27});
        assert_eq!(token[1].value.deref(), "b");
        // third
        assert_eq!(token[2].token_type, TokenType::StringLiteral);
        assert_eq!(token[2].raw_pos, 32);
        assert_eq!(token[2].get_pos(), Position {line:1,character:1});
        assert_eq!(token[2].value.deref(), "double quote of newline");
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
        assert_eq!(token[0].get_pos(), Position {line:0,character:0});
        assert_eq!(token[0].value.deref(), "10");
        // second
        assert_eq!(token[1].token_type, TokenType::NumericLiteral);
        assert_eq!(token[1].raw_pos, 3);
        assert_eq!(token[1].get_pos(), Position {line:0,character:3});
        assert_eq!(token[1].value.deref(), "12.55");
        // third
        assert_eq!(token[2].token_type, TokenType::NumericLiteral);
        assert_eq!(token[2].raw_pos, 9);
        assert_eq!(token[2].get_pos(), Position {line:0,character:9});
        assert_eq!(token[2].value.deref(), "10000");
        // fourth
        assert_eq!(token[3].token_type, TokenType::NumericLiteral);
        assert_eq!(token[3].raw_pos, 16);
        assert_eq!(token[3].get_pos(), Position {line:1,character:1});
        assert_eq!(token[3].value.deref(), "77.1234134141412424");
    }


    #[ignore = "for benchmarking"]
    #[test]
    fn test_lex_1_000(){
        let path = PathBuf::from("./test/aNormalSizedClass.god");
        let _ =  match std::fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let file_content = std::fs::read_to_string(path).unwrap();
        
        let num_iter = 1_000;
        let now = std::time::Instant::now();
        for _i in 0..num_iter{

            // lexing
            let mut lexer = GoldLexer::new();
            let (_tokens, _lexer_errors) = lexer.lex(&file_content);
        }
        let elapse = now.elapsed();
        println!("Lex Ran {num_iter} iterations in {:.2?}", elapse);
    }
    
}


