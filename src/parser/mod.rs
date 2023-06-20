
use crate::lexer::tokens::{Token, TokenType, Range, Position};
use crate::ast::{AstClass, AstUses, AstTerminal, IAstNode, AstEmpty, AstTypeBasicFixedSize, AstTypeBasicDynamicSize, AstTypeEnum};

#[derive(Debug, Clone)]
pub struct MyError<'a>{
   pub input: &'a [Token],
   pub msg: String
}

#[derive(Debug, Clone)]
pub struct ParserError {
   token: Token,
   msg:String
}

pub fn parse_gold<'a>(input : &'a [Token]) -> ((&'a [Token],  Vec<Box<dyn IAstNode>>), Vec<ParserError>) {
   let parsers = [
      parse_class,
      parse_uses,
      parse_type,
   ];
   let mut result = Vec::<Box<dyn IAstNode>>::new();
   let mut errors = Vec::<ParserError>::new();
   let mut next = input;
   while next.len() > 0 {
      next = match alt_parse(&parsers)(next){
         Ok((r,n))=> {result.push(n); r},
         Err(e)=> {
            let mut iter = next.iter();
            errors.push(ParserError{
               token: iter.next().unwrap().clone(), 
               msg: e.msg.clone()}
            );
            iter.as_slice()
         }
      };
   }
   ((next, result), errors)
}

fn parse_class<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError> {
   // class keyword
   let (mut next, mut token) = match exp_token(TokenType::Class)(input) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   (next, token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };
   let class_name_token = token;
   // '('
   (next, _) =  match exp_token(TokenType::OBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // parent class
   (next, token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   let parent_class_name = token;
   // ')'
   (next, _) =  match exp_token(TokenType::CBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // TODO change behaviour when class name empty
   return Ok((next, Box::new(AstClass{
      raw_pos: class_name_token.raw_pos,
      name: class_name_token.value.unwrap().to_owned(),
      parent_class: parent_class_name.value.unwrap().to_owned()
   })));

   // let (i,r) = take(1)(input);
   // if input.peek().unwrap().token_type == TokenType::Identifier{
   //    return Ok((input, Box::new(AST{})));
   // } else {
   //    return Err(Err::Error(Error::new(input, ErrorKind::Tag)))
   // }   
}

fn parse_uses<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError> {
   let (next, uses) = match exp_token(TokenType::Uses)(input){
      Ok((r, t)) => (r, t),
      Err(e) => return Err(e),
   };
   let (next, idents) = match parse_separated_list(next, TokenType::Identifier, TokenType::Comma) {
      Ok((r, l)) => (r, l),
      Err(e) => return Err(e),
   };
   return Ok((next, Box::new(AstUses { raw_pos: uses.raw_pos, list_of_uses: idents })));
}

fn parse_type<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>>{
   let parsers = [
      parse_type_basic_fixed_size,
      parse_type_basic_dynamic_size,
      parse_type_enum,
   ];
   let parse_result = alt_parse(&parsers)(input);
   return parse_result;
}

fn parse_type_basic_fixed_size<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>> {
   let parse_result = alt_token(&[
      exp_token(TokenType::Int1),
      exp_token(TokenType::Int2),
      exp_token(TokenType::Int4),
      exp_token(TokenType::Int8),
      exp_token(TokenType::Boolean),
      exp_token(TokenType::Char),
      exp_token(TokenType::Num4),
      exp_token(TokenType::Num8),
      exp_token(TokenType::Num10),
      exp_token(TokenType::Decimal),
      exp_token(TokenType::CString),
      exp_token(TokenType::String),
      exp_token(TokenType::Identifier),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypeBasicFixedSize{raw_pos:t.raw_pos, type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_basic_dynamic_size<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>> {
   let parse_result = alt_token(&[
      exp_token(TokenType::Text),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypeBasicDynamicSize{raw_pos:t.raw_pos, type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_enum<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>>{
   let (next, s_raw_pos, s_pos) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t.raw_pos, t.pos),
      Err(e) => return Err(e)
   };
   let (next, tokens) = match parse_separated_list(next, TokenType::Identifier, TokenType::Comma){
       Ok((r, ts)) => (r, ts),
       Err(e) => return Err(e)
   };
   let (next, e_pos) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t.pos),
      Err(e) => return Err(e)
   };
   return Ok((next, Box::new(AstTypeEnum{
      raw_pos: s_raw_pos,
      pos: s_pos.clone(),
      range: Range{start: s_pos, end: e_pos},
      variants: tokens
   })));
}

fn parse_separated_list<'a>(input : &'a [Token], item : TokenType, seperator: TokenType) 
-> Result<(&'a [Token],  Vec<Token>), MyError<'a>> {
   let mut identifiers = Vec::<Token>::new();
   // match first identifier
   let r = _parse_seperated_list_recursive(input, &item, &seperator, &mut identifiers);
   match r{
      Ok(r) => return Ok((r, identifiers)),
      Err(e) => return Err(e)
   };
}

fn _parse_seperated_list_recursive<'a, 'b>(
   input : &'a [Token],
   item: &'b TokenType,
   sep: &'b TokenType,
   result: &'b mut Vec<Token>) 
-> Result<&'a [Token], MyError<'a>> {
   // match first identifier
   let mut next = match exp_token(item.clone())(input){
      Ok((r, t)) => {result.push(t); r},
      Err(e) => return Err(MyError { input: e.input, msg:  String::from("Failed to parse uses list")})
   };
   next = match exp_token(sep.clone())(next){
      Ok((r, _)) => r,
      Err(e) => return Ok(e.input)
   };
   return _parse_seperated_list_recursive(next, item, sep, result)
}

fn exp_token(token_type : TokenType)
   -> impl Fn(&[Token]) -> Result<(&[Token],  Token), MyError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Token), MyError> {
      let mut it = input.iter();
      match it.next() {
      Some(t) if t.token_type == token_type => Ok((it.as_slice(), t.clone())),
      Some(t) => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found {:?}",token_type,t.token_type))}),
      None => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found None",token_type))})
      }
   }
}

/*
   wraps the parser so that it doesn't throw error
 */
fn opt_parse(parser : impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), MyError>) 
   -> impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), MyError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Box<dyn IAstNode>), MyError> {
      match parser(input) {
          Ok(r) => return Ok(r),
          _ => ()
      };
      // TODO better way to return Ok? AstEmpty will just be thrown away
      return Ok((input, Box::new(AstEmpty{})));
   }
}

fn alt_token(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Token), MyError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Token), MyError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Token), MyError> {
      let mut next = input;
      for parser in list_of_parsers {
         let r = parser(input);
         match r {
            Ok(r) => return Ok(r),
            Err(e) => {next = e.input}
         }
      }
      return Err(MyError{ input: next, msg: String::from("Failed to parse using alternatives") });
   }
}

fn alt_parse(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), MyError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), MyError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Box<dyn IAstNode>), MyError> {
      let mut next = input;
      for parser in list_of_parsers {
         let r = parser(input);
         match r {
            Ok(r) => return Ok(r),
            Err(e) => {next = e.input}
         }
      }
      return Err(MyError{ input: next, msg: String::from("Failed to parse using alternatives") });
   }
}

fn seq_parse(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), MyError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Vec<Box<dyn IAstNode>>), MyError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Vec<Box<dyn IAstNode>>), MyError> {
      let mut i = 0;
      let mut next = input;
      let mut nodes = Vec::<Box<dyn IAstNode>>::new();
      while i < list_of_parsers.len(){
         next = match list_of_parsers[i](next){
            Ok(r) => {nodes.push(r.1); r.0},
            Err(e) => return Err(MyError{ input: e.input, msg: String::from("Failed to parse using alternatives") })
         };
         i+=1;
      }
      return Ok((next, nodes));
   }
}

#[cfg(test)]
mod test {
    use crate::{lexer::tokens::{Token, TokenType, Position}, parser::{MyError, parse_uses}, ast::{AstTerminal, AstClass, AstUses, AstTypeBasicFixedSize, AstTypeBasicDynamicSize, AstTypeEnum}};

    use super::{parse_class, parse_type};

   fn gen_list_of_tokens(list : &[(TokenType, Option<String>)]) -> Vec<Token> {
      let mut result = Vec::<Token>::new();
      let mut raw_pos = 0;
      for (tok_type, val) in list.to_vec() {
         result.push(Token { 
            raw_pos: raw_pos, 
            pos: Position{line: raw_pos/20, character: raw_pos%20}, 
            token_type: tok_type, 
            value: val.clone() 
         });
         raw_pos+=5;
      }  
      return result;
   }

   #[test]
   fn test_parse_class(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::CBracket, None),
      ]);
      let r = parse_class(&input).unwrap();
      let class = r.1.as_any().downcast_ref::<AstClass>().unwrap();
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.name, "aTestClass");
      assert_eq!(class.raw_pos, 5);
      assert_eq!(class.parent_class, "aParentClass");
   }

   #[test]
   fn test_parse_class_too_short(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
      ]);
      assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_class_wrong_token(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Plus, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
         assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_uses(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
      let r = parse_uses(&input).unwrap();
      // ensure returned input is empty
      assert_eq!(r.0.len(), 0);
      let uses_node = r.1.as_any().downcast_ref::<AstUses>().unwrap();

      // first uses
      let token = &uses_node.list_of_uses[0];
      assert_eq!(token.raw_pos, 5);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aTestClass");

      // second uses
      let token = &uses_node.list_of_uses[1];
      assert_eq!(token.raw_pos, 15);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aParentClass");
   }

   #[test]
   fn test_parse_uses_trailing_comma(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::Comma, None),
         ]);
      assert!(parse_uses(&input).is_err());
   }

   #[test]
   fn test_parse_type_basic_fixed_size() {
      let input = gen_list_of_tokens(&[
         (TokenType::Int1, None),
         (TokenType::Int2, None),
         (TokenType::Int4, None),
         (TokenType::Int8, None),
         (TokenType::Boolean, None),
         (TokenType::Char, None),
         (TokenType::Num4, None),
         (TokenType::Num8, None),
         (TokenType::Num10, None),
         (TokenType::Decimal, None),
         (TokenType::CString, None),
         (TokenType::String, None),
         (TokenType::Identifier, Some("tCustomType".to_string())),
      ]);
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasicFixedSize>().unwrap();
         assert_eq!(
            downcasted.raw_pos,
            input[count].raw_pos
         );
         assert_eq!(
            downcasted.type_token.token_type,
            input[count].token_type
         );
         count += 1;
         next = remaining;
         // additional checking last token
         if next.is_empty(){
            assert_eq!(downcasted.type_token.value.as_ref().unwrap().as_str(), "tCustomType")
         }
      }
   }

   #[test]
   fn test_parse_type_basic_dynamic_size() {
      let input = gen_list_of_tokens(&[
         (TokenType::Text, None),
      ]);
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasicDynamicSize>().unwrap();
         assert_eq!(
            downcasted.raw_pos,
            input[count].raw_pos
         );
         assert_eq!(
            downcasted.type_token.token_type,
            input[count].token_type
         );
         count += 1;
         next = remaining;
      }
   }

   #[test]
   fn test_parse_type_enum() {
      let input = gen_list_of_tokens(&[
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some("Variant1".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("Variant2".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("Variant3".to_string())),
         (TokenType::CBracket, None),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeEnum>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 1);
      assert_eq!(downcasted.range.end.character, 10);
   }
}