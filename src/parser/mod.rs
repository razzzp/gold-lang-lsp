
use crate::lexer::tokens::{Token, TokenType};
use crate::ast::{AstNode, AstClass, AstUses, AstTerminal, IAstNode, AstEmpty, AstTypePrimitiveFixedSize, AstTypePrimitiveDynamicSize};

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

// why is the syntax so hard? maybe considered a hack? dunno lah, no used for now
trait MyParser : Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError> + {}
// extension trait
impl<T: Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError>> MyParser for T {}

pub fn parse_gold<'a>(input : &'a [Token]) -> ((&'a [Token],  Vec<Box<dyn IAstNode>>), Vec<ParserError>) {
   let parsers = [
      parse_class,
      parse_uses
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
   let (mut next,mut token) = match exp_token(TokenType::Class)(input) {
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
      pos: class_name_token.pos,
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
   return Ok((next, Box::new(AstUses { pos: uses.pos, list_of_uses: idents })));
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

fn parse_type<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>>{
   let parsers = [
      parse_type_primitive_fixed_size,
      parse_type_primitive_dynamic_size,
   ];
   let parse_result = alt_parse(&parsers)(input);
   return parse_result;
}

fn parse_type_primitive_fixed_size<'a>(input : &'a [Token]) 
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
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypePrimitiveFixedSize{pos:t.pos, type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_primitive_dynamic_size<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>> {
   let parse_result = alt_token(&[
      exp_token(TokenType::Text),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypePrimitiveDynamicSize{pos:t.pos, type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_enum<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), MyError<'a>>{
   let (next, pos) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t.pos),
      Err(e) => return Err(e)
   };
   todo!()
}


fn create_unexpected_node_error_msg(n1 : &AstNode) -> String{
   return String::from(format!("Unexpected node: {:?}", n1.type_as_str()));
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
    use crate::{lexer::tokens::{Token, TokenType}, parser::{MyError, AstNode, parse_uses}, ast::{AstTerminal, AstClass, AstUses, AstTypePrimitiveFixedSize, AstTypePrimitiveDynamicSize}};

    use super::{parse_class, parse_type};


   #[test]
   fn test_parse_class(){
      let input = [
         Token { pos:10, token_type:TokenType::Class, value: None},
         Token { pos:15, token_type:TokenType::Identifier, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::OBracket, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         Token { pos:27, token_type:TokenType::CBracket, value: None},];
      let r = parse_class(&input).unwrap();
      let class = r.1.as_any().downcast_ref::<AstClass>().unwrap();
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.name, "aTestClass");
      assert_eq!(class.pos, 15);
      assert_eq!(class.parent_class, "aParentClass");
   }

   #[test]
   fn test_parse_class_too_short(){
      let input = [
         Token { pos:10, token_type:TokenType::Class, value: None},
         Token { pos:15, token_type:TokenType::Identifier, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::OBracket, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         ];
      assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_class_wrong_token(){
      let input = [
         Token { pos:10, token_type:TokenType::Class, value: None},
         Token { pos:15, token_type:TokenType::Plus, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::OBracket, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         ];
         assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_uses(){
      let input = [
         Token { pos:10, token_type:TokenType::Uses, value: None},
         Token { pos:15, token_type:TokenType::Identifier, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::Comma, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         ];
      let r = parse_uses(&input).unwrap();
      // ensure returned input is empty
      assert_eq!(r.0.len(), 0);
      let uses_node = r.1.as_any().downcast_ref::<AstUses>().unwrap();

      // first uses
      let token = &uses_node.list_of_uses[0];
      assert_eq!(token.pos, 15);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aTestClass");

      // second uses
      let token = &uses_node.list_of_uses[1];
      assert_eq!(token.pos, 21);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aParentClass");
   }

   #[test]
   fn test_parse_uses_trailing_comma(){
      let input = [
         Token { pos:10, token_type:TokenType::Uses, value: None},
         Token { pos:15, token_type:TokenType::Identifier, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::Comma, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         Token { pos:20, token_type:TokenType::Comma, value: None},
         ];
      assert!(parse_uses(&input).is_err());
   }

   #[test]
   fn test_parse_type_primitive_fixed_size() {
      let input = [
         Token { pos:0, token_type:TokenType::Int1, value: None},
         Token { pos:10, token_type:TokenType::Int2, value: None},
         Token { pos:20, token_type:TokenType::Int4, value: None},
         Token { pos:30, token_type:TokenType::Int8, value: None},
         Token { pos:40, token_type:TokenType::Boolean, value: None},
         Token { pos:50, token_type:TokenType::Char, value: None},
         Token { pos:60, token_type:TokenType::Num4, value: None},
         Token { pos:70, token_type:TokenType::Num8, value: None},
         Token { pos:80, token_type:TokenType::Num10, value: None},
         Token { pos:90, token_type:TokenType::Decimal, value: None},
         Token { pos:100, token_type:TokenType::CString, value: None},
         Token { pos:110, token_type:TokenType::String, value: None},
      ];
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypePrimitiveFixedSize>().unwrap();
         assert_eq!(
            downcasted.pos,
            input[count].pos
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
   fn test_parse_type_primitive_dynamic_size() {
      let input = [
         Token { pos:110, token_type:TokenType::Text, value: None},
      ];
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypePrimitiveDynamicSize>().unwrap();
         assert_eq!(
            downcasted.pos,
            input[count].pos
         );
         assert_eq!(
            downcasted.type_token.token_type,
            input[count].token_type
         );
         count += 1;
         next = remaining;
      }
   }
}