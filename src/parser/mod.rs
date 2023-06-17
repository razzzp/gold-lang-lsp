
use crate::lexer::tokens::{Token, TokenType};
use crate::ast::{AstNode, AstClass, AstUses, AstTerminal};

#[derive(Debug, Clone)]
pub struct MyError<'a>{
   input: &'a [Token],
   msg: String
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

fn create_unexpected_node_error_msg(n1 : &AstNode) -> String{
   return String::from(format!("Unexpected node: {:?}", n1.type_as_str()));
}

fn expect(token_type : TokenType)
   -> impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError> 
{
   move |input: &[Token]| -> Result<(&[Token],  AstNode), MyError> {
      _expect_token(&token_type.clone(), input)
   }
}

fn _expect_token<'a>(token_type : &TokenType, input : &'a [Token]) -> Result<(&'a [Token],  AstNode), MyError<'a>>{
   let mut it = input.iter();
   match it.next() {
      Some(t) if t.token_type == *token_type => Ok((it.as_slice(), AstNode::Terminal(AstTerminal { token: t.clone() }))),
      Some(t) => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found {:?}",token_type,t.token_type))}),
      None => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found None",token_type))})
   }
}

/*
   wraps the parser so that it doesn't throw error
 */
fn optional(parser : impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError>) 
   -> impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError> 
{
   move |input: &[Token]| -> Result<(&[Token],  AstNode), MyError> {
      match parser(input) {
          Ok(r) => return Ok(r),
          _ => ()
      };
      return Ok((input, AstNode::None));
   }
}

fn alternatives(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  AstNode), MyError> {
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

fn sequence(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  AstNode), MyError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Vec<AstNode>), MyError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Vec<AstNode>), MyError> {
      let mut i = 0;
      let mut next = input;
      let mut nodes = Vec::<AstNode>::new();
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

fn parse_class<'a>(input : &'a [Token]) -> Result<(&'a [Token],  AstNode), MyError> {
   // class keyword
   let (mut next,mut node) = match expect(TokenType::Class)(input) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   (next, node) =  match expect(TokenType::Identifier)(next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };
   let class_name_token = match node{
      AstNode::Terminal(s) => s.token,
      _ => return Err(MyError { input: next, msg: create_unexpected_node_error_msg(&node) })
   };
   // '('
   (next, _) =  match expect(TokenType::OBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // parent class
   (next, node) =  match expect(TokenType::Identifier)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   let parent_class_name = match node {
      AstNode::Terminal(s) => s.token.value.unwrap().clone(),
      _ => return Err(MyError { input: next, msg: create_unexpected_node_error_msg(&node) })
   };
   // ')'
   (next, _) =  match expect(TokenType::CBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // TODO change behaviour when class name empty
   return Ok((next, AstNode::Class(AstClass{
      pos: class_name_token.pos,
      name: class_name_token.value.unwrap_or("unknown".to_owned()),
      parent_class: parent_class_name
   })));

   // let (i,r) = take(1)(input);
   // if input.peek().unwrap().token_type == TokenType::Identifier{
   //    return Ok((input, Box::new(AST{})));
   // } else {
   //    return Err(Err::Error(Error::new(input, ErrorKind::Tag)))
   // }   
}

fn parse_uses_list_recursive<'a, 'b>(input : &'a [Token], result: &'b mut Vec<AstNode>) -> Result<&'a [Token], MyError<'a>> {
   // match first identifier
   let mut next = match expect(TokenType::Identifier)(input){
      Ok((r, n)) => {result.push(n); r},
      Err(e) => return Err(MyError { input: e.input, msg:  String::from("Failed to parse uses list")})
   };
   next = match expect(TokenType::Comma)(next){
      Ok((r, _)) => r,
      Err(e) => return Ok(e.input)
   };
   return parse_uses_list_recursive(next, result)
}

fn parse_uses_list<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<AstNode>), MyError<'a>> {
   let mut identifiers = Vec::<AstNode>::new();
   // match first identifier
   let r = parse_uses_list_recursive(input, &mut identifiers);
   match r{
      Ok(r) => return Ok((r, identifiers)),
      Err(e) => return Err(e)
   };
}

fn parse_uses<'a>(input : &'a [Token]) -> Result<(&'a [Token],  AstNode), MyError> {
   let (next, uses) = match expect(TokenType::Uses)(input){
      Ok((r,AstNode::Terminal(n))) => (r, n),
      Ok(_) => return  Err(MyError { input: input, msg: String::from("Expected terminal node")}),
      Err(e) => return Err(e),
   };
   let (next, idents) = match parse_uses_list(next) {
      Ok((r, l)) => (r, l),
      Err(e) => return Err(e),
   };
   return Ok((next, AstNode::Uses(AstUses { pos: uses.token.pos,list_of_uses: idents })));
}

pub fn parse_tokens<'a>(input : &'a [Token]) -> ((&'a [Token],  Vec<AstNode>), Vec<ParserError>) {
   let parsers = [
      parse_class,
      parse_uses
   ];
   let mut result = Vec::<AstNode>::new();
   let mut errors = Vec::<ParserError>::new();
   let mut next = input;
   while next.len() > 0 {
      next = match alternatives(&parsers)(next){
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

#[cfg(test)]
mod test {
    use crate::{lexer::tokens::{Token, TokenType}, parser::{MyError, AstNode, parse_uses}};

    use super::parse_class;


   #[test]
   fn test_parse_class(){
      let input = [
         Token { pos:10, token_type:TokenType::Class, value: None},
         Token { pos:15, token_type:TokenType::Identifier, value: Some(String::from("aTestClass"))},
         Token { pos:20, token_type:TokenType::OBracket, value: None},
         Token { pos:21, token_type:TokenType::Identifier, value: Some(String::from("aParentClass"))},
         Token { pos:27, token_type:TokenType::CBracket, value: None},];
      let r = parse_class(&input).unwrap();
      assert_eq!(r.0.len(), 0);
      assert!(matches!(&r.1, AstNode::Class(c) if c.name == "aTestClass"));
      assert!(matches!(&r.1, AstNode::Class(c) if c.pos == 15));
      assert!(matches!(&r.1, AstNode::Class(c) if c.parent_class == "aParentClass"));
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
      let list_of_uses = match &r.1 {
         AstNode::Uses(n) => &n.list_of_uses,
         _ => panic!()
      };

      // first uses
      let token = match &list_of_uses[0] {
         AstNode::Terminal(n) => &n.token,
         _ => panic!()
      };
      assert_eq!(token.pos, 15);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aTestClass");

      // second uses
      let token = match &list_of_uses[1] {
         AstNode::Terminal(n) => &n.token,
         _ => panic!()
      };
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
}