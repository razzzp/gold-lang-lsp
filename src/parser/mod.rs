

use std::{any::Any, fmt::format};

use nom::{error::{ Error, ErrorKind}, Err, bytes::complete::take, Parser};

use crate::lexer::tokens::{Token, TokenType, Tokens};

#[derive(Debug)]
struct AstTerminal{
   pub token: Token,
}

#[derive(Debug)]
struct AstClass{
   pub pos: usize,
   pub name: String,
   pub parent_class: String
}


#[derive(Debug, Clone)]
struct MyError<'a>{
   input: &'a [Token],
   msg: String
}

#[derive(Debug)]
enum AstNode {
   None,
   Class(AstClass),
   Terminal(AstTerminal)
}

impl AstNode {
   fn type_as_str(&self) -> &'static str{
      match self {
         AstNode::None => "None",
         AstNode::Class(_) => "Class",
         AstNode::Terminal(_) => "Terminal",
      }
   }
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
      expect_token(&token_type.clone(), input)
   }
}

fn expect_token<'a>(token_type : &TokenType, input : &'a [Token]) -> Result<(&'a [Token],  AstNode), MyError<'a>>{
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
   (next, node) =  match expect(TokenType::OBracket)(next) {
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
   (next, node) =  match expect(TokenType::CBracket)(next) {
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

// fn parse_uses<'a>(input : &'a [Token]) -> Result<(&'a [Token],  AstClass), MyError> {
//    let (mut next, mut node) = match optional(expect(TokenType::Memory))(input) {
//       Ok(r)=> (r.0, r.1),
//       Err(e) => return Err(e)
//    };
// }

fn parse_tokens<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<AstNode>), MyError> {
   let parsers = [
      parse_class,
   ];
   let result = Vec::<AstNode>::new();
   let mut cur = input;
   while cur.len() > 0 {
      for parser in parsers.iter() {

      }
   }
   Ok((input, result))
}

mod test {
    use crate::{lexer::tokens::{Token, TokenType}, parser::{MyError, AstNode}};

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
}