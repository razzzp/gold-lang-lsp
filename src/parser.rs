

use std::any::Any;

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
   Class(AstClass),
   Terminal(AstTerminal)
}
    
// type MyParser<'a> = fn(&'a [Token]) -> nom::IResult<&'a [Token],  Box<dyn AstNode>> ;

fn expect_token<'a>(token_type : TokenType, input : &'a [Token]) -> Result<(&'a [Token],  AstTerminal), MyError>{
   let mut it = input.iter();
   match it.next() {
      Some(t) if t.token_type == token_type => Ok((it.as_slice(), AstTerminal{token:t.clone()})),
      Some(t) => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found {:?}",token_type,t.token_type))}),
      None => Err(MyError {input: input, msg: String::from(format!("Expected {:?}, found None",token_type))})
   }
}

fn parse_class<'a>(input : &'a [Token]) -> Result<(&'a [Token],  AstClass), MyError> {
   // class keyword
   let (mut next,mut node) = match expect_token(TokenType::Class, input) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   (next, node) =  match expect_token(TokenType::Identifier, next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };
   let class_name_token = node.token.clone();
   // '('
   (next, node) =  match expect_token(TokenType::OBracket, next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // parent class
   (next, node) =  match expect_token(TokenType::Identifier, next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   let parent_class_name = match node.token.clone().value {
      Some(s)=> s.clone(),
      None => return Err(MyError {input: input, msg: String::from("Parent class name is empty")})
   };
   // ')'
   (next, node) =  match expect_token(TokenType::CBracket, next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // TODO change behaviour when class name empty
   return Ok((next, AstClass{
      pos: class_name_token.pos,
      name: class_name_token.value.unwrap_or("unknown".to_owned()),
      parent_class: parent_class_name
   }));

   // let (i,r) = take(1)(input);
   // if input.peek().unwrap().token_type == TokenType::Identifier{
   //    return Ok((input, Box::new(AST{})));
   // } else {
   //    return Err(Err::Error(Error::new(input, ErrorKind::Tag)))
   // }   
}

fn parse_tokens<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<AstNode>), MyError> {
   todo!()
   // parse_class(input)
}

mod test {
    use crate::{lexer::tokens::{Token, TokenType}, parser::MyError};

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
      assert_eq!(r.1.name, "aTestClass");
      assert_eq!(r.1.pos, 15);
      assert_eq!(r.1.parent_class, "aParentClass");
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