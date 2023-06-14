use std::{iter::Peekable, slice::Iter};

use nom::{error::{ Error, ErrorKind}, Err};

use crate::lexer::tokens::{Token, TokenType};

pub trait AstNode {
    
}

struct AST{}

impl AstNode for AST {
    
}

fn parse_expr<'a>(input : &'a mut Peekable<Iter<'_, &'a Token>>) -> nom::IResult<&'a mut Peekable<Iter<'a, &'a Token>>, Box<dyn AstNode>> {
   if input.peek().unwrap().token_type == TokenType::Identifier{
      return Ok((input, Box::new(AST{})));
   } else {
      return Err(Err::Error(Error::new(input, ErrorKind::Tag)))
   }   
}

fn parse_tokens<'a>(input : &'a mut Peekable<Iter<'_, &'a Token>>) -> nom::IResult<&'a mut Peekable<Iter<'a, &'a Token>>, Box<dyn AstNode>> {
   nom::branch::alt((parse_expr,))(input)
}