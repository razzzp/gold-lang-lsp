use crate::lexer::Token;


pub trait AstNode {
    
}

fn parse_expr(input : &Vec<Token>) -> nom::IResult<&Vec<Token>, impl AstNode> {
   
}

fn parse_tokens(input : &Vec<Token>) -> nom::IResult<&Vec<Token>, impl AstNode> {
   nom::branch::alt<&Vec<Token>, impl AstNode, nom::Err, Vec>((parse_expr(input)))(input)
}