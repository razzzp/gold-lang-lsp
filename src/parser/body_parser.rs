
use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal}};

use super::{GoldParserError, exp_token, alt_parse, alt_token};

/// expr = ident
///     | bin_op
/// 
/// bin_op = expr + expr
///     | expr - expr
/// 
/// bin_op = bin_op + expr
/// bin_op = 

fn parse_expr<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
    let parser = [
        parse_terminal,
    ];
    let result= alt_parse(&parser)(input)?;
    return Ok(result);
}

fn parse_terminal<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError> {
    let (next, ident_token) = alt_token(&[
        exp_token(TokenType::Identifier),
        exp_token(TokenType::StringConstant),
        exp_token(TokenType::NumericConstant),
        exp_token(TokenType::TSelf),
        exp_token(TokenType::Result),])(input)?;
    return Ok((next, Box::new(AstTerminal{
        token: ident_token
    })))
}

fn parse_unary_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    todo!()
}

fn parse_bracket_closure<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    // should we make a separate node for this?
    let (next, obracket_token) = exp_token(TokenType::OBracket)(input)?;
    let (next, expr_node) = parse_expr(input)?;
    let (next, cbracket_token) = exp_token(TokenType::OBracket)(input)?;
    return Ok((next,expr_node))
}

fn parse_binary_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let parse_op = [
        exp_token(TokenType::Plus),
        exp_token(TokenType::Minus),
        exp_token(TokenType::Multiply),
        exp_token(TokenType::Divide),
        exp_token(TokenType::Modulus),
        exp_token(TokenType::Equals),
        exp_token(TokenType::LessThan),
        exp_token(TokenType::LessThanOrEqual),
        exp_token(TokenType::GreaterThan),
        exp_token(TokenType::GreaterThanOrEqual),
        exp_token(TokenType::Equals),
        exp_token(TokenType::StringConcat),
        exp_token(TokenType::LeftShift),
        exp_token(TokenType::RightShift),
        exp_token(TokenType::NotEquals),
        exp_token(TokenType::Dot),
    ];
    todo!()
}