
use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal, AstBinaryOp}, utils::create_new_range};

use super::{GoldParserError, exp_token, alt_parse, alt_token, utils::prepend_msg_to_error};

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
        parse_bracket_closure
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


fn parse_bracket_closure<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    // should we make a separate node for this?
    let (next, obracket_token) = exp_token(TokenType::OBracket)(input)?;
    let (next, expr_node) = parse_expr(next)?;
    let (next, cbracket_token) = exp_token(TokenType::OBracket)(next)?;
    return Ok((next,expr_node))
}

fn parse_primary<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let parsers = [
        parse_terminal,
        parse_bracket_closure,
    ];
    let (next, node) = alt_parse(&parsers)(input)?;
    return Ok((next, node));
}

fn parse_reference_ops<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (mut next, left_node) = parse_primary(input)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_factor(next, left_node.unwrap()) {
            Ok((n, node)) => (n, Some(node)),
            Err(ln)=> {
                left_node = Some(ln);
                break;
            }
        };
    }
    return Ok((next, left_node.unwrap()));
} 

fn parse_reference_op<'a>(input: &'a[Token], left_node: Box<dyn IAstNode>) -> Result<(&'a [Token], Box<dyn IAstNode>), Box<dyn IAstNode>>{
    let (next, op_token) = match alt_token(&[
        exp_token(TokenType::Dot),
    ])(input){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    let (next, right_node) = match parse_primary(next){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token: op_token,
        left_node: left_node,
        right_node: right_node
    })))
}

fn parse_factors<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (mut next, left_node) = parse_primary(input)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_factor(next, left_node.unwrap()) {
            Ok((n, node)) => (n, Some(node)),
            Err(ln)=> {
                left_node = Some(ln);
                break;
            }
        };
    }
    return Ok((next, left_node.unwrap()));
} 

fn parse_factor<'a>(input: &'a[Token], left_node: Box<dyn IAstNode>) -> Result<(&'a [Token], Box<dyn IAstNode>), Box<dyn IAstNode>>{
    let (next, op_token) = match alt_token(&[
        exp_token(TokenType::Multiply),
        exp_token(TokenType::Divide),
        exp_token(TokenType::Modulus)
    ])(input){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    let (next, right_node) = match parse_primary(next){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token: op_token,
        left_node: left_node,
        right_node: right_node
    })))
}

fn parse_terms<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (mut next, left_node) = parse_factors(input)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_term(next, left_node.unwrap()) {
            Ok((n, node)) => (n, Some(node)),
            Err(ln)=> {
                left_node = Some(ln);
                break;
            }
        };
    }
    return Ok((next, left_node.unwrap()));
} 

fn parse_term<'a>(input: &'a[Token], left_node: Box<dyn IAstNode>) -> Result<(&'a [Token], Box<dyn IAstNode>), Box<dyn IAstNode>>{
    let (next, op_token) = match alt_token(&[
        exp_token(TokenType::Plus),
        exp_token(TokenType::Minus),
    ])(input){
        Ok(r) => r,
        Err(_) => return Err(left_node)
    };
    let (next, right_node) = match parse_factors(next){
        Ok(r) => r,
        Err(_) => return Err(left_node)
    };
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token: op_token,
        left_node: left_node,
        right_node: right_node
    })))
}

fn parse_unary_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    todo!()
}

fn parse_binary_ops<'a>(
    input: &'a[Token],
    op_tokens: &'a[TokenType],
    expr_parser: impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), GoldParserError>,
) 
-> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (mut next, left_node) = expr_parser(input)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_binary_op(next, left_node.unwrap()) {
            Ok((n, node)) => (n, Some(node)),
            Err(ln)=> {
                left_node = Some(ln);
                break;
            }
        };
    }
    return Ok((next, left_node.unwrap()));
} 

fn parse_binary_op<'a>(
    input: &'a[Token],
    left_node: Box<dyn IAstNode>,
    op_tokens: &'a[TokenType]) -> Result<(&'a [Token], Box<dyn IAstNode>), Box<dyn IAstNode>>{
    let op_token_parsers = op_tokens.iter().map(|token_type| {exp_token(token_type.clone())}).collect();
    let (next, op_token) = match alt_token(op_token_parsers)(input){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    let (next, right_node) = match parse_primary(next){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token: op_token,
        left_node: left_node,
        right_node: right_node
    })))
}

#[cfg(test)]
mod test{

    use crate::ast::AstBinaryOp;
    use crate::utils::print_ast_brief;
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors};

    #[test]
    fn test_parse_factors(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Multiply, Some("*".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Multiply, Some("/".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_factors(&input).unwrap();
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let left_bin_op = &bin_op.left_node;
        let right_bin_op = &bin_op.right_node;
        println!("{}", print_ast_brief(bin_op));
    }

    #[test]
    fn test_parse_terms(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Multiply, Some("/".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_terms(&input).unwrap();
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let left_bin_op = &bin_op.left_node;
        let right_bin_op = &bin_op.right_node;
        println!("{}", print_ast_brief(bin_op));
    }
}
