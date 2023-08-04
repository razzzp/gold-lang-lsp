
use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal, AstBinaryOp}, utils::create_new_range};

use super::{GoldParserError, exp_token, utils::prepend_msg_to_error, alt_parse};

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
    let (next, ident_token) = alt_parse(&[
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

fn parse_dot_ops<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_parser = exp_token(TokenType::Dot);
    return parse_binary_ops(input, &op_parser, &parse_primary);
} 

fn parse_factors<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Multiply),
        exp_token(TokenType::Divide),
        exp_token(TokenType::Modulus)
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_dot_ops);
} 

fn parse_terms<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Plus),
        exp_token(TokenType::Minus),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_factors);
} 

fn parse_unary_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    todo!()
}

fn parse_binary_ops<'a>(
    input: &'a[Token],
    op_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Token), GoldParserError>,
    expr_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), GoldParserError>,
) 
-> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError<'a>>{
    let (mut next, left_node) = expr_parser(input)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_binary_op(next, left_node.unwrap(), op_parser, expr_parser) {
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
    op_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Token), GoldParserError>,
    expr_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), GoldParserError>) -> Result<(&'a [Token], Box<dyn IAstNode>), Box<dyn IAstNode>>{

    let (next, op_token) = match op_parser(input){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    let (next, right_node) = match expr_parser(next){
        Ok(r) => r,
        Err(e) => return Err(left_node)
    };
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })))
}

#[cfg(test)]
mod test{

    use crate::ast::AstBinaryOp;
    use crate::utils::{print_ast_brief_recursive, inorder, print_ast_brief};
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors, parse_dot_ops};

    #[test]
    fn test_parse_dot_ops(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_dot_ops(&input).unwrap();
        assert_eq!(next.len(), 0);
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let inorder = inorder(bin_op);
        for (i, node) in &mut inorder.into_iter().enumerate() {
            assert_eq!(node.get_identifier(), input.get(i).unwrap().value.as_ref().unwrap().clone());
        }
    }

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
        assert_eq!(next.len(), 0);
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let inorder = inorder(bin_op);
        for (i, node) in &mut inorder.into_iter().enumerate() {
            assert_eq!(node.get_identifier(), input.get(i).unwrap().value.as_ref().unwrap().clone());
        }
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
        assert_eq!(next.len(), 0);
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let inorder = inorder(bin_op);
        for (i, node) in &mut inorder.into_iter().enumerate() {
            assert_eq!(node.get_identifier(), input.get(i).unwrap().value.as_ref().unwrap().clone());
        }
    }
}
