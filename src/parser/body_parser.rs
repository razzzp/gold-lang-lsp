use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal, AstBinaryOp, AstCast, AstUnaryOp, AstMethodCall}, utils::{create_new_range, IRange}};

use super::{GoldParserError, exp_token, utils::prepend_msg_to_error, alt_parse, parse_type_basic, parse_separated_list, create_closure};

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
        parse_logical_or,
    ];
    let result= alt_parse(&parser)(input)?;
    return Ok(result);
}

fn parse_literals<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError> {
    let (next, ident_token) = alt_parse(&[
        exp_token(TokenType::StringLiteral),
        exp_token(TokenType::NumericLiteral),
        exp_token(TokenType::BooleanTrue),
        exp_token(TokenType::BooleanFalse)])(input)?;
    return Ok((next, Box::new(AstTerminal{
        token: ident_token
    })))
}

fn parse_method_call<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError> {
    let (next, ident_token) = exp_token(TokenType::Identifier)(input)?;
    let (next, _) = exp_token(TokenType::OBracket)(next)?;
    let (next, parameter_list) = parse_separated_list(next, parse_expr, TokenType::Comma)?;
    let (next, cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    return Ok((next, Box::new(AstMethodCall{
        raw_pos: ident_token.get_raw_pos(),
        pos: ident_token.get_pos(),
        range: create_new_range(ident_token.as_range(), cbracket_token.as_range()),
        identifier: ident_token,
        parameter_list,
    })))
}

fn parse_dot_op_left<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError> {
    let (next, ident_token) = alt_parse(&[
        exp_token(TokenType::Identifier),
        exp_token(TokenType::TSelf),
        exp_token(TokenType::Result),
        ])(input)?;
    return Ok((next, Box::new(AstTerminal{
        token: ident_token
    })))
}

fn parse_dot_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError> {
    let parsers = [
        parse_method_call,
        parse_dot_op_left,
        ];
    return alt_parse(&parsers)(input);
}

fn parse_dot_ops<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_parser = exp_token(TokenType::Dot);
    return parse_binary_ops(input, &op_parser, &parse_dot_op);
} 

fn parse_bracket_closure<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    // should we make a separate node for this?
    let (next, obracket_token) = exp_token(TokenType::OBracket)(input)?;
    let (next, mut expr_node) = parse_expr(next)?;
    let (next, cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    expr_node.set_range(create_new_range(obracket_token.as_range(), cbracket_token.as_range()));
    return Ok((next,expr_node))
}

fn parse_cast<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (next, type_node) = parse_type_basic(input)?;
    let (next, _) = exp_token(TokenType::OBracket)(next)?;
    let (next, expr_node) = parse_expr(next)?;
    let (next, cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    return Ok((next, Box::new(AstCast{
        raw_pos: type_node.get_raw_pos(),
        pos: type_node.get_pos(),
        range: create_new_range(type_node.as_range(), cbracket_token.as_range()),
        type_node,
        expr_node
    })));
}

fn parse_primary<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let parsers = [
        parse_dot_ops,
        parse_bracket_closure,
        parse_unary_op,
        parse_literals,
        parse_cast
    ];
    let (next, node) = alt_parse(&parsers)(input)?;
    return Ok((next, node));
}

fn parse_factors<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Multiply),
        exp_token(TokenType::Divide),
        exp_token(TokenType::Modulus)
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_primary);
} 

fn parse_terms<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Plus),
        exp_token(TokenType::Minus),
        exp_token(TokenType::StringConcat),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_factors);
} 

fn parse_bit_ops_1<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::BAnd),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_terms);
} 


fn parse_bit_ops_2<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::BOr),
        exp_token(TokenType::BXor),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_bit_ops_1);
} 

fn parse_shifts<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::LeftShift),
        exp_token(TokenType::RightShift),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_bit_ops_2);
} 

fn parse_compare<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Equals),
        exp_token(TokenType::NotEquals),
        exp_token(TokenType::LessThan),
        exp_token(TokenType::LessThanOrEqual),
        exp_token(TokenType::GreaterThan),
        exp_token(TokenType::GreaterThanOrEqual),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_shifts);
} 

fn parse_logical_and<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::And),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_compare);
}

fn parse_logical_or<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_token_parsers = [
        exp_token(TokenType::Or),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops(input, &op_parser, &parse_logical_and);
}

fn parse_unary_op<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let op_parsers = [
        exp_token(TokenType::Not),
        exp_token(TokenType::BNot),
        exp_token(TokenType::AddressOf)
    ];
    let (next, op_token) = alt_parse(&op_parsers)(input)?;
    let (next, expr_node) = parse_primary(next)?;
    return Ok((next, Box::new(AstUnaryOp{
        raw_pos: op_token.get_raw_pos(),
        pos: op_token.get_pos(),
        range: create_new_range(op_token.as_range(), expr_node.as_range()),
        op_token,
        expr_node
    })))
}

fn parse_assignment<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
    let (next, left_node) = parse_dot_ops(input)?;
    let op_parsers = [
        exp_token(TokenType::Equals),
        exp_token(TokenType::DecrementAssign),
        exp_token(TokenType::IncrementAssign),
        exp_token(TokenType::DeepAssign)
    ];
    let (next, op_token) = alt_parse(&op_parsers)(next)?;
    let (next, right_node) = parse_expr(next)?;
    return Ok((next, Box::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        pos: left_node.get_pos(),
        range: create_new_range(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })));
}

fn parse_if_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], Box<dyn IAstNode>), GoldParserError>{
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

    use std::ops::Deref;

    use crate::ast::{AstBinaryOp, AstCast, AstTerminal, AstUnaryOp, AstMethodCall, IAstNode};
    use crate::utils::{print_ast_brief_recursive, inorder, print_ast_brief, dfs, IRange, create_new_range};
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors, parse_dot_ops, parse_cast, parse_bracket_closure, parse_bit_ops_2, parse_shifts, parse_compare, parse_logical_or, parse_unary_op, parse_method_call};


    #[test]
    fn test_parse_bracket_closure(){
        let input = gen_list_of_tokens(&[
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let (next, node) = parse_bracket_closure(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        
        let node = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        assert_eq!(node.left_node.get_identifier(), "First");
        assert_eq!(node.op_token.value.as_ref().unwrap(), "+");
        assert_eq!(node.right_node.get_identifier(), "Second");
    }

    #[test]
    fn test_parse_cast(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("CastType".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("Expression".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let (next, node) = parse_cast(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let node = node.as_any().downcast_ref::<AstCast>().unwrap();
        assert_eq!(node.type_node.get_identifier(), "CastType");
        assert_eq!(node.expr_node.get_identifier(), "Expression");
    }

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
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         .
        //        / \
        //       .   Third
        //      / \
        //   First Second
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value());
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
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect same tree struct as dot_ops
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value());
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
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         +
        //        /  \
        //     First '/'
        //           / \
        //        Second Third
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value());
    }

    #[test]
    fn test_parse_bit_ops(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::BOr, Some("bOr".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::BAnd, Some("bAnd".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_bit_ops_2(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         bOr
        //        /  \
        //     First bAnd
        //           / \
        //        Second Third
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value());
    }

    #[test]
    fn test_parse_shifts(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::LeftShift, Some("<<".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::RightShift, Some(">>".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_shifts(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         >>
        //        /  \
        //      <<   Third
        //     / \
        //  First Second
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value());
    }

    #[test]
    fn test_parse_compare(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::LessThanOrEqual, Some("<=".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_compare(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         >>
        //        /  \
        //      <<   Third
        //     / \
        //  First Second
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value());
    }

    #[test]
    fn test_parse_logical(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Or, Some("or".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::And, Some("and".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_logical_or(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect
        //         or
        //        /  \
        //     First and
        //           / \
        //        Second Third
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value());
    }

    #[test]
    fn test_parse_unary_op(){
        let input = gen_list_of_tokens(&[
            (TokenType::Not, Some("not".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::BNot, Some("bNot".to_string())),
            (TokenType::AddressOf, Some("@".to_string())),
            (TokenType::Identifier, Some("Expression".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let (next, node) = parse_unary_op(&input).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value(), "not");
        assert_eq!(node.expr_node.get_identifier(), "bNot");
    }


    #[test]
    fn test_parse_logical_only_terminal(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
        ]);
        let (next, node) = parse_terms(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstTerminal>().unwrap();
        let dfs = dfs(bin_op);
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
    }

    #[test]
    fn test_parse_method_call(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("Method".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("Param1".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("Param2".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("Val1".to_string())),
            (TokenType::Multiply, Some("*".to_string())),
            (TokenType::Identifier, Some("Val2".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let (next, node) = parse_method_call(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let node = node.as_any().downcast_ref::<AstMethodCall>().unwrap();
        assert_eq!(node.get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(node.parameter_list.len(), 3);
    }

    #[test]
    fn test_parse_method_calls(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Method1".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("Param1".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("Param1".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Method2".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            
        ]);
        let (next, node) = parse_dot_ops(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        dfs.iter().for_each(|n| {println!("{}",print_ast_brief(n.as_ast_node()))});
        // 
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(6).unwrap().get_value());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value());

        assert_eq!(dfs.get(5).unwrap().get_identifier(), input.get(11).unwrap().get_value());
        assert_eq!(dfs.get(6).unwrap().get_identifier(), input.get(13).unwrap().get_value());
        assert_eq!(dfs.get(7).unwrap().get_identifier(), input.get(12).unwrap().get_value());
        assert_eq!(dfs.get(8).unwrap().get_identifier(), input.get(9).unwrap().get_value());
        assert_eq!(dfs.get(9).unwrap().get_identifier(), input.get(8).unwrap().get_value());
    }
}
