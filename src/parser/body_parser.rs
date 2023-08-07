use nom::error;

use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal, AstBinaryOp, AstCast, AstUnaryOp, AstMethodCall, AstIfBlock, AstConditionalBlock, AstEmpty}, utils::{create_new_range_from_irange, IRange}, parser::take_until};

use super::{GoldParserError, exp_token, utils::prepend_msg_to_error, alt_parse, parse_type_basic, parse_separated_list, create_closure};

/// expr = ident
///     | bin_op
/// 
/// bin_op = expr + expr
///     | expr - expr
/// 
/// bin_op = bin_op + expr
/// bin_op = 
/// 
struct BlockParser<'a> {
    errors: Vec<GoldParserError<'a>>
} 

impl<'a> BlockParser<'a> {
    pub fn new(input : &'a [Token]) -> BlockParser<'a>{
        BlockParser{
            errors: Vec::new()
        }
    }
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
        range: create_new_range_from_irange(ident_token.as_range(), cbracket_token.as_range()),
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
    expr_node.set_range(create_new_range_from_irange(obracket_token.as_range(), cbracket_token.as_range()));
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
        range: create_new_range_from_irange(type_node.as_range(), cbracket_token.as_range()),
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
        range: create_new_range_from_irange(op_token.as_range(), expr_node.as_range()),
        op_token,
        expr_node
    })))
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

fn parse_expr<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
    let parser = [
        parse_logical_or,
    ];
    let result= alt_parse(&parser)(input)?;
    return Ok(result);
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
        range: create_new_range_from_irange(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })));
}

fn parse_if_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode + 'static>, Vec<GoldParserError>)), GoldParserError>{
    let (next, if_token) = exp_token(TokenType::If)(input)?;
    
    let stop_tokens = [
        TokenType::ElseIf,
        TokenType::Else,
        TokenType::EndIf, 
        TokenType::End
    ];
    // parse main if block
    let (mut next, if_block_tokens, mut end_token) = take_until(&stop_tokens)(next)?;
    let (if_block_tokens, if_cond_node) = parse_expr(if_block_tokens)?;
    let mut result: AstIfBlock;
    let mut errors: Vec<GoldParserError> = Vec::new();
    match parse_block(&if_block_tokens) {
        Ok((nodes, errs)) => {
            let end_node = match nodes.is_empty(){
                true => {if_cond_node.as_ast_node()}
                _=> {nodes.last().unwrap().as_ast_node()}
            };
            result=  AstIfBlock{
                raw_pos: if_token.get_raw_pos(),
                pos: if_token.get_pos(),
                range: create_new_range_from_irange(if_token.as_range(), if_cond_node.as_range()),
                if_block: Box::new(AstConditionalBlock{
                    raw_pos: if_token.get_raw_pos(),
                    pos: if_token.get_pos(),
                    range: create_new_range_from_irange(if_token.as_range(), end_node.as_range()),
                    condition: if_cond_node,
                    statements: nodes
                }),
                else_if_blocks: None,
                end_token: None
            };
            errors.extend(errs.into_iter());
        },
        Err(e) => return Err(e)
    }
    // parse next else if/else block
    let mut cur_end_token= end_token.clone();
    let mut cur_next=next;
    let mut cur_next_inner;
    (next, end_token) = loop {
        match cur_end_token.clone() {
            Some(t) => {
                if t.token_type == TokenType::End || t.token_type == TokenType::EndIf {break (cur_next, cur_end_token)} 
                
                // next blocks
                let mut cur_block_tokens;
                (cur_next_inner, cur_block_tokens, cur_end_token) = take_until(&stop_tokens)(cur_next)?;
                let cur_cond_node: Box<dyn IAstNode>;
                (cur_block_tokens, cur_cond_node) = if t.token_type == TokenType::ElseIf {
                    // parse conditional id elseif
                    parse_expr(cur_block_tokens)?
                } else {
                    // represents else block
                    (cur_block_tokens, AstEmpty::new(t.get_raw_pos(),t.get_pos(), t.get_range()))
                };

                match parse_block(&cur_block_tokens) {
                    Ok((nodes, errs)) => {
                        let end_node = match nodes.is_empty(){
                            true => {cur_cond_node.as_ast_node()}
                            _=> {nodes.last().unwrap().as_ast_node()}
                        };
                        result.add_else_if_block(Box::new(AstConditionalBlock{
                            raw_pos: t.get_raw_pos(),
                            pos: t.get_pos(),
                            range: create_new_range_from_irange(t.as_range(), end_node.as_range()),
                            condition: cur_cond_node,
                            statements: nodes
                        }));
                        errors.extend(errs.into_iter());
                    },
                    Err(e) => return Err(e)
                }
                cur_next= cur_next_inner;
            },
            None => break (cur_next, cur_end_token)
        }
    };
    // set final range of if block
    match end_token{
        Some(t) => result.set_range(create_new_range_from_irange(&if_token.get_range(), &t.get_range())),
        _ => ()
    }
    return Ok((next,(Box::new(result), errors)));
}

fn parse_statement<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldParserError>)), GoldParserError>{
    // try match block statements first
    let last_error;
    // TODO can't use alt_parse, why?
    // match alt_parse([parse_if_block].as_ref())(input) {
    //     Ok(r) => return Ok(r),
    //     Err(e) => {last_error = e}
    // };
    match parse_if_block(input) {
        Ok(r) => return Ok(r),
        Err(e) => {last_error = e}
    };
    match alt_parse([
        parse_assignment,
        parse_expr,
    ].as_ref())(input) {
        Ok(r) => return Ok((r.0, (r.1, Vec::new()))),
        Err(e) => {
            if e.input.len() < last_error.input.len() {
                return Err(e)
            } else {
                return Err(last_error)
            }
        }
    };
}

fn parse_block<'a>(input :&'a[Token]) -> Result<(Vec<Box<dyn IAstNode>>, Vec<GoldParserError>), GoldParserError>{
    let mut result = Vec::<Box<dyn IAstNode>>::new();
    let mut errors = Vec::<GoldParserError>::new();
    if input.len() == 0 {
        return Ok((result, errors));
     }
    let mut next = input;
    while next.len() > 0 {
        next = match parse_statement(next){
            Ok((r,(n,e)))=> {
                result.push(n);
                errors.extend(e.into_iter());
                r
            },
            Err(e)=> {
                let mut it = e.input.iter();
                // try parsing from next elem in next loop
                it.next();
                errors.push(e);
                it.as_slice()
            }
        };
    }
    return Ok((result, errors));
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
        range: create_new_range_from_irange(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })))
}

#[cfg(test)]
mod test{

    use std::ops::Deref;

    use crate::ast::{AstBinaryOp, AstCast, AstTerminal, AstUnaryOp, AstMethodCall, IAstNode, AstIfBlock};
    use crate::utils::{print_ast_brief_recursive, inorder, print_ast_brief, dfs, IRange, create_new_range_from_irange, bfs};
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors, parse_dot_ops, parse_cast, parse_bracket_closure, parse_bit_ops_2, parse_shifts, parse_compare, parse_logical_or, parse_unary_op, parse_method_call, parse_assignment, parse_if_block};


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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
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

    #[test]
    fn test_parse_assignment(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let (next, node) = parse_assignment(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let bfs = bfs(bin_op);
        // expect
        //         =
        //        / \
        //       .   Third
        //      / \
        //   First Second
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(0).unwrap().get_value());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(2).unwrap().get_value());
    }

    #[test]
    fn test_if_only(){
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Fourth".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Fifth".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        let bfs = bfs(if_node);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // expect
        //              if
        //              |
        //            cond if
        //         /           \
        //      =(eq)      statements
        //      /   \           |
        //    first second     ...
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().get_pos().to_string());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(0).unwrap().get_pos().to_string());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(2).unwrap().get_value());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(5).unwrap().get_value());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(bfs.get(6).unwrap().data.get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(bfs.get(7).unwrap().data.get_identifier(), input.get(7).unwrap().get_value());
        assert_eq!(bfs.get(8).unwrap().data.get_identifier(), input.get(6).unwrap().get_value());
        assert_eq!(bfs.get(9).unwrap().data.get_identifier(), input.get(8).unwrap().get_value());
    }

    #[test]
    fn test_if_else(){
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Else, Some("else".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 2);
    }

    #[test]
    fn test_if_elseif_else(){
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::ElseIf, Some("elseif".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::Identifier, Some("Fourth".to_string())),
            (TokenType::Else, Some("else".to_string())),
            (TokenType::Identifier, Some("Fifth".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(1).unwrap().get_children().unwrap().len(), 2);
    }

    #[test]
    fn test_if_elseif_else_empty(){
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::ElseIf, Some("elseif".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::Else, Some("else".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(1).unwrap().get_children().unwrap().len(), 1);
    }

    #[test]
    fn test_if_nested(){
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::ElseIf, Some("if".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block(&input).unwrap();
        println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
        
        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(1).unwrap().get_children().unwrap().len(), 1);
    }
}
