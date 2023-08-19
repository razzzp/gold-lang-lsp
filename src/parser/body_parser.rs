use nom::error;

use crate::{lexer::tokens::{Token, TokenType}, ast::{IAstNode, AstTerminal, AstBinaryOp, AstCast, AstUnaryOp, AstMethodCall, AstIfBlock, AstConditionalBlock, AstEmpty, AstForBlock, AstForEachBlock, AstWhileBlock, AstLoopBlock, AstLocalVariableDeclaration}, utils::{create_new_range_from_irange, IRange, create_new_range, create_new_range_from_token_slices}, parser::take_until};

use super::{GoldParserError, exp_token, utils::prepend_msg_to_error, alt_parse, parse_type_basic, parse_separated_list, create_closure, GoldDocumentError, parse_comment, opt_parse, parse_type, parse_constant_declaration};

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
    // TODO need to fix other pos also
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
        exp_token(TokenType::In)
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

#[deprecated]
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
                    condition: Some(if_cond_node),
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
                    //  need to assign to tmp var first, or compiler error
                    let tmp : Box<dyn IAstNode> = Box::new(AstEmpty::new(t.get_raw_pos(),t.get_pos(), t.get_range()));
                    (cur_block_tokens, tmp)
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
                            condition: Some(cur_cond_node),
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

#[deprecated]
fn parse_if_block_v2<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldParserError>)), GoldParserError>{
    let (next, if_token) = exp_token(TokenType::If)(input)?;
    
    let stop_tokens = [
        exp_token(TokenType::ElseIf),
        exp_token(TokenType::Else),
        exp_token(TokenType::EndIf), 
        exp_token(TokenType::End)
    ];
    let (next, if_cond_node) = parse_expr(next)?;
    // initialize the if node
    let mut result: AstIfBlock = AstIfBlock{
        raw_pos: if_token.get_raw_pos(),
        pos: if_token.get_pos(),
        range: create_new_range_from_irange(if_token.as_range(), if_cond_node.as_range()),
        if_block: Box::new(AstConditionalBlock{
            raw_pos: if_token.get_raw_pos(),
            pos: if_token.get_pos(),
            range: create_new_range_from_irange(if_token.as_range(), if_token.as_range()),
            condition: Some(if_cond_node),
            statements: Vec::new()
        }),
        else_if_blocks: None,
        end_token: None
    };
    let mut errors: Vec<GoldParserError> = Vec::new();
    let mut cur_cond_block = result.if_block.as_mut();
    let mut next = next;
    // parse statements until it reaches the stop tokens
    let (next, end_token)  = loop {
        if next.len() == 0 {break (next, None);}
        // check if it sees a delimiting token
        next = match alt_parse(&stop_tokens)(next) {
            Ok((next, t)) => {            
                let (next, cur_cond_node) = match t.token_type{
                    TokenType::EndIf | TokenType::End => {
                        // if endif finished parsing, break
                        update_cond_block_range(cur_cond_block);
                        break (next, Some(t));
                    },
                    // else create new block and continue parsing
                    TokenType::ElseIf => {
                        update_cond_block_range(cur_cond_block);
                        parse_expr(next)?
                    },
                    TokenType::Else => {
                        update_cond_block_range(cur_cond_block);
                        let tmp : Box<dyn IAstNode> = Box::new(AstEmpty::new(t.get_raw_pos(),t.get_pos(), t.get_range()));
                        (next, tmp)
                    },
                    _ => {
                        return Err(GoldParserError { input: next, msg: "error while parsing if, something went wrong".to_string() })
                    }
                };
                // create new block to append to
                result.add_else_if_block(Box::new(AstConditionalBlock { 
                    raw_pos: t.get_raw_pos(), 
                    pos: t.get_pos(),
                    range: t.get_range(), 
                    condition: Some(cur_cond_node), 
                    statements: Vec::new() 
                }));
                cur_cond_block = result.else_if_blocks.as_mut().unwrap().last_mut().unwrap();
                next
            },
            Err(_) => {
                // parse statements and adds to current block
                let new_statement_node; let errs;
                (next, (new_statement_node, errs)) = parse_statement(next)?;
                errors.extend(errs.into_iter());
                cur_cond_block.append_node(new_statement_node);
                next
            }
        };
    };
    match end_token {
        Some(t) => {
            result.set_range(create_new_range(result.get_range(), t.get_range()));
            result.end_token = Some(t);
            return Ok((next,(Box::new(result), errors)));
        },
        None => {
            return Err(GoldParserError { input: next, msg: "unclosed if block".to_string() })
        }
    }
    
}

fn parse_if_block_v3<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    let (next, if_token) = exp_token(TokenType::If)(input)?;

    let stop_tokens = [
        exp_token(TokenType::ElseIf),
        exp_token(TokenType::Else),
        exp_token(TokenType::EndIf), 
        exp_token(TokenType::End)
    ];
    let stop_parser = alt_parse(stop_tokens.as_ref());

    // parse first if conditional node
    let (next, if_cond_node) = parse_expr(next)?;
    // initialize the if node
    let mut result: AstIfBlock = AstIfBlock{
        raw_pos: if_token.get_raw_pos(),
        pos: if_token.get_pos(),
        range: create_new_range(if_token.get_range(), if_cond_node.get_range()),
        if_block: Box::new(AstConditionalBlock{
            raw_pos: if_token.get_raw_pos(),
            pos: if_token.get_pos(),
            range: create_new_range(if_token.get_range(), if_token.get_range()),
            condition: Some(if_cond_node),
            statements: Vec::new()
        }),
        else_if_blocks: None,
        end_token: None
    };
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut cur_cond_block = result.if_block.as_mut();
    let mut next = next;
    // parse statements until it reaches the stop tokens
    let (next, end_token)  = loop {
        if next.len() == 0 {break (next, None);}
        
        let nodes; let errors_until; let end_token;
        // parse statements until it sees an end token
        (next, nodes, errors_until, end_token) = parse_until(next, &stop_parser, parse_statement_v2);
        // update cur block
        cur_cond_block.statements.extend(nodes.into_iter());
        errors.extend(errors_until.into_iter());

        // decide what to do based on the end token found
        next = match end_token {
            Some(t) => {            
                let (next, cur_cond_node) = match t.token_type{
                    TokenType::EndIf | TokenType::End => {
                        // if endif finished parsing, break
                        update_cond_block_range(cur_cond_block);
                        // break out of loop
                        break (next, Some(t));
                    },
                    // else create new block and continue parsing
                    TokenType::ElseIf => {
                        update_cond_block_range(cur_cond_block);
                        match parse_expr(next) {
                            Ok((n, node)) => (n, Some(node)),
                            Err(e) => return Err(e)
                        }
                    },
                    TokenType::Else => {
                        update_cond_block_range(cur_cond_block);
                        (next, None)
                    },
                    _ => {
                        return Err(GoldParserError { input: next, msg: "error while parsing if, something went wrong".to_string() })
                    }
                };
                // create new block to append to
                result.add_else_if_block(Box::new(AstConditionalBlock { 
                    raw_pos: t.get_raw_pos(), 
                    pos: t.get_pos(),
                    range: t.get_range(), 
                    condition: cur_cond_node, 
                    statements: Vec::new() 
                }));
                cur_cond_block = result.else_if_blocks.as_mut().unwrap().last_mut().unwrap();
                next
            },
            None => {
                // add error: no end token found
                errors.push(GoldDocumentError { range: if_token.get_range(), msg: "no end token found".to_string()});
                next
            }
        };
    };
    match end_token {
        Some(t) => {
            result.set_range(create_new_range(result.get_range(), t.get_range()));
            result.end_token = Some(t);
            return Ok((next,(Box::new(result), errors)));
        },
        None => {
            return Ok((next,(Box::new(result), errors)));
        }
    }
}

// fn parse_for_block_condition<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldParserError>)), GoldParserError>{
// }

fn parse_for_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    let (next, for_token) = exp_token(TokenType::For)(input)?;
    let (next, var_token) = exp_token(TokenType::Identifier)(next)?;
    let (next, eq_token) = exp_token(TokenType::Equals)(next)?;

    let range_op_tokens = [exp_token(TokenType::To), exp_token(TokenType::DownTo)];
    let range_op_parsers = alt_parse(&range_op_tokens);
    let (next, range_node) = parse_binary_ops(next, &range_op_parsers, &parse_expr)?;
    let (mut next, step_token) = opt_parse(exp_token(TokenType::Step))(next)?;

    let mut step_expr: Option<Box<dyn IAstNode>>= None;
    if step_token.is_some(){
        (next, step_expr) = opt_parse(parse_expr)(next)?;
    }
    
    let stop_tokens = [exp_token(TokenType::EndFor), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, errors, end_token) = parse_until(next, &stop_parser, &parse_statement_v2);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => for_token.clone()
    };
    return Ok((
        next, 
        (Box::new(AstForBlock{
            raw_pos: for_token.get_raw_pos(),
            pos: for_token.get_pos(),
            range: create_new_range(for_token.get_range(), end_pos_token.get_range()),
            counter_token: var_token,
            range_node,
            step_node: step_expr,
            statements: Some(statement_nodes),
            end_token
        }),
        errors)));
}

fn parse_foreach_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    let (next, foreach_token) = exp_token(TokenType::ForEach)(input)?;
    let in_op_parser = exp_token(TokenType::In);
    let (next, in_expr_node) = parse_binary_ops(next, &in_op_parser, &parse_expr)?;
    
    let stop_tokens = [exp_token(TokenType::EndFor), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, errors, end_token) = parse_until(next, &stop_parser, &parse_statement_v2);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => foreach_token.clone()
    };
    return Ok((
        next, 
        (Box::new(AstForEachBlock{
            raw_pos: foreach_token.get_raw_pos(),
            pos: foreach_token.get_pos(),
            range: create_new_range(foreach_token.get_range(), end_pos_token.get_range()),
            in_expr_node,
            statements: Some(statement_nodes),
            end_token
        }),
        errors)));
}

fn parse_while_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    let (next, while_token) = exp_token(TokenType::While)(input)?;
    let (next, cond_node) = parse_expr(next)?;
    
    
    let stop_tokens = [exp_token(TokenType::EndWhile), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, errors, end_token) = parse_until(next, &stop_parser, &parse_statement_v2);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => while_token.clone()
    };
    let cond_block_end = match statement_nodes.last() {
        Some(n) => n.get_range(),
        _ => cond_node.get_range()
    };
    let result = AstWhileBlock{
        raw_pos: while_token.get_raw_pos(),
        pos: while_token.get_pos(),
        range: create_new_range(while_token.get_range(), end_pos_token.get_range()),
        cond_block: Box::new(AstConditionalBlock { 
            raw_pos: cond_node.get_raw_pos(), 
            pos: cond_node.get_pos(), 
            range: create_new_range(cond_node.get_range(), cond_block_end), 
            condition: Some(cond_node), 
            statements: statement_nodes 
        }),
        end_token
    };
    return Ok((
        next, 
        (Box::new(result),
        errors)));
}

fn parse_loop_block<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    let (next, loop_token) = exp_token(TokenType::Loop)(input)?;
    
    let stop_tokens = [exp_token(TokenType::EndLoop), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, errors, end_token) = parse_until(next, &stop_parser, &parse_statement_v2);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => loop_token.clone()
    };
    let result = AstLoopBlock{
        raw_pos: loop_token.get_raw_pos(),
        pos: loop_token.get_pos(),
        range: create_new_range(loop_token.get_range(), end_pos_token.get_range()),
        statements: statement_nodes,
        end_token
    };
    return Ok((
        next, 
        (Box::new(result),
        errors)));
}

fn update_cond_block_range(cond_block: &mut AstConditionalBlock){
    let end_node = if cond_block.statements.len() > 0 {
        // get last statement node, if present
        cond_block.statements.last().unwrap().as_ast_node()
    } else {
        // else use the condition node
        if cond_block.condition.is_some() {cond_block.condition.as_ref().unwrap().as_ast_node()}
        else {cond_block.as_ast_node()}
    };
    cond_block.set_range(create_new_range(cond_block.get_range(), end_node.get_range()))
}

#[deprecated]
fn parse_statement<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldParserError>)), GoldParserError>{
    // try match block statements first
    let last_error;
    // TODO can't use alt_parse, why?
    // match alt_parse([parse_if_block].as_ref())(input) {
    //     Ok(r) => return Ok(r),
    //     Err(e) => {last_error = e}
    // };
    match parse_if_block_v2(input) {
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

fn parse_local_var_decl<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
    // var
    let (next, var_token) = exp_token(TokenType::Var)(input)?;
    // ident
    let (next, identifier_token) = exp_token(TokenType::Identifier)(next)?;
    // colon token
    let (next, _) = exp_token(TokenType::Colon)(next)?;
    // parse type
    let (next, type_node) = parse_type(next)?;
    let mut end_range = type_node.get_range();
    // parse absolute
    let (mut next, abs_token) = opt_parse(exp_token(TokenType::Absolute))(next)?;
    let mut absolute_node : Option<Box<dyn IAstNode>>= None;
    if abs_token.is_some() {
        (next, absolute_node) = match parse_dot_op_left(next) {
            Ok((n, node)) => {
                end_range = node.get_range();
                (n, Some(node))
            },
            Err(e) => return Err(e)
        };
    }

    return Ok((
        next,
        Box::new(AstLocalVariableDeclaration {
            raw_pos: var_token.get_raw_pos(),
            pos: var_token.get_pos(),
            range: create_new_range(var_token.get_range(), end_range),
            identifier: identifier_token,
            type_node: type_node,
            absolute: absolute_node,
        })
    ));
 }

pub fn parse_statement_v2<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>{
    // try match block statements first
    let mut last_error;
    // TODO can't use alt_parse, why?
    // match alt_parse([parse_if_block].as_ref())(input) {
    //     Ok(r) => return Ok(r),
    //     Err(e) => {last_error = e}
    // };
    match parse_if_block_v3(input) {
        Ok(r) => return Ok(r),
        Err(e) => {last_error=e}
    };
    match parse_for_block(input) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_foreach_block(input) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_while_block(input) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_loop_block(input) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    
    match alt_parse([
        parse_comment,
        parse_local_var_decl,
        parse_constant_declaration,
        parse_assignment,
        parse_expr,
    ].as_ref())(input) {
        Ok(r) => return Ok((r.0, (r.1, Vec::new()))),
        Err(e) => {
            let most_matched = if e.input.len() < last_error.input.len() {e} else {last_error};
            return Err(most_matched)
        }
    }
}

#[deprecated]
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

pub fn parse_until<'a>(
    input: &'a[Token],
    stop_parser: impl Fn(&[Token]) -> Result<(&[Token],  Token), GoldParserError>,
    parser: impl Fn(&[Token]) -> Result<(&[Token],  (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>
) -> (&'a [Token], Vec<Box<dyn IAstNode>>, Vec<GoldDocumentError>, Option<Token>){

    let mut result: Vec<Box<dyn IAstNode>> = Vec::new();
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {break}
        // check if it sees a delimiting token
        next = match stop_parser(next) {
            Ok((next, t)) => {            
                return (next, result, errors, Some(t));
            },
            Err(_) => {
                // parse statements and adds to current block
                let new_next = match parser(next){
                    Ok((next, (new_statement_node, errs)))=> {
                        result.push(new_statement_node);
                        errors.extend(errs.into_iter());
                        next
                    },
                    Err(e) => {
                        let error_at = match e.input.first() {
                            Some(t) => t.get_range(),
                            None => Default::default(),
                        };
                        errors.push(GoldDocumentError { range: error_at, msg: "unexpected token".to_string() });
                        let mut new_it = e.input.iter();
                        // if iterator has not been moved, move by one, to prevent
                        //  infinite loop
                        if e.input.len() == next.len() {new_it.next();}
                        new_it.as_slice()
                    }
                };
                new_next
            }
        };
    };
    // end token not found
    return (next, result, errors, None);
}

pub fn parse_repeat<'a>(
    input: &'a[Token],
    parser: impl Fn(&[Token]) -> Result<(&[Token],  (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError>
) -> (&'a [Token], Vec<Box<dyn IAstNode>>, Vec<GoldDocumentError>){

    let mut result: Vec<Box<dyn IAstNode>> = Vec::new();
    let mut errors: Vec<GoldDocumentError> = Vec::new();
    let mut next = input;
    loop {
        if next.len() == 0 {break}
        // parse statements and adds to current block
        next = match parser(next){
            Ok((next, (new_statement_node, errs)))=> {
                result.push(new_statement_node);
                errors.extend(errs.into_iter());
                next
            },
            Err(e) => {
                let error_at = match e.input.first() {
                    Some(t) => t.get_range(),
                    None => Default::default(),
                };
                errors.push(GoldDocumentError { range: error_at, msg: "unexpected token".to_string() });
                let mut new_it = e.input.iter();
                // if iterator has not been moved, move by one, to prevent
                //  infinite loop
                if e.input.len() == next.len() {new_it.next();}
                new_it.as_slice()
            }
        };          
    };
    // end token not found
    return (next, result, errors);
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

    use crate::ast::{AstBinaryOp, AstCast, AstTerminal, AstUnaryOp, AstMethodCall, IAstNode, AstIfBlock, AstForBlock, AstForEachBlock, AstWhileBlock, AstLoopBlock, AstLocalVariableDeclaration};
    use crate::parser::test::check_node_pos_and_range;
    use crate::utils::{print_ast_brief_recursive, inorder, print_ast_brief, dfs, IRange, create_new_range_from_irange, bfs};
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors, parse_dot_ops, parse_cast, parse_bracket_closure, parse_bit_ops_2, parse_shifts, parse_compare, parse_logical_or, parse_unary_op, parse_method_call, parse_assignment, parse_if_block, parse_if_block_v2, parse_if_block_v3, parse_for_block, parse_foreach_block, parse_while_block, parse_loop_block, parse_local_var_decl};


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
        check_node_pos_and_range(node.as_ast_node(), &input);
        
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        let (next, (node, errors)) = parse_if_block_v3(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
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
        let (next, (node, errors)) = parse_if_block_v3(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 1);
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
        let (next, (node, errors)) = parse_if_block_v3(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(1).unwrap().get_children().unwrap().len(), 1);
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
        let (next, (node, errors)) = parse_if_block_v3(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(0).unwrap().get_children().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.as_ref().unwrap().get(1).unwrap().get_children().unwrap().len(), 0);
    }

    #[test]
    fn test_if_nested(){
        // if First
        //     if Third
        //         Fourth
        //         Fifth
        //     endif
        //     Sixth
        //     Seventh
        // endif
        let input = gen_list_of_tokens(&[
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::If, Some("if".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::Identifier, Some("Fourth".to_string())),
            (TokenType::Identifier, Some("Fifth".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            (TokenType::Identifier, Some("Sixth".to_string())),
            (TokenType::Identifier, Some("Seventh".to_string())),
            (TokenType::EndIf, Some("endif".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_if_block_v3(&input).unwrap();
        println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        
        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children().unwrap().len(), 4);
        assert!(if_node.else_if_blocks.is_none());
        let children = if_node.if_block.get_children().unwrap();
        let nested_if = children.get(1).unwrap();
        let nested_if = nested_if.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(nested_if.if_block.get_children().unwrap().len(), 3);

    }

    #[test]
    fn test_for_block_empty(){
        let input = gen_list_of_tokens(&[
            (TokenType::For, Some("for".to_string())),
            (TokenType::Identifier, Some("Counter".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::To, Some("to".to_string())),
            (TokenType::NumericLiteral, Some("30".to_string())),
            (TokenType::Step, Some("step".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),
            
        ]);
        let (next, (node, errors)) = parse_for_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let for_node = node.as_any().downcast_ref::<AstForBlock>().unwrap();
        // check counter var
        assert_eq!(for_node.counter_token, input.get(1).unwrap().clone());
        assert_eq!(for_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        let bfs = bfs(for_node);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // expect
        //                  for (counter_token)
        //              /           |            \
        //    range(to/downton)    step          statements
        //         /   \            |              |
        //       1      30      expr_node      ... 
   
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(5).unwrap().get_value());
        
    }

    #[test]
    fn test_for_block_with_statements(){
        let input = gen_list_of_tokens(&[
            (TokenType::For, Some("for".to_string())),
            (TokenType::Identifier, Some("Counter".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::To, Some("to".to_string())),
            (TokenType::NumericLiteral, Some("30".to_string())),
            (TokenType::Step, Some("step".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::Identifier, Some("Temp1".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Temp2".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),     
        ]);
        let (next, (node, errors)) = parse_for_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let for_node = node.as_any().downcast_ref::<AstForBlock>().unwrap();
        // check counter var
        assert_eq!(for_node.counter_token, input.get(1).unwrap().clone());
        assert_eq!(for_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        let bfs = bfs(for_node);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // expect
        //                  for (counter_token)
        //              /           |            \
        //    range(to/downton)    step          statements
        //         /   \            |              |
        //       1      30      expr_node        ... 
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value());
        // body here
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(9).unwrap().get_value());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(5).unwrap().get_value());
        
    }

    #[test]
    fn test_for_block_nested(){
        let input = gen_list_of_tokens(&[
            (TokenType::For, Some("for".to_string())),
            (TokenType::Identifier, Some("Counter".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::To, Some("to".to_string())),
            (TokenType::NumericLiteral, Some("30".to_string())),
            (TokenType::Step, Some("step".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::For, Some("for".to_string())),
            (TokenType::Identifier, Some("Counter".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Temp2".to_string())),
            (TokenType::DownTo, Some("downto".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),
        ]);
        let (next, (node, errors)) = parse_for_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let for_node = node.as_any().downcast_ref::<AstForBlock>().unwrap();
        // check counter var
        assert_eq!(for_node.counter_token, input.get(1).unwrap().clone());
        assert_eq!(for_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        let bfs = bfs(for_node);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // expect
        //                  for (counter_token)
        //              /           |            \
        //    range(to/downton)    step          statements
        //         /   \            |              |
        //       1      30      expr_node        ... 
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value());
        // body here, check that it is a for node
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(8).unwrap().to_string_val_and_pos());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(3).unwrap().get_value());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(5).unwrap().get_value());
        
    }

    #[test]
    fn test_foreach_block_with_statements(){
        let input = gen_list_of_tokens(&[
            (TokenType::ForEach, Some("foreach".to_string())),
            (TokenType::Identifier, Some("LoopVar".to_string())),
            (TokenType::In, Some("in".to_string())),
            (TokenType::Identifier, Some("List".to_string())),
            (TokenType::Identifier, Some("Temp1".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Temp2".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),     
        ]);
        let (next, (node, errors)) = parse_foreach_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let foreach_node = node.as_any().downcast_ref::<AstForEachBlock>().unwrap();
        // check counter var
        assert_eq!(foreach_node.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(foreach_node.in_expr_node.to_string_ident_pos(), format!("in:{}", input.get(1).unwrap().get_pos().to_string_brief()));
        assert_eq!(foreach_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        assert_eq!(foreach_node.statements.as_ref().unwrap().first().unwrap().to_string_ident_pos(), format!("=:{}", input.get(4).unwrap().get_pos().to_string_brief()));
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // expect
        //                foreach 
        //              /           \
        //    in_expr_node       statements
        //         /     \             |
        //       left    right        ... 
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });      
    }

    #[test]
    fn test_while_block_with_statements(){
        let input = gen_list_of_tokens(&[
            (TokenType::While, Some("while".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::LessThan, Some("<".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Identifier, Some("Temp1".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Temp2".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::EndWhile, Some("endwhile".to_string())),     
        ]);
        let (next, (node, errors)) = parse_while_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let while_node = node.as_any().downcast_ref::<AstWhileBlock>().unwrap();
        // check counter var
        assert_eq!(while_node.get_identifier(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(while_node.cond_block.get_identifier(), format!("cond_block:{}",input.get(1).unwrap().get_pos().to_string_brief()));
        assert_eq!(while_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });      
    }

    #[test]
    fn test_loop_block_with_statements(){
        let input = gen_list_of_tokens(&[
            (TokenType::Loop, Some("loop".to_string())),
            (TokenType::Identifier, Some("Temp1".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("Temp2".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::EndLoop, Some("endloop".to_string())),     
        ]);
        let (next, (node, errors)) = parse_loop_block(&input).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(errors.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let while_node = node.as_any().downcast_ref::<AstLoopBlock>().unwrap();
        // check counter var
        assert_eq!(while_node.to_string_type_pos(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(while_node.get_children().unwrap().len(), 1);
        assert_eq!(while_node.get_children().unwrap().first().unwrap().get_identifier(), "=".to_string());
        assert_eq!(while_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        // bfs.iter().for_each(|n|{
        //     println!("{}", print_ast_brief(n.data))
        // });      
    }

    #[test]
    fn test_local_var_decl(){
        let input = gen_list_of_tokens(&[
            (TokenType::Var, Some("var".to_string())),
            (TokenType::Identifier, Some("VarName".to_string())),
            (TokenType::Colon, Some(":".to_string())),
            (TokenType::Identifier, Some("SomeType".to_string())), 
        ]);
        let (next, node) = parse_local_var_decl(&input).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let var_decl = node.as_any().downcast_ref::<AstLocalVariableDeclaration>().unwrap();
        
        // check ident
        assert_eq!(var_decl.get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(var_decl.get_children().unwrap().len(), 1);  
    }

    #[test]
    fn test_local_var_with_abs(){
        let input = gen_list_of_tokens(&[
            (TokenType::Var, Some("var".to_string())),
            (TokenType::Identifier, Some("VarName".to_string())),
            (TokenType::Colon, Some(":".to_string())),
            (TokenType::Identifier, Some("SomeType".to_string())), 
            (TokenType::Absolute, Some("absolute".to_string())),
            (TokenType::Identifier, Some("OtheVar".to_string())), 
        ]);
        let (next, node) = parse_local_var_decl(&input).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let var_decl = node.as_any().downcast_ref::<AstLocalVariableDeclaration>().unwrap();
        
        // check ident
        assert_eq!(var_decl.get_identifier(), input.get(1).unwrap().get_value());
        assert_eq!(var_decl.get_children().unwrap().len(), 2);
        // check abs
        assert_eq!(var_decl.get_children().unwrap().get(1).unwrap().get_identifier(), input.last().unwrap().get_value());  
    }
}
