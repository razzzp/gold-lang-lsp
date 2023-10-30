
use std::sync::Arc;

use crate::{lexer::tokens::{Token, TokenType}, parser::ast::{IAstNode, AstTerminal, AstBinaryOp, AstCast, AstUnaryOp, AstMethodCall, AstIfBlock, AstConditionalBlock, AstEmpty, AstForBlock, AstForEachBlock, AstWhileBlock, AstLoopBlock, AstLocalVariableDeclaration, AstReturnNode, AstSetLiteral, AstWhenBlock, AstSwitchBlock}, utils::{create_new_range_from_irange, IRange, create_new_range, Range}};

use super::{ParseError, exp_token, utils::{parse_separated_list_w_context, alt_parse_w_context, parse_until_w_context, parse_until_no_match_w_context, opt_parse_w_context}, alt_parse, parse_type_basic, ParserDiagnostic, parse_comment, opt_parse, parse_type, parse_constant_declaration, parse_uses, parse_type_declaration, ast::{AstArrayAccess, AstRepeatBlock}, IParserContext, ParserContext, oql_parser::parse_oql_expr, CACHE_PARSE_PRIMARY, CACHE_PARSE_EXPR, ParseCache};

/// expr = ident
///     | bin_op
/// 
/// bin_op = expr + expr
///     | expr - expr
/// 
/// bin_op = bin_op + expr
/// bin_op = 
/// 

#[allow(unused)]
struct BlockParser<'a> {
    errors: Vec<ParseError<'a>>
} 

#[allow(unused)]
impl<'a> BlockParser<'a> {
    pub fn new(input : &'a [Token]) -> BlockParser<'a>{
        BlockParser{
            errors: Vec::new()
        }
    }
}


fn parse_literal_set<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    let (next, obracket_token) = exp_token(TokenType::OSqrBracket)(input)?;
    let (next, set_items) = parse_separated_list_w_context(parse_primary, TokenType::Comma)(next,context)?;
    let (next, cbracket_token) = exp_token(TokenType::CSqrBracket)(next)?;
    return Ok((next, Arc::new(AstSetLiteral{
        raw_pos: obracket_token.get_raw_pos(),
        range: create_new_range(obracket_token.get_range(), cbracket_token.get_range()),
        set_items,
    })))
}

pub fn parse_literal_basic<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], _context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    let (next, ident_token) = alt_parse(&[
        exp_token(TokenType::StringLiteral),
        exp_token(TokenType::NumericLiteral),
        exp_token(TokenType::BooleanTrue),
        exp_token(TokenType::BooleanFalse),
        exp_token(TokenType::Nil),
    ])(input)?;
    return Ok((next, Arc::new(AstTerminal{
        token: ident_token
    })))
}

pub fn parse_ident_token<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], _context : &mut C) -> Result<(&'a [Token], Token), ParseError<'a>> {
    // keywords can also me used as member identifiers
    return alt_parse(&[
        exp_token(TokenType::Identifier),
        exp_token(TokenType::Type),
        exp_token(TokenType::Distinct),
        exp_token(TokenType::From),
        exp_token(TokenType::Select),
        exp_token(TokenType::Top),
        exp_token(TokenType::Using),
        exp_token(TokenType::Where),
        exp_token(TokenType::AllVersionsOf),
        exp_token(TokenType::PhantomsToo),
        exp_token(TokenType::Conditional),
        exp_token(TokenType::Descending),
        exp_token(TokenType::Order),
        exp_token(TokenType::By),
        exp_token(TokenType::Fetch),
        exp_token(TokenType::Into),
    ])(input);
}

pub fn parse_identifier<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    // reserved keywords can also be used a class members
    let (next, ident_token) = parse_ident_token(input, context)?;
    return Ok((next, Arc::new(AstTerminal{
        token: ident_token
    })))
}

fn parse_literals<'a, C: IParserContext<'a> + 'a >(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    return alt_parse_w_context([
        parse_literal_basic,
        parse_literal_set,
    ].as_ref())(input, context);
}

fn parse_method_call<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    if let Some(cached_result) = context.get_cache(ParseCache::ParseMethodCall.into_usize(),input.len()) {
        return  cached_result;
    }
    let (next, ident_node) = parse_identifier(input, context)?;
    let (next, _) = exp_token(TokenType::OBracket)(next)?;
    let (next, parameter_list) = parse_separated_list_w_context(parse_expr, TokenType::Comma)(next, context)?;
    let (next, cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    let result : Result<(_, Arc<dyn IAstNode>), _> = Ok((next, Arc::new(AstMethodCall{
        raw_pos: ident_node.get_raw_pos(),
        pos: ident_node.get_pos(),
        range: create_new_range_from_irange(ident_node.as_range(), cbracket_token.as_range()),
        identifier: ident_node,
        parameter_list,
    })));
    context.set_cache(ParseCache::ParseMethodCall.into_usize(),input.len(), result);
    return context.get_cache(ParseCache::ParseMethodCall.into_usize(), input.len()).unwrap();
    // return result;
}

fn parse_array_access<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    let (next, ident_node) = parse_identifier(input, context)?;
    let (next, _) = exp_token(TokenType::OSqrBracket)(next)?;
    let (next, index_node) = parse_expr(next, context)?;
    let (next, cbracket_token) = exp_token(TokenType::CSqrBracket)(next)?;
    return Ok((next, Arc::new(AstArrayAccess{
        raw_pos: ident_node.get_raw_pos(),
        range: create_new_range(ident_node.get_range(), cbracket_token.get_range()),
        left_node: ident_node,
        index_node,
    })))
}

// fn parse_dot_op_left<'a>(input: &'a[Token]) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError> {
//     let (next, ident_node) = parse_identifier(input)?;
//     return Ok((next, ident_node))
// }

fn parse_dot_op<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    let parsers = [
        parse_method_call,
        parse_array_access,
        parse_identifier,
        ];
    return alt_parse_w_context(&parsers)(input, context);
}

pub fn parse_dot_ops<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_parser = exp_token(TokenType::Dot);
    return parse_binary_ops_w_context(input, &op_parser, &parse_dot_op, context);
} 

fn parse_bracket_closure<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    // should we make a separate node for this?
    let (next, _obracket_token) = exp_token(TokenType::OBracket)(input)?;
    let (next, expr_node) = parse_expr(next, context)?;
    let (next, _cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    // TODO need to fix other pos also
    // expr_node.set_range(create_new_range_from_irange(obracket_token.as_range(), cbracket_token.as_range()));
    return Ok((next,expr_node))
}


fn parse_unary_op_pre<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_parsers = [
        exp_token(TokenType::Not),
        exp_token(TokenType::BNot),
        exp_token(TokenType::AddressOf),
        exp_token(TokenType::Inherited),
        exp_token(TokenType::Minus),
    ];
    let (next, op_token) = alt_parse(&op_parsers)(input)?;
    let (next, expr_node) = parse_primary(next, context)?;
    return Ok((next, Arc::new(AstUnaryOp{
        raw_pos: op_token.get_raw_pos(),
        pos: op_token.get_pos(),
        range: create_new_range_from_irange(op_token.as_range(), expr_node.as_range()),
        op_token,
        expr_node
    })))
}

fn parse_unary_op_post<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_parsers = [
        exp_token(TokenType::Increment),
        exp_token(TokenType::Decrement),
    ];
    let (next, expr_node) = parse_dot_ops(input, context)?;
    let (next, op_token) = alt_parse(&op_parsers)(next)?;
    return Ok((next, Arc::new(AstUnaryOp{
        raw_pos: expr_node.get_raw_pos(),
        pos: expr_node.get_pos(),
        range: create_new_range_from_irange(expr_node.as_range(), op_token.as_range()),
        op_token,
        expr_node
    })))
}

fn parse_unary_op<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        parse_unary_op_pre,
        parse_unary_op_post,
    ].as_ref())(input,context);
}

pub fn parse_primary<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    if let Some(cached_result) = context.get_cache(ParseCache::ParsePrimary.into_usize(),input.len()) {
        return  cached_result;
    }

    let parsers = [
        parse_bracket_closure,
        parse_unary_op,
        parse_dot_ops,
        parse_literals,
    ];
    let result = alt_parse_w_context(&parsers)(input,context);
    context.set_cache(ParseCache::ParsePrimary.into_usize(),input.len(), result);
    return context.get_cache(ParseCache::ParsePrimary.into_usize(), input.len()).unwrap();
    // return result;
}

fn parse_factors<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::Asterisk),
        exp_token(TokenType::Divide),
        exp_token(TokenType::Modulus)
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_primary, context);
} 

fn parse_terms<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::Plus),
        exp_token(TokenType::Minus),
        exp_token(TokenType::StringConcat),
        exp_token(TokenType::StringConcat2),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_factors, context);
} 

fn parse_bit_ops_1<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::BAnd),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_terms, context);
} 


fn parse_bit_ops_2<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::BOr),
        exp_token(TokenType::BXor),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_bit_ops_1, context);
} 

fn parse_shifts<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::LeftShift),
        exp_token(TokenType::RightShift),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_bit_ops_2, context);
} 

pub fn parse_compare<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::Equals),
        exp_token(TokenType::NotEquals),
        exp_token(TokenType::LessThan),
        exp_token(TokenType::LessThanOrEqual),
        exp_token(TokenType::GreaterThan),
        exp_token(TokenType::GreaterThanOrEqual),
        exp_token(TokenType::In),
        exp_token(TokenType::Like),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_shifts, context);
} 

fn parse_logical_and<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::And),
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_compare, context);
}

fn parse_logical_or<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let op_token_parsers = [
        exp_token(TokenType::Or),
        exp_token(TokenType::Xor)
    ];
    let op_parser = alt_parse(&op_token_parsers);
    return parse_binary_ops_w_context(input, &op_parser, &parse_logical_and, context);
}

pub fn parse_expr<'a, C: IParserContext<'a> + 'a>(input : &'a [Token], context : &mut C) 
-> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> 
{
    if let Some(cached_result) = context.get_cache(ParseCache::ParseExpr.into_usize(),input.len()) {
        return  cached_result;
    }
    let parser = [
        parse_logical_or,
    ];
    let result =  alt_parse_w_context(&parser)(input, context);
    context.set_cache(ParseCache::ParseExpr.into_usize(), input.len(), result);
    return context.get_cache(ParseCache::ParseExpr.into_usize(), input.len()).unwrap();
    // return result;
}



fn parse_assignment<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, left_node) = parse_dot_ops(input, context)?;
    let op_parsers = [
        exp_token(TokenType::Equals),
        exp_token(TokenType::DecrementAssign),
        exp_token(TokenType::IncrementAssign),
        exp_token(TokenType::DeepAssign)
    ];
    let (next, op_token) = alt_parse(&op_parsers)(next)?;
    let (next, right_node) = parse_expr(next, context)?;
    return Ok((next, Arc::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        range: create_new_range_from_irange(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })));
}

fn parse_if_block_v3<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, if_token) = exp_token(TokenType::If)(input)?;

    let stop_tokens = [
        exp_token(TokenType::ElseIf),
        exp_token(TokenType::Else),
        exp_token(TokenType::EndIf), 
        exp_token(TokenType::End)
    ];
    let stop_parser = alt_parse(stop_tokens.as_ref());

    // parse first if conditional node
    let (next, if_cond_node) = parse_expr(next, context)?;
    let if_cond_node_range = if_cond_node.get_range();
    let mut if_block = AstConditionalBlock{
        raw_pos: if_token.get_raw_pos(),
        range: create_new_range(if_token.get_range(), if_token.get_range()),
        condition: Some(if_cond_node),
        statements: Vec::new()
    };
    // initialize the if node
    
    let mut cur_cond_block = &mut if_block;
    let mut next = next;
    let mut else_if_blocks = Vec::new();
    // parse statements until it reaches the stop tokens
    let (next, end_token)  = loop {
        if next.len() == 0 {break (next, None);}
        
        let nodes; let end_token;
        // parse statements until it sees an end token
        (next, nodes, end_token) = parse_until_w_context(next, &stop_parser, parse_statement_v2, context);
        // update cur block
        cur_cond_block.statements.extend(nodes.into_iter());

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
                        match parse_expr(next, context) {
                            Ok((n, node)) => (n, Some(node)),
                            Err(e) => return Err(e)
                        }
                    },
                    TokenType::Else => {
                        update_cond_block_range(cur_cond_block);
                        (next, None)
                    },
                    _ => {
                        return Err(ParseError { input: next, msg: "error while parsing if, something went wrong".to_string() })
                    }
                };
                // create new block to append to
                else_if_blocks.push(AstConditionalBlock { 
                    raw_pos: t.get_raw_pos(), 
                    range: t.get_range(), 
                    condition: cur_cond_node, 
                    statements: Vec::new() 
                });
                cur_cond_block = else_if_blocks.last_mut().unwrap();
                next
            },
            None => {
                // add error: no end token found
                context.add_diagnostic(ParserDiagnostic { range: if_token.get_range(), msg: "no end token found".to_string()});
                next
            }
        };
    };
    let mut result: AstIfBlock = AstIfBlock{
        raw_pos: if_token.get_raw_pos(),
        pos: if_token.get_pos(),
        range: create_new_range(if_token.get_range(), if_cond_node_range),
        if_block: Arc::new(if_block),
        else_if_blocks: else_if_blocks.into_iter().map(|b|{
            let b :Arc<dyn IAstNode>= Arc::new(b);
            b
        }).collect(),
        end_token: None
    };
    match end_token {
        Some(t) => {
            result.set_range(create_new_range(result.get_range(), t.get_range()));
            result.end_token = Some(t);
            return Ok((next,Arc::new(result)));
        },
        None => {
            return Ok((next,Arc::new(result)));
        }
    }
}

fn parse_to_op<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, left) = parse_literal_basic(input,context)?;
    let (next, op_tok) = exp_token(TokenType::To)(next)?;
    let (next, right) = parse_literal_basic(next,context)?;
    return Ok((
        next,
        Arc::new(AstBinaryOp{
            raw_pos: left.get_raw_pos(),
            range: create_new_range(left.get_range(), right.get_range()),
            op_token: op_tok,
            left_node: left,
            right_node: right
        })
    ))
}

fn parse_separated_values<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let parser = alt_parse_w_context([
        parse_literal_basic,
        parse_identifier
    ].as_ref());
    let (next, set_items) = parse_separated_list_w_context(parser, TokenType::Comma)(input, context)?;
    // TODO should we make seperate struct for this? for now reuse set literal
    let start; let end;
    if set_items.first().is_some() {
        start = set_items.first().unwrap().get_range();
        end = set_items.last().unwrap().get_range();
    } else {
        return Err(ParseError { input: next, msg: "Empty list".to_string() });
    }
    return Ok((next, Arc::new(AstSetLiteral{
        raw_pos: set_items.first().unwrap().get_raw_pos(),
        range: create_new_range(start, end),
        set_items,
    })))
}

fn parse_when_expr<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a[Token], Arc<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        parse_to_op,
        parse_separated_values,
    ].as_ref())(input, context);
}

fn parse_when_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a[Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, when_tok) = exp_token(TokenType::When)(input)?;
    let (next, when_expr) = parse_when_expr(next, context)?;
    let (next, statements, end_tok) = parse_until_w_context(next, exp_token(TokenType::EndWhen), parse_statement_v2, context);
    let end = end_tok.as_ref().unwrap_or(&when_tok).get_range();
    return Ok((
        next,
        Arc::new(AstWhenBlock { 
            raw_pos: when_tok.get_raw_pos(), 
            range: create_new_range(when_tok.get_range(), end), 
            when_expr: Some(when_expr), 
            statements: statements })
    ));
}

fn parse_switch_else_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a[Token], Option<Arc<AstWhenBlock>>, Option<Token>), ParseError<'a>>{
    let (next, else_tok) = match exp_token(TokenType::Else)(input){
        Ok((n, tok)) => (n, tok),
        Err(_e) => {
            // check end tok
            let (next, end_tok) = opt_parse(exp_token(TokenType::EndSwitch))(input)?;
            return Ok((next, None, end_tok)) 
        }
    };
    let mut end =  else_tok.get_range();
    let (next, else_statements, end_tok) = parse_until_w_context(next, exp_token(TokenType::EndSwitch), parse_statement_v2, context);
    if end_tok.is_some(){
        end = end_tok.as_ref().unwrap().get_range();
    }
    return Ok((
        next,
        Some(Arc::new(AstWhenBlock{
            raw_pos: else_tok.get_raw_pos(),
            range: create_new_range(else_tok.get_range(), end),
            when_expr: None,
            statements: else_statements,
        })),
        end_tok
    ))
}

fn parse_switch_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token],Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, switch_tok) = exp_token(TokenType::Switch)(input)?;
    let (next, switch_expr) = parse_expr(next, context)?;

    // parse when blocks
    let (next, mut when_blocks) = parse_until_no_match_w_context(next, parse_when_block, context);
    // else
    let (next, else_block, end_tok) = parse_switch_else_block(next, context)?;
    match else_block {
        Some(node) => when_blocks.push(node),
        _ => ()
    };
    let end = end_tok.as_ref().unwrap_or(&switch_tok).get_range();
    return Ok((
        next,
        (
            Arc::new(AstSwitchBlock{
            raw_pos: switch_tok.get_raw_pos(),
            range: create_new_range(switch_tok.get_range(), end),
            switch_expr: switch_expr,
            when_blocks: when_blocks,
            end_token: end_tok
            })
        )
    ))
}

// fn parse_for_block_condition<'a>(input: &'a[Token]) -> Result<(&'a [Token], (Arc<dyn IAstNode>, Vec<GoldParserError>)), GoldParserError>{
// }

fn parse_for_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, for_token) = exp_token(TokenType::For)(input)?;
    let (next, var_token) = exp_token(TokenType::Identifier)(next)?;
    let (next, _eq_token) = exp_token(TokenType::Equals)(next)?;

    let range_op_tokens = [exp_token(TokenType::To), exp_token(TokenType::DownTo)];
    let range_op_parsers = alt_parse(&range_op_tokens);
    let (next, range_node) = parse_binary_ops_w_context(next, &range_op_parsers, &parse_expr, context)?;
    let (mut next, step_token) = opt_parse(exp_token(TokenType::Step))(next)?;

    let mut step_expr: Option<Arc<dyn IAstNode>>= None;
    if step_token.is_some(){
        (next, step_expr) = opt_parse_w_context(parse_expr)(next, context)?;
    }
    
    let stop_tokens = [exp_token(TokenType::EndFor), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, end_token) = parse_until_w_context(next, &stop_parser, &parse_statement_v2, context);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => for_token.clone()
    };
    return Ok((
        next, 
        Arc::new(AstForBlock{
            raw_pos: for_token.get_raw_pos(),
            pos: for_token.get_pos(),
            range: create_new_range(for_token.get_range(), end_pos_token.get_range()),
            counter_token: var_token,
            range_node,
            step_node: step_expr,
            statements: Some(statement_nodes),
            end_token
        })
    ));
}

fn parse_foreach_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, foreach_token) = exp_token(TokenType::ForEach)(input)?;
    // curVar in List
    let in_op_parser = exp_token(TokenType::In);
    // this some weird syntax
    let parsers: [for<'b> fn(&'a[Token], &'b mut C) -> Result<(&'a[Token], Arc<dyn IAstNode>), ParseError<'a>>;2]  = [parse_oql_expr, parse_expr];
    let right_hand_parser = alt_parse_w_context(&parsers);
    let (next, in_expr_node) = parse_binary_ops_w_context(next, &in_op_parser, &right_hand_parser, context)?;
    // downto
    let (next, downto_token) = opt_parse(exp_token(TokenType::DownTo))(next)?;
    // using someVar
    let (mut next, using_token) = opt_parse(exp_token(TokenType::Using))(next)?;
    let mut using_var : Option<Arc<dyn IAstNode>> = None;
    if using_token.is_some() {
        (next, using_var) = match parse_identifier(next, context){
            Ok((n, using_node)) => (n, Some(using_node)),
            Err(e) => return Err(e),
        };
    }
    let stop_tokens = [exp_token(TokenType::EndFor), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, end_token) = parse_until_w_context(next, &stop_parser, &parse_statement_v2, context);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => foreach_token.clone()
    };
    return Ok((
        next, 
        Arc::new(AstForEachBlock{
            raw_pos: foreach_token.get_raw_pos(),
            pos: foreach_token.get_pos(),
            range: create_new_range(foreach_token.get_range(), end_pos_token.get_range()),
            in_expr_node,
            using_var,
            statements: Some(statement_nodes),
            end_token,
            is_downto: downto_token.is_some()
        })
    ));
}

fn parse_while_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, while_token) = exp_token(TokenType::While)(input)?;
    let (next, cond_node) = parse_expr(next, context)?;
    
    
    let stop_tokens = [exp_token(TokenType::EndWhile), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, end_token) = parse_until_w_context(next, &stop_parser, &parse_statement_v2, context);
    let end_pos_token = match &end_token {
        Some(t) => t.clone(),
        _ => while_token.clone()
    };
    let _cond_block_end = match statement_nodes.last() {
        Some(n) => n.get_range(),
        _ => cond_node.get_range()
    };
    let result = AstWhileBlock{
        raw_pos: while_token.get_raw_pos(),
        pos: while_token.get_pos(),
        range: create_new_range(while_token.get_range(), end_pos_token.get_range()),
        cond_block: Arc::new(AstConditionalBlock { 
            raw_pos: cond_node.get_raw_pos(), 
            range: create_new_range(while_token.get_range(), end_pos_token.get_range()), 
            condition: Some(cond_node), 
            statements: statement_nodes 
        }),
        end_token
    };
    return Ok((
        next, 
        Arc::new(result)
    ));
}

fn parse_loop_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, loop_token) = exp_token(TokenType::Loop)(input)?;
    
    let stop_tokens = [exp_token(TokenType::EndLoop), exp_token(TokenType::End)];
    let stop_parser = alt_parse(&stop_tokens);
    let (next, statement_nodes, end_token) = parse_until_w_context(next, &stop_parser, &parse_statement_v2, context);
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
        Arc::new(result)));
}


fn parse_repeat_block<'a, C: IParserContext<'a> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, repeat_token) = exp_token(TokenType::Repeat)(input)?;
    
    let stop_parser = exp_token(TokenType::Until);
    let (next, statement_nodes, end_token) = parse_until_w_context(next, &stop_parser, &parse_statement_v2, context);   
    let (next, cond_node) = match &end_token {
        Some(_t) =>{
            match parse_expr(next, context) {
                Ok((next, node)) => (next, Some(node)),
                Err(e) => return Err(e),
            }
        }
        _=> (next, None)
    };
    let end = match &cond_node {
        Some(n) => n.get_range(),
        _ => repeat_token.get_range()
    };
    let result = AstRepeatBlock{
        raw_pos: repeat_token.get_raw_pos(),
        range: create_new_range(repeat_token.get_range(), end.clone()),
        cond_block: Arc::new(AstConditionalBlock { 
            raw_pos: repeat_token.get_raw_pos(), 
            range: create_new_range(repeat_token.get_range(), end), 
            condition: cond_node, 
            statements: statement_nodes 
        }),
        end_token
    };
    return Ok((
        next, 
        Arc::new(result)
    ));
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

fn parse_local_var_decl<'a, C: IParserContext<'a> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
    // var
    let (next, var_token) = exp_token(TokenType::Var)(input)?;
    // ident
    let (next, identifier_token) = exp_token(TokenType::Identifier)(next)?;
    // colon token
    let (next, _) = exp_token(TokenType::Colon)(next)?;
    // parse type
    let (next, type_node) = parse_type(next, context)?;
    let mut end_range = type_node.get_range();
    // parse absolute
    let (mut next, abs_token) = opt_parse(exp_token(TokenType::Absolute))(next)?;
    let mut absolute_node : Option<Arc<dyn IAstNode>>= None;
    if abs_token.is_some() {
        (next, absolute_node) = match parse_identifier(next, context) {
            Ok((n, node)) => {
                end_range = node.get_range();
                (n, Some(node))
            },
            Err(e) => return Err(e)
        };
    }

    return Ok((
        next,
        Arc::new(AstLocalVariableDeclaration {
            raw_pos: var_token.get_raw_pos(),
            pos: var_token.get_pos(),
            range: create_new_range(var_token.get_range(), end_range),
            identifier: identifier_token,
            type_node: type_node,
            absolute: absolute_node,
        })
    ));
 }

fn parse_return_statement<'a, C: IParserContext<'a> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, return_token) = exp_token(TokenType::Return)(input)?;
    let (next, return_expr) = parse_expr(next, context)?;
    return Ok((next, Arc::new(AstReturnNode{
        raw_pos: return_token.get_raw_pos(),
        pos: return_token.get_pos(),
        range: create_new_range(return_token.get_range(), return_expr.get_range()),
        return_expr: return_expr,
    })));
}

fn _parse_control_statements<'a, C: IParserContext<'a> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, control_token) = alt_parse([
        exp_token(TokenType::Exit),
        exp_token(TokenType::Break),
        exp_token(TokenType::Continue),
    ].as_ref())(input)?;
    return Ok((next, Arc::new(AstTerminal{
        token: control_token,
    })));
}

fn parse_control_statements<'a, C: IParserContext<'a> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        _parse_control_statements,
        parse_return_statement,
    ].as_ref())(input,context);
}

pub fn parse_statement_v2<'a, C: IParserContext<'a>+'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    // try match block statements first
    let mut last_error;
    // TODO can't use alt_parse, why?
    // match alt_parse([parse_if_block].as_ref())(input) {
    //     Ok(r) => return Ok(r),
    //     Err(e) => {last_error = e}
    // };
    match parse_if_block_v3(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {last_error=e}
    };
    match parse_for_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_foreach_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_while_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_loop_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_switch_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match parse_repeat_block(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {if last_error.input.len() > e.input.len() {last_error=e}}
    };
    match alt_parse_w_context([
        parse_comment,
        parse_uses,
        parse_constant_declaration,
        parse_type_declaration,
        parse_local_var_decl,
        parse_control_statements,
        parse_oql_expr,
        parse_assignment,
        parse_expr,
    ].as_ref())(input, context) {
        Ok(r) => return Ok(r),
        Err(e) => {
            let most_matched = if e.input.len() < last_error.input.len() {e} else {last_error};
            return Err(most_matched)
        }
    }
}


pub fn parse_binary_ops<'a>(
    input: &'a[Token],
    op_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Token), ParseError>,
    expr_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Arc<dyn IAstNode>), ParseError>,
) 
-> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
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
    left_node: Arc<dyn IAstNode>,
    op_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Token), ParseError>,
    expr_parser: &impl Fn(&[Token]) -> Result<(&[Token],  Arc<dyn IAstNode>), ParseError>) 
-> Result<(&'a [Token], Arc<dyn IAstNode>), Arc<dyn IAstNode>>{

    let (next, op_token) = match op_parser(input){
        Ok(r) => r,
        Err(_e) => return Err(left_node)
    };
    let (next, right_node) = match expr_parser(next){
        Ok(r) => r,
        Err(_e) => return Err(left_node)
    };
    return Ok((next, Arc::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        range: create_new_range_from_irange(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })))
}

pub fn parse_binary_ops_w_context<'a, 'b, C:IParserContext<'a> +'a>(
    input: &'a[Token],
    op_parser: &impl Fn(&'a [Token]) -> Result<(&'a[Token],  Token), ParseError<'a>>,
    expr_parser: &impl Fn(&'a [Token], &mut C) -> Result<(&'a[Token],  Arc<dyn IAstNode>), ParseError<'a>>,
    context : &mut C
) 
-> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (mut next, left_node) = expr_parser(input, context)?;
    let mut left_node = Some(left_node);
    loop {
        (next, left_node) = match parse_binary_op_w_context(next, left_node.unwrap(), op_parser, expr_parser, context) {
            Ok((n, node)) => (n, Some(node)),
            Err(ln)=> {
                left_node = Some(ln);
                break;
            }
        };
    }
    return Ok((next, left_node.unwrap()));
} 

fn parse_binary_op_w_context<'a, C:IParserContext<'a> +'a>(
    input: &'a[Token],
    left_node: Arc<dyn IAstNode>,
    op_parser: &impl Fn(&'a [Token]) -> Result<(&[Token],  Token), ParseError>,
    expr_parser: &impl Fn(&'a [Token], &mut C) -> Result<(&'a[Token],  Arc<dyn IAstNode>), ParseError<'a>>,
    context : &mut C)
-> Result<(&'a [Token], Arc<dyn IAstNode>), Arc<dyn IAstNode>>{

    let (next, op_token) = match op_parser(input){
        Ok(r) => r,
        Err(_e) => return Err(left_node)
    };
    let (next, right_node) = match expr_parser(next, context){
        Ok(r) => r,
        Err(_e) => {
            if op_token.token_type == TokenType::Dot {
                // allow empty nodes, if dot operator
                //  to handle dereference op and auto-complete in the future
                let start = Range{
                    start: op_token.get_range().start.offset_char(1),
                    end: op_token.get_range().start.offset_char(2),
                };
                let end = match _e.input.first() {
                    Some(t) => t.get_range(),
                    _=> start.clone()
                };
                let right_node = Arc::new(AstEmpty{
                    raw_pos: op_token.get_raw_pos()+1,
                    range: create_new_range(start, end)
                });
                return Ok((
                    next,
                    Arc::new(AstBinaryOp{
                        raw_pos: left_node.get_raw_pos(),
                        range: create_new_range(left_node.get_range(), right_node.get_range()),
                        left_node,
                        right_node,
                        op_token
                    })
                ))
            } else {
                return Err(left_node)
            }
        }
    };
    return Ok((next, Arc::new(AstBinaryOp{
        raw_pos: left_node.get_raw_pos(),
        range: create_new_range_from_irange(left_node.as_range(), right_node.as_range()),
        op_token,
        left_node,
        right_node,
    })))
}

#[cfg(test)]
mod test{

    use std::ops::Deref;

    use crate::parser::IParserContext;
    use crate::parser::ast::{AstBinaryOp, AstCast, AstTerminal, AstUnaryOp, AstMethodCall, IAstNode, AstIfBlock, AstForBlock, AstForEachBlock, AstWhileBlock, AstLoopBlock, AstLocalVariableDeclaration, AstSetLiteral, AstSwitchBlock, AstArrayAccess, AstRepeatBlock};
    use crate::parser::test::{check_node_pos_and_range, create_context};
    use crate::utils::{ast_to_string_brief_recursive, ast_to_string_brief, dfs, bfs};
    use crate::{parser::test::gen_list_of_tokens, lexer::tokens::TokenType};
    use crate::parser::body_parser::{parse_terms, parse_factors, parse_dot_ops, parse_bracket_closure, parse_bit_ops_2, parse_shifts, parse_compare, parse_logical_or, parse_unary_op, parse_method_call, parse_assignment, parse_if_block_v3, parse_for_block, parse_foreach_block, parse_while_block, parse_loop_block, parse_local_var_decl, parse_literal_set, parse_switch_block, parse_repeat_block};

    

    #[test]
    fn test_parse_bracket_closure(){
        let input = gen_list_of_tokens(&[
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_bracket_closure(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        // TODO what to do with pos and range?
        // check_node_pos_and_range(node.as_ast_node(), &input);
        
        let node = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        assert_eq!(node.left_node.get_identifier(), "First");
        assert_eq!(node.op_token.get_value().deref(), "+");
        assert_eq!(node.right_node.get_identifier(), "Second");
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
        let mut context = create_context();
        let (next, node) = parse_dot_ops(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
    }

    #[test]
    fn test_parse_factors(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Asterisk, Some("*".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Asterisk, Some("/".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_factors(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        // expect same tree struct as dot_ops
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
    }

    #[test]
    fn test_parse_terms(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Asterisk, Some("/".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_terms(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_bit_ops_2(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_shifts(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_compare(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_logical_or(&input, &mut context).unwrap();
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
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(dfs.get(4).unwrap().get_identifier(), input.get(1).unwrap().get_value_as_str());
    }

    #[test]
    fn test_parse_unary_op_pre(){
        let input = gen_list_of_tokens(&[
            (TokenType::Not, Some("not".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::BNot, Some("bNot".to_string())),
            (TokenType::AddressOf, Some("@".to_string())),
            (TokenType::Identifier, Some("Expression".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_unary_op(&input, &mut context).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        // TODO related to bracket closure, pos & range
        // check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), "not");
        assert_eq!(node.expr_node.get_identifier(), "bNot");
    }


    #[test]
    fn test_parse_unary_op_pre_minus(){
        let input = gen_list_of_tokens(&[
            (TokenType::Minus, Some("-".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::BNot, Some("bNot".to_string())),
            (TokenType::AddressOf, Some("@".to_string())),
            (TokenType::Identifier, Some("Expression".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_unary_op(&input, &mut context).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        // TODO related to bracket closure, pos & range
        // check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), "-");
        assert_eq!(node.expr_node.get_identifier(), "bNot");
    }

    #[test]
    fn test_parse_unary_op_pre_minus_num(){
        let input = gen_list_of_tokens(&[
            (TokenType::Minus, Some("-".to_string())),
            (TokenType::NumericLiteral, Some("100".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_unary_op(&input, &mut context).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        // TODO related to bracket closure, pos & range
        // check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), "-");
        assert_eq!(node.expr_node.get_identifier(), "100");
    }

    #[test]
    fn test_parse_unary_op_post(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("self".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Increment, Some("++".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_unary_op(&input, &mut context).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        // TODO related to bracket closure, pos & range
        check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), "++");
        assert_eq!(node.expr_node.get_identifier(), ".");
        assert_eq!(node.expr_node.get_children_ref().unwrap().get(0).unwrap().get_identifier(), ".");
    }

    #[test]
    fn test_parse_unary_op_post_single_left(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Increment, Some("++".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_unary_op(&input, &mut context).unwrap();
        // print!("{:#?}", node.as_ref());
        // print!("{:#?}", input);
        // print!("{}",print_ast_brief_recursive(node.as_ref()));
        assert_eq!(next.len(), 0);
        // TODO related to bracket closure, pos & range
        check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstUnaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), "++");
        assert_eq!(node.expr_node.get_identifier(), "First");
    }

    #[test]
    fn test_parse_logical_only_terminal(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("First".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_terms(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let bin_op = node.as_any().downcast_ref::<AstTerminal>().unwrap();
        let dfs = dfs(bin_op);
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
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
            (TokenType::Asterisk, Some("*".to_string())),
            (TokenType::Identifier, Some("Val2".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_method_call(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstMethodCall>().unwrap();
        assert_eq!(node.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(node.parameter_list.len(), 3);
    }

    #[test]
    fn test_parse_array_access(){
        let input = gen_list_of_tokens(&[
            (TokenType::Identifier, Some("Var".to_string())),
            (TokenType::OSqrBracket, Some("[".to_string())),
            (TokenType::Identifier, Some("Index".to_string())),
            (TokenType::CSqrBracket, Some("]".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Var".to_string())),
            (TokenType::OSqrBracket, Some("[".to_string())),
            (TokenType::Identifier, Some("Some".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::NumericLiteral, Some("10".to_string())),
            (TokenType::CSqrBracket, Some("]".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_dot_ops(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let node = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        assert_eq!(node.op_token.get_value_as_str(), ".");
        let _left_node = node.left_node.as_any().downcast_ref::<AstArrayAccess>().unwrap();
        let _right_node = node.right_node.as_any().downcast_ref::<AstArrayAccess>().unwrap();
        let _right_index_node = _right_node.index_node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
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
            (TokenType::Identifier, Some("Param2".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Method2".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_dot_ops(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let bin_op = node.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        let dfs = dfs(bin_op);
        assert_eq!(dfs.len(), 10);
        dfs.iter().for_each(|n| {println!("{}",ast_to_string_brief(n.as_ast_node()))});
        // 
        assert_eq!(dfs.get(0).unwrap().get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(dfs.get(1).unwrap().get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(dfs.get(2).unwrap().get_identifier(), input.get(6).unwrap().get_value_as_str());
        assert_eq!(dfs.get(3).unwrap().get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(dfs.get(5).unwrap().get_identifier(), input.get(11).unwrap().get_value_as_str());
        assert_eq!(dfs.get(6).unwrap().get_identifier(), input.get(13).unwrap().get_value_as_str());
        assert_eq!(dfs.get(7).unwrap().get_identifier(), input.get(12).unwrap().get_value_as_str());
        assert_eq!(dfs.get(8).unwrap().get_identifier(), input.get(9).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_assignment(&input, &mut context).unwrap();
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(2).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_if_block_v3(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(2).unwrap().get_value_as_str());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(5).unwrap().get_value_as_str());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(bfs.get(6).unwrap().data.get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(bfs.get(7).unwrap().data.get_identifier(), input.get(7).unwrap().get_value_as_str());
        assert_eq!(bfs.get(8).unwrap().data.get_identifier(), input.get(6).unwrap().get_value_as_str());
        assert_eq!(bfs.get(9).unwrap().data.get_identifier(), input.get(8).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_if_block_v3(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.len(), 1);
        assert_eq!(if_node.else_if_blocks.get(0).unwrap().get_children_ref().unwrap().len(), 1);
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
        let mut context = create_context();
        let (next, node) = parse_if_block_v3(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.len(), 2);
        assert_eq!(if_node.else_if_blocks.get(0).unwrap().get_children_ref().unwrap().len(), 2);
        assert_eq!(if_node.else_if_blocks.get(1).unwrap().get_children_ref().unwrap().len(), 1);
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
        let mut context = create_context();
        let (next, node) = parse_if_block_v3(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children_ref().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.len(), 2);
        assert_eq!(if_node.else_if_blocks.get(0).unwrap().get_children_ref().unwrap().len(), 1);
        assert_eq!(if_node.else_if_blocks.get(1).unwrap().get_children_ref().unwrap().len(), 0);
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
        let mut context = create_context();
        let (next, node) = parse_if_block_v3(&input, &mut context).unwrap();
        println!("{}", ast_to_string_brief_recursive(node.as_ast_node()));
        
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        
        let if_node = node.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(if_node.if_block.get_children_ref().unwrap().len(), 4);
        assert_eq!(if_node.else_if_blocks.len(), 0);
        let children = if_node.if_block.get_children_ref().unwrap();
        let nested_if = children.get(1).unwrap();
        let nested_if = nested_if.as_any().downcast_ref::<AstIfBlock>().unwrap();
        assert_eq!(nested_if.if_block.get_children_ref().unwrap().len(), 3);

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
        let mut context = create_context();
        let (next, node) = parse_for_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value_as_str());
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(5).unwrap().get_value_as_str());
        
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
        let mut context = create_context();
        let (next, node) = parse_for_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value_as_str());
        // body here
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(9).unwrap().get_value_as_str());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(5).unwrap().get_value_as_str());
        
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
        let mut context = create_context();
        let (next, node) = parse_for_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
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
        assert_eq!(bfs.get(0).unwrap().data.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(bfs.get(1).unwrap().data.get_identifier(), input.get(4).unwrap().get_value_as_str());
        assert_eq!(bfs.get(2).unwrap().data.get_identifier(), input.get(7).unwrap().get_value_as_str());
        // body here, check that it is a for node
        assert_eq!(bfs.get(3).unwrap().data.get_identifier(), input.get(8).unwrap().get_value_as_str());
        assert_eq!(bfs.get(4).unwrap().data.get_identifier(), input.get(3).unwrap().get_value_as_str());
        assert_eq!(bfs.get(5).unwrap().data.get_identifier(), input.get(5).unwrap().get_value_as_str());
        
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
        let mut context = create_context();
        let (next, node) = parse_foreach_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let foreach_node = node.as_any().downcast_ref::<AstForEachBlock>().unwrap();
        // check counter var
        assert_eq!(foreach_node.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(foreach_node.in_expr_node.get_identifier(), "in");
        assert_eq!(foreach_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        assert_eq!(foreach_node.statements.as_ref().unwrap().first().unwrap().get_identifier(), "=");
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
    fn test_foreach_block_downto_with_using(){
        let input = gen_list_of_tokens(&[
            (TokenType::ForEach, Some("foreach".to_string())),
            (TokenType::Identifier, Some("LoopVar".to_string())),
            (TokenType::In, Some("in".to_string())),
            (TokenType::Identifier, Some("List".to_string())),
            (TokenType::DownTo, Some("downto".to_string())),
            (TokenType::Using, Some("using".to_string())),
            (TokenType::Identifier, Some("SomeVar".to_string())),
            (TokenType::EndFor, Some("endfor".to_string())),     
        ]);
        let mut context = create_context();
        let (next, node) = parse_foreach_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let foreach_node = node.as_any().downcast_ref::<AstForEachBlock>().unwrap();
        // check counter var
        assert_eq!(foreach_node.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(foreach_node.using_var.as_ref().unwrap().get_identifier(), "SomeVar");
        assert_eq!(foreach_node.in_expr_node.get_identifier(), "in");
        assert_eq!(foreach_node.end_token.as_ref().unwrap().clone(), input.last().unwrap().clone());
        assert_eq!(foreach_node.statements.as_ref().unwrap().len(), 0);   
        assert!(foreach_node.is_downto);
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
        let mut context = create_context();
        let (next, node) = parse_while_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let while_node = node.as_any().downcast_ref::<AstWhileBlock>().unwrap();
        // check counter var
        assert_eq!(while_node.get_identifier(), input.get(0).unwrap().get_value_as_str());
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
        let mut context = create_context();
        let (next, node) = parse_loop_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let while_node = node.as_any().downcast_ref::<AstLoopBlock>().unwrap();
        // check counter var
        assert_eq!(while_node.to_string_type_pos(), input.get(0).unwrap().to_string_val_and_pos());
        assert_eq!(while_node.get_children_ref().unwrap().len(), 1);
        assert_eq!(while_node.get_children_ref().unwrap().first().unwrap().get_identifier(), "=".to_string());
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
        let mut context = create_context();
        let (next, node) = parse_local_var_decl(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let var_decl = node.as_any().downcast_ref::<AstLocalVariableDeclaration>().unwrap();
        
        // check ident
        assert_eq!(var_decl.get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(var_decl.get_children_ref().unwrap().len(), 1);  
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
        let mut context = create_context();
        let (next, node) = parse_local_var_decl(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let var_decl = node.as_any().downcast_ref::<AstLocalVariableDeclaration>().unwrap();
        
        // check ident
        assert_eq!(var_decl.get_identifier(), input.get(1).unwrap().get_value_as_str());
        assert_eq!(var_decl.get_children_ref().unwrap().len(), 2);
        // check abs
        assert_eq!(var_decl.get_children_ref().unwrap().get(1).unwrap().get_identifier(), input.last().unwrap().get_value_as_str());  
    }

    #[test]
    fn test_parse_literal_set(){
        let input = gen_list_of_tokens(&[
            (TokenType::OSqrBracket, Some("[".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("Third".to_string())),
            (TokenType::CSqrBracket, Some("]".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_literal_set(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);

        print!("{}",ast_to_string_brief_recursive(node.as_ast_node()));

        let node = node.as_any().downcast_ref::<AstSetLiteral>().unwrap();
        assert_eq!(node.set_items.len(), 3);
    }

    #[test]
    fn test_parse_literal_set_empty(){
        let input = gen_list_of_tokens(&[
            (TokenType::OSqrBracket, Some("[".to_string())),
            (TokenType::CSqrBracket, Some("]".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_literal_set(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        
        let node = node.as_any().downcast_ref::<AstSetLiteral>().unwrap();
        assert_eq!(node.set_items.len(), 0);
    }

    #[test]
    fn test_parse_literal_set_one_item(){
        let input = gen_list_of_tokens(&[
            (TokenType::OSqrBracket, Some("[".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::CSqrBracket, Some("]".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_literal_set(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        
        let node = node.as_any().downcast_ref::<AstSetLiteral>().unwrap();
        assert_eq!(node.set_items.len(), 1);
    }

    #[test]
    fn test_switch_when_list(){
        let input = gen_list_of_tokens(&[
            (TokenType::Switch, Some("switch".to_string())),
            (TokenType::Identifier, Some("Var".to_string())),
            (TokenType::When, Some("when".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::Identifier, Some("Expr".to_string())),
            (TokenType::EndWhen, Some("endwhen".to_string())),
            (TokenType::Else, Some("else".to_string())),
            (TokenType::Identifier, Some("Expr".to_string())),
            (TokenType::EndSwitch, Some("endswitch".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_switch_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let switch_node = node.as_any().downcast_ref::<AstSwitchBlock>().unwrap();
        assert_eq!(switch_node.get_children_ref().unwrap().len(), 3);
        // check when expr
        assert_eq!(switch_node.get_children_ref().unwrap().get(1).unwrap().get_children_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_switch_when_range(){
        let input = gen_list_of_tokens(&[
            (TokenType::Switch, Some("switch".to_string())),
            (TokenType::Identifier, Some("Var".to_string())),
            (TokenType::When, Some("when".to_string())),
            (TokenType::NumericLiteral, Some("1".to_string())),
            (TokenType::To, Some("to".to_string())),
            (TokenType::NumericLiteral, Some("2".to_string())),
            (TokenType::Identifier, Some("Expr".to_string())),
            (TokenType::EndWhen, Some("endwhen".to_string())),
            (TokenType::EndSwitch, Some("endswitch".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_switch_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        // println!("{}", print_ast_brief_recursive(node.as_ast_node()));

        let switch_node = node.as_any().downcast_ref::<AstSwitchBlock>().unwrap();
        assert_eq!(switch_node.get_children_ref().unwrap().len(), 2);
        // check when expr
        assert_eq!(switch_node.get_children_ref().unwrap().get(1).unwrap().get_children_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_repeat_block_with_statements(){
        let input = gen_list_of_tokens(&[
            (TokenType::Repeat, Some("repeat".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::Plus, Some("+".to_string())),
            (TokenType::Identifier, Some("Temp3".to_string())),
            (TokenType::Until, Some("until".to_string())),
            (TokenType::Identifier, Some("First".to_string())),
            (TokenType::LessThan, Some(">".to_string())),
            (TokenType::Identifier, Some("Second".to_string())),  
        ]);
        let mut context = create_context();
        let (next, node) = parse_repeat_block(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let repeat_node = node.as_any().downcast_ref::<AstRepeatBlock>().unwrap();

        assert_eq!(repeat_node.get_identifier(), input.get(0).unwrap().get_value_as_str());
        assert_eq!(repeat_node.cond_block.get_children_ref().unwrap().len(), 2);
     
    }
}
