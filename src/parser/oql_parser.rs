use crate::{lexer::tokens::{Token, TokenType}, utils::{IRange, create_new_range}};

use super::{IParserContext, ParserDiagnostic, ast::{IAstNode, AstTerminal, AstOQLSelect}, ParseError, utils::{exp_token, alt_parse_w_context, opt_parse, seq_parse, opt_parse_w_context, parse_separated_list_w_context}, body_parser::{parse_literal_basic, parse_identifier, parse_primary}};



fn parse_oql_expr<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Box<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        parse_oql_select,
    ].as_ref())(input, context);
}

fn parse_oql_select<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Box<dyn IAstNode>), ParseError<'a>>{
    // OQL Select
    let (next, oql_token) = exp_token(TokenType::OQL)(input)?;
    let (next, select_token) = exp_token(TokenType::Select)(next)?;
    let mut end = select_token.get_range();
    // Top N
    let (next, limit_node) = opt_parse_w_context(parse_top_n)(next, context)?;

    // select items
    let (next, select_nodes) = parse_separated_list_w_context(parse_select_item, TokenType::Comma)(next, context)?;
    end = match select_nodes.last() {
        Some(n) => n.get_range(),
        _=>end 
    };
    return Ok((
        next,
        Box::new(
            AstOQLSelect{
                raw_pos: oql_token.get_raw_pos(),
                range: create_new_range(oql_token.get_range(), end),
                limit_node,
                select_nodes,
            }
        )
    ))
}

fn parse_top_n<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Box<dyn IAstNode>), ParseError<'a>>{
    let (next, top_token) = exp_token(TokenType::Top)(input)?;
    return alt_parse_w_context([
        parse_literal_basic,
        parse_identifier
    ].as_ref())(next, context);
}

fn parse_select_item<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Box<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        parse_asterisk,
        parse_primary,
    ].as_ref())(input, context);
}

fn parse_asterisk<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Box<dyn IAstNode>), ParseError<'a>>{
    let (next, top_token) = exp_token(TokenType::Asterisk)(input)?;
    return Ok((
        next,
        Box::new(
            AstTerminal::new(top_token)
        )
    )) 
}

#[cfg(test)]
mod test{
    use crate::lexer::tokens::*;
    use crate::parser::ast::*;
    use crate::parser::oql_parser::*;
    use crate::parser::test::*;

    #[test]
    fn test_oql_select(){
        let input = gen_list_of_tokens(&[
            (TokenType::OQL, Some("oql".to_string())),
            (TokenType::Select, Some("select".to_string())),
            (TokenType::Top, Some("top".to_string())),
            (TokenType::NumericLiteral, Some("10".to_string())),
            (TokenType::Asterisk, Some("*".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("column2".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("oqlmax".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("column3".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_oql_expr(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        let repeat_node = node.as_any().downcast_ref::<AstOQLSelect>().unwrap();

        assert_eq!(repeat_node.limit_node.as_ref().unwrap().get_identifier(), "10");
        assert_eq!(repeat_node.select_nodes.len(), 3);
        assert_eq!(repeat_node.select_nodes.get(0).unwrap().get_identifier(), "*");
        assert_eq!(repeat_node.select_nodes.get(1).unwrap().get_identifier(), "column2");
        assert_eq!(repeat_node.select_nodes.get(2).unwrap().get_identifier(), "oqlmax")
    }
}