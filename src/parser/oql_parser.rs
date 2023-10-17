use std::sync::Arc;

use crate::{lexer::tokens::{Token, TokenType}, utils::{IRange, create_new_range}};

use super::{IParserContext, ParserDiagnostic, ast::{IAstNode, AstTerminal, AstOQLSelect, AstOQLFromNode, AstOQLJoin, AstOQLOrderBy, AstMethodCall, AstOQLFetch}, ParseError, utils::{exp_token, alt_parse_w_context, opt_parse, opt_parse_w_context, parse_separated_list_w_context, exp_ident_with_value, alt_parse, parse_until_no_match_w_context}, body_parser::{parse_literal_basic, parse_identifier, parse_compare, parse_expr, parse_dot_ops}};



pub fn parse_oql_expr<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) 
-> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>
{
    return alt_parse_w_context([
        parse_oql_select,
        parse_oql_fetch
    ].as_ref())(input, context);
}

fn parse_oql_select<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    //     OQL Select {Top <n>} {Distinct} <Select variable list> 
    //    from {Conditional} {allVersionsOf} {PhantomsToo} <Alias variable 1> in <Class identifier 1>{++ {RestrictTo <ClassId>}} {<OuterJoins>}
    //    {, <Alias variable 2> in <Class 2 identifier>{++ {RestrictTo <ClassId2>}} {<OuterJoins>}...}
    //    {where <Criterion>} {{table}order by <Sort variable> {descending} {,<Sort variable 2> {descending}...}}
    //    {using <Cursor variable>}

    // OQL Select
    let (next, oql_token) = exp_token(TokenType::OQL)(input)?;
    let (next, select_token) = exp_token(TokenType::Select)(next)?;
    let mut end = select_token.get_range();
    // Top N
    let (next, limit_node) = opt_parse_w_context(parse_top_n)(next, context)?;
    // distint
    let (next, distinct_token) = opt_parse(exp_token(TokenType::Distinct))(next)?;
    // select items
    let (next, select_nodes) = parse_separated_list_w_context(parse_select_item, TokenType::Comma)(next, context)?;
    end = match select_nodes.last() {
        Some(n) => n.get_range(),
        _=>end 
    };
    // from ...
    let (next, _from_token) = exp_token(TokenType:: From)(next)?;
    let (next, from_nodes) = parse_separated_list_w_context(parse_from_item, TokenType::Comma)(next, context)?;
    end = match from_nodes.last() {
        Some(n) => n.get_range(),
        _=>end 
    };
    // where...
    let (next, where_node) = opt_parse_w_context(parse_where)(next, context)?;
    end = match &where_node {
        Some(n) => n.get_range(),
        _=>end 
    };
    // order by...
    let (next, order_by_nodes) = opt_parse_w_context(parse_order_by)(next, context)?;
    end = match &order_by_nodes {
        Some(nodes) => {
            match nodes.last() {
                Some(n) => n.get_range(),
                _=>end 
            }
        },
        _=>end 
    };

    let (next, using_node) = opt_parse_w_context(parse_using)(next, context)?;
    end = match &using_node {
        Some(n) => n.get_range(),
        _=>end 
    };

    return Ok((
        next,
        Arc::new(
            AstOQLSelect{
                raw_pos: oql_token.get_raw_pos(),
                range: create_new_range(oql_token.get_range(), end),
                limit_node,
                select_nodes,
                from_nodes,
                where_node,
                order_by_nodes,
                using_node,
                is_distinct: distinct_token.is_some(),
            }
        )
    ))
}

fn parse_top_n<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, _top_token) = exp_token(TokenType::Top)(input)?;
    return alt_parse_w_context([
        parse_literal_basic,
        parse_identifier
    ].as_ref())(next, context);
}

fn parse_select_item<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    return alt_parse_w_context([
        parse_asterisk,
        parse_oql_method_call,
        parse_dot_ops
    ].as_ref())(input, context);
}

fn parse_oql_method_call<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>> {
    // to handle asterisk in method calls
    //  e.g. OQLCount(*)
    let (next, ident_node) = parse_identifier(input, context)?;
    let (next, _) = exp_token(TokenType::OBracket)(next)?;
    let (next, parameter_list) = parse_separated_list_w_context(parse_asterisk, TokenType::Comma)(next, context)?;
    let (next, cbracket_token) = exp_token(TokenType::CBracket)(next)?;
    return Ok((next, Arc::new(AstMethodCall{
        raw_pos: ident_node.get_raw_pos(),
        pos: ident_node.get_pos(),
        range: create_new_range(ident_node.get_range(), cbracket_token.get_range()),
        identifier: ident_node,
        parameter_list,
    })))
}

fn parse_asterisk<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], _context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, top_token) = exp_token(TokenType::Asterisk)(input)?;
    return Ok((
        next,
        Arc::new(
            AstTerminal::new(top_token)
        )
    )) 
}

fn parse_from_item<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    // [conditional] {alias} in {source_class}[++]
    // conditional
    let (next, cond_token) = opt_parse(exp_token(TokenType::Conditional))(input)?;
    let (next, allver_token) = opt_parse(exp_token(TokenType::AllVersionsOf))(next)?;
    let (next, phantoms_token) = opt_parse(exp_token(TokenType::PhantomsToo))(next)?;

    let (next, alias_token) = exp_token(TokenType::Identifier)(next)?;
    let (s_raw_pos, start) = match &cond_token{
        Some (t) => (t.get_raw_pos(),t.get_range()),
        _=> (alias_token.get_raw_pos(), alias_token.get_range())
    };
    // in
    let (next, _in_token) = exp_token(TokenType::In)(next)?;
    // source class
    let (next, source_node) = parse_identifier(next, context)?;
    // ++
    let (next, subclass_token) = opt_parse(exp_token(TokenType::Increment))(next)?;

    // parse joins
    let (next, join_nodes) = parse_until_no_match_w_context(next, parse_join_item, context);

    let end = match join_nodes.last(){
        Some(n) => n.get_range(),
        _=> {
            match &subclass_token{
                Some(t) => t.get_range(),
                _=> source_node.get_range()
            }
        },
    };
    return Ok((
        next,
        Arc::new(
            AstOQLFromNode{
                raw_pos: s_raw_pos,
                range: create_new_range(start, end),
                alias_token,
                source_node,
                join_nodes,
                is_conditional: cond_token.is_some(),
                is_all_versions: allver_token.is_some(),
                is_phantoms_too: phantoms_token.is_some(),
                includes_subclasses: subclass_token.is_some()
            }
        )
    )) 
}

fn parse_join_item<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, join_token) = alt_parse([
        exp_ident_with_value("outerjoinon"),
        exp_ident_with_value("leftouterjoinon"),
        exp_ident_with_value("rightouterjoinon"),
        exp_ident_with_value("fullouterjoinon"),
    ].as_ref())(input)?;
    let (next, cond_node) = parse_compare(next, context)?;
    return Ok((
        next,
        Arc::new(
            AstOQLJoin{
                raw_pos: join_token.get_raw_pos(),
                range: create_new_range(join_token.get_range(), cond_node.get_range()),
                join_token,
                cond_node,
            }
        )
    ))
}

fn parse_where<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, _where_token) = exp_token(TokenType::Where)(input)?;
    return parse_expr(next, context);
}

fn parse_order_by<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Vec<Arc<dyn IAstNode>>), ParseError<'a>>{
    let (next, _order_token) = exp_token(TokenType::Order)(input)?;
    let (next, _by_token) = exp_token(TokenType::By)(next)?;
    return parse_separated_list_w_context(parse_order_by_item, TokenType::Comma)(next, context);
}

fn parse_order_by_item<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    
    let (next, field_node) = parse_dot_ops(input, context)?;
    let (next, desc_token) = opt_parse(exp_token(TokenType::Descending))(next)?;
    let end = match &desc_token{
        Some(t) => t.get_range(),
        _=> field_node.get_range()
    };
    return Ok((
        next,
        Arc::new(
            AstOQLOrderBy{
                raw_pos: field_node.get_raw_pos(),
                range: create_new_range(field_node.get_range(), end),
                field_node,
                is_descending: desc_token.is_some()
            }
        )
    ))
}

fn parse_oql_fetch<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let(next, oql_token) = exp_token(TokenType::OQL)(input)?;
    let(next, _fetch_token) = exp_token(TokenType::Fetch)(next)?;
    let(next, _into_token) = exp_token(TokenType::Into)(next)?;

    let(next, into_nodes) = parse_separated_list_w_context(parse_dot_ops, TokenType::Comma)(next, context)?;

    let(next, using_node) = opt_parse_w_context(parse_using)(next, context)?;
    let end = match &using_node{
        Some(n) => n.get_range(),
        _=> {
            match into_nodes.last() {
                Some(n) => n.get_range(),
                _=> _into_token.get_range()
            }
        }
    };
    return Ok((
        next,
        Arc::new(AstOQLFetch{
            raw_pos: oql_token.get_raw_pos(),
            range: create_new_range(oql_token.get_range(), end),
            into_field_nodes: into_nodes,
            using_node,
        })
    ))
}

fn parse_using<'a, C: IParserContext<ParserDiagnostic> + 'a>(input: &'a[Token], context : &mut C) -> Result<(&'a [Token], Arc<dyn IAstNode>), ParseError<'a>>{
    let (next, _using_token) = exp_token(TokenType::Using)(input)?;
    return parse_identifier(next, context);
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
            (TokenType::Distinct, Some("distinct".to_string())),
            (TokenType::Asterisk, Some("*".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("column2".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("oqlmax".to_string())),
            (TokenType::OBracket, Some("(".to_string())),
            (TokenType::Identifier, Some("column3".to_string())),
            (TokenType::CBracket, Some(")".to_string())),
            (TokenType::From, Some("from".to_string())),
            (TokenType::Identifier, Some("y".to_string())),
            (TokenType::In, Some("in".to_string())),
            (TokenType::Identifier, Some("SomeClass".to_string())),
            (TokenType::Where, Some("where".to_string())),
            (TokenType::Identifier, Some("y".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Name".to_string())),
            (TokenType::Equals, Some("=".to_string())),
            (TokenType::StringLiteral, Some("%something".to_string())),
            (TokenType::Order, Some("order".to_string())),
            (TokenType::By, Some("by".to_string())),
            (TokenType::Identifier, Some("y".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("Name".to_string())),
            (TokenType::Descending, Some("descending".to_string())),
            (TokenType::Using, Some("using".to_string())),
            (TokenType::Identifier, Some("cursor".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_oql_expr(&input, &mut context).unwrap();
        // print!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        assert_eq!(node.get_children_ref_dynamic().unwrap().len(), 8);
        let downcasted = node.as_any().downcast_ref::<AstOQLSelect>().unwrap();

        assert_eq!(downcasted.limit_node.as_ref().unwrap().get_identifier(), "10");
        assert_eq!(downcasted.select_nodes.len(), 3);
        assert_eq!(downcasted.select_nodes.get(0).unwrap().get_identifier(), "*");
        assert_eq!(downcasted.select_nodes.get(1).unwrap().get_identifier(), "column2");
        assert_eq!(downcasted.select_nodes.get(2).unwrap().get_identifier(), "oqlmax");
        assert_eq!(downcasted.from_nodes.len(), 1);
        assert_eq!(downcasted.where_node.as_ref().unwrap().get_identifier(), "=");
        assert_eq!(downcasted.order_by_nodes.as_ref().unwrap().len(), 1);
        assert_eq!(downcasted.using_node.as_ref().unwrap().get_identifier(), "cursor");
        assert!(downcasted.is_distinct);
    }


    #[test]
    fn test_oql_from_node(){
        let input = gen_list_of_tokens(&[
            (TokenType::Conditional, Some("conditional".to_string())),
            (TokenType::AllVersionsOf, Some("allversionsof".to_string())),
            (TokenType::PhantomsToo, Some("phantomstoo".to_string())),
            (TokenType::Identifier, Some("y".to_string())),
            (TokenType::In, Some("in".to_string())),
            (TokenType::Identifier, Some("SomeClass".to_string())),
            (TokenType::Increment, Some("++".to_string())),
            (TokenType::Identifier, Some("OuterJoinOn".to_string())),
            (TokenType::Identifier, Some("y.JoinVar".to_string())),
            (TokenType::Identifier, Some("OuterJoinOn".to_string())),
            (TokenType::Identifier, Some("y.AnotherJoinVar".to_string())),
        ]);
        let mut context = create_context();
        let (next, node) = parse_from_item(&input, &mut context).unwrap();
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        assert_eq!(node.get_children_ref_dynamic().unwrap().len(), 3);
        let downcasted = node.as_any().downcast_ref::<AstOQLFromNode>().unwrap();

        assert_eq!(downcasted.alias_token.get_value(), "y");
        assert_eq!(downcasted.source_node.get_identifier(), "SomeClass");
        assert_eq!(downcasted.join_nodes.len(), 2);
        assert!(downcasted.is_conditional);
        assert!(downcasted.is_all_versions);
        assert!(downcasted.is_phantoms_too);
        assert!(downcasted.includes_subclasses);
    }

    #[test]
    fn test_oql_fetch(){
        let input = gen_list_of_tokens(&[
            (TokenType::OQL, Some("oql".to_string())),
            (TokenType::Fetch, Some("fetch".to_string())),
            (TokenType::Into, Some("into".to_string())),
            (TokenType::Identifier, Some("var1".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("var2".to_string())),
            (TokenType::Comma, Some(",".to_string())),
            (TokenType::Identifier, Some("object.".to_string())),
            (TokenType::Dot, Some(".".to_string())),
            (TokenType::Identifier, Some("var3".to_string())),
            (TokenType::Using, Some("using".to_string())),
            (TokenType::Identifier, Some("cursor".to_string())),
            
        ]);
        let mut context = create_context();
        let (next, node) = parse_oql_expr(&input, &mut context).unwrap();
        // print!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        assert_eq!(next.len(), 0);
        assert_eq!(context.get_diagnostics().len(), 0);
        check_node_pos_and_range(node.as_ast_node(), &input);
        assert_eq!(node.get_children_ref_dynamic().unwrap().len(), 4);
        let downcasted = node.as_any().downcast_ref::<AstOQLFetch>().unwrap();

        assert_eq!(downcasted.into_field_nodes.len(), 3);
        assert_eq!(downcasted.into_field_nodes.last().unwrap().get_identifier(), ".");
        assert_eq!(downcasted.using_node.as_ref().unwrap().get_identifier(), "cursor");
    }
}