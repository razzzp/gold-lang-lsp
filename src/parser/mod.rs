
use crate::lexer::tokens::{Token, TokenType};
use crate::utils::{Range, get_end_pos, create_new_range_from_irange, IRange, create_new_range};
use crate::ast::{AstClass, AstUses, IAstNode, AstTypeBasic, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration, AstParameterDeclaration, AstParameterDeclarationList, AstProcedure, AstMethodModifiers, AstComment, AstMethodBody, AstFunction, AstMemberModifiers};

use self::body_parser::{parse_repeat, parse_statement_v2};
use self::utils::{prepend_msg_to_error};

pub mod utils;
pub mod body_parser;

#[derive(Debug, Clone)]
pub struct GoldParserError<'a>{
   pub input: &'a [Token],
   pub msg: String
}

#[derive(Debug, Clone)]
pub struct GoldDocumentError{
   pub range: Range,
   pub msg: String
}

impl IRange for GoldDocumentError{
    fn get_range(&self) -> Range {
        self.range.clone()
    }

    fn as_range(&self) -> &dyn IRange {
        self
    }
}

impl GoldDocumentError {
   pub fn get_msg(&self) -> String{
      self.msg.clone()
   }
}

pub fn parse_gold<'a>(input : &'a [Token]) -> ((&'a [Token],  Vec<Box<dyn IAstNode>>), Vec<GoldDocumentError>) {
   let parsers = [
      parse_comment,
      parse_class,
      parse_uses,
      parse_type_declaration,
      parse_constant_declaration,
      parse_global_variable_declaration,
   ];
   let block_parsers = [
      parse_procedure_declaration,
      parse_function_declaration
   ];
   let mut result = Vec::<Box<dyn IAstNode>>::new();
   let mut errors = Vec::<GoldDocumentError>::new();
   if input.len() == 0 {
      return ((input, result), errors)
   }
   let mut next = input;
   while next.len() > 0 {
      let mut most_matched: Option<GoldParserError> = None;
      match alt_parse(&block_parsers)(next){
         Ok((r,(node,errs)))=> {
            result.push(node); 
            errors.extend(errs.into_iter());
            next = r;
            continue
         },
         Err(e)=> {
            // update most matched
            if most_matched.is_some(){
               if most_matched.as_ref().unwrap().input.len() > e.input.len() {
                  most_matched = Some(e);
               } 
            } else {
               most_matched = Some(e);
            }
         }
      };
      next = match alt_parse(&parsers)(next){
         Ok((r,n))=> {result.push(n); r},
         Err(e)=> {
            // update most matched
            if most_matched.is_some(){
               if most_matched.as_ref().unwrap().input.len() > e.input.len() {
                  most_matched = Some(e);
               } 
            } else {
               most_matched = Some(e);
            }
            let most_matched = most_matched.unwrap();
            let mut iter = most_matched.input.iter();
            let first_error_token = next.first();
            let last_error_token = if most_matched.input.first().is_some(){most_matched.input.first().unwrap()} else {input.last().unwrap()};
            let doc_error = GoldDocumentError {
               range: create_new_range_from_irange(first_error_token.unwrap(), last_error_token),
               msg: most_matched.msg
            };
            // move one to prevent infinite loop
            if most_matched.input.len() == next.len(){
               iter.next();
            }
            errors.push(doc_error);
            // set next as the input of the most matched error
            iter.as_slice()
         }
      };
   }
   ((next, result), errors)
}

fn parse_comment<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
   let (next, comment_token) = match exp_token(TokenType::Comment)(input){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("falied to parse comment: ", e))
   };
   return Ok((next, Box::new(AstComment{
      raw_pos: comment_token.raw_pos,
      pos: comment_token.pos.clone(),
      range: comment_token.range.clone(),
      comment: comment_token.value.unwrap()
   })))
}

fn parse_class<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
   // class keyword
   let (next, class_token) = match exp_token(TokenType::Class)(input) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   let (next, class_name_token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };
   // '('
   let (next, _) =  match exp_token(TokenType::OBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // parent class
   let (next, parent_class_name) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // ')'
   let (next, end_token) =  match exp_token(TokenType::CBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // TODO change behaviour when class name empty
   return Ok((next, Box::new(AstClass{
      raw_pos: class_token.raw_pos,
      pos: class_token.pos.clone(),
      range: create_new_range_from_irange(&class_token, &end_token),
      name: class_name_token.value.unwrap().to_owned(),
      parent_class: parent_class_name.value.unwrap().to_owned()
   })));

   // let (i,r) = take(1)(input);
   // if input.peek().unwrap().token_type == TokenType::Identifier{
   //    return Ok((input, Box::new(AST{})));
   // } else {
   //    return Err(Err::Error(Error::new(input, ErrorKind::Tag)))
   // }   
}

fn parse_constant_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
   // const keyword
   let (next, const_token) = match exp_token(TokenType::Const)(input){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl: ", e))
   };
   // identifier
   let (next, ident_token) = match exp_token(TokenType::Identifier)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl: ", e))
   };
   // equals
   let (next, _) = match exp_token(TokenType::Equals)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl: ", e))
   };
   // string or numeric value
   let (next, value_token) = match alt_parse(
      &[
         exp_token(TokenType::StringLiteral),
         exp_token(TokenType::NumericLiteral)]
   )(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl: ", e))
   };
   // multi lang
   let (next, multilang_token) = match exp_token(TokenType::MultiLang)(next){
      Ok(r) => (r.0, Some(r.1)),
      Err(e) => (e.input, None)
   };

   return Ok((
      next,
      Box::new(AstConstantDeclaration {
         raw_pos: const_token.raw_pos,
         pos: const_token.pos.clone(),
         identifier: ident_token,
         value: value_token.clone(),
         range: create_new_range_from_irange(&const_token, &value_token),
         is_multi_lang : if multilang_token.is_some() {true} else {false} 
      })
   ))
}

fn parse_uses<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError> {
   // uses
   let (next, uses_token) = match exp_token(TokenType::Uses)(input){
      Ok((r, t)) => (r, t),
      Err(e) => return Err(e),
   };
   // list of uses: uses1, uses2, ...
   let (next, idents) = match parse_separated_list_token(next, TokenType::Identifier, TokenType::Comma) {
      Ok((r, l)) => (r, l),
      Err(e) => return Err(e),
   };
   return Ok((
      next, 
      Box::new(AstUses { 
         raw_pos: uses_token.raw_pos,
         pos: uses_token.pos.clone(),
         range: Range { 
            start: uses_token.pos, 
            end: if idents.last().is_some() {idents.last().unwrap().range.end.clone()} else {uses_token.range.end.clone()}},
         list_of_uses: idents })
   ));
}

fn parse_type_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // type keyword, identifier, then colon
   let (next, tokens) = match seq_parse(&[
      exp_token(TokenType::Type),
      exp_token(TokenType::Identifier),
      exp_token(TokenType::Colon)
   ])(input){
      Ok(r) => r,
      Err(e) => return Err(e)
   };
   // parse the type
   let (next, type_node) = match  parse_type(next){
       Ok(r) => r,
       Err(e) => return Err(e)
   };
   let type_declaration_node = AstTypeDeclaration {
      raw_pos: tokens[0].raw_pos,
      pos: tokens[0].pos.clone(),
      range: Range { start: tokens[0].pos.clone(), end: type_node.get_range().end },
      identifier: tokens[1].clone(),
      type_node: type_node
   };
   return Ok((next, Box::new(type_declaration_node)));
}

fn parse_type<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // TODO record types
   let parsers = [
      parse_type_basic,
      parse_type_enum,
      parse_type_reference,
   ];
   let parse_result = alt_parse(&parsers)(input);
   return parse_result;
}

fn parse_type_basic<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>> {
   // basic fixed size
   let parse_result = alt_parse(&[
      exp_token(TokenType::Int1),
      exp_token(TokenType::Int2),
      exp_token(TokenType::Int4),
      exp_token(TokenType::Int8),
      exp_token(TokenType::Boolean),
      exp_token(TokenType::Char),
      exp_token(TokenType::Num4),
      exp_token(TokenType::Num8),
      exp_token(TokenType::Num10),
      exp_token(TokenType::Decimal),
      exp_token(TokenType::CString),
      exp_token(TokenType::String),
      exp_token(TokenType::Text),
      exp_token(TokenType::Identifier),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypeBasic{
         raw_pos:t.raw_pos,
         pos: t.pos.clone(),
         range: t.range.clone(),
         type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_enum<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // opening (
   let (next, obracket_token) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(e)
   };
   // list of enums: enum1, enum2, enum3, ...
   let (next, tokens) = match parse_separated_list_token(next, TokenType::Identifier, TokenType::Comma){
       Ok((r, ts)) => (r, ts),
       Err(e) => return Err(e)
   };
   // closing )
   let (next, cbracket_token) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(e)
   };
   return Ok((next, Box::new(AstTypeEnum{
      raw_pos: obracket_token.raw_pos,
      pos: obracket_token.pos.clone(),
      range: create_new_range_from_irange(&obracket_token, &cbracket_token),
      variants: tokens
   })));
}

fn parse_type_reference<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // refto/listof
   let result = alt_parse(&[
      exp_token(TokenType::RefTo),
      exp_token(TokenType::ListOf)
   ])(input);
   let (next, ref_token) = match result {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   // [P,A,T,I,V]
   let (next, mut option_tokens) = match parse_type_reference_options(next){
      Ok((r,t)) => (r,t),
      Err(e) => (e.input, Vec::<Token>::new())
   };
   // ident
   let (next, ident_token) = match exp_token(TokenType::Identifier)(next) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };

   // inverse
   let (mut next, inverse_token) = opt_parse(exp_token(TokenType::Inverse))(next)?;
   let mut inverse_var = None;
   if inverse_token.is_some(){
      (next, inverse_var) = match exp_token(TokenType::Identifier)(next) {
         Ok((n, inv_tok)) => (n, Some(inv_tok)),
         Err(e) => return Err(e),
      };
   }

   // calc end pos
   let mut end_range = ident_token.get_range();
   if inverse_var.is_some(){
      end_range = inverse_var.as_ref().unwrap().get_range();
   }
   // if there are options remove open and close sqr brackets
   if option_tokens.len() > 0 {
      option_tokens.pop();
      option_tokens.remove(0);
   }

   return Ok((next, Box::new(AstTypeReference{
      raw_pos: ref_token.raw_pos,
      pos: ref_token.pos.clone(),
      ref_type: ref_token.clone(),
      range: create_new_range(ref_token.get_range(), end_range),
      options: option_tokens,
      ident_token,
      inverse_var_token: inverse_var,
   })));
}

fn parse_type_reference_options<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<Token>), GoldParserError<'a>>{
   // opening [
   let mut result = Vec::<Token>::new();
   let (next, open_token) = match exp_token(TokenType::OSqrBracket)(input) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   // options P,A,T,I,V
   let (next, mut option_tokens) = match parse_separated_list_token(next, TokenType::Identifier, TokenType::Comma){
      Ok((r, ts)) => (r,ts),
      Err(e) => return Err(e)
   };
   // closing ]
   let (next, closing_token) = match exp_token(TokenType::CSqrBracket)(next) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   result.push(open_token);
   result.append(&mut option_tokens);
   result.push(closing_token);
   return Ok((next, result));
}

fn parse_global_variable_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // memory?
   let (next, memory_token) = match exp_token(TokenType::Memory)(input){
      Ok((n, t)) => (n,Some(t)),
      Err(e) => (e.input, None)
   };
   // ident
   let (next, identifier_token) = match exp_token(TokenType::Identifier)(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   // colon token
   let (next, _) = match exp_token(TokenType::Colon)(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   // parse type
   let (next, type_node) = match parse_type(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   // modifiers
   let (next, member_modifiers) = parse_member_modifiers(next)?;
   let raw_pos = if memory_token.is_some() {memory_token.as_ref().unwrap().raw_pos.clone()} else {identifier_token.raw_pos.clone()};
   let start = if memory_token.is_some() {memory_token.as_ref().unwrap().get_range()} else {identifier_token.get_range()};
   let end;
   if member_modifiers.is_some(){
      end = member_modifiers.as_ref().unwrap().get_range();
   } else {
      end = type_node.get_range()
   }
   return Ok((
      next,
      Box::new(AstGlobalVariableDeclaration {
         is_memory: if memory_token.is_some() {true} else {false},
         raw_pos: raw_pos,
         pos: start.start.clone(),
         range: create_new_range(start, end),
         identifier: identifier_token,
         type_node: type_node,
         modifiers: member_modifiers,
      })
   ));
}


fn parse_procedure_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError<'a>>{
   // parse proc [ident]
   let (next, first_tokens) = match seq_parse(&[
      exp_token(TokenType::Proc),
      exp_token(TokenType::Identifier),
   ]) (input){
      Ok(r) => r,
      Err(e) => return Err(e)
   };
   let mut end = first_tokens.get(1).unwrap().range.end.clone();
   // parse params
   let (next, param_nodes) = match parse_parameter_declaration_list(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Failed to parse proc decl: ", e))
   };
   end = if param_nodes.is_some() {get_end_pos(param_nodes.as_ref().unwrap())} else {end};
   // modifiers (private, protected, etc.)
   let (mut next, modifier_node) = match parse_method_modifiers(next) {
      Ok((n, node)) => (n, node),
      Err(e) => return Err(prepend_msg_to_error("Failed to parse proc decl: ", e))
   };
   end = if modifier_node.is_some() {get_end_pos(modifier_node.as_ref().unwrap())} else {end};

   let mut errors = Vec::<GoldDocumentError>::new();
   // if proc is not forward and not external, parse body
   let mut method_body: Option<AstMethodBody> = None;
   let mut end_method_token = None;
   if has_method_body(&modifier_node) {
      let body_tokens; 
      (next, body_tokens, end_method_token) = take_until([TokenType::EndProc, TokenType::End].as_ref())(next)?;
      match parse_method_body(body_tokens){
         Ok((_, (mut node, errs))) => {
            if node.is_none() {
               node = Some(AstMethodBody{
                  raw_pos: first_tokens.get(0).unwrap().get_raw_pos(),
                  pos: first_tokens.get(0).unwrap().get_pos(),
                  range: first_tokens.get(0).unwrap().get_range(),
                  statements: Vec::new()
               });
            }
            method_body = node;
            errors.extend(errs.into_iter());
         },
         // TODO improve structure for error handling
         Err(_) => ()
      }
      if end_method_token.is_none(){
      errors.push(GoldDocumentError { 
         range: first_tokens.get(0).unwrap().get_range(), 
         msg: "proc end token not found".to_string() }
      )
   }
   }
   end = if end_method_token.is_some() {end_method_token.as_ref().unwrap().get_range().end} else {end};

   return Ok((next, (Box::new(AstProcedure{
      raw_pos: first_tokens[0].raw_pos,
      pos: first_tokens[0].pos.clone(),
      range: Range { start: first_tokens[0].pos.clone(), end: end},
      identifier: first_tokens[1].clone(),
      parameter_list: param_nodes,
      modifiers: modifier_node,
      body: method_body,
      end_token: end_method_token
   }),errors)))
}


fn parse_function_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  (Box<dyn IAstNode>, Vec<GoldDocumentError>)), GoldParserError<'a>>{
   // parse func [ident]
   let (next, first_tokens) = match seq_parse(&[
      exp_token(TokenType::Func),
      exp_token(TokenType::Identifier),
   ]) (input){
      Ok(r) => r,
      Err(e) => return Err(e)
   };
   
   // parse params
   let (next, param_nodes) = match parse_parameter_declaration_list(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Failed to parse func decl: ", e))
   };
   // return 
   let (next, _) = match exp_token(TokenType::Return)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Failed to parse func decl: ", e))
   };
   // return type
   let return_type_parsers = [
      parse_type_basic
   ];
   let (next, return_type_node) = match alt_parse(&return_type_parsers)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Failed to parse func decl: failed to parse return type: ", e))
   };
   let mut end = get_end_pos(return_type_node.as_range());
   // modifiers (private, protected, etc.)
   let (mut next, modifier_node) = match parse_method_modifiers(next) {
      Ok((n, node)) => (n, node),
      Err(e) => return Err(prepend_msg_to_error("Failed to parse func decl: ", e))
   };
   end = if modifier_node.is_some() {get_end_pos(modifier_node.as_ref().unwrap())} else {end};

   let mut errors = Vec::<GoldDocumentError>::new();
   // if proc is not forward and not external, parse body
   let mut method_body: Option<AstMethodBody> = None;
   let mut end_method_token = None;
   if has_method_body(&modifier_node) {
      let body_tokens; 
      (next, body_tokens, end_method_token) = take_until([TokenType::EndFunc, TokenType::End].as_ref())(next)?;
      match parse_method_body(body_tokens){
         Ok((_, (mut node, errs))) => {
            if node.is_none() {
               node = Some(AstMethodBody{
                  raw_pos: first_tokens.get(0).unwrap().get_raw_pos(),
                  pos: first_tokens.get(0).unwrap().get_pos(),
                  range: first_tokens.get(0).unwrap().get_range(),
                  statements: Vec::new()
               });
            }
            method_body = node;
            errors.extend(errs.into_iter());
         },
         // TODO improve structure for error handling
         Err(_) => ()
      }
      if end_method_token.is_none(){
         errors.push(GoldDocumentError { 
            range: first_tokens.get(0).unwrap().get_range(), 
            msg: "proc end token not found".to_string() }
         )
      }
   }
   end = if end_method_token.is_some() {end_method_token.as_ref().unwrap().get_range().end} else {end};
   
   return Ok((next, (Box::new(AstFunction{
      raw_pos: first_tokens[0].raw_pos,
      pos: first_tokens[0].pos.clone(),
      range: Range { start: first_tokens[0].pos.clone(), end: end},
      identifier: first_tokens[1].clone(),
      parameter_list: param_nodes,
      return_type: return_type_node,
      modifiers: modifier_node,
      body: method_body,
      end_token: end_method_token
   }),errors)))
}

fn has_method_body(modifier_node: &Option<AstMethodModifiers>) -> bool {
   if modifier_node.is_none() {return true}
   let node_ref = modifier_node.as_ref().unwrap();
   return !node_ref.is_forward && node_ref.external_dll_name.is_none()
}

fn parse_parameter_declaration_list<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Option<AstParameterDeclarationList>), GoldParserError<'a>>{
   // opening (
   let (next, obracket_token) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t),
      Err(e) => return Ok((e.input, None))
   };
   // param decl list
   let (next, param_decl_list) = match parse_separated_list(
      next,
      parse_parameter_declaration,
      TokenType::Comma) {
         Ok(r) => r,
         Err(e) => return Err(prepend_msg_to_error("Failed to parse param list decl: ", e))
   };
   // closing )
   let (next, cbracket_token) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(prepend_msg_to_error("Failed to parse param list decl: ", e))
   };

   return Ok((next, Some(AstParameterDeclarationList{
      raw_pos: obracket_token.raw_pos,
      pos: obracket_token.pos.clone(),
      range: Range{start: obracket_token.pos, end: cbracket_token.range.end},
      parameter_list: param_decl_list,
   })));
}

fn parse_parameter_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), GoldParserError<'a>>{
   // optional var/inout/const
   let (next, modifier_token) = match alt_parse(&[
      exp_token(TokenType::Const),
      exp_token(TokenType::Var),
      exp_token(TokenType::InOut),
   ])(input){
      Ok((n, t)) => (n,Some(t)),
      Err(e) => (e.input, None)
   };
   // identifier
   let (next, ident_token) = match exp_token(TokenType::Identifier)(next) {
       Ok(r) => r,
       Err(e) => return Err(prepend_msg_to_error("Failed parsing parameter decl: ", e))
   };
   // calculate pos and range
   let raw_pos;
   let pos;
   if modifier_token.is_some() {
      raw_pos = modifier_token.as_ref().unwrap().raw_pos;
      pos = Some(modifier_token.as_ref().unwrap().pos.clone());
   } else {
      raw_pos = ident_token.raw_pos;
      pos = Some(ident_token.pos.clone());
   };
   let pos = pos.unwrap();
   let mut range = Range{start: pos.clone(), end: ident_token.pos.clone()};
   // colon
   let (next, colon) = match exp_token(TokenType::Colon)(next) {
      Ok((n, t)) => (n, Some(t)),
      Err(e) => (e.input, None)
   };
   if colon.is_some(){
      let (next, type_node) = match parse_type(next) {
         Ok(r) => r,
         Err(e) => return Err(prepend_msg_to_error("Failed parsing parameter decl: ", e))
      };
      range.end = type_node.get_range().end.clone();
      return Ok((next, Box::new(AstParameterDeclaration{
         raw_pos,
         pos,
         range,
         identifier: ident_token,
         modifier: modifier_token,
         type_node: Some(type_node)
      })));
   } else {
      return Ok((next, Box::new(AstParameterDeclaration{
         raw_pos,
         pos,
         range,
         identifier: ident_token,
         modifier: modifier_token,
         type_node: None
      })));
   }
}

/// parsers all modifiers, whether it is valid will be done in sematic analysis
fn parse_member_modifiers<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Option<Box<AstMemberModifiers>>), GoldParserError<'a>>{
   // private
   let mut start = None;
   let mut end = None;
   let mut raw_pos : Option<usize>= None;
   let (next, private_token) = match opt_token(TokenType::Private)(input) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   start = if start == None && private_token.is_some() {Some(private_token.as_ref().unwrap().pos.clone())} else {start};
   end = if private_token.is_some() {Some(get_end_pos(private_token.as_ref().unwrap()))} else {end};
   raw_pos = if raw_pos == None && private_token.is_some() {Some(private_token.as_ref().unwrap().raw_pos)} else {raw_pos};
   
   // protected
   let (next, protected_token) = match opt_token(TokenType::Protected)(next) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   start = if start == None && protected_token.is_some() {Some(protected_token.as_ref().unwrap().pos.clone())} else {start};
   end = if protected_token.is_some() {Some(get_end_pos(protected_token.as_ref().unwrap()))} else {end};
   raw_pos = if raw_pos == None && protected_token.is_some() {Some(protected_token.as_ref().unwrap().raw_pos)} else {raw_pos};
   
   // final
   let (next, final_token) = match opt_token(TokenType::Final)(next) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   start = if start == None && final_token.is_some() {Some(final_token.as_ref().unwrap().pos.clone())} else {start};
   end = if final_token.is_some() {Some(get_end_pos(final_token.as_ref().unwrap()))} else {end};
   raw_pos = if raw_pos == None && final_token.is_some() {Some(final_token.as_ref().unwrap().raw_pos)} else {raw_pos};
   
   // override
   let (next, override_token) = match opt_token(TokenType::Override)(next) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   start = if start == None && override_token.is_some() {Some(override_token.as_ref().unwrap().pos.clone())} else {start};
   end = if override_token.is_some() {Some(get_end_pos(override_token.as_ref().unwrap()))} else {end};
   raw_pos = if raw_pos == None && override_token.is_some() {Some(override_token.as_ref().unwrap().raw_pos)} else {raw_pos};
   
   
   if private_token.is_none() && protected_token.is_none() && final_token.is_none() &&
   override_token.is_none(){
      return Ok((input, None))
   } else {
      return Ok((next, Some(Box::new(AstMemberModifiers{
         raw_pos: raw_pos.unwrap(),
         range: Range { start: start.unwrap(), end: end.unwrap()},
         is_private: private_token.is_some(),
         is_protected: protected_token.is_some(),
         is_final: final_token.is_some(),
         is_override: override_token.is_some(),
      }))))
   }
} 

/// parsers all modifiers, whether it is valid will be done in sematic analysis
fn parse_method_modifiers<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Option<AstMethodModifiers>), GoldParserError<'a>>{
   // private
   let mut start = None;
   let mut end = None;
   let mut raw_pos : Option<usize>= None;
   let (next, member_modifiers) = parse_member_modifiers(input)?;
   start = if start == None && member_modifiers.is_some() {Some(member_modifiers.as_ref().unwrap().get_pos())} else {start};
   end = if member_modifiers.is_some() {Some(get_end_pos(member_modifiers.as_ref().unwrap().as_range()))} else {end};
   raw_pos = if raw_pos == None && member_modifiers.is_some() {Some(member_modifiers.as_ref().unwrap().raw_pos)} else {raw_pos};
   
   // external
   let (next, external_token_list) = match seq_parse(&[
      exp_token(TokenType::External),
      exp_token(TokenType::StringLiteral)
   ])(next) {
      Ok((n, token_list)) => (n, Some(token_list)),
      Err(e) => (e.input, None)
   };
   start = if start == None && external_token_list.is_some() {Some(external_token_list.as_ref().unwrap()[1].pos.clone())} else {start};
   end = if external_token_list.is_some() {Some(get_end_pos(external_token_list.as_ref().unwrap().get(1).unwrap()))} else {end};
   raw_pos = if raw_pos == None && external_token_list.is_some() {Some(external_token_list.as_ref().unwrap()[1].raw_pos)} else {raw_pos};

   // forward
   let (next, forward_token) = match opt_token(TokenType::Forward)(next) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   start = if start == None && forward_token.is_some() {Some(forward_token.as_ref().unwrap().pos.clone())} else {start};
   end = if forward_token.is_some() {Some(get_end_pos(forward_token.as_ref().unwrap()))} else {end};
   raw_pos = if raw_pos == None && forward_token.is_some() {Some(forward_token.as_ref().unwrap().raw_pos)} else {raw_pos};

   if member_modifiers.is_none()&& external_token_list.is_none() && forward_token.is_none(){
      return Ok((input, None))
   } else {
      return Ok((next, Some(AstMethodModifiers{
         raw_pos: raw_pos.unwrap(),
         range: Range { start: start.unwrap(), end: end.unwrap()},
         modifiers: member_modifiers,
         external_dll_name: if external_token_list.is_some() {
            external_token_list.as_ref().unwrap()[1].value.clone()
         } else {
            None
         },
         is_forward: forward_token.is_some()
      })))
   }
} 


/// parsers all modifiers, whether it is valid will be done in sematic analysis
fn parse_method_modifiers_<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Option<AstMethodModifiers>), GoldParserError<'a>>{
   // private
   let (next, token_list) = match seq_parse(&[
      opt_token(TokenType::Private),
      opt_token(TokenType::Protected),
      opt_token(TokenType::Final),
      opt_token(TokenType::Override),
      opt_token(TokenType::External),
      opt_token(TokenType::StringLiteral),
      opt_token(TokenType::Forward),
   ])(input) {
      Ok((n, token_list)) => (n, Some(token_list)),
      Err(_) => (input, None)
   };
   // external
   let (next, external_token_list) = match seq_parse(&[
      exp_token(TokenType::External),
      exp_token(TokenType::StringLiteral)
   ])(next) {
      Ok((n, token_list)) => (n, Some(token_list)),
      Err(e) => (e.input, None)
   };
   // forward
   let (next, forward_token) = match opt_token(TokenType::Forward)(next) {
      Ok((n,t)) => (n, t),
      Err(e) => (e.input, None)
   };
   // if private_token.is_none() && protected_token.is_none() && final_token.is_none() &&
   // override_token.is_none() && external_token_list.is_none() && forward_token.is_none(){
   //    return Ok((input, None))
   // } else {
   //    return Ok((next, Some(AstMethodModifiers{
   //       raw_pos
   //       is_private: private_token.is_some(),
   //       is_protected: protected_token.is_some(),
   //       is_final: final_token.is_some(),
   //       is_override: override_token.is_some(),
   //       external_dll_name: if external_token_list.is_some() {
   //          external_token_list.unwrap()[0].value
   //       } else {
   //          None
   //       },
   //       is_forward: forward_token.is_some()
   //    })))
   // }
   // maybe a better way to do it?
   todo!()
} 

fn parse_method_body<'a>(input : &'a [Token]) -> Result<(&'a [Token], (Option<AstMethodBody>, Vec<GoldDocumentError>)), GoldParserError<'a>>{

   if input.len() == 0 {
      return Ok((input, (None, Vec::new())))
   }

   let (next, statements, errors) = parse_repeat(input, parse_statement_v2);
   
   let raw_pos = input.first().unwrap().get_raw_pos();
   let start_pos = input.first().unwrap().get_pos();
   let end_pos = input.last().unwrap().get_pos();
   // range is until the endProc
   let range = Range{start: start_pos.clone(), end: end_pos};
   return Ok((next, (Some(AstMethodBody{
      raw_pos,
      pos: start_pos,
      range,
      statements,
   }), errors)))
}

fn parse_separated_list<'a>(
   input : &'a [Token],
   parser : impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), GoldParserError>,
   separator: TokenType) 
-> Result<(&'a [Token],  Vec<Box<dyn IAstNode>>), GoldParserError<'a>> {
   let mut identifiers = Vec::<Box<dyn IAstNode>>::new();
   // match first identifier
   let r = _parse_seperated_list_recursive(input, &parser, &separator, &mut identifiers);
   match r{
      Ok(r) => return Ok((r, identifiers)),
      Err(e) => return Err(e)
   };
}

fn _parse_seperated_list_recursive<'a, 'b>(
   input : &'a [Token],
   parser :&'b impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), GoldParserError>,
   sep: &'b TokenType,
   result: &'b mut Vec<Box<dyn IAstNode>>) 
-> Result<&'a [Token], GoldParserError<'a>> {
   // match first identifier
   let next = match parser(input){
      Ok((r, n)) => {result.push(n); r},
      Err(e) => return Err(GoldParserError { input: e.input, msg:  String::from("Failed to parse uses list")})
   };
   let next = match exp_token(sep.clone())(next){
      Ok((r, _)) => r,
      Err(e) => return Ok(e.input)
   };
   return _parse_seperated_list_recursive(next, parser, sep, result)
}

fn parse_separated_list_token<'a>(input : &'a [Token], item : TokenType, separator: TokenType) 
-> Result<(&'a [Token],  Vec<Token>), GoldParserError<'a>> {
   let mut identifiers = Vec::<Token>::new();
   // match first identifier
   let r = _parse_seperated_list_token_recursive(input, &item, &separator, &mut identifiers);
   match r{
      Ok(r) => return Ok((r, identifiers)),
      Err(e) => return Err(e)
   };
}

fn _parse_seperated_list_token_recursive<'a, 'b>(
   input : &'a [Token],
   item: &'b TokenType,
   sep: &'b TokenType,
   result: &'b mut Vec<Token>) 
-> Result<&'a [Token], GoldParserError<'a>> {
   // match first identifier
   let mut next = match exp_token(item.clone())(input){
      Ok((r, t)) => {result.push(t); r},
      Err(e) => return Err(GoldParserError { input: e.input, msg:  format!("Failed to parse token list: {}", e.msg)})
   };
   next = match exp_token(sep.clone())(next){
      Ok((r, _)) => r,
      Err(e) => return Ok(e.input)
   };
   return _parse_seperated_list_token_recursive(next, item, sep, result)
}

/// Returns parser that expects the given token
fn exp_token(token_type : TokenType)
   -> impl Fn(&[Token]) -> Result<(&[Token],  Token), GoldParserError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Token), GoldParserError> {
      let mut it = input.iter();
      match it.next() {
         Some(t) if t.token_type == token_type => Ok((it.as_slice(), t.clone())),
         Some(t) => Err(GoldParserError {input: input, msg: String::from(format!("Expected {:?}, found {:?}",token_type,t.token_type))}),
         None => Err(GoldParserError {input: input, msg: String::from(format!("Expected {:?}, found None",token_type))})
      }
   }
}

/// Wraps the parser so that it doesn't throw error
fn opt_token(token_type: TokenType) 
   -> impl Fn(&[Token]) -> Result<(&[Token],  Option<Token>), GoldParserError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Option<Token>), GoldParserError> {
      let mut it = input.iter();
      match it.next() {
         Some(t) if t.token_type == token_type => Ok((it.as_slice(), Some(t.clone()))),
         _ => Ok((input, None))
      }
   }
}

fn take_until(token_types: &[TokenType])
   -> impl Fn(&[Token]) -> Result<(&[Token],  &[Token], Option<Token>), GoldParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  &[Token], Option<Token>), GoldParserError> {
      let mut it = input.iter();
      let mut count: usize = 0;

      let mut next = it.next();
      let mut end_method_token : Option<Token> = None;
      while next.is_some() {
         let next_token = next.unwrap();
         count += 1;
         if token_types.iter().any(|token_type| {next_token.token_type == token_type.clone()}) {
            end_method_token = Some(next_token.clone()); 
            break
         }
         next = it.next()
      }
      count = if count == 0 {count} else {count-1};
      return Ok((it.as_slice(), &input[0..count], end_method_token));
   }
}

/// Wraps the parser so that it doesn't throw error
fn opt_parse<T>(parser : impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError>) 
   -> impl Fn(&[Token]) -> Result<(&[Token],  Option<T>), GoldParserError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Option<T>), GoldParserError> {
      match parser(input) {
          Ok((r, n)) => return Ok((r, Some(n))),
          _ => ()
      };

      return Ok((input, None));
   }
}

/// Returns parser which parses with one of the provided parsers.
/// Parser returns the first successful parse.
/// If unable to parse, will return the error of the parser which was able
/// to parse the most
fn alt_parse<'a, T>(list_of_parsers : &'a[impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError> + 'a
{
   move |input: &[Token]| -> Result<(&[Token],  T), GoldParserError> {
      let mut most_matched: Option<GoldParserError> = None;
      for parser in list_of_parsers {
         let r = parser(input);
         match r {
            Ok(r) => return Ok(r),
            Err(e) => {
               // update most matched
               if most_matched.is_some(){
                  if most_matched.as_ref().unwrap().input.len() > e.input.len() {
                     most_matched = Some(e);
                  } 
               } else {
                  most_matched = Some(e);
               }
            }
         }
      }
      return Err(most_matched.unwrap());
   }
}

/// Returns parser which parses with the given sequence of parsers.
fn seq_parse<T>(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Vec<T>), GoldParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Vec<T>), GoldParserError> {
      let mut i = 0;
      let mut next = input;
      let mut nodes = Vec::<T>::new();
      while i < list_of_parsers.len(){
         next = match list_of_parsers[i](next){
            Ok(r) => {nodes.push(r.1); r.0},
            Err(e) => return Err(GoldParserError{ input: e.input, msg: format!("failed to parse sequence: {}",e.msg) })
         };
         i+=1;
      }
      return Ok((next, nodes));
   }
}

fn create_closure<T>(func : impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError>)
   -> impl Fn(&[Token]) -> Result<(&[Token],  T), GoldParserError> {
   move |input: &[Token]| -> Result<(&[Token],  T), GoldParserError>{
      func(input)
   }
}

#[cfg(test)]
mod test {
   use crate::{lexer::tokens::{Token, TokenType}, parser::{parse_uses, parse_type_enum, parse_type_reference, parse_type_declaration, parse_constant_declaration, parse_global_variable_declaration, parse_procedure_declaration, parse_parameter_declaration_list, parse_method_modifiers, parse_function_declaration, parse_type_basic}, ast::{AstClass, AstUses, AstTypeBasic, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration, AstProcedure, AstParameterDeclaration, AstParameterDeclarationList, AstMethodModifiers, IAstNode, AstFunction}};
   use crate::utils::{Position,Range, create_new_range_from_irange, test_utils::cast_and_unwrap};
   use super::{parse_class, parse_type};

   pub fn gen_list_of_tokens(list : &[(TokenType, Option<String>)]) -> Vec<Token> {
      let mut result = Vec::<Token>::new();
      let mut raw_pos = 0;
      for (tok_type, val) in list.to_vec() {
         let start_pos = Position{line: raw_pos/20, character: raw_pos%20};
         let end_pos = Position{line:start_pos.line, character: start_pos.character+val.as_ref().unwrap_or(&"".to_string()).len()};
         result.push(Token { 
            raw_pos: raw_pos, 
            pos: start_pos.clone(), 
            range: Range{start:start_pos, end:end_pos},
            token_type: tok_type, 
            value: val.clone() 
         });
         raw_pos+=val.as_ref().unwrap().len()+5;
      }  
      return result;
   }

   pub fn check_node_pos_and_range(node: &dyn IAstNode,input: &Vec<Token>){
      assert_eq!(node.get_raw_pos(), input.first().unwrap().raw_pos);
      assert_eq!(node.get_pos(), input.first().unwrap().pos);
      assert_eq!(node.get_range(), create_new_range_from_irange(input.first().unwrap(), input.last().unwrap()));
   }

   #[test]
   fn test_parse_class(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, Some("class".to_string())),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let r = parse_class(&input).unwrap();
      let class = r.1.as_any().downcast_ref::<AstClass>().unwrap();
      check_node_pos_and_range(class, &input);
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.name, "aTestClass");
      assert_eq!(class.raw_pos, 0);
      assert_eq!(class.parent_class, "aParentClass");
   }

   #[test]
   fn test_parse_class_too_short(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, Some("class".to_string())),
         (TokenType::Identifier, Some("aTestClass".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("aParentClass".to_string())),
      ]);
      assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_class_wrong_token(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, Some("class".to_string())),
         (TokenType::Plus, Some(String::from("aTestClass"))),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
         assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_uses(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, Some("uses".to_string())),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
      let r = parse_uses(&input).unwrap();
      // ensure returned input is empty
      assert_eq!(r.0.len(), 0);
      let uses_node = r.1.as_any().downcast_ref::<AstUses>().unwrap();
      check_node_pos_and_range(uses_node, &input);

      // first uses
      let token = &uses_node.list_of_uses[0];
      assert_eq!(token.raw_pos, input.get(1).unwrap().get_raw_pos());
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aTestClass");

      // second uses
      let token = &uses_node.list_of_uses[1];
      assert_eq!(token.raw_pos, input.get(3).unwrap().get_raw_pos());
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aParentClass");
   }

   #[test]
   fn test_parse_uses_trailing_comma(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, Some("uses".to_string())),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::Comma, Some(",".to_string())),
         ]);
      assert!(parse_uses(&input).is_err());
   }

   #[test]
   fn test_parse_type_basic() {
      let input = gen_list_of_tokens(&[
         (TokenType::Int1, Some("int1".to_string())),
         (TokenType::Int2, Some("int2".to_string())),
         (TokenType::Int4, Some("Int4".to_string())),
         (TokenType::Int8, Some("Int8".to_string())),
         (TokenType::Boolean, Some("Boolean".to_string())),
         (TokenType::Char, Some("Char".to_string())),
         (TokenType::Num4, Some("Num4".to_string())),
         (TokenType::Num8, Some("Num8".to_string())),
         (TokenType::Num10, Some("Num10".to_string())),
         (TokenType::Decimal, Some("Decimal".to_string())),
         (TokenType::CString, Some("CString".to_string())),
         (TokenType::String, Some("String".to_string())),
         (TokenType::Text, Some("Text".to_string())),
         (TokenType::Identifier, Some("tCustomType".to_string())),
      ]);
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type_basic(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasic>().unwrap();
         assert_eq!(downcasted.raw_pos,input[count].raw_pos);
         assert_eq!(downcasted.pos,input[count].pos);
         assert_eq!(downcasted.range,input[count].range);
         assert_eq!(
            downcasted.type_token.token_type,
            input[count].token_type
         );
         count += 1;
         next = remaining;
         // additional checking last token
         if next.is_empty(){
            assert_eq!(downcasted.type_token.value.as_ref().unwrap().as_str(), "tCustomType")
         }
      }
   } 

   #[test]
   fn test_parse_type_enum() {
      let input = gen_list_of_tokens(&[
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("Variant1".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Variant2".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Variant3".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_enum(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeEnum>().unwrap();
      check_node_pos_and_range(downcasted, &input);
   }

   #[test]
   fn test_parse_type_reference_refto() {
      let input = gen_list_of_tokens(&[
         (TokenType::RefTo, Some("refto".to_string())),
         (TokenType::OSqrBracket, Some("[".to_string())),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, Some("]".to_string())),
         (TokenType::Identifier, Some("aType".to_string())),
         (TokenType::Inverse, Some("inverse".to_string())),
         (TokenType::Identifier, Some("InvVar".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_reference(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeReference>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.ref_type.token_type, TokenType::RefTo);
      assert_eq!(downcasted.options.len(), 3);
      assert_eq!(downcasted.ident_token.get_value(), "aType".to_string());
      assert_eq!(downcasted.inverse_var_token.as_ref().unwrap().get_value(), "InvVar".to_string());
      assert_eq!(downcasted.options[0].value.as_ref().unwrap().as_str(), "A");
      assert_eq!(downcasted.options[1].value.as_ref().unwrap().as_str(), "P");
      assert_eq!(downcasted.options[2].value.as_ref().unwrap().as_str(), "T");
   }

   #[test]
   fn test_parse_type_declaration_refto() {
      let input = gen_list_of_tokens(&[
         (TokenType::Type, Some("type".to_string())),
         (TokenType::Identifier, Some("tTestType".to_owned())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::RefTo, Some("refto".to_string())),
         (TokenType::OSqrBracket, Some("[".to_string())),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, Some("]".to_string())),
         (TokenType::Identifier, Some("aType".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeDeclaration>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "tTestType");

      // test refto type
      let downcasted = downcasted.type_node.as_any().downcast_ref::<AstTypeReference>().unwrap();
      assert_eq!(downcasted.ref_type.token_type, TokenType::RefTo);
      assert_eq!(downcasted.options.len(), 3);
      assert_eq!(downcasted.options[0].value.as_ref().unwrap().as_str(), "A");
      assert_eq!(downcasted.options[1].value.as_ref().unwrap().as_str(), "P");
      assert_eq!(downcasted.options[2].value.as_ref().unwrap().as_str(), "T");
   }

   #[test]
   fn test_parse_constant_declaration_string() {
      let input = gen_list_of_tokens(&[
         (TokenType::Const, Some("const".to_string())),
         (TokenType::Identifier, Some("cAConstant".to_string())),
         (TokenType::Equals, Some("=".to_string())),
         (TokenType::StringLiteral, Some("a constant string".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_constant_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstConstantDeclaration>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "cAConstant");
      assert_eq!(downcasted.value.value.as_ref().unwrap().as_str(), "a constant string");
   }

   #[test]
   fn test_parse_global_variable_declaration() {
      let input = gen_list_of_tokens(&[
         (TokenType::Memory, Some("memory".to_string())),
         (TokenType::Identifier, Some("aVariable".to_owned())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::RefTo, Some("refto".to_string())),
         (TokenType::OSqrBracket, Some("[".to_string())),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, Some("]".to_string())),
         (TokenType::Identifier, Some("aType".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Override, Some("override".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_global_variable_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstGlobalVariableDeclaration>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "aVariable");
      assert!(downcasted.modifiers.as_ref().unwrap().is_override);
      assert!(downcasted.modifiers.as_ref().unwrap().is_protected);

      // test refto type
      let downcasted = downcasted.type_node.as_any().downcast_ref::<AstTypeReference>().unwrap();
      assert_eq!(downcasted.ref_type.token_type, TokenType::RefTo);
      assert_eq!(downcasted.options.len(), 3);
      assert_eq!(downcasted.options[0].value.as_ref().unwrap().as_str(), "A");
      assert_eq!(downcasted.options[1].value.as_ref().unwrap().as_str(), "P");
      assert_eq!(downcasted.options[2].value.as_ref().unwrap().as_str(), "T");
   }

   #[test]
   fn test_parse_procedure_declaration() {
      let input = gen_list_of_tokens(&[
         (TokenType::Proc, Some("procedure".to_string())),
         (TokenType::Identifier, Some("FirstMethod".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("FirstParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("FirstParamType".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("SecondParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("SecondParamType".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
         (TokenType::Private, Some("private".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Final, Some("final".to_string())),
         (TokenType::Override, Some("override".to_string())),
         (TokenType::External, Some("external".to_string())),
         (TokenType::StringLiteral, Some("SomeDLL.Method".to_string())),
         (TokenType::Forward, Some("forward".to_string())),
      ]);
      let next : &[Token] = &input;
      let (_, (node, errors)) = parse_procedure_declaration(next).unwrap();
      let downcasted = cast_and_unwrap::<AstProcedure>(&node);
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "FirstMethod");
      assert_eq!(errors.len(), 0);

      // test params
      let params = downcasted.parameter_list.as_ref().unwrap();
      assert_eq!(params.parameter_list.len(), 2);
      let expected_param_idents = ["FirstParam", "SecondParam"];
      let expected_param_types = ["FirstParamType", "SecondParamType"];
      for (i, param_node) in params.parameter_list.iter().enumerate() {
         let param_node = cast_and_unwrap::<AstParameterDeclaration>(param_node);
         let ident = param_node.identifier.value.as_ref().unwrap().as_str();
         let type_node = cast_and_unwrap::<AstTypeBasic>(&param_node.type_node.as_ref().unwrap());
         let type_ident = type_node.type_token.value.as_ref().unwrap().as_str();
         assert_eq!(ident, expected_param_idents[i]);
         assert_eq!(type_ident, expected_param_types[i]);
      }
      // test modifiers
      let modifiers_node = &downcasted.modifiers.as_ref().unwrap();
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_private);
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_protected);
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_final);
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_override);
      assert_eq!(modifiers_node.external_dll_name.as_ref().unwrap().as_str(), "SomeDLL.Method");
      assert!(modifiers_node.is_forward);
   }

   #[test]
   fn test_parse_function_declaration() {
      let input = gen_list_of_tokens(&[
         (TokenType::Func, Some("function".to_string())),
         (TokenType::Identifier, Some("FirstMethod".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::InOut, Some("inout".to_string())),
         (TokenType::Identifier, Some("FirstParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("FirstParamType".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("SecondParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("SecondParamType".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
         (TokenType::Return, Some("return".to_string())),
         (TokenType::Identifier, Some("aReturnType".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Override, Some("override".to_string())),
         (TokenType::EndFunc, Some("endFunc".to_string())),

      ]);
      let next : &[Token] = &input;
      let (_, (node, errors)) = parse_function_declaration(next).unwrap();
      let downcasted = cast_and_unwrap::<AstFunction>(&node);
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "FirstMethod");
      assert_eq!(errors.len(), 0);

      // test params
      let params = downcasted.parameter_list.as_ref().unwrap();
      assert_eq!(params.parameter_list.len(), 2);
      let expected_param_idents = ["FirstParam", "SecondParam"];
      let expected_param_types = ["FirstParamType", "SecondParamType"];
      for (i, param_node) in params.parameter_list.iter().enumerate() {
         let param_node = cast_and_unwrap::<AstParameterDeclaration>(param_node);
         let ident = param_node.identifier.value.as_ref().unwrap().as_str();
         let type_node = cast_and_unwrap::<AstTypeBasic>(&param_node.type_node.as_ref().unwrap());
         let type_ident = type_node.type_token.value.as_ref().unwrap().as_str();
         assert_eq!(ident, expected_param_idents[i]);
         assert_eq!(type_ident, expected_param_types[i]);
      }
      // test return type
      let return_node = downcasted.return_type.as_any().downcast_ref::<AstTypeBasic>().unwrap();
      assert_eq!(return_node.type_token.value.as_ref().unwrap().as_str(), "aReturnType");
      // test modifiers
      let modifiers_node = &downcasted.modifiers.as_ref().unwrap();
      assert!(!modifiers_node.modifiers.as_ref().unwrap().is_private);
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_protected);
      assert!(!modifiers_node.modifiers.as_ref().unwrap().is_final);
      assert!(modifiers_node.modifiers.as_ref().unwrap().is_override);
      assert!(modifiers_node.external_dll_name.is_none());
      assert!(!modifiers_node.is_forward);
   }

   #[test]
   fn test_parse_parameter_declaration_list() {
      let input = gen_list_of_tokens(&[
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::InOut, Some("inout".to_string())),
         (TokenType::Identifier, Some("FirstParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("FirstParamType".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Var, Some("var".to_string())),
         (TokenType::Identifier, Some("SecondParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("SecondParamType".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Var, Some("const".to_string())),
         (TokenType::Identifier, Some("ThirdParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("ThirdParamType".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      // test pos
      let next : &[Token] = &input;
      let (_, node) = parse_parameter_declaration_list(next).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(&node, &input);

      // test params
      assert_eq!(node.parameter_list.len(), 3);
      for (i, param_node) in node.parameter_list.iter().enumerate() {
         let param_node = cast_and_unwrap::<AstParameterDeclaration>(param_node);
         let ident = param_node.identifier.value.as_ref().unwrap().as_str();
         let modifier = param_node.modifier.as_ref().unwrap().value.as_ref().unwrap().as_str();
         let type_node = cast_and_unwrap::<AstTypeBasic>(&param_node.type_node.as_ref().unwrap());
         let type_ident = type_node.type_token.value.as_ref().unwrap().as_str();
         assert_eq!(modifier, input[1+0+i*5].value.as_ref().unwrap().as_str());
         assert_eq!(ident, input[1+1+i*5].value.as_ref().unwrap().as_str());
         assert_eq!(type_ident, input[1+3+i*5].value.as_ref().unwrap().as_str());
      }
   }

   #[test]
   fn test_parse_parameter_declaration_list_2() {
      let input = gen_list_of_tokens(&[
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::InOut, Some("inout".to_string())),
         (TokenType::Identifier, Some("FirstParam".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("SecondParam".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("SecondParamType".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      // test pos
      let next : &[Token] = &input;
      let (_, node) = parse_parameter_declaration_list(next).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(&node, &input);

      assert_eq!(node.parameter_list.len(), 2);
      // test param 1
      let param_node = cast_and_unwrap::<AstParameterDeclaration>(&node.parameter_list[0]);
      let ident = param_node.identifier.value.as_ref().unwrap().as_str();
      let modifier = param_node.modifier.as_ref().unwrap().value.as_ref().unwrap().as_str();
      let type_node = &param_node.type_node;
      assert!(type_node.is_none());
      assert_eq!(modifier, "inout");
      assert_eq!(ident, "FirstParam");

      // test param 2
      let param_node = cast_and_unwrap::<AstParameterDeclaration>(&node.parameter_list[1]);
      let ident = param_node.identifier.value.as_ref().unwrap().as_str();
      let modifier = &param_node.modifier;
      let type_node = cast_and_unwrap::<AstTypeBasic>(&param_node.type_node.as_ref().unwrap());
      let type_ident = type_node.type_token.value.as_ref().unwrap().as_str();
      assert!(modifier.is_none());
      assert_eq!(ident, "SecondParam");
      assert_eq!(type_ident, "SecondParamType");

   }

   #[test]
   fn test_parse_method_modifiers() {
      let input = gen_list_of_tokens(&[
         (TokenType::Private, Some("private".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Final, Some("final".to_string())),
         (TokenType::Override, Some("override".to_string())),
         (TokenType::External, Some("external".to_string())),
         (TokenType::StringLiteral, Some("SomeDLL.Method".to_string())),
         (TokenType::Forward, Some("forward".to_string())),
      ]);
      // test pos
      let next : &[Token] = &input;
      let (_, node) = parse_method_modifiers(next).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(&node, &input);

      // test modifiers
      assert!(node.modifiers.as_ref().unwrap().is_private);
      assert!(node.modifiers.as_ref().unwrap().is_protected);
      assert!(node.modifiers.as_ref().unwrap().is_override);
      assert!(node.modifiers.as_ref().unwrap().is_final);
      assert_eq!(node.external_dll_name.unwrap(), "SomeDLL.Method".to_string());
      assert!(node.is_forward);
   }

   #[test]
   fn test_parse_proc_decl_nested() {
      let input = gen_list_of_tokens(&[
         (TokenType::Proc, Some("procedure".to_string())),
         (TokenType::Identifier, Some("FirstMethod".to_string())),
         (TokenType::Proc, Some("procedure".to_string())),
         (TokenType::Identifier, Some("NestedMethod".to_string())),
         (TokenType::EndProc, Some("endproc".to_string())),
         (TokenType::EndProc, Some("endproc".to_string()))
      ]);
      let next = &input;
      let (_, (node, errors)) = parse_procedure_declaration(next).unwrap();
      let downcasted = cast_and_unwrap::<AstProcedure>(&node);
      // check_node_pos_and_range(downcasted, &input);
      assert_eq!(errors.len(), 1);
   }
}