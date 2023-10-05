

use std::sync::Arc;

use crate::lexer::tokens::{Token, TokenType};
use crate::parser::body_parser::parse_identifier;
use crate::utils::{Range, create_new_range_from_irange, IRange, create_new_range};
use crate::parser::ast::{AstClass, AstUses, IAstNode, AstTypeBasic, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration, AstParameterDeclaration, AstParameterDeclarationList, AstProcedure, AstMethodModifiers, AstComment, AstMethodBody, AstFunction, AstMemberModifiers, AstEmpty, AstEnumVariant, AstTypeSet, AstTypeRecordField, AstTypeRecord, AstTypePointer, AstTypeArray, AstTypeRange};

use self::ast::{AstTypeProcedure, AstTypeFunction, AstTypeInstanceOf, AstMethodNameWithEvent, AstTerminal, AstModule, MemberModifiers, AstTypeSized, AstRoot};
use self::body_parser::{parse_statement_v2, parse_literal_basic, parse_ident_token, parse_binary_ops_w_context};
use self::utils::{prepend_msg_to_error, exp_token, take_until, alt_parse, opt_parse, parse_separated_list_token, seq_parse, opt_token, alt_parse_w_context, parse_separated_list_w_context, parse_until_strict_w_context, opt_parse_w_context, parse_repeat_w_context, parse_until_no_match_w_context, seq_parse_w_context};

pub mod utils;
pub mod body_parser;
pub mod ast;
pub mod oql_parser;

#[derive(Debug, Clone)]
pub struct ParseError<'a>{
   pub input: &'a [Token],
   pub msg: String
}

#[derive(Debug, Clone)]
pub struct ParserDiagnostic{
   pub range: Range,
   pub msg: String
}

pub trait IParserContext<T> {
   fn add_diagnostic(&mut self, diagnostic: T);
   fn extend_diagnostics<U : IntoIterator<Item = T>>(&mut self, diagnostics: U);
   fn get_diagnostics(self)->Vec<T>;
}

#[derive(Default)]
pub struct ParserContext{
   diagnostics: Vec<ParserDiagnostic>
}
impl ParserContext{
   pub fn new()->ParserContext{
      return ParserContext{
         ..Default::default()
      };
   }
}
impl IParserContext<ParserDiagnostic> for ParserContext{
    fn add_diagnostic(&mut self, diagnostic: ParserDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn extend_diagnostics<U : IntoIterator<Item = ParserDiagnostic>>(&mut self, diagnostics: U) {
        self.diagnostics.extend(diagnostics.into_iter());
    }
    fn get_diagnostics(self)-> Vec<ParserDiagnostic>{
        return self.diagnostics;
    }
}

impl IRange for ParserDiagnostic{
    fn get_range(&self) -> Range {
        self.range.clone()
    }

    fn as_range(&self) -> &dyn IRange {
        self
    }
}

impl ParserDiagnostic {
   pub fn get_msg(&self) -> String{
      self.msg.clone()
   }
}

pub fn parse_gold<'a>(input : &'a [Token]) -> ((&'a [Token],  Arc<dyn IAstNode>), Vec<ParserDiagnostic>) {
   let mut context = ParserContext::new();
   let parsers = [
      parse_comment,
      parse_class,
      parse_module,
      parse_uses,
      parse_type_declaration,
      parse_constant_declaration,
      parse_global_variable_declaration,
   ];
   let block_parsers = [
      parse_procedure_declaration,
      parse_function_declaration
   ];
   let mut statements = Vec::<Arc<dyn IAstNode>>::new();

   if input.len() == 0 {
      let root_node = Arc::new(AstRoot::new(statements));
      return ((input, root_node), context.get_diagnostics())
   }
   let mut next = input;
   while next.len() > 0 {
      let mut most_matched: Option<ParseError> = None;
      match alt_parse_w_context(&block_parsers)(next, &mut context){
         Ok((r,node))=> {
            statements.push(node); 
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
      next = match alt_parse_w_context(&parsers)(next, &mut context){
         Ok((r,n))=> {statements.push(n); r},
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
            context.add_diagnostic(ParserDiagnostic {
               range: create_new_range_from_irange(first_error_token.unwrap(), last_error_token),
               msg: most_matched.msg
            });
            // move one to prevent infinite loop
            if most_matched.input.len() == next.len(){
               iter.next();
            }
            // set next as the input of the most matched error
            iter.as_slice()
         }
      };
   }
   let root_node = Arc::new(AstRoot::new(statements));
   ((next, root_node), context.get_diagnostics())
}


fn parse_comment<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
   let (next, comment_token) = exp_token(TokenType::Comment)(input)?;
   return Ok((next, Arc::new(AstComment{
      raw_pos: comment_token.raw_pos,
      pos: comment_token.get_pos(),
      range: comment_token.range.clone(),
      comment: comment_token.value.unwrap()
   })))
}

fn parse_annotations<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
   // TODO for now annotations ignored
   let (next, _) = exp_token(TokenType::OSqrBracket)(input)?;
   let (next, _, _) = take_until([TokenType::CSqrBracket].as_ref())(next)?;
   return Ok((
      next,
      Arc::new(AstEmpty::default())
   ))
}

fn parse_class<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   // class keyword
   let (next, class_token) = match exp_token(TokenType::Class)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   let (next, class_name_token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };

   // parent class
   let (next, result) =  opt_parse_w_context(parse_parent_class)(next, context)?;
   let parent_class_token = match &result{
      Some(r) => Some(r.0.clone()),
      _=> None
   };
   let end =  match &result {
      // end tok
      Some(r)=> r.1.get_range(),
      _ => class_name_token.get_range()
   };

   return Ok((next, Arc::new(AstClass{
      raw_pos: class_token.raw_pos,
      range: create_new_range(class_token.get_range(), end),
      identifier: class_name_token,
      parent_class: parent_class_token
   }))); 
}

fn parse_parent_class<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  (Token,Token)), ParseError<'a>> {
   let (next, mut tokens) = seq_parse(&[
      exp_token(TokenType::OBracket),
      exp_token(TokenType::Identifier),
      exp_token(TokenType::CBracket)
   ])(input)?;
   let end_tok = tokens.remove(2);
   let parent_class_tok = tokens.remove(1);
   return Ok((next, (parent_class_tok, end_tok)));
}

fn parse_module<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   // module 
   let (next, module_token) = exp_token(TokenType::Module)(next)?;
   // module name
   let (next, module_name) =  exp_token(TokenType::Identifier)(next)?;
   
   return Ok((next, Arc::new(AstModule{
      raw_pos: module_token.raw_pos,
      range: create_new_range_from_irange(&module_token, &module_name),
      name: module_name.get_value(),
   }))); 
}


fn parse_constant_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
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
      Arc::new(AstConstantDeclaration {
         raw_pos: const_token.raw_pos,
         pos: const_token.get_pos(),
         identifier: ident_token,
         value: value_token.clone(),
         range: create_new_range_from_irange(&const_token, &value_token),
         is_multi_lang : if multilang_token.is_some() {true} else {false} 
      })
   ))
}


fn parse_uses<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
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
      Arc::new(AstUses { 
         raw_pos: uses_token.raw_pos,
         pos: uses_token.get_pos(),
         range: Range { 
            start: uses_token.get_pos(), 
            end: if idents.last().is_some() {idents.last().unwrap().range.end.clone()} else {uses_token.range.end.clone()}},
         list_of_uses: idents })
   ));
}


fn parse_type_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   // type keyword, identifier, then colon
   let (next, tokens) = seq_parse(&[
      exp_token(TokenType::Type),
      exp_token(TokenType::Identifier),
      exp_token(TokenType::Colon)
   ])(next)?;
   // parse the type
   let (next, type_node) = match  parse_type(next, context){
       Ok(r) => r,
       Err(e) => return Err(e)
   };
   let type_declaration_node = AstTypeDeclaration {
      raw_pos: tokens[0].raw_pos,
      pos: tokens[0].get_pos(),
      range: Range { start: tokens[0].get_pos(), end: type_node.get_range().end },
      identifier: tokens[1].clone(),
      type_node: type_node
   };
   return Ok((next, Arc::new(type_declaration_node)));
}

fn parse_type<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   //
   let parsers = [
      parse_type_sized,
      parse_type_composed,
      parse_type_basic,
      parse_type_reference,
      parse_type_range,
      parse_type_set,
      parse_type_record,
      parse_type_pointer,
      parse_type_array,
      parse_type_procedure,
      parse_type_function,
      parse_type_instanceof
   ];
   let parse_result = alt_parse_w_context(&parsers)(input, context);
   return parse_result;
}

// pub fn parse_type_primitive<'a>(input : &'a [Token]) 
// -> Result<(&'a [Token],  Token), ParseError<'a>> {
//    return alt_parse(&[
//       exp_token(TokenType::Int1),
//       exp_token(TokenType::Int2),
//       exp_token(TokenType::Int4),
//       exp_token(TokenType::Int8),
//       exp_token(TokenType::Boolean),
//       exp_token(TokenType::Char),
//       exp_token(TokenType::Num4),
//       exp_token(TokenType::Num8),
//       exp_token(TokenType::Num10),
//       exp_token(TokenType::Decimal),
//       exp_token(TokenType::CString),
//       exp_token(TokenType::String),
//       exp_token(TokenType::Text),
//    ])(input);
// }

fn parse_type_basic<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C)
-> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>> {
   // basic fixed size
   let parse_result = alt_parse(&[
      // treat native types as identifiers
      //  handle later in semantic analyzer
      // exp_token(TokenType::Int1),
      // exp_token(TokenType::Int2),
      // exp_token(TokenType::Int4),
      // exp_token(TokenType::Int8),
      // exp_token(TokenType::Boolean),
      // exp_token(TokenType::Char),
      // exp_token(TokenType::Num4),
      // exp_token(TokenType::Num8),
      // exp_token(TokenType::Num10),
      // exp_token(TokenType::Decimal),
      // exp_token(TokenType::CString),
      // exp_token(TokenType::String),
      // exp_token(TokenType::Text),
      exp_token(TokenType::Identifier),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Arc::new(AstTypeBasic{
         raw_pos:t.raw_pos,
         range: t.range.clone(),
         type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_enum_variant<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   let (next, variant_ident) = exp_token(TokenType::Identifier)(next)?;
   // optional assign to int
   let (next, assign_tokens) = opt_parse(seq_parse([
      exp_token(TokenType::Equals),
      exp_token(TokenType::NumericLiteral),
   ].as_ref()))(next)?;
   let value_token = match assign_tokens{
      Some(toks) => Some(toks[1].clone()),
      _=> None
   };
   return Ok((
      next,
      Arc::new(AstEnumVariant{
         raw_pos: variant_ident.get_raw_pos(),
         range: variant_ident.get_range(),
         identifier: variant_ident,
         value_token
      })
   ))
}

fn parse_type_sized<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // int(3), cstring(4)
   let(next, ident_token) = exp_token(TokenType::Identifier)(input)?;
   let(next, _obracket) = exp_token(TokenType::OBracket)(next)?;
   let(next, size_token) = exp_token(TokenType::NumericLiteral)(next)?;
   let(next, cbracket) = exp_token(TokenType::CBracket)(next)?;

   return Ok((
      next,
      Arc::new(AstTypeSized{
         raw_pos: ident_token.get_raw_pos(),
         range: create_new_range(ident_token.get_range(), cbracket.get_range()),
         type_token: ident_token,
         size_token,
      })
   ))
}

fn parse_type_enum<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // opening (
   let (next, obracket_token) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(e)
   };
   // list of enums: enum1, enum2, enum3, ...
   let (next, variants) = match parse_separated_list_w_context(parse_enum_variant, TokenType::Comma)(next,context){
       Ok((r, ts)) => (r, ts),
       Err(e) => return Err(e)
   };
   // closing )
   let (next, cbracket_token) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(e)
   };
   return Ok((next, Arc::new(AstTypeEnum{
      raw_pos: obracket_token.raw_pos,
      pos: obracket_token.get_pos(),
      range: create_new_range_from_irange(&obracket_token, &cbracket_token),
      variants: variants
   })));
}

fn parse_type_composed<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // type t : tEnumType + (cEnumVar, cEnumVar)
   let enum_parser = alt_parse_w_context([
      parse_type_basic,
      parse_type_enum
   ].as_ref());
   return parse_binary_ops_w_context(input, &exp_token(TokenType::Plus), &enum_parser, context);
}

fn parse_type_reference<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
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
   let (next, mut option_tokens) = match parse_type_reference_options(next, context){
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

   return Ok((next, Arc::new(AstTypeReference{
      raw_pos: ref_token.raw_pos,
      pos: ref_token.get_pos(),
      ref_type: ref_token.clone(),
      range: create_new_range(ref_token.get_range(), end_range),
      options: option_tokens,
      ident_token,
      inverse_var_token: inverse_var,
   })));
}

fn parse_type_reference_options<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Vec<Token>), ParseError<'a>>{
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

fn parse_type_range<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // e.g.: 
   //    1 to 10
   //    'a' to 'b'   
   let (next, from) = parse_literal_basic(input,context)?;
   let (next, _to_token) = exp_token(TokenType::To)(next)?;
   let (next, to) = parse_literal_basic(next,context)?;
   return Ok((next, Arc::new(AstTypeRange{
      raw_pos: from.get_raw_pos(),
      range: create_new_range(from.get_range(), to.get_range()),
      from,
      to,
   })));
}

fn parse_type_set<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   let (next, obracket_token) = exp_token(TokenType::OSqrBracket)(input)?;
   let (next, set_type) = parse_type_basic(next,context)?;
   let (next, cbracket_token) = exp_token(TokenType::CSqrBracket)(next)?;
   return Ok((next, Arc::new(AstTypeSet{
      raw_pos: obracket_token.get_raw_pos(),
      range: create_new_range(obracket_token.get_range(), cbracket_token.get_range()),
      set_type,
   })))
}

fn parse_type_record_field<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   // ident
   let (next, ident_token) = exp_token(TokenType::Identifier)(next)?;
   let (next, _) = exp_token(TokenType::Colon)(next)?;
   let (next, type_node) = parse_type(next,context)?;
   return Ok((next, Arc::new(AstTypeRecordField{
      raw_pos: ident_token.get_raw_pos(),
      range: create_new_range(ident_token.get_range(), type_node.get_range()),
      identifier: ident_token,
      type_node
   })));
}

fn parse_type_record<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   let (next, record_token) = exp_token(TokenType::Record)(input)?;
   // parent record
   let (next, mut parent_tokens) = opt_parse(seq_parse([
      exp_token(TokenType::OBracket),
      exp_token(TokenType::Identifier),
      exp_token(TokenType::CBracket)
   ].as_ref()))(next)?;
   let parent_node = match &mut parent_tokens {
      Some(tokens)=> {
         let boxed_node : Arc<dyn IAstNode> = Arc::new(AstTerminal{token: tokens.remove(1)});
         Some(boxed_node)
      },
      _=> None
   };

   let (next, fields, endrecord_token) = parse_until_strict_w_context(next, exp_token(TokenType::EndRecord), parse_type_record_field, context)?;

   let end = match endrecord_token {
      Some(tok) => tok.get_range(),
      _=> match fields.last() {
          Some(node) => node.get_range(),
          _=> record_token.get_range()
      }
   };
   return Ok((next, Arc::new(AstTypeRecord{
      raw_pos: record_token.get_raw_pos(),
      range: create_new_range(record_token.get_range(), end),
      fields,
      parent: parent_node
   })))
}

fn parse_type_pointer<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   let (next, dot_token) = exp_token(TokenType::Dot)(input)?;
   let (next, type_node) = parse_type_basic(next, context)?;
   return Ok((next, Arc::new(AstTypePointer{
      raw_pos: dot_token.get_raw_pos(),
      range: create_new_range(dot_token.get_range(), type_node.get_range()),
      type_node,
   })))
}

fn parse_type_array_index<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // [
   let (next, _o_bracket) = exp_token(TokenType::OSqrBracket)(input)?;
   // index node
   let (next, index_node) = alt_parse_w_context([
      parse_type_basic,
      parse_type_range
   ].as_ref())(next, context)?;
   // ]
   let (next, _c_bracket) = exp_token(TokenType::CSqrBracket)(next)?;
   return Ok((next, index_node));
}

fn parse_type_array<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // array/sequence
   let (next, array_seq_token) = alt_parse([
      exp_token(TokenType::Array),
      exp_token(TokenType::Sequence),
   ].as_ref())(input)?;

   // index type
   let mut index_nodes = Vec::new();
   let (next, index_node) = parse_type_array_index(next, context)?;
   index_nodes.push(index_node);

   let (next, sec_index) = opt_parse_w_context(parse_type_array_index)(next, context)?;
   match sec_index {
      Some(node) => index_nodes.push(node),
      _=> ()
   };

   // of
   let (next, _of_token) = exp_token(TokenType::Of)(next)?;
   // object type
   let (next, object_type) = parse_type_basic(next, context)?;

   return Ok((next, Arc::new(AstTypeArray{
      raw_pos: array_seq_token.get_raw_pos(),
      range: create_new_range(array_seq_token.get_range(), object_type.get_range()),
      array_seq_token: array_seq_token,
      index_nodes,
      object_type,
   })))
}

fn parse_type_procedure<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // parse proc [ident]
   let (next, proc_token) = exp_token(TokenType::Proc)(input)?;
   let mut end = proc_token.get_range();
   // parse params
   let (next, param_nodes) = match parse_parameter_declaration_list(next, context){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("failed to parse proc type: ", e))
   };
   end = if param_nodes.is_some() {param_nodes.as_ref().unwrap().get_range()} else {end};
   return Ok((next, Arc::new(AstTypeProcedure{
      raw_pos: proc_token.get_raw_pos(),
      range: create_new_range(proc_token.get_range(), end),
      parameter_list: param_nodes
   })))
}

fn parse_type_function<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // parse proc [ident]
   let (next, func_token) = exp_token(TokenType::Func)(input)?;
   // parse params
   let (next, param_nodes) = parse_parameter_declaration_list(next, context)?;
   // return
   let (next, _ret_tok) = exp_token(TokenType::Return)(next)?;
   // return type
   let (next, return_type) = parse_type_basic(next, context)?;

   return Ok((next, Arc::new(AstTypeFunction{
      raw_pos: func_token.get_raw_pos(),
      range: create_new_range(func_token.get_range(), return_type.get_range()),
      parameter_list: param_nodes,
      return_type,
   })))
}

fn parse_type_instanceof<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   let (next, instof_tok) = exp_token(TokenType::InstanceOf)(input)?;
   let (next, inst_type) = parse_type_basic(next, context)?;
   return Ok((next, Arc::new(AstTypeInstanceOf{
      raw_pos: instof_tok.get_raw_pos(),
      range: create_new_range(instof_tok.get_range(), inst_type.get_range()),
      instance_type: inst_type
   })))
}

fn parse_global_variable_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // annotations
   let (next, _) = opt_parse_w_context(parse_annotations)(input, context)?;
   // memory?
   let (next, memory_token) = match exp_token(TokenType::Memory)(next){
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
   let (next, type_node) = match parse_type(next, context){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   // modifiers
   let (next, member_modifiers) = parse_member_modifiers(next, context)?;
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
      Arc::new(AstGlobalVariableDeclaration {
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

fn parse_method_name_uievent<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   let (next, method_name) = parse_identifier(input, context)?;
   let (next, _pound) = exp_token(TokenType::Pound)(next)?;
   let (next, event) = parse_identifier(next, context)?;
   return Ok((
      next,
      Arc::new(AstMethodNameWithEvent{
         raw_pos: method_name.get_raw_pos(),
         range: create_new_range(method_name.get_range(), event.get_range()),
         method_name,
         event,
      })
   ));
}

fn parse_method_name<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   return alt_parse_w_context([
      parse_method_name_uievent,
      parse_identifier,
   ].as_ref())(input, context);
}

fn parse_procedure_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // parse proc [ident]
   let (next, first_token) = exp_token(TokenType::Proc)(input)?;

   // ident
   let (next, identifier) = parse_method_name(next, context)?;

   // keep track of last end node
   let mut end = identifier.as_ast_node();
   // parse params
   let (next, param_nodes) = parse_parameter_declaration_list(next, context)?;
   end = if param_nodes.is_some() {param_nodes.as_ref().unwrap().as_ast_node()} else {end};
   // modifiers (private, protected, etc.)
   let (mut next, modifier_node) =  parse_method_modifiers(next, context)?;
   end = if modifier_node.is_some() {modifier_node.as_ref().unwrap().as_ast_node()} else {end};

   // if proc is not forward and not external, parse body
   let mut method_body: Option<AstMethodBody> = None;
   let mut end_method_token = None;
   if has_method_body(&modifier_node) {
      let body_tokens; 
      (next, body_tokens, end_method_token) = take_until([TokenType::EndProc, TokenType::End].as_ref())(next)?;
      match parse_method_body(body_tokens, context){
         Ok((_, mut node)) => {
            if node.is_none() {
               node = Some(AstMethodBody{
                  raw_pos: end.get_raw_pos(),
                  range: end.get_range(),
                  statements: Vec::new()
               });
            }
            method_body = node;
         },
         Err(e) => return Err(e)
      }
      if end_method_token.is_none(){
      context.add_diagnostic(ParserDiagnostic { 
         range: first_token.get_range(), 
         msg: "proc end token not found".to_string() }
      )
   }
   }
   let end = if end_method_token.is_some() {end_method_token.as_ref().unwrap().get_range()} else {end.get_range()};
   // copnvert to arc
   let modifier_node : Option<Arc<dyn IAstNode>> = match modifier_node {
       Some(n) => Some(Arc::new(n)),
       _=> None
   };
   let method_body: Option<Arc<dyn IAstNode>> = match method_body {
       Some(n) => Some(Arc::new(n)),
       _=> None
   };
   return Ok((
      next, 
      Arc::new(AstProcedure{
         raw_pos: first_token.get_raw_pos(),
         range: create_new_range(first_token.get_range(), end),
         identifier,
         parameter_list: param_nodes,
         modifiers: modifier_node,
         body: method_body,
         end_token: end_method_token
      })
   ))
}


fn parse_function_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
   // parse func [ident]
   let (next, first_token) = exp_token(TokenType::Func)(input)?;

   // ident
   let (next, identifier) = parse_method_name(next, context)?;
   
   // parse params
   let (next, param_nodes) = parse_parameter_declaration_list(next, context)?;
   // return 
   let (next, _) =  exp_token(TokenType::Return)(next)?;
   // return type
   let return_type_parsers = [
      parse_type_basic
   ];
   let (next, return_type_node) =  alt_parse_w_context(&return_type_parsers)(next, context)?;

   let mut end = return_type_node.as_ast_node();
   // modifiers (private, protected, etc.)
   let (mut next, modifier_node) = parse_method_modifiers(next, context)?;
   end = if modifier_node.is_some() {modifier_node.as_ref().unwrap().as_ast_node()} else {end};

   // if proc is not forward and not external, parse body
   let mut method_body: Option<AstMethodBody> = None;
   let mut end_method_token = None;
   if has_method_body(&modifier_node) {
      let body_tokens; 
      (next, body_tokens, end_method_token) = take_until([TokenType::EndFunc, TokenType::End].as_ref())(next)?;
      match parse_method_body(body_tokens, context){
         Ok((_, mut node)) => {
            if node.is_none() {
               node = Some(AstMethodBody{
                  raw_pos: end.get_raw_pos(),
                  range: end.get_range(),
                  statements: Vec::new()
               });
            }
            method_body = node;
         },
         Err(e) => return Err(e)
      }
      if end_method_token.is_none(){
         context.add_diagnostic(ParserDiagnostic { 
            range: first_token.get_range(), 
            msg: "func end token not found".to_string() }
         )
      }
   }
   let end = if end_method_token.is_some() {end_method_token.as_ref().unwrap().get_range()} else {end.get_range()};
   let modifier_node : Option<Arc<dyn IAstNode>> = match modifier_node {
      Some(n) => Some(Arc::new(n)),
      _=> None
   };
   let method_body: Option<Arc<dyn IAstNode>> = match method_body {
      Some(n) => Some(Arc::new(n)),
      _=> None
   };
   return Ok((
      next, 
      Arc::new(AstFunction{
         raw_pos: first_token.get_raw_pos(),
         range: create_new_range(first_token.get_range(), end),
         identifier,
         parameter_list: param_nodes,
         return_type: return_type_node,
         modifiers: modifier_node,
         body: method_body,
         end_token: end_method_token
      })
   ))
}

fn has_method_body(modifier_node: &Option<AstMethodModifiers>) -> bool {
   if modifier_node.is_none() {return true}
   let node_ref = modifier_node.as_ref().unwrap();
   return !node_ref.is_forward && node_ref.external_dll_name.is_none()
}

fn parse_parameter_declaration_list<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Option<Arc<dyn IAstNode>>), ParseError<'a>>{
   // opening (
   let (next, obracket_token) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t),
      Err(e) => return Ok((e.input, None))
   };
   // param decl list
   let (next, param_decl_list) = match parse_separated_list_w_context(
      parse_parameter_declaration,
      TokenType::Comma,
   )(next, context) {
         Ok(r) => r,
         Err(e) => return Err(prepend_msg_to_error("Failed to parse param list decl: ", e))
   };
   // closing )
   let (next, cbracket_token) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t),
      Err(e) => return Err(prepend_msg_to_error("Failed to parse param list decl: ", e))
   };

   return Ok((next, Some(Arc::new(AstParameterDeclarationList{
      raw_pos: obracket_token.raw_pos,
      pos: obracket_token.get_pos(),
      range: Range{start: obracket_token.get_pos(), end: cbracket_token.range.end},
      parameter_list: param_decl_list,
   }))));
}

fn parse_parameter_declaration<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Arc<dyn IAstNode>), ParseError<'a>>{
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
   let (next, ident_token) = parse_ident_token(next, context)?;
   // calculate pos and range
   let raw_pos;
   let pos;
   if modifier_token.is_some() {
      raw_pos = modifier_token.as_ref().unwrap().raw_pos;
      pos = Some(modifier_token.as_ref().unwrap().get_pos());
   } else {
      raw_pos = ident_token.raw_pos;
      pos = Some(ident_token.get_pos());
   };
   let pos = pos.unwrap();
   let mut range = Range{start: pos.clone(), end: ident_token.get_pos()};
   // colon
   let (next, colon) = match exp_token(TokenType::Colon)(next) {
      Ok((n, t)) => (n, Some(t)),
      Err(e) => (e.input, None)
   };
   if colon.is_some(){
      let (next, type_node) = match parse_type(next, context) {
         Ok(r) => r,
         Err(e) => return Err(prepend_msg_to_error("Failed parsing parameter decl: ", e))
      };
      range.end = type_node.get_range().end.clone();
      return Ok((next, Arc::new(AstParameterDeclaration{
         raw_pos,
         pos,
         range,
         identifier: ident_token,
         modifier: modifier_token,
         type_node: Some(type_node)
      })));
   } else {
      return Ok((next, Arc::new(AstParameterDeclaration{
         raw_pos,
         pos,
         range,
         identifier: ident_token,
         modifier: modifier_token,
         type_node: None
      })));
   }
}

fn parse_member_modifier_tokens<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Token), ParseError<'a>>{
   return alt_parse([
      exp_token(TokenType::Private),
      exp_token(TokenType::Protected),
      exp_token(TokenType::Final),
      exp_token(TokenType::Override),
   ].as_ref())(input);
}

fn get_member_modifers(tokens: &Vec<Token>) -> MemberModifiers{
   let (mut is_private,mut is_protected,mut is_final,mut is_override) 
   = (false,false,false,false);
   tokens.iter().for_each(|t|{
      match t.token_type {
            TokenType::Private => {is_private=true},
            TokenType::Protected => {is_protected=true},
            TokenType::Final => {is_final=true},
            TokenType::Override => {is_override=true},
            _ => (),
      }
   });
   return MemberModifiers { is_private
      , is_protected, is_final, is_override }
}

/// parsers all modifiers, whether it is valid will be done in sematic analysis
fn parse_member_modifiers<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Option<Arc<AstMemberModifiers>>), ParseError<'a>>{
   // private/protected/final/override
   let(next, modifier_tokens) = parse_until_no_match_w_context(input, parse_member_modifier_tokens, context);
   if modifier_tokens.len()==0{
      return Ok((input, None))
   }
   
   let raw_pos = modifier_tokens.first().unwrap().get_raw_pos();
   let start = modifier_tokens.first().unwrap().get_range();
   let end = modifier_tokens.last().unwrap().get_range();
   let modifiers = get_member_modifers(&modifier_tokens);
   return Ok((next, Some(Arc::new(AstMemberModifiers{
      raw_pos: raw_pos,
      range: create_new_range(start, end),
      modifier_tokens,
      modifiers,
   }))))
} 

fn parse_method_external<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Token), ParseError<'a>>{
   match seq_parse(&[
      exp_token(TokenType::External),
      exp_token(TokenType::StringLiteral)
   ])(input) {
      Ok((n, mut token_list)) => {
         let str_t = token_list.pop().unwrap();
         let ext_t = token_list.pop().unwrap();
         // TODO dirty but returning both tokens will require too much changes/addtions to utils api,
         //  not worth it for now
         return Ok((n, Token{
            raw_pos: ext_t.get_raw_pos(),
            range: create_new_range(ext_t.get_range(), str_t.get_range()),
            token_type: str_t.token_type,
            value: Some(str_t.get_value()),
         }))
      },
      Err(e) => return Err(e)
   };
}

fn parse_method_forward<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], _context : &mut C) -> Result<(&'a [Token],  Token), ParseError<'a>>{
   return exp_token(TokenType::Forward)(input);
}

/// parsers all modifiers, whether it is valid will be done in semantic analysis
fn parse_method_modifiers<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Option<AstMethodModifiers>), ParseError<'a>>{
   let method_modifier_parser = alt_parse_w_context([
      parse_member_modifier_tokens,
      parse_method_external,
      parse_method_forward,
   ].as_ref());
   
   
   let(next, modifier_tokens) = parse_until_no_match_w_context(input, method_modifier_parser, context);
   if modifier_tokens.len()==0{
      return Ok((input, None))
   }

   let raw_pos = modifier_tokens.first().unwrap().get_raw_pos();
   let start = modifier_tokens.first().unwrap().get_range();
   let end = modifier_tokens.last().unwrap().get_range();
   let member_modifiers = get_member_modifers(&modifier_tokens);

   let (mut external_dll_name,mut is_forward) = (None,false);
   modifier_tokens.iter().for_each(|t|{
      match t.token_type {
            TokenType::Forward => {is_forward=true},
            TokenType::StringLiteral => {external_dll_name=Some(t.get_value())},
            _ => (),
      }
   });

   if modifier_tokens.len()==0{
      return Ok((input, None))
   } else {
      return Ok((next, Some(AstMethodModifiers{
         raw_pos,
         range: create_new_range(start, end),
         modifier_tokens,
         member_modifiers,
         external_dll_name,
         is_forward,
      })))
   }
} 

#[deprecated]
#[allow(unused)]
/// parsers all modifiers, whether it is valid will be done in sematic analysis
fn parse_method_modifiers_<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token],  Option<AstMethodModifiers>), ParseError<'a>>{
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

fn parse_method_body<'a, C: IParserContext<ParserDiagnostic> + 'a>(input : &'a [Token], context : &mut C) -> Result<(&'a [Token], Option<AstMethodBody>), ParseError<'a>>{

   if input.len() == 0 {
      return Ok((input, None))
   }

   let (next, statements) = parse_repeat_w_context(input, parse_statement_v2, context);
   
   let raw_pos = input.first().unwrap().get_raw_pos();
   let start_pos = input.first().unwrap().get_pos();
   let end_pos = input.last().unwrap().get_pos();
   // range is until the endProc
   let range = Range{start: start_pos.clone(), end: end_pos};
   return Ok((
      next, 
      Some(AstMethodBody{
         raw_pos,
         range,
         statements,
      })
   ));
}



#[cfg(test)]
mod test {
   use crate::{lexer::tokens::{Token, TokenType}, parser::{parse_uses, parse_type_enum, parse_type_reference, parse_type_declaration, parse_constant_declaration, parse_global_variable_declaration, parse_procedure_declaration, parse_parameter_declaration_list, parse_method_modifiers, parse_function_declaration, parse_type_basic, parse_type_composed, parse_type_range, ast::{AstTypeProcedure, AstTypeFunction}}, parser::{ast::{AstClass, AstUses, AstTypeBasic, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration, AstProcedure, AstParameterDeclaration, IAstNode, AstFunction, AstBinaryOp, AstTypeSet, AstTypeRecord, AstTypePointer, AstTypeArray, AstTypeRange, AstTypeInstanceOf, AstMethodNameWithEvent, AstEnumVariant, AstModule, AstTypeSized, AstParameterDeclarationList, AstMethodModifiers}, IParserContext, parse_module}, utils::ast_to_string_brief_recursive};
   use crate::utils::{Position,Range, create_new_range_from_irange, test_utils::cast_and_unwrap};
   use super::{parse_class, parse_type, ParserContext};

   pub fn create_context()-> ParserContext{
      return ParserContext::new();
  }

   pub fn gen_list_of_tokens(list : &[(TokenType, Option<String>)]) -> Vec<Token> {
      let mut result = Vec::<Token>::new();
      let mut raw_pos = 0;
      for (tok_type, val) in list.to_vec() {
         let start_pos = Position{line: raw_pos/20, character: raw_pos%20};
         let end_pos = Position{line:start_pos.line, character: start_pos.character+val.as_ref().unwrap_or(&"".to_string()).len()};
         result.push(Token { 
            raw_pos: raw_pos, 
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
      assert_eq!(node.get_pos(), input.first().unwrap().get_pos());
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
      let mut context = create_context();
      let r = parse_class(&input, &mut context).unwrap();
      let class = r.1.as_any().downcast_ref::<AstClass>().unwrap();
      check_node_pos_and_range(class, &input);
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.get_identifier(), "aTestClass");
      assert_eq!(class.raw_pos, 0);
      assert_eq!(class.parent_class.as_ref().unwrap().get_value(), "aParentClass");
   }

   #[test]
   fn test_parse_module(){
      let input = gen_list_of_tokens(&[
         (TokenType::Module, Some("module".to_string())),
         (TokenType::Identifier, Some(String::from("SomeModule"))),
      ]);
      let mut context = create_context();
      let r = parse_module(&input, &mut context).unwrap();
      let class = r.1.as_any().downcast_ref::<AstModule>().unwrap();
      check_node_pos_and_range(class, &input);
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.name, "SomeModule");
      assert_eq!(class.raw_pos, 0);
   }

   #[test]
   fn test_parse_class_no_parent(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, Some("class".to_string())),
         (TokenType::Identifier, Some("aTestClass".to_string())),
      ]);
      let mut context = create_context();
      let (_,class) = parse_class(&input, &mut context).unwrap();
      let class = class.as_any().downcast_ref::<AstClass>().unwrap();
      assert!(class.parent_class.is_none());
   }

   #[test]
   fn test_parse_class_wrong_token(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, Some("class".to_string())),
         (TokenType::Plus, Some(String::from("aTestClass"))),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
         let mut context = create_context();
         assert!(parse_class(&input, &mut context).is_err());
   }

   #[test]
   fn test_parse_uses(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, Some("uses".to_string())),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
      let mut context = create_context();
      let r = parse_uses(&input, &mut context).unwrap();
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
      let mut context = create_context();
      assert!(parse_uses(&input, &mut context).is_err());
   }

   #[test]
   fn test_parse_type_basic() {
      let input = gen_list_of_tokens(&[
         (TokenType::Identifier, Some("int1".to_string())),
         (TokenType::Identifier, Some("int2".to_string())),
         (TokenType::Identifier, Some("Int4".to_string())),
         (TokenType::Identifier, Some("Int8".to_string())),
         (TokenType::Identifier, Some("Boolean".to_string())),
         (TokenType::Identifier, Some("Char".to_string())),
         (TokenType::Identifier, Some("Num4".to_string())),
         (TokenType::Identifier, Some("Num8".to_string())),
         (TokenType::Identifier, Some("Num10".to_string())),
         (TokenType::Identifier, Some("Decimal".to_string())),
         (TokenType::Identifier, Some("CString".to_string())),
         (TokenType::Identifier, Some("String".to_string())),
         (TokenType::Identifier, Some("Text".to_string())),
         (TokenType::Identifier, Some("tCustomType".to_string())),
      ]);

      let mut context = create_context();
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type_basic(next, &mut context) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasic>().unwrap();
         assert_eq!(downcasted.raw_pos,input[count].raw_pos);
         assert_eq!(downcasted.get_pos(),input[count].get_pos());
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
         (TokenType::Equals, Some("=".to_string())),
         (TokenType::NumericLiteral, Some("10".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Variant3".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let next : &[Token] = &input;

      let mut context = create_context();
      let (_, node) = match parse_type_enum(next, &mut context) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeEnum>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.get_children_ref().unwrap().len(), 3);
      let second_node = downcasted
         .get_children_ref().unwrap()
         .get(1).unwrap()
         .as_any().downcast_ref::<AstEnumVariant>().unwrap();
      assert_eq!(second_node.value_token.as_ref().unwrap().get_value(), "10");
   }

   #[test]
   fn test_parse_type_composed() {
      let input = gen_list_of_tokens(&[
         (TokenType::Identifier, Some("tEnumType".to_string())),
         (TokenType::Plus, Some("+".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("Variant1".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Variant2".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type_composed(&input, &mut context).unwrap();
      assert!(next.is_empty());
      let downcasted = node.as_ref().as_any().downcast_ref::<AstBinaryOp>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.get_children_ref().unwrap().len(), 2);
      let children = downcasted.get_children_ref().unwrap();
      assert_eq!(children.get(0).unwrap().get_identifier(), "tEnumType");
      let enum_node = children.get(1).unwrap();
      enum_node.as_any().downcast_ref::<AstTypeEnum>().unwrap();
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
      let mut context = create_context();
      let (_, node) = match parse_type_reference(next, &mut context) {
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
      let mut context = create_context();
      let (_, node) = match parse_type_declaration(next, &mut context) {
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
      let mut context = create_context();
      let (_, node) = match parse_constant_declaration(next, &mut context) {
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
      let mut context = create_context();
      let (_, node) = match parse_global_variable_declaration(next, &mut context) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstGlobalVariableDeclaration>().unwrap();
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "aVariable");
      assert!(downcasted.modifiers.as_ref().unwrap().modifiers.is_override);
      assert!(downcasted.modifiers.as_ref().unwrap().modifiers.is_protected);

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
         (TokenType::External, Some("external".to_string())),
         (TokenType::StringLiteral, Some("SomeDLL.Method".to_string())),
         (TokenType::Forward, Some("forward".to_string())),
         (TokenType::Private, Some("private".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Final, Some("final".to_string())),
         (TokenType::Override, Some("override".to_string())),
      ]);
      let next : &[Token] = &input;
      let mut context = create_context();
      let (_, node) = parse_procedure_declaration(next, &mut context).unwrap();
      let downcasted = cast_and_unwrap::<AstProcedure>(&node);
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.get_identifier(), "FirstMethod");
      assert_eq!(context.get_diagnostics().len(), 0);

      // test params
      let params = downcasted.parameter_list.as_ref().unwrap();
      let params = params.as_any().downcast_ref::<AstParameterDeclarationList>().unwrap();
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
      let modifiers_node = modifiers_node.as_any().downcast_ref::<AstMethodModifiers>().unwrap();
      assert!(modifiers_node.member_modifiers.is_private);
      assert!(modifiers_node.member_modifiers.is_protected);
      assert!(modifiers_node.member_modifiers.is_final);
      assert!(modifiers_node.member_modifiers.is_override);
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
      let mut context = create_context();
      let (_, node) = parse_function_declaration(next, &mut context).unwrap();
      let downcasted = cast_and_unwrap::<AstFunction>(&node);
      check_node_pos_and_range(downcasted, &input);
      assert_eq!(downcasted.identifier.get_identifier(), "FirstMethod");
      assert_eq!(context.get_diagnostics().len(), 0);

      // test params
      let params = downcasted.parameter_list.as_ref().unwrap();
      let params = params.as_any().downcast_ref::<AstParameterDeclarationList>().unwrap();
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
      let modifiers_node = modifiers_node.as_any().downcast_ref::<AstMethodModifiers>().unwrap();
      assert!(!modifiers_node.member_modifiers.is_private);
      assert!(modifiers_node.member_modifiers.is_protected);
      assert!(!modifiers_node.member_modifiers.is_final);
      assert!(modifiers_node.member_modifiers.is_override);
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
      let mut context = create_context();
      let (_, node) = parse_parameter_declaration_list(next, &mut context).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(node.as_ast_node(), &input);

      // test params
      let node = node.as_any().downcast_ref::<AstParameterDeclarationList>().unwrap();
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
      let mut context = create_context();
      let (_, node) = parse_parameter_declaration_list(next, &mut context).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(node.as_ast_node(), &input);

      let node = node.as_any().downcast_ref::<AstParameterDeclarationList>().unwrap();
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
         (TokenType::External, Some("external".to_string())),
         (TokenType::StringLiteral, Some("SomeDLL.Method".to_string())),
         (TokenType::Forward, Some("forward".to_string())),
         (TokenType::Private, Some("private".to_string())),
         (TokenType::Protected, Some("protected".to_string())),
         (TokenType::Final, Some("final".to_string())),
         (TokenType::Override, Some("override".to_string())),
      ]);
      // test pos
      let next : &[Token] = &input;
      let mut context = create_context();
      let (_, node) = parse_method_modifiers(next, &mut context).unwrap();
      let node = node.unwrap();
      check_node_pos_and_range(&node, &input);

      // test modifiers
      assert!(node.member_modifiers.is_private);
      assert!(node.member_modifiers.is_protected);
      assert!(node.member_modifiers.is_override);
      assert!(node.member_modifiers.is_final);
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
      let mut context = create_context();
      let (_, node) = parse_procedure_declaration(next, &mut context).unwrap();
      let _downcasted = cast_and_unwrap::<AstProcedure>(&node);
      // check_node_pos_and_range(downcasted, &input);
      assert_eq!(context.get_diagnostics().len(), 1);
   }

   #[test]
   fn test_parse_type_range() {
      let input = gen_list_of_tokens(&[
         (TokenType::NumericLiteral, Some("'a'".to_string())),
         (TokenType::To, Some("to".to_string())),
         (TokenType::NumericLiteral, Some("22".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type_range(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeRange>().unwrap();

      assert_eq!(downcasted.from.get_identifier(), "'a'");
      assert_eq!(downcasted.to.get_identifier(), "22");
   }

   #[test]
   fn test_parse_type_set() {
      let input = gen_list_of_tokens(&[
         (TokenType::OSqrBracket, Some("'['".to_string())),
         (TokenType::Identifier, Some("CString".to_string())),
         (TokenType::CSqrBracket, Some("]".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeSet>().unwrap();

      assert_eq!(downcasted.set_type.get_identifier(), "CString");
   }

   #[test]
   fn test_parse_type_record() {
      let input = gen_list_of_tokens(&[
         (TokenType::Record, Some("'record'".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("Parent".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
         (TokenType::Identifier, Some("First".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("tType".to_string())),
         (TokenType::Identifier, Some("Second".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("cstring".to_string())),
         (TokenType::EndRecord, Some("endrecord".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeRecord>().unwrap();

      assert_eq!(downcasted.get_children_ref().unwrap().len(), 3);
      assert_eq!(downcasted.parent.as_ref().unwrap().get_identifier(), "Parent");
   }

   #[test]
   fn test_parse_type_pointer() {
      let input = gen_list_of_tokens(&[
         (TokenType::Dot, Some(".".to_string())),
         (TokenType::Identifier, Some("SomeType".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypePointer>().unwrap();

      assert_eq!(downcasted.type_node.get_identifier(), "SomeType");
   }

   #[test]
   fn test_parse_type_array() {
      let input = gen_list_of_tokens(&[
         (TokenType::Array, Some("array".to_string())),
         (TokenType::OSqrBracket, Some("[".to_string())),
         (TokenType::NumericLiteral, Some("1".to_string())),
         (TokenType::To, Some("to".to_string())),
         (TokenType::NumericLiteral, Some("10".to_string())),
         (TokenType::CSqrBracket, Some("]".to_string())),
         (TokenType::Of, Some("of".to_string())),
         (TokenType::Identifier, Some("Type".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeArray>().unwrap();

      assert_eq!(downcasted.array_seq_token.get_value(), "array");
      downcasted.index_nodes.first().unwrap().as_any().downcast_ref::<AstTypeRange>().unwrap();
      assert_eq!(downcasted.object_type.get_identifier(), "Type");
   }

   #[test]
   fn test_parse_type_proc() {
      let input = gen_list_of_tokens(&[
         (TokenType::Proc, Some("procedure".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("Param1".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("Type".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Param1".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("Int4".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeProcedure>().unwrap();

      assert!(downcasted.parameter_list.is_some());
      assert_eq!(downcasted.parameter_list.as_ref().unwrap().get_children_ref().unwrap().len(), 2);
   }

   #[test]
   fn test_parse_type_func() {
      let input = gen_list_of_tokens(&[
         (TokenType::Func, Some("func".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::Identifier, Some("Param1".to_string())),
         (TokenType::Comma, Some(",".to_string())),
         (TokenType::Identifier, Some("Param1".to_string())),
         (TokenType::Colon, Some(":".to_string())),
         (TokenType::Identifier, Some("Int4".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
         (TokenType::Return, Some("return".to_string())),
         (TokenType::Identifier, Some("Type".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeFunction>().unwrap();

      assert!(downcasted.parameter_list.is_some());
      assert_eq!(downcasted.parameter_list.as_ref().unwrap().get_children_ref().unwrap().len(), 2);
      print!("{}",ast_to_string_brief_recursive(downcasted));
   }

   #[test]
   fn test_parse_type_instanceof() {
      let input = gen_list_of_tokens(&[
         (TokenType::InstanceOf, Some("instanceof".to_string())),
         (TokenType::Identifier, Some("SomeType".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeInstanceOf>().unwrap();

      assert_eq!(downcasted.instance_type.get_identifier(), "SomeType");
   }

   #[test]
   fn test_parse_method_with_event(){
      let input = gen_list_of_tokens(&[
         (TokenType::Proc, Some("procedure".to_string())),
         (TokenType::Identifier, Some("Method".to_string())),
         (TokenType::Pound, Some("#".to_string())),
         (TokenType::Identifier, Some("SomeEvent".to_string())),
         (TokenType::EndProc, Some("endproc".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_procedure_declaration(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstProcedure>().unwrap();
      let method_name = downcasted.identifier.as_any().downcast_ref::<AstMethodNameWithEvent>().unwrap();
      assert_eq!(method_name.get_identifier(), "Method#SomeEvent");
   }

   #[test]
   fn test_parse_type_sized(){
      let input = gen_list_of_tokens(&[
         (TokenType::Identifier, Some("Int".to_string())),
         (TokenType::OBracket, Some("(".to_string())),
         (TokenType::NumericLiteral, Some("10".to_string())),
         (TokenType::CBracket, Some(")".to_string())),
      ]);
      let mut context = create_context();
      let (next, node) = parse_type(&input, &mut context).unwrap();
      assert!(next.is_empty());
      check_node_pos_and_range(node.as_ref(), &input);

      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeSized>().unwrap();
      assert_eq!(downcasted.get_identifier(), "Int");
      assert_eq!(downcasted.size_token.get_value(), "10");
   }

}