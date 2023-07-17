
use crate::lexer::tokens::{Token, TokenType, Range, Position};
use crate::ast::{AstClass, AstUses, AstTerminal, IAstNode, AstEmpty, AstTypeBasicFixedSize, AstTypeBasicDynamicSize, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration};

use self::utils::prepend_msg_to_error;

pub mod utils;

#[derive(Debug, Clone)]
pub struct ParserError<'a>{
   pub input: &'a [Token],
   pub msg: String
}

pub fn parse_gold<'a>(input : &'a [Token]) -> ((&'a [Token],  Vec<Box<dyn IAstNode>>), Vec<ParserError>) {
   let parsers = [
      parse_class,
      parse_uses,
      parse_type_declaration,
      parse_constant_declaration,
      parse_global_variable_declaration,
   ];
   let mut result = Vec::<Box<dyn IAstNode>>::new();
   let mut errors = Vec::<ParserError>::new();
   let mut next = input;
   while next.len() > 0 {
      next = match alt_parse(&parsers)(next){
         Ok((r,n))=> {result.push(n); r},
         Err(e)=> {
            let mut iter = e.input.iter();
            // move one 
            iter.next();
            errors.push(e);
            // set next as the input of the most matched error
            iter.as_slice()
         }
      };
   }
   ((next, result), errors)
}

fn parse_class<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError> {
   // class keyword
   let (next, class_token) = match exp_token(TokenType::Class)(input) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // class name
   let (next, token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r) => (r.0, r.1),
      Err(e) => return Err(e)
   };
   let class_name_token = token;
   // '('
   let (next, _) =  match exp_token(TokenType::OBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // parent class
   let (next, token) =  match exp_token(TokenType::Identifier)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   let parent_class_name = token;
   // ')'
   let (next, end_token) =  match exp_token(TokenType::CBracket)(next) {
      Ok(r)=> (r.0, r.1),
      Err(e) => return Err(e)
   };
   // TODO change behaviour when class name empty
   return Ok((next, Box::new(AstClass{
      raw_pos: class_name_token.raw_pos,
      pos: class_token.pos.clone(),
      range: Range { start: class_token.pos, end: end_token.pos},
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

fn parse_constant_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError> {
   // const keyword
   let (next, const_token) = match exp_token(TokenType::Const)(input){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl:", e))
   };
   // identifier
   let (next, ident_token) = match exp_token(TokenType::Identifier)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl:", e))
   };
   // equals
   let (next, _) = match exp_token(TokenType::Equals)(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl:", e))
   };
   // string or numeric value
   let (next, value_token) = match alt_token(
      &[
         exp_token(TokenType::StringConstant),
         exp_token(TokenType::NumericConstant)]
   )(next){
      Ok(r) => r,
      Err(e) => return Err(prepend_msg_to_error("Cannot parse constant decl:", e))
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
         range: Range { start: const_token.pos, end: value_token.pos },
         is_multi_lang : if multilang_token.is_some() {true} else {false} 
      })
   ))
}

fn parse_uses<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError> {
   let (next, uses_token) = match exp_token(TokenType::Uses)(input){
      Ok((r, t)) => (r, t),
      Err(e) => return Err(e),
   };
   let (next, idents) = match parse_separated_list(next, TokenType::Identifier, TokenType::Comma) {
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

fn parse_type_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   // type keyword, identifier, then colon
   let (next, tokens) = match seq_token(&[
      exp_token(TokenType::Type),
      exp_token(TokenType::Identifier),
      exp_token(TokenType::Colon)
   ])(input){
      Ok(r) => r,
      Err(e) => return Err(e)
   };
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

fn parse_type<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   let parsers = [
      parse_type_basic_fixed_size,
      parse_type_basic_dynamic_size,
      parse_type_enum,
      parse_type_reference,
   ];
   let parse_result = alt_parse(&parsers)(input);
   return parse_result;
}

fn parse_type_basic_fixed_size<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>> {
   let parse_result = alt_token(&[
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
      exp_token(TokenType::Identifier),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypeBasicFixedSize{
         raw_pos:t.raw_pos,
         pos: t.pos.clone(),
         range: t.range.clone(),
         type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_basic_dynamic_size<'a>(input : &'a [Token]) 
-> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>> {
   let parse_result = alt_token(&[
      exp_token(TokenType::Text),
   ])(input);
   return match parse_result {
      Ok((r, t)) => Ok((r, Box::new(AstTypeBasicDynamicSize{
         raw_pos:t.raw_pos, 
         pos: t.pos.clone(),
         range: t.range.clone(),
         type_token: t}))),
      Err(e) => Err(e)
   }
}

fn parse_type_enum<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   let (next, s_raw_pos, s_pos) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, t.raw_pos, t.pos),
      Err(e) => return Err(e)
   };
   let (next, tokens) = match parse_separated_list(next, TokenType::Identifier, TokenType::Comma){
       Ok((r, ts)) => (r, ts),
       Err(e) => return Err(e)
   };
   let (next, e_pos) = match exp_token(TokenType::CBracket)(next){
      Ok((r,t)) => (r, t.pos),
      Err(e) => return Err(e)
   };
   return Ok((next, Box::new(AstTypeEnum{
      raw_pos: s_raw_pos,
      pos: s_pos.clone(),
      range: Range{start: s_pos, end: e_pos},
      variants: tokens
   })));
}

fn parse_type_reference<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   let result = alt_token(&[
      exp_token(TokenType::RefTo),
      exp_token(TokenType::ListOf)
   ])(input);
   let (next, ref_token) = match result {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   let (next, mut option_tokens) = match parse_type_reference_options(next){
      Ok((r,t)) => (r,t),
      Err(e) => (e.input, Vec::<Token>::new())
   };
   let (next, ident_token) = match exp_token(TokenType::Identifier)(next) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   // calc end pos
   let mut end_pos = ident_token.pos;
   end_pos.character += ident_token.value.unwrap().len();
   // if there are options remove open and close sqr brackets
   if option_tokens.len() > 0 {
      option_tokens.pop();
      option_tokens.remove(0);
   }

   return Ok((next, Box::new(AstTypeReference{
      raw_pos: ref_token.raw_pos,
      pos: ref_token.pos.clone(),
      ref_type: ref_token.clone(),
      range: Range{start:ref_token.pos,end:end_pos},
      options: option_tokens
   })));
}

fn parse_type_reference_options<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<Token>), ParserError<'a>>{
   let mut result = Vec::<Token>::new();
   let (next, open_token) = match exp_token(TokenType::OSqrBracket)(input) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   let (next, mut option_tokens) = match parse_separated_list(next, TokenType::Identifier, TokenType::Comma){
      Ok((r, ts)) => (r,ts),
      Err(e) => return Err(e)
   };
   let (next, closing_token) = match exp_token(TokenType::CSqrBracket)(next) {
      Ok((r,t)) => (r,t),
      Err(e) => return Err(e)
   };
   result.push(open_token);
   result.append(&mut option_tokens);
   result.push(closing_token);
   return Ok((next, result));
}

fn parse_global_variable_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   // memory?
   let (next, memory_token) = match exp_token(TokenType::Memory)(input){
      Ok((n, t)) => (n,Some(t)),
      Err(e) => (e.input, None)
   };
   
   let (next, identifier_token) = match exp_token(TokenType::Identifier)(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   // colon token
   let (next, _) = match exp_token(TokenType::Colon)(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   let (next, type_node) = match parse_type(next){
      Ok((n, t)) => (n,t),
      Err(e) => return Err(e)
   };
   let raw_pos = if memory_token.is_some() {memory_token.as_ref().unwrap().raw_pos.clone()} else {identifier_token.raw_pos.clone()};
   let start = if memory_token.is_some() {memory_token.as_ref().unwrap().pos.clone()} else {identifier_token.pos.clone()};
   return Ok((
      next,
      Box::new(AstGlobalVariableDeclaration {
         is_memory: if memory_token.is_some() {true} else {false},
         raw_pos: raw_pos,
         pos: start.clone(),
         range: Range {start:start, end: type_node.get_range().end.clone()},
         identifier: identifier_token,
         type_node: type_node,
      })
   ));
}


fn parse_procedure_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Box<dyn IAstNode>), ParserError<'a>>{
   todo!();
   // parse proc [ident]
   let (next, first_tokens) = match seq_token(&[
      exp_token(TokenType::Proc),
      exp_token(TokenType::Identifier),
   ]) (input){
      Ok(r) => r,
      Err(e) => return Err(e)
   };
   // parse params
   let (next, param_nodes) = match parse_parameter_declaration(next){
      Ok(r) => r,
      Err(e) => (next, Vec::<Box<dyn IAstNode>>::new())
   };
}

fn parse_parameter_declaration<'a>(input : &'a [Token]) -> Result<(&'a [Token],  Vec<Box<dyn IAstNode>>), ParserError<'a>>{
   todo!();
   let (next, obracket_token) = match exp_token(TokenType::OBracket)(input){
      Ok((r,t)) => (r, Some(t)),
      Err(e) => (input, None)
   };
}

fn parse_separated_list<'a>(input : &'a [Token], item : TokenType, seperator: TokenType) 
-> Result<(&'a [Token],  Vec<Token>), ParserError<'a>> {
   let mut identifiers = Vec::<Token>::new();
   // match first identifier
   let r = _parse_seperated_list_recursive(input, &item, &seperator, &mut identifiers);
   match r{
      Ok(r) => return Ok((r, identifiers)),
      Err(e) => return Err(e)
   };
}

fn _parse_seperated_list_recursive<'a, 'b>(
   input : &'a [Token],
   item: &'b TokenType,
   sep: &'b TokenType,
   result: &'b mut Vec<Token>) 
-> Result<&'a [Token], ParserError<'a>> {
   // match first identifier
   let mut next = match exp_token(item.clone())(input){
      Ok((r, t)) => {result.push(t); r},
      Err(e) => return Err(ParserError { input: e.input, msg:  String::from("Failed to parse uses list")})
   };
   next = match exp_token(sep.clone())(next){
      Ok((r, _)) => r,
      Err(e) => return Ok(e.input)
   };
   return _parse_seperated_list_recursive(next, item, sep, result)
}

fn exp_token(token_type : TokenType)
   -> impl Fn(&[Token]) -> Result<(&[Token],  Token), ParserError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Token), ParserError> {
      let mut it = input.iter();
      match it.next() {
      Some(t) if t.token_type == token_type => Ok((it.as_slice(), t.clone())),
      Some(t) => Err(ParserError {input: input, msg: String::from(format!("Expected {:?}, found {:?}",token_type,t.token_type))}),
      None => Err(ParserError {input: input, msg: String::from(format!("Expected {:?}, found None",token_type))})
      }
   }
}

/*
   wraps the parser so that it doesn't throw error
 */
fn opt_parse(parser : impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), ParserError>) 
   -> impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), ParserError> 
{
   move |input: &[Token]| -> Result<(&[Token],  Box<dyn IAstNode>), ParserError> {
      match parser(input) {
          Ok(r) => return Ok(r),
          _ => ()
      };
      // TODO better way to return Ok? AstEmpty will just be thrown away
      return Ok((input, Box::new(AstEmpty{})));
   }
}

fn alt_token(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Token), ParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Token), ParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Token), ParserError> {
      let next = input;
      for parser in list_of_parsers {
         let r = parser(input);
         match r {
            Ok(r) => return Ok(r),
            Err(_) => continue
         }
      }
      return Err(ParserError{ input: next, msg: String::from("Failed to parse using alternatives") });
   }
}

fn alt_parse(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), ParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), ParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Box<dyn IAstNode>), ParserError> {
      let mut most_matched: Option<ParserError> = None;
      for parser in list_of_parsers {
         let r = parser(input);
         match r {
            Ok(r) => return Ok(r),
            Err(e) => {
               // update most matched
               if most_matched.is_some() && most_matched.as_ref().unwrap().input.len() > e.input.len() {
                  most_matched = Some(e);
               } else {
                   most_matched = Some(e);
               }
            }
         }
      }
      return Err(most_matched.unwrap());
   }
}

fn seq_token(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Token), ParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Vec<Token>), ParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Vec<Token>), ParserError> {
      let mut i = 0;
      let mut next = input;
      let mut nodes = Vec::<Token>::new();
      while i < list_of_parsers.len(){
         next = match list_of_parsers[i](next){
            Ok(r) => {nodes.push(r.1); r.0},
            Err(e) => return Err(ParserError{ input: e.input, msg: String::from("Failed to parse using alternatives") })
         };
         i+=1;
      }
      return Ok((next, nodes));
   }
}

fn seq_parse(list_of_parsers : &[impl Fn(&[Token]) -> Result<(&[Token],  Box<dyn IAstNode>), ParserError>])
   -> impl Fn(&[Token]) -> Result<(&[Token],  Vec<Box<dyn IAstNode>>), ParserError> + '_
{
   move |input: &[Token]| -> Result<(&[Token],  Vec<Box<dyn IAstNode>>), ParserError> {
      let mut i = 0;
      let mut next = input;
      let mut nodes = Vec::<Box<dyn IAstNode>>::new();
      while i < list_of_parsers.len(){
         next = match list_of_parsers[i](next){
            Ok(r) => {nodes.push(r.1); r.0},
            Err(e) => return Err(ParserError{ input: e.input, msg: String::from("Failed to parse using alternatives") })
         };
         i+=1;
      }
      return Ok((next, nodes));
   }
}

#[cfg(test)]
mod test {
    use crate::{lexer::tokens::{Token, TokenType, Position, Range}, parser::{ParserError, parse_uses, parse_type_enum, parse_type_reference, parse_type_declaration, parse_constant_declaration, parse_global_variable_declaration}, ast::{AstTerminal, AstClass, AstUses, AstTypeBasicFixedSize, AstTypeBasicDynamicSize, AstTypeEnum, AstTypeReference, AstTypeDeclaration, AstConstantDeclaration, AstGlobalVariableDeclaration}};

    use super::{parse_class, parse_type};

   fn gen_list_of_tokens(list : &[(TokenType, Option<String>)]) -> Vec<Token> {
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
         raw_pos+=5;
      }  
      return result;
   }

   #[test]
   fn test_parse_class(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::CBracket, None),
      ]);
      let r = parse_class(&input).unwrap();
      let class = r.1.as_any().downcast_ref::<AstClass>().unwrap();
      assert_eq!(r.0.len(), 0);
      assert_eq!(class.name, "aTestClass");
      assert_eq!(class.raw_pos, 5);
      assert_eq!(class.parent_class, "aParentClass");
   }

   #[test]
   fn test_parse_class_too_short(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
      ]);
      assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_class_wrong_token(){
      let input = gen_list_of_tokens(&[
         (TokenType::Class, None),
         (TokenType::Plus, Some(String::from("aTestClass"))),
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
         assert!(parse_class(&input).is_err());
   }

   #[test]
   fn test_parse_uses(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         ]);
      let r = parse_uses(&input).unwrap();
      // ensure returned input is empty
      assert_eq!(r.0.len(), 0);
      let uses_node = r.1.as_any().downcast_ref::<AstUses>().unwrap();

      // first uses
      let token = &uses_node.list_of_uses[0];
      assert_eq!(token.raw_pos, 5);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aTestClass");

      // second uses
      let token = &uses_node.list_of_uses[1];
      assert_eq!(token.raw_pos, 15);
      assert_eq!(token.token_type, TokenType::Identifier);
      assert_eq!(token.value.as_ref().unwrap().as_str(), "aParentClass");
   }

   #[test]
   fn test_parse_uses_trailing_comma(){
      let input = gen_list_of_tokens(&[
         (TokenType::Uses, None),
         (TokenType::Identifier, Some(String::from("aTestClass"))),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some(String::from("aParentClass"))),
         (TokenType::Comma, None),
         ]);
      assert!(parse_uses(&input).is_err());
   }

   #[test]
   fn test_parse_type_basic_fixed_size() {
      let input = gen_list_of_tokens(&[
         (TokenType::Int1, None),
         (TokenType::Int2, None),
         (TokenType::Int4, None),
         (TokenType::Int8, None),
         (TokenType::Boolean, None),
         (TokenType::Char, None),
         (TokenType::Num4, None),
         (TokenType::Num8, None),
         (TokenType::Num10, None),
         (TokenType::Decimal, None),
         (TokenType::CString, None),
         (TokenType::String, None),
         (TokenType::Identifier, Some("tCustomType".to_string())),
      ]);
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasicFixedSize>().unwrap();
         assert_eq!(
            downcasted.raw_pos,
            input[count].raw_pos
         );
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
   fn test_parse_type_basic_dynamic_size() {
      let input = gen_list_of_tokens(&[
         (TokenType::Text, None),
      ]);
      let mut next : &[Token] = &input;
      let mut count = 0;
      while !next.is_empty(){
         let (remaining, node) = match parse_type(next) {
            Ok((r, n)) => (r,n),
            Err(e) => panic!("{}",e.msg.to_owned())
         };
         let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeBasicDynamicSize>().unwrap();
         assert_eq!(
            downcasted.raw_pos,
            input[count].raw_pos
         );
         assert_eq!(
            downcasted.type_token.token_type,
            input[count].token_type
         );
         count += 1;
         next = remaining;
      }
   }

   #[test]
   fn test_parse_type_enum() {
      let input = gen_list_of_tokens(&[
         (TokenType::OBracket, None),
         (TokenType::Identifier, Some("Variant1".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("Variant2".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("Variant3".to_string())),
         (TokenType::CBracket, None),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_enum(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeEnum>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 1);
      assert_eq!(downcasted.range.end.character, 10);
   }

   #[test]
   fn test_parse_type_reference_refto() {
      let input = gen_list_of_tokens(&[
         (TokenType::RefTo, None),
         (TokenType::OSqrBracket, None),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, None),
         (TokenType::Identifier, Some("aType".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_reference(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeReference>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 2);
      assert_eq!(downcasted.range.end.character, 5);
      assert_eq!(downcasted.ref_type.token_type, TokenType::RefTo);
      assert_eq!(downcasted.options.len(), 3);
      assert_eq!(downcasted.options[0].value.as_ref().unwrap().as_str(), "A");
      assert_eq!(downcasted.options[1].value.as_ref().unwrap().as_str(), "P");
      assert_eq!(downcasted.options[2].value.as_ref().unwrap().as_str(), "T");
   }

   #[test]
   fn test_parse_type_declaration_refto() {
      let input = gen_list_of_tokens(&[
         (TokenType::Type, None),
         (TokenType::Identifier, Some("tTestType".to_owned())),
         (TokenType::Colon, None),
         (TokenType::RefTo, None),
         (TokenType::OSqrBracket, None),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, None),
         (TokenType::Identifier, Some("aType".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_type_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstTypeDeclaration>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 2);
      assert_eq!(downcasted.range.end.character, 20);
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
         (TokenType::Const, None),
         (TokenType::Identifier, Some("cAConstant".to_string())),
         (TokenType::Equals, None),
         (TokenType::StringConstant, Some("a constant string".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_constant_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstConstantDeclaration>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 0);
      assert_eq!(downcasted.range.end.character, 15);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "cAConstant");
      assert_eq!(downcasted.value.value.as_ref().unwrap().as_str(), "a constant string");
   }

   #[test]
   fn test_parse_global_variable_declaration() {
      let input = gen_list_of_tokens(&[
         (TokenType::Memory, None),
         (TokenType::Identifier, Some("aVariable".to_owned())),
         (TokenType::Colon, None),
         (TokenType::RefTo, None),
         (TokenType::OSqrBracket, None),
         (TokenType::Identifier, Some("A".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("P".to_string())),
         (TokenType::Comma, None),
         (TokenType::Identifier, Some("T".to_string())),
         (TokenType::CSqrBracket, None),
         (TokenType::Identifier, Some("aType".to_string())),
      ]);
      let next : &[Token] = &input;

      let (_, node) = match parse_global_variable_declaration(next) {
         Ok((r, n)) => (r,n),
         Err(e) => panic!("{}",e.msg.to_owned())
      };
      let downcasted = node.as_ref().as_any().downcast_ref::<AstGlobalVariableDeclaration>().unwrap();
      assert_eq!(downcasted.raw_pos, 0);
      assert_eq!(downcasted.pos.line, 0);
      assert_eq!(downcasted.pos.character, 0);
      assert_eq!(downcasted.range.start.line, 0);
      assert_eq!(downcasted.range.start.character, 0);
      assert_eq!(downcasted.range.end.line, 2);
      assert_eq!(downcasted.range.end.character, 20);
      assert_eq!(downcasted.identifier.value.as_ref().unwrap().as_str(), "aVariable");

      // test refto type
      let downcasted = downcasted.type_node.as_any().downcast_ref::<AstTypeReference>().unwrap();
      assert_eq!(downcasted.ref_type.token_type, TokenType::RefTo);
      assert_eq!(downcasted.options.len(), 3);
      assert_eq!(downcasted.options[0].value.as_ref().unwrap().as_str(), "A");
      assert_eq!(downcasted.options[1].value.as_ref().unwrap().as_str(), "P");
      assert_eq!(downcasted.options[2].value.as_ref().unwrap().as_str(), "T");
   }
}