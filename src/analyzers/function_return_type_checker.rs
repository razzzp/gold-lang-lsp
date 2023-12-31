
use crate::{parser::ast::{IAstNode, AstFunction, AstTypeBasic}, utils::DynamicChild, lexer::tokens::TokenType, implem_as_ivisitor};

use super::{IAnalyzer, IVisitor};


pub struct FunctionReturnTypeChecker{
   diagnostics: Vec<lsp_types::Diagnostic>
}
impl FunctionReturnTypeChecker{
   pub fn new()-> FunctionReturnTypeChecker{
      return FunctionReturnTypeChecker{
         diagnostics:Vec::new()
      }
   }
   fn create_diagnostic(&self, 
      node: &dyn IAstNode, 
      severity : lsp_types::DiagnosticSeverity,
      msg : &str) -> lsp_types::Diagnostic{

      return lsp_types::Diagnostic::new(
         node.get_range().as_lsp_type_range(), 
         Some(severity), 
         None,
         Some("gold".to_string()),
         msg.to_string(),
         None,
         None
     )
   }

   fn notify_param_decl_node(&mut self, node : &DynamicChild<dyn IAstNode>){
      let func_node = node.data.as_any().downcast_ref::<AstFunction>().unwrap();
      let return_type_node = match func_node.return_type.as_any().downcast_ref::<AstTypeBasic>() {
         Some(n) => n,
         _=> return
      };
      let token = &return_type_node.id_token;
      if token.token_type == TokenType::Identifier {
         let tok_val = token.get_value_as_str().to_uppercase();
         if tok_val == "TVARBYTEARRAY" {
            self.diagnostics.push(
               self.create_diagnostic(
                  return_type_node, 
                  lsp_types::DiagnosticSeverity::WARNING,
                  "tVarByteArray type should not be returned by functions, pass it as inout/var param instead"
            ))  
         } else if tok_val == "ALISTOFINSTANCES" {
            self.diagnostics.push(
               self.create_diagnostic(
                  return_type_node, 
                  lsp_types::DiagnosticSeverity::WARNING,
                  "aListOfInstances type should not be returned by functions, pass it as inout/var param instead"
            ))  
         }else if tok_val == "TEXT" {
            self.diagnostics.push(
               self.create_diagnostic(
                  return_type_node, 
                  lsp_types::DiagnosticSeverity::WARNING,
                  "Text type should not be returned by functions, pass it as inout/var param instead"
            ))  
         } 
      } 
   }
}

impl IVisitor for FunctionReturnTypeChecker{
   implem_as_ivisitor!();
   fn visit(&mut self, node: &crate::utils::DynamicChild<dyn crate::parser::ast::IAstNode>) {
      match node.data.as_any().downcast_ref::<AstFunction>(){
         Some(_) => self.notify_param_decl_node(node),
         _=>()
      };
    }
    fn notify_end(&mut self) {
      ()
  }
}

impl IAnalyzer for FunctionReturnTypeChecker{
    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>) {
        self.diagnostics.iter()
        .for_each(|d|{
            result.push(d.clone())
        })
    }

    fn get_diagnostics_count(&self) ->  usize {
        self.diagnostics.len()
    }
}