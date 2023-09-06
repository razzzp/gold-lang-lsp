use lsp_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag};

use crate::{parser::{ParserDiagnostic, ast::{IAstNode, AstParameterDeclaration}}, utils::{DynamicChild, IRange}, lexer::tokens::TokenType};

use super::IAnalyzer;


pub struct VarByteArrayParamChecker{
   diagnostics: Vec<Diagnostic>
}
impl VarByteArrayParamChecker{
   pub fn new()-> VarByteArrayParamChecker{
      return VarByteArrayParamChecker{
         diagnostics:Vec::new()
      }
   }
   fn create_diagnostic(&self, node: &AstParameterDeclaration) -> lsp_types::Diagnostic{
      return lsp_types::Diagnostic::new(
         node.identifier.get_range().as_lsp_type_range(), 
         Some(DiagnosticSeverity::WARNING), 
         None,
         Some("gold".to_string()),
         "tVaryByteArray params should be inout/var".to_string(),
         None,
         None
     )
   }

   fn notify_param_decl_node(&mut self, node : &DynamicChild<dyn IAstNode>){
      let param_decl_node = node.data.as_any().downcast_ref::<AstParameterDeclaration>().unwrap();
      match param_decl_node.type_node.as_ref() {
         Some(n) => {
            if n.get_identifier().to_uppercase() == "TVARBYTEARRAY"{
               match &param_decl_node.modifier {
                  Some(t) =>{
                     let modifier = t.token_type;
                     if !(modifier == TokenType::InOut || modifier == TokenType::Var){
                        self.diagnostics.push(self.create_diagnostic(param_decl_node))
                     }
                  }
                  _=> {
                     self.diagnostics.push(self.create_diagnostic(param_decl_node))
                  }
               }
            }
         }
         _=> ()
      }
   }
}


impl IAnalyzer for VarByteArrayParamChecker{
    fn visit(&mut self, node: &crate::utils::DynamicChild<dyn crate::parser::ast::IAstNode>) {
      match node.data.as_any().downcast_ref::<AstParameterDeclaration>(){
         Some(_) => self.notify_param_decl_node(node),
         _=>()
      };
    }

    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>) {
        self.diagnostics.iter()
        .for_each(|d|{
            result.push(d.clone())
        })
    }

    fn notify_end(&mut self) {
        ()
    }
}