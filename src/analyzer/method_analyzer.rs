use lsp_types::DiagnosticSeverity;

use crate::parser::ast::{AstProcedure, IAstNode};

use super::{IAnalyzer, AnalyzerDiagnostic};



pub struct MethodAnalyzer{
    diagnostics: Vec<lsp_types::Diagnostic>
}
impl MethodAnalyzer {
    pub fn new() -> MethodAnalyzer {
        return MethodAnalyzer { diagnostics: Vec::new() }
    }
}
impl IAnalyzer for MethodAnalyzer{
    fn visit(&mut self, node: &crate::utils::DynamicChild<dyn crate::parser::ast::IAstNode>) {
        match node.data.as_any().downcast_ref::<AstProcedure>() {
            Some(proc_node) => {
                self.diagnostics.push(lsp_types::Diagnostic{
                    range: proc_node.identifier.range.as_lsp_type_range(),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    message: format!("found proc {}", proc_node.get_identifier()),
                    ..Default::default()
                })
            }   
            _=> ()
        }
    }

    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>) {
        self.diagnostics.iter().for_each(|diag|{
            result.push(diag.clone());
        })
    }
}