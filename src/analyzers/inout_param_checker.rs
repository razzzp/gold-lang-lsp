use lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::{
    lexer::tokens::TokenType,
    parser::ast::{AstParameterDeclaration, IAstNode},
    utils::{DynamicChild, IRange}, implem_as_ivisitor,
};

use super::{IAnalyzer, IVisitor};

pub struct InoutParamChecker {
    diagnostics: Vec<Diagnostic>,
}
impl InoutParamChecker {
    pub fn new() -> InoutParamChecker {
        return InoutParamChecker {
            diagnostics: Vec::new(),
        };
    }
    fn create_diagnostic(
        &self,
        node: &AstParameterDeclaration,
        param_type: &str,
    ) -> lsp_types::Diagnostic {
        return lsp_types::Diagnostic::new(
            node.get_range().as_lsp_type_range(),
            Some(DiagnosticSeverity::WARNING),
            None,
            Some("gold".to_string()),
            format!("{} params should be inout/var", param_type),
            None,
            None,
        );
    }
    fn param_type_needs_inout(&self, param_type: &str) -> bool {
        match param_type.to_uppercase().as_str() {
            "TVARBYTEARRAY" | "ALISTOFINSTANCES" | "TEXT" => true,
            _ => false,
        }
    }

    fn notify_param_decl_node(&mut self, node: &DynamicChild<dyn IAstNode>) {
        let param_decl_node = node
            .data
            .as_any()
            .downcast_ref::<AstParameterDeclaration>()
            .unwrap();
        match param_decl_node.type_node.as_ref() {
            Some(n) => {
                let ident = n.get_identifier();
                if self.param_type_needs_inout(ident) {
                    match &param_decl_node.modifier {
                        Some(t) => {
                            let modifier = t.token_type;
                            if !(modifier == TokenType::InOut || modifier == TokenType::Var) {
                                self.diagnostics
                                    .push(self.create_diagnostic(param_decl_node, ident))
                            }
                        }
                        _ => self
                            .diagnostics
                            .push(self.create_diagnostic(param_decl_node, ident)),
                    }
                }
            }
            _ => (),
        }
    }
}

impl IVisitor for InoutParamChecker {
   implem_as_ivisitor!();
    fn visit(&mut self, node: &crate::utils::DynamicChild<dyn crate::parser::ast::IAstNode>) {
        match node.data.as_any().downcast_ref::<AstParameterDeclaration>() {
            Some(_) => self.notify_param_decl_node(node),
            _ => (),
        };
    }
    fn notify_end(&mut self) {
        ()
    }
}

impl IAnalyzer for InoutParamChecker {
    fn append_diagnostics(&self, result: &mut Vec<lsp_types::Diagnostic>) {
        self.diagnostics.iter().for_each(|d| result.push(d.clone()))
    }
    fn get_diagnostics_count(&self) ->  usize {
        self.diagnostics.len()
    }
}
