use std::collections::HashSet;

use lsp_types::DiagnosticSeverity;

use super::{annotated_ast_walker::IAnnotatedNodeVisitor, IDiagnosticCollectorArx};

use crate::{analyzers_v2::annotated_ast_walker::IAnnotatedAstWalkerContext, parser::ast::{AstProcedure, AstFunction, AstUnaryOp, AstBinaryOp}, lexer::tokens::TokenType, manager::utils::DIAGNOSTIC_SOURCE_GOLD};
use super::AnnotatedAstNodeArx;

/// Checks that inherited is called for certain functions
#[derive(Debug)]
pub struct InheritedChecker{
    is_inherited_called: bool,
    current_method: Option<AnnotatedAstNodeArx>,
    methods_to_check : HashSet<&'static str>,
    diag_collector : IDiagnosticCollectorArx
}

impl InheritedChecker{
    pub fn new(diag_collector : IDiagnosticCollectorArx) -> InheritedChecker{
        let mut methods_to_check = HashSet::new();
        methods_to_check.insert("INIT");
        methods_to_check.insert("TERMINATE");
        methods_to_check.insert("NOTIFYINTI");
        methods_to_check.insert("NOTIFYTERMINATE");
        InheritedChecker { 
            is_inherited_called:false,
            current_method: None,
            methods_to_check,
            diag_collector
        }
    }

    fn check_inherited_called(&mut self){
        if let Some(cur_method) = self.current_method.as_ref() {
            // if need to check method and inherited NOT called, add warning
            if self.methods_to_check.contains(cur_method.read().unwrap().get_identifier()) 
            && !self.is_inherited_called {
                self.diag_collector.lock().unwrap().add_diagnostic(lsp_types::Diagnostic { 
                    range: cur_method.read().unwrap().data.get_range().as_lsp_type_range(), 
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                    message: "This method should call its inherited implem.".to_string(), 
                    ..Default::default()
                })
            }
        }
    }

    fn handle_method_node(&mut self, node: &AnnotatedAstNodeArx, _context: &dyn IAnnotatedAstWalkerContext){
        // check inherited called for previous method
        self.check_inherited_called();
        // reset vars
        self.is_inherited_called = false;
        self.current_method = Some(node.clone());
    }

    fn handle_inherited_node(&mut self, node: &AstUnaryOp, context: &dyn IAnnotatedAstWalkerContext){
        if let Some(cur_method) = context.get_current_method(){
            if let Some(bin_op) = node.expr_node.as_any().downcast_ref::<AstBinaryOp>() {
                // just check right node for now
                if bin_op.right_node.get_identifier() == cur_method.read().unwrap().get_identifier(){
                    self.is_inherited_called = true;
                }
            }
        }
    }
}
impl IAnnotatedNodeVisitor for InheritedChecker{
    fn visit(&mut self, _node : &AnnotatedAstNodeArx) {
        todo!()
    }

    fn visit_w_context(&mut self, node : &AnnotatedAstNodeArx, context: &dyn IAnnotatedAstWalkerContext) {
        match node.read().unwrap().data.as_any().downcast_ref::<AstProcedure>(){
            Some(_) => self.handle_method_node(node, context),
            _ => ()
        };
        match node.read().unwrap().data.as_any().downcast_ref::<AstFunction>(){
            Some(_) => self.handle_method_node(node, context),
            _ => ()
        };
        match node.read().unwrap().data.as_any().downcast_ref::<AstUnaryOp>(){
            Some(unary_op) => {
                // check if inherited op
                if unary_op.op_token.token_type == TokenType::Inherited{
                    self.handle_inherited_node(unary_op, context)
                }
            }
            _ => ()
        };
    }

    fn notify_end(&mut self) {
        self.check_inherited_called()
    }
}

#[cfg(test)]
mod test{
    

    use crate::{utils::test_utils::*, lexer::tokens::{TokenType}};

    #[test]
    fn test_inherited_not_called(){
        let _proc = create_test_proc_node(
            "Test",
            Some(vec![
                create_test_unary_op_node(
                    create_test_bin_op_ndoe(
                        create_test_id_node("self"), 
                        create_test_id_node("Test"),
                        create_test_token(TokenType::Dot, ".")
                    ), 
                    create_test_token(TokenType::Inherited, "inherited"))
            ])
        );
    }
}