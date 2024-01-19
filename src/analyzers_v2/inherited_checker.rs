use std::collections::HashSet;

use lsp_types::DiagnosticSeverity;

use super::{annotated_ast_walker::IAnnotatedNodeVisitor, IDiagnosticCollectorArx};

use crate::{analyzers_v2::annotated_ast_walker::IAnnotatedAstWalkerContext, parser::ast::{AstProcedure, AstFunction, AstUnaryOp, AstBinaryOp, AstTerminal}, lexer::tokens::TokenType, manager::utils::DIAGNOSTIC_SOURCE_GOLD, utils::Range};
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
        methods_to_check.insert("NOTIFYINIT");
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
            if self.methods_to_check.contains(cur_method.read().unwrap().get_identifier().to_uppercase().as_str()) 
            && !self.is_inherited_called {
                // get range to highlight, 
                let method_id = cur_method.read().unwrap().data.get_identifier().to_string();
                let mut sel_range = cur_method.read().unwrap().data.get_range();
                sel_range = cur_method.read().unwrap().data.as_any().downcast_ref::<AstProcedure>().map(|n|{
                    n.identifier.get_range()
                }).unwrap_or(sel_range);
                sel_range = cur_method.read().unwrap().data.as_any().downcast_ref::<AstFunction>().map(|n|{
                    n.identifier.get_range()
                }).unwrap_or(sel_range);

                self.diag_collector.lock().unwrap().add_diagnostic(lsp_types::Diagnostic { 
                    range: sel_range.as_lsp_type_range(), 
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                    message: format!("Method '{}' should call its inherited implem.", method_id), 
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
                if bin_op.right_node.get_identifier().to_uppercase() == cur_method.read().unwrap().get_identifier().to_uppercase(){
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
        match node.read().unwrap().data.as_any().downcast_ref::<AstTerminal>(){
            Some(n) => {
                if n.token.get_value().to_uppercase().as_str() == "PASS"{
                    self.is_inherited_called = true
                }
            },
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
    

    use std::sync::Arc;

    use lsp_types::{DiagnosticSeverity, Diagnostic};

    use crate::{utils::test_utils::*, lexer::tokens::{TokenType}, analyzers_v2::{test_utils::{annotate_ast}, annotated_ast_walker::{IAnnotatedNodeVisitor, AnnotatedAstWalkerPreOrder}, ast_annotator}, parser::ast::IAstNode, analyzers::ast_walker};

    use super::InheritedChecker;

    fn check_ast(node:  &Arc<dyn IAstNode>)-> Vec<Diagnostic>{
        let diag_collector = create_test_diag_collector();
        let checker = InheritedChecker::new(diag_collector.clone());
        let mut ast_walker = AnnotatedAstWalkerPreOrder::new();
        ast_walker.register_visitor(Box::new(checker));

        let annotated_node = annotate_ast(&node);
        ast_walker.walk(&annotated_node);

        return diag_collector.lock().unwrap().take_diagnostics();
    }

    #[test]
    fn test_inherited_called(){
        let node : Arc<dyn IAstNode> = create_test_proc_node(
            "Init",
            Some(vec![
                create_test_unary_op_node(
                    create_test_bin_op_ndoe(
                        create_test_id_node("self"), 
                        create_test_id_node("init"),
                        create_test_token(TokenType::Dot, ".")
                    ), 
                    create_test_token(TokenType::Inherited, "inherited"))
            ])
        );
        let diags = check_ast(&node);
        // check diags
        assert_eq!(diags.len(), 0);
        diags.into_iter().for_each(|d|{
            assert_eq!(d.severity, Some(DiagnosticSeverity::WARNING))
        });
    }

    #[test]
    fn test_inherited_not_called(){
        let node : Arc<dyn IAstNode> = create_test_proc_node(
            "init",
            Some(vec![
                create_test_unary_op_node(
                    create_test_bin_op_ndoe(
                        create_test_id_node("self"), 
                        create_test_id_node("notinit"),
                        create_test_token(TokenType::Dot, ".")
                    ), 
                    create_test_token(TokenType::Inherited, "inherited"))
            ])
        );
        let diags = check_ast(&node);
        // check diags
        assert_eq!(diags.len(), 1);
        diags.into_iter().for_each(|d|{
            assert_eq!(d.severity, Some(DiagnosticSeverity::WARNING))
        });
    }

    #[test]
    fn test_pass(){
        let node : Arc<dyn IAstNode> = create_test_proc_node(
            "init",
            Some(vec![
                create_test_id_node("pass")
            ])
        );
        let diags = check_ast(&node);
        // check diags
        assert_eq!(diags.len(), 0);
        diags.into_iter().for_each(|d|{
            assert_eq!(d.severity, Some(DiagnosticSeverity::WARNING))
        });
    }
}