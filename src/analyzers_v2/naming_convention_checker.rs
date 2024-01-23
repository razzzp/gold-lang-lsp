use std::sync::{Arc, Mutex, RwLock};

use lsp_types::DiagnosticSeverity;

use crate::{utils::{is_overriding_member, IDiagnosticCollector, IRange, Range}, parser::ast::{IAstNode, AstProcedure, AstFunction, AstLocalVariableDeclaration, AstTypeDeclaration, AstGlobalVariableDeclaration, AstConstantDeclaration, AstParameterDeclaration}};

use super::{annotated_ast_walker::IAnnotatedNodeVisitor, annotated_node::AnnotatedNode};
use crate::manager::utils::DIAGNOSTIC_SOURCE_GOLD;


#[derive(Debug)]
pub struct NamingConventionChecker{
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<lsp_types::Diagnostic>>>,
}

impl NamingConventionChecker{
    pub fn new(diag_collector: Arc<Mutex<dyn IDiagnosticCollector<lsp_types::Diagnostic>>>) -> NamingConventionChecker{
        return NamingConventionChecker { 
            diag_collector,
        }
    }

    fn is_uppercase_first_char(&self, id: &str) -> bool{
        if let Some(c) = id.chars().next(){
            if c.is_uppercase() {return true}
        }
        return false
    }

    fn is_underscore_first_char(&self, id: &str) -> bool{
        if let Some(c) = id.chars().next(){
            if c == '_' {return true}
        }
        return false
    }

    fn is_first_char(&self, id: &str, char: char) -> bool{
        if let Some(c) = id.chars().next(){
            if c == char {return true}
        }
        return false
    }

    fn handle_check_uppercase_first_char(&self, id: &str, range: &Range, msg: String){
        if !self.is_underscore_first_char(id) && !self.is_uppercase_first_char(id){
            self.diag_collector.lock().unwrap().add_diagnostic(
                lsp_types::Diagnostic { 
                    range: range.as_lsp_type_range(), 
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                    message: msg, 
                    ..Default::default()
                }
            )
        }
    }

    fn handle_member_and_param_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // methods and fields should have Capital First Letter
        match node.read().unwrap().data.as_any().downcast_ref::<AstProcedure>(){
            Some(proc_decl) => {
                // only check non-overriding members to prevent pollution
                if !is_overriding_member(node){
                    self.handle_check_uppercase_first_char(
                        proc_decl.get_identifier(), 
                        &proc_decl.identifier.get_range(),
                        "Procedure names should have capital first letter".to_string()
                    )
                }
            }
            _=>()
        }
        match node.read().unwrap().data.as_any().downcast_ref::<AstFunction>(){
            Some(func_decl) => {
                if !is_overriding_member(node){
                    self.handle_check_uppercase_first_char(
                        func_decl.get_identifier(), 
                        &func_decl.identifier.get_range(),
                        "Function names should have capital first letter".to_string()
                    )
                }
            }
            _=>()
        }

        match node.read().unwrap().data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            Some(field_decl) => {
                if !is_overriding_member(node){
                    self.handle_check_uppercase_first_char(
                        field_decl.get_identifier(), 
                        &field_decl.identifier.get_range(),
                        "Field names should have capital first letter".to_string()
                    )
                }
            }
            _=>()
        }

        match node.read().unwrap().data.as_any().downcast_ref::<AstParameterDeclaration>(){
            Some(field_decl) => {
                // don't need to check if parent is overriding method to prevent pollution
                // method node is the grandparent
                if let Some(parent_node) = node.read().unwrap().get_parent(){
                    if let Some(gparent_node) = parent_node.read().unwrap().get_parent(){
                        if !is_overriding_member(&gparent_node){
                            self.handle_check_uppercase_first_char(
                                field_decl.get_identifier(), 
                                &field_decl.identifier.get_range(),
                                "Parameter names should have capital first letter".to_string()
                            )
                        }
                    }
                }
            }
            _=>()
        }
    }

    fn handle_local_var_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // local vars should have lowercase first letter
        match node.read().unwrap().data.as_any().downcast_ref::<AstLocalVariableDeclaration>(){
            Some(var_decl) => {
                let id = var_decl.get_identifier();
                let range = var_decl.identifier.get_range();
                if !self.is_underscore_first_char(id) && self.is_uppercase_first_char(id){
                    self.diag_collector.lock().unwrap().add_diagnostic(
                        lsp_types::Diagnostic { 
                            range: range.as_lsp_type_range(), 
                            severity: Some(DiagnosticSeverity::WARNING),
                            source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                            message: "Local variable names should have lowercase first letter".to_string(), 
                            ..Default::default()
                        }
                    )
                }
            }
            _=>()
        }
    }

    fn handle_type_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // types should start with t, e.g. tSomeType
        match node.read().unwrap().data.as_any().downcast_ref::<AstTypeDeclaration>(){
            Some(type_decl) => {
                if !self.is_first_char(type_decl.get_identifier(), 't'){
                    self.diag_collector.lock().unwrap().add_diagnostic(
                        lsp_types::Diagnostic { 
                            range: type_decl.identifier.get_range().as_lsp_type_range(), 
                            severity: Some(DiagnosticSeverity::WARNING),
                            source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                            message: "Type names should start with t, e.g. tSomeType".to_string(), 
                            ..Default::default()
                        }
                    )
                }
            }
            _=>()
        }
    }

    fn handle_constant_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // types should start with t, e.g. tSomeType
        match node.read().unwrap().data.as_any().downcast_ref::<AstConstantDeclaration>(){
            Some(const_decl) => {
                if !self.is_first_char(const_decl.get_identifier(), 'c') && !const_decl.get_identifier().starts_with("ml"){
                    self.diag_collector.lock().unwrap().add_diagnostic(
                        lsp_types::Diagnostic { 
                            range: const_decl.identifier.get_range().as_lsp_type_range(), 
                            severity: Some(DiagnosticSeverity::WARNING),
                            source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                            message: "Constant names should start with c, e.g. cSomeConstant".to_string(), 
                            ..Default::default()
                        }
                    )
                }
            }
            _=>()
        }
    }
}

impl IAnnotatedNodeVisitor for NamingConventionChecker{
    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) {
        self.handle_member_and_param_decl(node);
        self.handle_local_var_decl(node);
        self.handle_type_decl(node);
        self.handle_constant_decl(node)
    }

    fn notify_end(&mut self) {
        ()
    }

    fn visit_w_context(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, _context: &dyn super::annotated_ast_walker::IAnnotatedAstWalkerContext) {
        self.visit(node)
    }
}

#[cfg(test)]
mod test{
    

    use lsp_types::Diagnostic;

    use crate::{analyzers_v2::{annotated_ast_walker::AnnotatedAstWalkerPreOrder, AnnotatedAstNodeArx}, manager::{semantic_analysis_service::AnalyzeRequestOptions, test::{create_test_project_manager, create_uri_from_path}}, utils::test_utils::create_test_diag_collector};

    use super::NamingConventionChecker;

    fn check_ast(node:  &AnnotatedAstNodeArx)-> Vec<Diagnostic>{
        let diag_collector = create_test_diag_collector();
        let checker = NamingConventionChecker::new(diag_collector.clone());
        let mut ast_walker = AnnotatedAstWalkerPreOrder::new();
        ast_walker.register_visitor(Box::new(checker));

        ast_walker.walk(&node);

        return diag_collector.lock().unwrap().take_diagnostics();
    }


    #[test]
    fn test_name_checker(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aNameCheckerTest.god");
        let doc = proj_manager.analyze_doc(&test_input, AnalyzeRequestOptions::default().set_cache(true)).unwrap();
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();

        let diags = check_ast(&ast);
        assert_eq!(diags.len(), 6);
    }

    #[test]
    fn test_name_checker_override(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aNameCheckerTestOverride.god");
        let doc = proj_manager.analyze_doc(&test_input, AnalyzeRequestOptions::default().set_cache(true)).unwrap();
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();

        let diags = check_ast(&ast);
        assert_eq!(diags.len(), 0);
    }
}