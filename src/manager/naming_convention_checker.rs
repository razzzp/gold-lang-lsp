use std::sync::{Arc, Mutex, RwLock};

use lsp_types::DiagnosticSeverity;

use crate::{utils::{IDiagnosticCollector, Range, IRange}, parser::ast::{IAstNode, AstProcedure, AstFunction, AstLocalVariableDeclaration, AstTypeDeclaration, AstGlobalVariableDeclaration, AstConstantDeclaration, AstParameterDeclaration}};

use super::{annotated_ast_walker::IAnnotatedNodeVisitor, annotated_node::AnnotatedNode, utils::DIAGNOSTIC_SOURCE_GOLD};


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
                self.handle_check_uppercase_first_char(
                    proc_decl.get_identifier(), 
                    &proc_decl.identifier.get_range(),
                    "Procedure names should have capital first letter".to_string()
                )
            }
            _=>()
        }
        match node.read().unwrap().data.as_any().downcast_ref::<AstFunction>(){
            Some(func_decl) => {
                self.handle_check_uppercase_first_char(
                    func_decl.get_identifier(), 
                    &func_decl.identifier.get_range(),
                    "Function names should have capital first letter".to_string()
                )
            }
            _=>()
        }

        match node.read().unwrap().data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            Some(field_decl) => {
                self.handle_check_uppercase_first_char(
                    field_decl.get_identifier(), 
                    &field_decl.identifier.get_range(),
                    "Field names should have capital first letter".to_string()
                )
            }
            _=>()
        }

        match node.read().unwrap().data.as_any().downcast_ref::<AstParameterDeclaration>(){
            Some(field_decl) => {
                self.handle_check_uppercase_first_char(
                    field_decl.get_identifier(), 
                    &field_decl.identifier.get_range(),
                    "Parameter names should have capital first letter".to_string()
                )
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
            Some(type_decl) => {
                if !self.is_first_char(type_decl.get_identifier(), 'c'){
                    self.diag_collector.lock().unwrap().add_diagnostic(
                        lsp_types::Diagnostic { 
                            range: type_decl.identifier.get_range().as_lsp_type_range(), 
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
}