use std::{collections::HashMap, string};

use lsp_types::{DiagnosticSeverity, DiagnosticTag};

use crate::{parser::ast::{AstProcedure, IAstNode, AstFunction, AstTerminal, AstLocalVariableDeclaration}, utils::{IRange, DynamicChild}, lexer::tokens::Token};

use super::{IAnalyzer, AnalyzerDiagnostic};

#[derive(Debug)]
struct VarInfo{
    use_count: usize,
    ident_token: Token
}

pub struct UnusedVarAnalyzer{
    diagnostics: Vec<lsp_types::Diagnostic>,
    cur_local_vars: HashMap<String, VarInfo>
}
impl UnusedVarAnalyzer {
    pub fn new() -> UnusedVarAnalyzer {
        return UnusedVarAnalyzer { 
            diagnostics: Vec::new(),
            cur_local_vars: HashMap::new(),
        }
    }
    fn check_unused_vars(&mut self){
        for (key, val) in self.cur_local_vars.iter(){
            if val.use_count == 0 {
                self.diagnostics.push(lsp_types::Diagnostic::new(
                    val.ident_token.get_range().as_lsp_type_range(), 
                    Some(DiagnosticSeverity::WARNING), 
                    None,
                    Some("gold".to_string()),
                    "Unused var".to_string(),
                    None,
                    Some([DiagnosticTag::UNNECESSARY].into_iter().collect())
                ))
            }
        }
    }
    fn notify_terminal_node(&mut self, node : &AstTerminal){
        match self.cur_local_vars.get_mut(&node.get_identifier()){
            Some(var_info) => {
                var_info.use_count = var_info.use_count+1;
            }
            _=> ()
        }
    }
    fn notify_local_var_node(&mut self, node: &AstLocalVariableDeclaration){
        if self.cur_local_vars.get(&node.get_identifier()).is_some(){
            self.diagnostics.push(lsp_types::Diagnostic::new(
                node.identifier.get_range().as_lsp_type_range(), 
                Some(DiagnosticSeverity::ERROR), 
                None,
                Some("gold".to_string()),
                "Var name already declared".to_string(),
                None,
                Some([DiagnosticTag::UNNECESSARY].into_iter().collect())
            ))
        } else {
            self.cur_local_vars.insert(
                node.get_identifier(),
                VarInfo { use_count:0, ident_token: node.identifier.clone() }
            );
        }
    }
}
impl IAnalyzer for UnusedVarAnalyzer{
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>) {
        match node.data.as_any().downcast_ref::<AstProcedure>() {
            Some(_proc_node) => {
                self.check_unused_vars();
                self.cur_local_vars = HashMap::new();
            }   
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstFunction>() {
            Some(_func_node) => {
                self.check_unused_vars();
                self.cur_local_vars = HashMap::new();
            }   
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstTerminal>() {
            Some(node) =>{
                self.notify_terminal_node(node)
            }
            _=>()
        }
        match node.data.as_any().downcast_ref::<AstLocalVariableDeclaration>() {
            Some(node) =>{
                self.notify_local_var_node(node)
            }
            _=>()
        }
    }

    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>) {
        self.diagnostics.iter().for_each(|diag|{
            result.push(diag.clone());
        })
    }
}