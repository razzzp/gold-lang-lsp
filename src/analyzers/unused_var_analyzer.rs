use std::collections::HashMap;

use lsp_types::{DiagnosticSeverity, DiagnosticTag};

use crate::{parser::ast::{AstProcedure, IAstNode, AstFunction, AstTerminal, AstLocalVariableDeclaration, AstBinaryOp}, utils::{IRange, DynamicChild}, lexer::tokens::{Token, TokenType}};

use super::IAnalyzer;

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
                    format!("Unused var: {}", key),
                    None,
                    Some([DiagnosticTag::UNNECESSARY].into_iter().collect())
                ))
            }
        }
    }
    fn is_left_node(node : &DynamicChild<dyn IAstNode>)-> bool{
        if node.parent.is_none() {return true}
        match node.parent.unwrap().as_any().downcast_ref::<AstBinaryOp>() {
            Some(bin_node) => {
                if bin_node.op_token.token_type != TokenType::Dot {
                    return true
                }
                if bin_node.left_node.to_string_ident_pos()== node.data.to_string_ident_pos(){
                    return true;
                } else {
                    return false;
                }
            }
            _=> true,
        }
    }
    fn notify_terminal_node(&mut self, node : &DynamicChild<dyn IAstNode>){
        // let ident = node.data.get_identifier();
        match self.cur_local_vars.get_mut(&node.data.get_identifier()){
            Some(var_info) => {
                // check is on the left side of bin_op to prevent false positive with
                //  member variables
                if Self::is_left_node(node)
                {var_info.use_count = var_info.use_count+1}
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
            Some(_data) =>{
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

    fn notify_end(&mut self) {
        self.check_unused_vars();
    }
}