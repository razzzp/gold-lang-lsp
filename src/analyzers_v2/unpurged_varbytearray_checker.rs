use std::{sync::{Arc, Mutex, RwLock}, collections::HashMap};

use lsp_types::DiagnosticSeverity;

use crate::{utils::{IDiagnosticCollector, Range, IRange}, parser::ast::{IAstNode, AstProcedure, AstFunction, AstLocalVariableDeclaration, AstMethodCall}};

use super::{annotated_ast_walker::IAnnotatedNodeVisitor, annotated_node::AnnotatedNode};
use crate::manager::utils::DIAGNOSTIC_SOURCE_GOLD;

#[derive(Debug)]
pub struct UnpurgedVarByteArrayChecker{
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<lsp_types::Diagnostic>>>,
    byte_array_seen: HashMap<String, Info>,
}

#[derive(Debug)]
pub struct Info{
    range: Range,
    is_purged: bool,
}

impl UnpurgedVarByteArrayChecker{
    pub fn new(diag_collector: Arc<Mutex<dyn IDiagnosticCollector<lsp_types::Diagnostic>>>) -> UnpurgedVarByteArrayChecker{
        return UnpurgedVarByteArrayChecker { 
            diag_collector,
            byte_array_seen: HashMap::new()
        }
    }
    fn generate_diags_for_unpurged(&self){
        for (id, info) in self.byte_array_seen.iter(){
            if !info.is_purged{
                self.diag_collector.lock().unwrap().add_diagnostic(
                    lsp_types::Diagnostic { 
                        range: info.range.as_lsp_type_range(), 
                        severity: Some(DiagnosticSeverity::WARNING),
                        source: Some(DIAGNOSTIC_SOURCE_GOLD.to_string()), 
                        message: format!("Local tVarByteArray '{}' is not purged", id), 
                        ..Default::default()
                    }
                )
            }
        }
    }

    fn handle_method_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // clear context if entering a new method
        match node.read().unwrap().data.as_any().downcast_ref::<AstProcedure>(){
            Some(_) => {
                // add diags for unpurged
                self.generate_diags_for_unpurged();
                self.byte_array_seen.clear()
            }
            _=>()
        }
        match node.read().unwrap().data.as_any().downcast_ref::<AstFunction>(){
            Some(_) => {
                // add diags for unpurged
                self.generate_diags_for_unpurged();
                self.byte_array_seen.clear()
            }
            _=>()
        }
    }

    fn handle_local_var_decl(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // register local tVarByteArrays
        match node.read().unwrap().data.as_any().downcast_ref::<AstLocalVariableDeclaration>(){
            Some(node) => {
                if node.type_node.get_identifier().to_uppercase().as_str() == "TVARBYTEARRAY"{
                    // register
                    self.byte_array_seen.insert(
                        node.get_identifier().to_string(), 
                        Info { 
                            range: node.identifier.get_range(),
                            is_purged: false }
                    );
                }
            }
            _=>()
        }
    }

    fn handle_method_call(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let inner_node = node.read().unwrap().data.clone();
        // check method calls to OcsByteArray.Purge
        match inner_node.as_any().downcast_ref::<AstMethodCall>(){
            Some(method_call) => {
                // maybe enough to just check method name
                // TODO need to check parent is dot op and lhs is OcsByteArray?
                if method_call.get_identifier().to_uppercase().as_str() == "PURGE"{
                    // get first param
                    if let Some(first_param) = node.read().unwrap().children.first(){
                        // set as true in map
                        if let Some(info) = self.byte_array_seen.get_mut(first_param.read().unwrap().get_identifier()){
                            info.is_purged = true
                        }
                    }
                }
            }
            _=>()
        }
    }
}

impl IAnnotatedNodeVisitor for UnpurgedVarByteArrayChecker{
    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) {
        self.handle_method_decl(node);
        self.handle_local_var_decl(node);
        self.handle_method_call(node);
    }

    fn notify_end(&mut self) {
        self.generate_diags_for_unpurged();
    }
}