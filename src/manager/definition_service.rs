

use std::sync::{Arc, RwLock, Mutex, RwLockWriteGuard, RwLockReadGuard, Weak};

use lsp_server::ErrorCode;
use lsp_types::{LocationLink, Url};

use crate::{parser::ast::{IAstNode, AstTerminal, AstBinaryOp}, utils::Position, lexer::tokens::TokenType};

use super::{ProjectManager, data_structs::{ProjectManagerError, Document}, annotated_node::{AnnotatedNode, EvalType}, semantic_analysis_service::{ISymbolTable, SemanticAnalysisService}, type_resolver::TypeResolver, document_service::DocumentService};



pub struct DefinitionService{
    semantic_analysis_service: SemanticAnalysisService,
    type_resolver: TypeResolver,
    source_uri: Url
}
impl DefinitionService{
    pub fn new(semantic_analysis_service: SemanticAnalysisService, source_uri: Url)->DefinitionService{
        DefinitionService { 
            type_resolver: TypeResolver::new(semantic_analysis_service.clone()),
            semantic_analysis_service: semantic_analysis_service,
            source_uri
        }
    }

    fn search_encasing_node(&self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
    {
        let r_node = node.read().unwrap();
        for child in &r_node.children{
            if child.read().unwrap().data.get_range().contains_pos(pos){
                return self.search_encasing_node(child, pos)
            }
        }
        return node.clone()
    }

    fn is_bin_op_dot_ops(&self, node : &Option<Weak<RwLock<AnnotatedNode<dyn IAstNode>>>>)->bool{
        if node.is_none(){
            return false;
        }
        let node = match node.as_ref().unwrap().upgrade(){
            Some(n) => n,
            _=> return false
        };
        let node = node.read().unwrap();
        if let Some(bin_op) = node.data.as_any().downcast_ref::<AstBinaryOp>(){
            if bin_op.op_token.token_type == TokenType::Dot{
                return true
            } else {return  false;}
        } else {
            return false;
        }
    }

    fn get_nearest_symbol_table(&self,node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
    root_st: &Arc<Mutex<dyn ISymbolTable>>) -> Arc<Mutex<dyn ISymbolTable>>{
        if let Some(sym_table) = node.symbol_table.as_ref(){
            return sym_table.clone();
        }
        let mut cur_node = node.parent.clone();
        while let Some(cur) = cur_node{
            if let Some(cur) = cur.upgrade(){
                if let Some(st) = &cur.read().unwrap().symbol_table{
                    return st.clone()
                }
                cur_node = cur.read().unwrap().parent.clone();
            } else {break}
        }
        return root_st.clone()
    }

    fn handle_terminal(
        &mut self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        root_st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        if let Some(term_node) = node.data.as_any().downcast_ref::<AstTerminal>(){
            // if part of dot ops, may need to check another class
            if self.is_bin_op_dot_ops(&node.parent){
                let bin_op_parent = match node.parent.as_ref().unwrap().upgrade(){
                    Some(p) => p,
                    None=> return Some(Err(ProjectManagerError::new("Failed to upgrade wek ref", ErrorCode::InternalError)))
                };
                let left_node_type = self.type_resolver.resolve_node_type(&bin_op_parent.read().unwrap().data, root_st);
                todo!()
            } else {
                // enough to check symbol table in current doc
                let st = self.get_nearest_symbol_table(node, root_st);
                let mut st_lock =st.lock().unwrap();
                match st_lock.get_symbol_info(&term_node.get_identifier()){
                    Some(s) =>{
                        let mut result = Vec::new();
                        result.push(LocationLink{
                            origin_selection_range: Some(node.data.get_range().as_lsp_type_range()),
                            target_range: s.range.as_lsp_type_range(),
                            target_selection_range: s.selection_range.as_lsp_type_range(),
                            target_uri: self.source_uri.clone()
                        });
                        return  Some(Ok(result));
                    },
                    _=> Some(Err(ProjectManagerError::new("Cannot find symbol definition", lsp_server::ErrorCode::RequestFailed)))
                }
            }
        } else {return None}
    }

    fn handle_node(
        &mut self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, 
        root_st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>{
        let r_node = node.read().unwrap();
        if let Some(r)= self.handle_terminal(&r_node, root_st){
            return  r;
        }
        return Err(ProjectManagerError::new("Failed to provide Go To Definition", lsp_server::ErrorCode::InternalError))
    }

    pub fn get_definition(&mut self, uri: &lsp_types::Url, pos : Position)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(uri)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let root_st = doc.lock().unwrap().symbol_table.as_ref().unwrap().clone();
        let enc_node =self.search_encasing_node(&ast, &pos);

        return self.handle_node(&enc_node, &root_st)
    }
}