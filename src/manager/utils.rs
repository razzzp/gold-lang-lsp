use std::{sync::{RwLock, Arc, Mutex}, ops::Deref};

use crate::{parser::ast::{IAstNode, AstBinaryOp}, utils::Position, lexer::tokens::TokenType};

use super::{annotated_node::{AnnotatedNode, EvalType}, symbol_table::{SymbolInfo, ISymbolTable}, semantic_analysis_service::SemanticAnalysisService};


/// searches for the smallest annotated node encasing the given position
pub fn search_encasing_node(node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
{
    let r_node = node.read().unwrap();
    for child in &r_node.children{
        // println!("[Req Definition] Cur Node {}",child.read().unwrap().data.to_string_type_pos());
        if child.read().unwrap().data.get_range().contains_pos(pos){
            return search_encasing_node(child, pos)
        }
    }
    return node.clone()
}

/// returns cloned self if it is a binary op node
pub fn check_dot_ops(node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>)
    ->Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
{
    let lock = node.read().unwrap();
    if let Some(bin_op) = lock.data.as_any().downcast_ref::<AstBinaryOp>(){
        if bin_op.op_token.token_type == TokenType::Dot{
            return Some(node.clone());
        } else {return  None;}
    } else {
        return None;
    }
}

/// returns the parent if it is a binary op node
pub fn check_parent_dot_ops(node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>)
    ->Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
{
    let parent_node = node.read().unwrap().parent.as_ref()?.upgrade()?;
    return check_dot_ops(&parent_node);
}

pub fn get_nearest_symbol_table(node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) 
    -> Option<Arc<Mutex<dyn ISymbolTable>>>
{
    // if node has its own sym table, return that
    if let Some(sym_table) = node.read().unwrap().symbol_table.as_ref(){
        return Some(sym_table.clone());
    }
    // other wise check parents until one is found
    let mut cur_node = node.read().unwrap().parent.clone();
    while let Some(cur) = &cur_node{
        if let Some(cur) = cur.upgrade(){
            if let Some(st) = &cur.read().unwrap().symbol_table{
                return Some(st.clone())
            }
            cur_node = cur.read().unwrap().parent.clone();
        } else {break}
    }
    return None
}

/// also returns the class the symbol is in
pub fn search_sym_info_w_class(id: &str, sym_table: &Arc<Mutex<dyn ISymbolTable>>, sem_service: &SemanticAnalysisService, search_uses: bool) -> Option<(String,Arc<SymbolInfo>)>{
    // already searches parent
    let mut result = sym_table.lock().unwrap().search_symbol_info(id);
    if search_uses && result.is_none() {
        // need to copy uses to local, otherwise symtable will
        // be lock, and since calls here are recursive it may cause deadlock
        // if there are circular depencies
        let uses_list = sym_table.lock().unwrap().get_list_of_uses();

        for uses in uses_list.iter(){
            let uses_sym_table = match sem_service.get_symbol_table_class_def_only(uses){
                Ok(r) => r,
                _=> continue
            };
            result=uses_sym_table.lock().unwrap().search_symbol_info(id);
            if result.is_some(){
                break;
            }
        }
    }
    return result;
}

/// searches symbol info for given node
pub fn search_sym_info_for_node(
    node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, 
    sem_service: &SemanticAnalysisService
) -> Option<(String, Arc<SymbolInfo>)> {
    let sym_table = get_nearest_symbol_table(node)?;
    let node_id = node.read().unwrap().data.get_identifier().to_string();
    // check if parent is dot op
    if let Some(parent_dot_op) = check_parent_dot_ops(node){
        let parent_lock = parent_dot_op.read().unwrap();
        let dot_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>()?;
        // check if left or right node
        if Arc::ptr_eq(&dot_op_node.left_node, &node.read().unwrap().data){
            // left node, search sym info with nearest st
            //  and search uses, since it can be a consant/ type
            return search_sym_info_w_class(
                &node_id, 
                &sym_table, 
                sem_service, 
                true);
        } else {
            // right node get st for left node
            // get type of left node
            let left_node_id = match parent_lock.children[0].read().unwrap().eval_type.clone()? {
                EvalType::Class(c)=> c,
                EvalType::Module(m) => m,
                _=> return None
            };
            let cur_class = sym_table.lock().unwrap().get_class()?;
            // if left type is same class as current class, use cur sym table
            let sym_table = if left_node_id.deref() == cur_class.as_str() {
                sym_table
            } else { 
                // otherwise get sym table
                sem_service.get_symbol_table_class_def_only(left_node_id.deref()).ok()?
            };
            return search_sym_info_w_class(&node_id, &sym_table, &sem_service, false);
        }
    } else {
        // if not under dot ops, just seacrh cur sym table
        return search_sym_info_w_class(
            &node_id, 
            &sym_table, 
            sem_service, 
            true);
    }
}