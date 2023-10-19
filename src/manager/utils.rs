use std::sync::{RwLock, Arc};

use crate::{parser::ast::{IAstNode, AstBinaryOp}, utils::Position, lexer::tokens::TokenType};

use super::annotated_node::AnnotatedNode;


/// searches for the smallest annotated node encasing the given position
pub fn search_encasing_node(node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
{
    let r_node = node.read().unwrap();
    for child in &r_node.children{
        println!("[Req Definition] Cur Node {}",child.read().unwrap().data.to_string_type_pos());
        if child.read().unwrap().data.get_range().contains_pos(pos){
            return search_encasing_node(child, pos)
        }
    }
    return node.clone()
}

/// returns the parent if it is a binary op node
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