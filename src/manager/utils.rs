use std::sync::{RwLock, Arc};

use crate::{parser::ast::IAstNode, utils::Position};

use super::annotated_node::AnnotatedNode;



pub fn search_encasing_node(node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
{
    let r_node = node.read().unwrap();
    for child in &r_node.children{
        // self.logger.log_info(format!("[Req Definition] Cur Node {}",child.read().unwrap().data.to_string_type_pos()).as_str());
        if child.read().unwrap().data.get_range().contains_pos(pos){
            return search_encasing_node(child, pos)
        }
    }
    return node.clone()
}