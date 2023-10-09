use std::sync::{Arc, Mutex};

use crate::parser::ast::{IAstNode, AstTypeBasic};

use super::{annotated_node::{EvalType, NativeType, AnnotatedNode}, ProjectManager};

pub struct TypeResolver {
    // proj_manager: &'a mut ProjectManager,
}
impl<'a> TypeResolver {
    pub fn new() ->TypeResolver{
        return TypeResolver {}
    }

    pub fn resolve_node_type(&mut self, type_node: &Arc<dyn IAstNode>) -> EvalType {
        let mut result = EvalType::Unknown;
        result = match type_node.as_any().downcast_ref::<AstTypeBasic>() {
            Some(t) => match t.get_identifier().to_uppercase().as_str() {
                "INT1" | "INT2" | "INT4" | "INT8" => EvalType::Native(NativeType::Int),
                "NUMERIC" => EvalType::Native(NativeType::Numeric),
                "STRING" => EvalType::Native(NativeType::String),
                "CSTRING" => EvalType::Native(NativeType::CString),
                _ => EvalType::Unknown,
            },
            _ => result,
        };
        return result;
    }

    pub fn resolve_annotated_node_type(&mut self, node: &Arc<Mutex<AnnotatedNode<dyn IAstNode>>>) -> EvalType {
        let mut result = EvalType::Unknown;
        let inner = &node.lock().unwrap().data;
        result = self.resolve_node_type(&inner);

        return result
    }
}
