use std::sync::{Arc, Mutex};

use crate::parser::ast::{IAstNode, AstTypeBasic, AstBinaryOp, AstTerminal};

use super::{annotated_node::{EvalType, NativeType, AnnotatedNode}, ProjectManager};

pub struct TypeResolver {
    // proj_manager: &'a mut ProjectManager,
}
impl TypeResolver {
    pub fn new() ->TypeResolver{
        return TypeResolver {}
    }

    fn resolve_type_basic(&mut self, node: &Arc<dyn IAstNode>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTypeBasic>()?; 
        //
        match node.get_identifier().to_uppercase().as_str() {
            "INT1" | "INT2" | "INT4" | "INT8" => Some(EvalType::Native(NativeType::Int)),
            "NUMERIC" => Some(EvalType::Native(NativeType::Numeric)),
            "STRING" => Some(EvalType::Native(NativeType::String)),
            "CSTRING" => Some(EvalType::Native(NativeType::CString)),
            _ => Some(EvalType::Unknown)
        }
    }

    fn resolve_terminal(&mut self, node: &Arc<dyn IAstNode>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTerminal>()?; 
        //
        match node.get_identifier().to_uppercase().as_str() {
            "INT1" | "INT2" | "INT4" | "INT8" => Some(EvalType::Native(NativeType::Int)),
            "NUMERIC" => Some(EvalType::Native(NativeType::Numeric)),
            "STRING" => Some(EvalType::Native(NativeType::String)),
            "CSTRING" => Some(EvalType::Native(NativeType::CString)),
            _ => Some(EvalType::Unknown)
        }
    }

    fn resolve_bin_op(&mut self, node: &Arc<dyn IAstNode>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTypeBasic>()?; 
        //
        match node.get_identifier().to_uppercase().as_str() {
            "INT1" | "INT2" | "INT4" | "INT8" => Some(EvalType::Native(NativeType::Int)),
            "NUMERIC" => Some(EvalType::Native(NativeType::Numeric)),
            "STRING" => Some(EvalType::Native(NativeType::String)),
            "CSTRING" => Some(EvalType::Native(NativeType::CString)),
            _ => Some(EvalType::Unknown)
        }
    }

    pub fn resolve_node_type(&mut self, node: &Arc<dyn IAstNode>) -> EvalType {
        let mut result = EvalType::Unknown;
        // case basic type node
        result = self.resolve_type_basic(node).unwrap_or(result);
        // binary op
        result = match node.as_any().downcast_ref::<AstBinaryOp>(){
            Some(t) => {
                let left_node_type = self.resolve_node_type(&t.left_node);

                result
            },
            _=> result
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
