use std::sync::{Arc, Mutex, RwLock};

use crate::parser::ast::{IAstNode, AstTypeBasic, AstBinaryOp, AstTerminal};

use super::{annotated_node::{EvalType, NativeType, AnnotatedNode}, ProjectManager, semantic_analysis_service::{ISymbolTable, SemanticAnalysisService}};

pub struct TypeResolver {
    semantic_analysis_service: SemanticAnalysisService,
}
impl TypeResolver {
    pub fn new(semantic_analysis_service: SemanticAnalysisService) ->TypeResolver{
        return TypeResolver {
            semantic_analysis_service
        }
    }

    fn resolve_type_basic(&mut self, node: &Arc<dyn IAstNode>,sym_table: &Arc<Mutex<dyn ISymbolTable>>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTypeBasic>()?; 
        //
        match node.get_identifier().to_uppercase().as_str() {
            "INT1" | "INT2" | "INT4" | "INT8" => Some(EvalType::Native(NativeType::Int)),
            "NUMERIC" => Some(EvalType::Native(NativeType::Numeric)),
            "STRING" => Some(EvalType::Native(NativeType::String)),
            "CSTRING" => Some(EvalType::Native(NativeType::CString)),
            id => {
                let sym = self.semantic_analysis_service.search_sym_info(&id.to_string(),sym_table, true);
                match sym{
                    Some(s)=> return Some(s.eval_type.as_ref().unwrap_or(&EvalType::Unknown).clone()),
                    _=> return Some(EvalType::Unknown)
                }
            }
        }
    }

    fn resolve_terminal(&mut self, node: &Arc<dyn IAstNode>, sym_table: &Arc<Mutex<dyn ISymbolTable>>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTerminal>()?; 
        //
        let id =  node.get_identifier().to_uppercase();
        let sym = self.semantic_analysis_service.search_sym_info(&id, sym_table, true);
        match sym{
            Some(s)=> return Some(s.eval_type.as_ref().unwrap_or(&EvalType::Unknown).clone()),
            _=> return Some(EvalType::Unknown)
        }
    }

    fn resolve_bin_op(&mut self, node: &Arc<dyn IAstNode>, sym_table: &Arc<Mutex<dyn ISymbolTable>>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstBinaryOp>()?; 
        //
        let left_node_type = self.resolve_node_type(&node.left_node, sym_table);
        match left_node_type{
            EvalType::Unknown => return Some(EvalType::Unknown),
            EvalType::Native(_) => return Some(EvalType::Unknown),
            EvalType::Defined(_t) => {
                todo!()
            },
            EvalType::Class(id)=> {
                let c_sym_table = match self.semantic_analysis_service.get_symbol_table_for_class(&id){
                    Ok(s) => s,
                    _=> return None,
                };
                let right_node_sym_info=c_sym_table.lock().unwrap().get_symbol_info(&node.right_node.get_identifier())?;
                return right_node_sym_info.eval_type.clone()
            }
        }


    }

    pub fn resolve_node_type(&mut self, node: &Arc<dyn IAstNode>,sym_table: &Arc<Mutex<dyn ISymbolTable>>) -> EvalType {
        let mut result = EvalType::Unknown;
        // case basic type node
        result = self.resolve_type_basic(node,sym_table).unwrap_or(result);
        // terminal node
        result = self.resolve_terminal(node, sym_table).unwrap_or(result);
        // binary op
        result = self.resolve_bin_op(node, sym_table).unwrap_or(result);
        return result;
    }

    pub fn resolve_annotated_node_type(
        &mut self, 
        node: &Arc<Mutex<AnnotatedNode<dyn IAstNode>>>,
        sym_table: &Arc<Mutex<dyn ISymbolTable>>
    ) -> EvalType {
        let mut result = EvalType::Unknown;
        let inner = &node.lock().unwrap().data;
        result = self.resolve_node_type(&inner,sym_table);

        return result
    }
}
