use std::sync::{Arc, Mutex, RwLock, RwLockReadGuard};

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

    pub fn get_nearest_symbol_table(node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
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

    pub fn resolve_annotated_node_type(
        &mut self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>,
        root_sym_table: &Arc<Mutex<dyn ISymbolTable>>
    ) -> EvalType {
        let node_lock = &node.read().unwrap();
        let sym_table = TypeResolver::get_nearest_symbol_table(&node_lock, root_sym_table);
        let result = self.resolve_node_type(&node_lock.data, &sym_table);

        return result
    }
}
