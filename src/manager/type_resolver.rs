use std::sync::{Arc, Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard};

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
            "NUM4" | "NUM8" | "NUM10" => Some(EvalType::Native(NativeType::Num)),
            "DECIMAL" => Some(EvalType::Native(NativeType::Decimal)),
            "STRING" => Some(EvalType::Native(NativeType::String)),
            "CSTRING" => Some(EvalType::Native(NativeType::CString)),
            "TEXT" => Some(EvalType::Native(NativeType::Text)),
            "BOOLEAN" => Some(EvalType::Native(NativeType::Boolean)),
            "CHAR" => Some(EvalType::Native(NativeType::Char)),
            id => {
                let sym = self.semantic_analysis_service.search_sym_info(&id.to_string(),sym_table, false);
                match sym{
                    Some(s)=> {
                        return Some(s.eval_type.as_ref().unwrap_or(&EvalType::Unknown).clone())
                    },
                    _=> {
                        // if cannot find try searching
                        if let Ok(_) = self.semantic_analysis_service
                            .doc_service.read().unwrap()
                            .get_uri_for_class(&id.to_string())
                        {
                            return Some(EvalType::Class(id.to_string()));
                        } else {
                            return Some(EvalType::Unresolved(node.get_identifier()))
                        }
                    }
                }
            }
        }
    }

    fn resolve_terminal(&mut self, node: &Arc<dyn IAstNode>, sym_table: &Arc<Mutex<dyn ISymbolTable>>) -> Option<EvalType> {
        let node = node.as_any().downcast_ref::<AstTerminal>()?; 
        //
        let id =  node.get_identifier().to_uppercase();
        let sym = self.semantic_analysis_service.search_sym_info(&id, sym_table, false);
        match sym{
            Some(s)=> {
                // println!("{:#?}", s);
                return Some(s.eval_type.as_ref().unwrap_or(&EvalType::Unknown).clone())
            },
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
                let c_sym_table = match self.semantic_analysis_service.get_symbol_table_for_class_def_only(&id){
                    Ok(s) => s,
                    _=> return None,
                };
                let right_node_sym_info=c_sym_table.lock().unwrap().get_symbol_info(&node.right_node.get_identifier())?;
                return right_node_sym_info.eval_type.clone()
            }
            _=>{
                return Some(EvalType::Unknown)
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
        while let Some(cur) = &cur_node{
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

    pub fn resolve_annotated_node_lock_type(
        &mut self, 
        node_lock: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>,
        root_sym_table: &Arc<Mutex<dyn ISymbolTable>>
    ) -> EvalType {
        let sym_table = TypeResolver::get_nearest_symbol_table(&node_lock, root_sym_table);
        let result = self.resolve_node_type(&node_lock.data, &sym_table);

        return result
    }

}

#[cfg(test)]
mod test{
    use std::sync::{RwLock, Arc, Mutex};

    use crate::{manager::{test::{create_test_project_manager, create_uri_from_path, create_test_logger, create_test_type_resolver}, semantic_analysis_service::SemanticAnalysisService, document_service::DocumentService, annotated_node::{EvalType, NativeType}}, utils::{IDiagnosticCollector, GenericDiagnosticCollector}, analyzers::AnalyzerDiagnostic};

    use super::TypeResolver;


    #[test]
    fn test_resolve_local_variable(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        let doc = proj_manager.analyze_doc(&test_input, false).unwrap();
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        
        // get node to test
        let proc_node = ast.read().unwrap().children.get(5).unwrap().clone();
        let method_body = proc_node.read().unwrap().children.get(2).unwrap().clone();
        let second_writeln = method_body.read().unwrap().children.get(5).unwrap().clone();
        // println!("{:#?}",second_writeln);
        let local_var_ref = second_writeln.read().unwrap().children.get(1).unwrap().clone();

        let mut type_resolver = create_test_type_resolver(proj_manager.doc_service.clone());
        let sym_table = doc.lock().unwrap().symbol_table.as_ref().unwrap().clone();
        let eval_type = type_resolver.resolve_annotated_node_type(&local_var_ref, &sym_table);
        assert_eq!(eval_type, EvalType::Native(NativeType::CString));
    }
}