use std::sync::{Arc, Mutex, RwLock};

use lsp_server::ErrorCode;
use lsp_types::{Url, CompletionItem, CompletionItemKind};

use crate::{manager::{
    document_service::DocumentService,
    semantic_analysis_service::SemanticAnalysisService,

}, utils::{ILoggerV2, Position, IRange}, parser::ast::{IAstNode, AstBinaryOp}};

use super::{data_structs::ProjectManagerError, utils::{search_encasing_node, check_parent_dot_ops, check_dot_ops}, type_resolver::TypeResolver, annotated_node::{AnnotatedNode, EvalType}, symbol_table::{ISymbolTable, SymbolType}};


#[derive(Debug, Clone)]
pub struct CompletionService{
    doc_service : DocumentService,
    semantic_analysis_service: SemanticAnalysisService,
    source_uri: Url,
    logger: Arc<dyn ILoggerV2>,
}

impl CompletionService{
    pub fn new(
        doc_service: DocumentService,
        semantic_analysis_service: SemanticAnalysisService,
        source_uri: Url,
        logger : Arc<dyn ILoggerV2>
    ) -> CompletionService{
        return CompletionService { 
            doc_service, 
            semantic_analysis_service, 
            source_uri, 
            logger
        }
    }

    fn generate_completion_items_rhs(&self, st: &Arc<Mutex<dyn ISymbolTable>>,) -> Vec<CompletionItem>{
        let st_lock = st.lock().unwrap();
        let result = st_lock
            .collect_unique_symbols_w_parents()
            .into_iter()
            .filter(|sym_info|{
                match sym_info.sym_type{
                    SymbolType::Field | SymbolType::Func |  SymbolType::Proc => return true,
                    _=> return false
                }
            })
            .map(|sym_info| {
                    let item_kind = match sym_info.sym_type{
                        SymbolType::Field => Some(CompletionItemKind::FIELD),
                        SymbolType::Func => Some(CompletionItemKind::FUNCTION),
                        SymbolType::Proc => Some(CompletionItemKind::FUNCTION),
                        _=> None
                    };
                    CompletionItem{
                        label: sym_info.id.clone(),
                        kind: item_kind,
                        ..Default::default()
                    }
            })
            .collect();
        return result
    }

    fn generate_completion_items_lhs(&self, st: &Arc<Mutex<dyn ISymbolTable>>,) -> Vec<CompletionItem>{
        let result = st.lock().unwrap()
            .collect_unique_symbols_w_parents()
            .into_iter()
            .filter(|sym_info|{
                // filter out class members or the list will be too large
                match sym_info.sym_type{
                    SymbolType::Class 
                    | SymbolType::Module 
                    | SymbolType::Type 
                    | SymbolType::Field
                    | SymbolType::Proc 
                    | SymbolType::Func=> return false,
                    _=> return true
                }
            })
            .map(|sym_info| {
                    let item_kind = match sym_info.sym_type{
                        SymbolType::Variable => Some(CompletionItemKind::VARIABLE),
                        SymbolType::Constant => Some(CompletionItemKind::CONSTANT),
                        _=> None
                    };
                    CompletionItem{
                        label: sym_info.id.clone(),
                        kind: item_kind,
                        ..Default::default()
                    }
            })
            .collect();
        return result
    }

    fn generate_rhs_of_entity(
        &self,
        entity: &str,
        st: &Arc<Mutex<dyn ISymbolTable>>,
    ) -> Result<Vec<CompletionItem>, ProjectManagerError>
    {
        // search right node in class
        let uri = match self.doc_service.get_uri_for_class(&entity){
            Ok(u) => u,
            _=> return Ok(Vec::new()),
        };
        // if class is self don't call sem service, because it will fail
        let class_sym_table = if uri == self.source_uri{
            st.clone()
        } else {
            match self.semantic_analysis_service.get_symbol_table_for_class_def_only(&entity){
                Ok(st) => st,
                _=> return Ok(Vec::new())
            }
        };
        // don't search uses because the member should be in the st itself
        return Ok(self.generate_completion_items_rhs(&class_sym_table))
    }

    fn generate_for_node(
        &self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>,
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    ) -> Result<Vec<CompletionItem>, ProjectManagerError> {
        if let Some(dot_op) = check_dot_ops(node){
            let lock = dot_op.read().unwrap();
            let dot_op_node = lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            // if cursor on rhs of dot
            if pos.ge(&dot_op_node.op_token.get_range().end){
                // if right node, check left node type first
                let left_node_type = lock.children[0].read().unwrap().eval_type.clone().unwrap_or_default();
                match left_node_type{
                    EvalType::Class(class_name)=>{
                        // search right node in class
                        return self.generate_rhs_of_entity(&class_name, st);
                    },
                    EvalType::Module(module_name)=>{
                        // search right node in class
                        return self.generate_rhs_of_entity(&module_name, st);
                    },
                    _=> return Ok(Vec::new()),
                }
            } else {
                // if left node, just search sym table
                return Ok(self.generate_completion_items_lhs(st))
            }
        } else if let Some(dot_op_parent) = check_parent_dot_ops(node){
            let parent_lock = dot_op_parent.read().unwrap();
            // should be safe to unwrap
            let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            let left_node = &bin_op_node.left_node;
            if Arc::ptr_eq(left_node, &node.read().unwrap().data){
                // if left node, just search sym table
                return Ok(self.generate_completion_items_lhs(st))
            } else {
                // if right node, check left node type first
                let left_node_type = parent_lock.children[0].read().unwrap().eval_type.clone().unwrap_or_default();
                match left_node_type{
                    EvalType::Class(class_name)=>{
                        // search right node in class
                        return self.generate_rhs_of_entity(&class_name, st);
                    },
                    EvalType::Module(module_name)=>{
                        // search right node in class
                        return self.generate_rhs_of_entity(&module_name, st);
                    },
                    _=> return Ok(Vec::new()),
                }
            }
        } else {
            // enough to check symbol table in current doc
            return Ok(self.generate_completion_items_lhs(st))
        }
    }

    pub fn generate_completion_proposals(
        &self,
        pos: &Position,
    ) -> Result<Vec<CompletionItem>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri, false)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        
        let enc_node = search_encasing_node(&ast, &pos, &self.logger);
        self.logger.log_info(format!("[Req Completion] Node: {}", enc_node.read().unwrap().data.get_identifier()).as_str());
        let st = match TypeResolver::get_nearest_symbol_table(&enc_node.read().unwrap()){
            Some(st) => st,
            _=> return Err(ProjectManagerError::new("Cannot find symbol table", ErrorCode::InternalError))
        };
        
        return self.generate_for_node(&enc_node, &st, &pos);
    }
}

#[cfg(test)]
mod test {
    use lsp_types::Url;

    use crate::{manager::{test::{create_test_project_manager, create_uri_from_path, create_test_sem_service, create_test_logger}, document_service::DocumentService}, utils::Position};

    use super::CompletionService;

    fn create_test_completion_service(
        doc_service: DocumentService,
        uri: Url
    ) -> CompletionService{
        return CompletionService { 
            doc_service: doc_service.clone(), 
            semantic_analysis_service: create_test_sem_service(doc_service), 
            source_uri: uri, 
            logger: create_test_logger()}
    }

    #[test]
    fn test_self_rhs_empty(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aCompletionTest.god");
        // pos input to test, local var
        let pos_input = Position::new(10, 9);

        let completion_service = create_test_completion_service(
            proj_manager.doc_service.clone(), test_input.clone());

        let mut result = completion_service.generate_completion_proposals(&pos_input).unwrap();

        assert_eq!(result.len(), 10);
        let prop = result.pop().unwrap();
        assert_eq!(prop.label, "ThirdProc");
    }

    #[test]
    fn test_self_rhs_filled(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aCompletionTest.god");
        // pos input to test, local var
        let pos_input = Position::new(11, 10);

        let completion_service = create_test_completion_service(
            proj_manager.doc_service.clone(), test_input.clone());

        let mut result = completion_service.generate_completion_proposals(&pos_input).unwrap();

        assert_eq!(result.len(), 10);
        let prop = result.pop().unwrap();
        assert_eq!(prop.label, "ThirdProc");
    }

    #[test]
    fn test_localvar_rhs_empty(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aCompletionTest.god");
        // pos input to test, local var
        let pos_input = Position::new(12, 13);

        let completion_service = create_test_completion_service(
            proj_manager.doc_service.clone(), test_input.clone());

        let mut result = completion_service.generate_completion_proposals(&pos_input).unwrap();

        assert_eq!(result.len(), 9);
        let prop = result.pop().unwrap();
        assert_eq!(prop.label, "ThirdProc");
    }

    #[test]
    fn test_not_in_dot_op(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aCompletionTest.god");
        // pos input to test, local var
        let pos_input = Position::new(14, 4);

        let completion_service = create_test_completion_service(
            proj_manager.doc_service.clone(), test_input.clone());

        let mut result = completion_service.generate_completion_proposals(&pos_input).unwrap();

        assert_eq!(result.len(), 2);
        let prop = result.pop().unwrap();
        assert_eq!(prop.label, "cRootConstant");
    }
}