use std::sync::{Arc, Mutex, RwLock};

use lsp_server::ErrorCode;
use lsp_types::{Url, CompletionItem, CompletionItemKind};

use crate::{manager::{
    document_service::DocumentService,
    semantic_analysis_service::SemanticAnalysisService,

}, utils::{ILoggerV2, Position}, parser::ast::{IAstNode, AstBinaryOp}};

use super::{data_structs::ProjectManagerError, utils::{search_encasing_node, check_parent_dot_ops}, type_resolver::TypeResolver, annotated_node::{AnnotatedNode, EvalType}, symbol_table::{ISymbolTable, SymbolType}};


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

    fn generate_completion_items(&self, st: &Arc<Mutex<dyn ISymbolTable>>,) -> Vec<CompletionItem>{
        let st_lock = st.lock().unwrap();
        let result = st_lock.iter_symbols()
            .filter(|sym_info|{
                match sym_info.sym_type{
                    SymbolType::Class | SymbolType::Field
                    | SymbolType::Func |  SymbolType::Proc => return true,
                    _=> return false
                }
            })
            .map(|sym_info| {
                    let item_kind = match sym_info.sym_type{
                        SymbolType::Class => Some(CompletionItemKind::CLASS),
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

    fn generate_rhs_of_entity(
        &self,
        entity: &String,
        st: &Arc<Mutex<dyn ISymbolTable>>,
    ) -> Result<Option<Vec<CompletionItem>>, ProjectManagerError>
    {
        // search right node in class
        let uri = match self.doc_service.get_uri_for_class(&entity){
            Ok(u) => u,
            _=> return Ok(None),
        };
        // if class is self don't call sem service, because it will fail
        let class_sym_table = if uri == self.source_uri{
            st.clone()
        } else {
            match self.semantic_analysis_service.get_symbol_table_class_def_only(&entity){
                Ok(st) => st,
                _=> return Ok(None)
            }
        };
        // don't search uses because the member should be in the st itself
        return Ok(Some(self.generate_completion_items(&class_sym_table)))
    }

    fn generate_for_node(
        &self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>,
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    ) -> Result<Option<Vec<CompletionItem>>, ProjectManagerError> {
        // if part of dot ops, may need to check another class
        if let Some(bin_op_parent) = check_parent_dot_ops(node){

            let parent_lock = bin_op_parent.read().unwrap();
            // should be safe to unwrap
            let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            let left_node = &bin_op_node.left_node;
            if Arc::ptr_eq(left_node, &node.read().unwrap().data){
                // if left node, just search sym table
                return Ok(Some(self.generate_completion_items(st)))
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
                    _=> return Ok(None),
                }
            }
        } else {
            // enough to check symbol table in current doc
            return Ok(Some(self.generate_completion_items(st)))
        }
    }

    pub fn generate_completion_proposals(
        &self,
        pos: &Position,
    ) -> Result<Option<Vec<CompletionItem>>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri, false)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        
        let enc_node = search_encasing_node(&ast, &pos);
        self.logger.log_info(format!("[Req Completion] Node: {}", enc_node.read().unwrap().data.get_identifier()).as_str());
        let st = match TypeResolver::get_nearest_symbol_table(&enc_node.read().unwrap()){
            Some(st) => st,
            _=> return Err(ProjectManagerError::new("Cannot find symbol table", ErrorCode::InternalError))
        };
        
        return self.generate_for_node(&enc_node, &st, pos);
    }
}