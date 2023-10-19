use std::sync::Arc;

use lsp_types::{Url, CompletionItem};

use crate::{manager::{
    document_service::DocumentService,
    semantic_analysis_service::SemanticAnalysisService,

}, utils::{ILoggerV2, Position}};

use super::data_structs::ProjectManagerError;


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

    pub fn generate_completion_proposals(
        &self,
        pos: &Position,
    ) -> Result<Vec<CompletionItem>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri, false)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        
        let enc_node = search_encasing_node(&ast, &pos);
        self.logger.log_info(format!("[Req Definition] Node: {}", enc_node.read().unwrap().data.get_identifier()).as_str());
        let st = match TypeResolver::get_nearest_symbol_table(&enc_node.read().unwrap()){
            Some(st) => st,
            _=> return Err(ProjectManagerError::new("Cannot find symbol table", ErrorCode::InternalError))
        };
    }
}