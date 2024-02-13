use lsp_server::ErrorCode;
use lsp_types::Url;

use crate::{analyzers::AnalyzerDiagnostic, utils::{IDiagnosticCollector, ILoggerV2, LogLevel, LogType}};
use core::fmt::Debug;
use std::sync::{Mutex, Arc, RwLock};

use super::{data_structs::{Document, ProjectManagerError, DocumentInfo}, document_service::DocumentService};
use crate::analyzers_v2::{
    ast_annotator::AstAnnotator,
    symbol_table::ISymbolTable,
};

#[derive(Debug,Clone,Copy,Default)]
pub struct AnalyzeRequestOptions{
    pub only_definitions: bool,
    pub cache_result: bool,
}
impl AnalyzeRequestOptions {
    pub fn set_cache(mut self, cache_result: bool) -> AnalyzeRequestOptions{
        self.cache_result = cache_result;
        return self
    }
    pub fn set_only_def(mut self, only_def: bool) -> AnalyzeRequestOptions{
        self.only_definitions = only_def;
        return self
    }
}

#[derive(Debug)]
pub struct SemanticAnalysisService {
    pub doc_service : DocumentService,
    logger: Box<dyn ILoggerV2>,
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
}
impl Clone for SemanticAnalysisService{
    fn clone(&self) -> Self {
        Self { doc_service: self.doc_service.clone(), logger: self.logger.clone_box(), diag_collector: self.diag_collector.clone() }
    }
}
impl SemanticAnalysisService {
    pub fn new(
        doc_service: DocumentService, 
        logger: Box<dyn ILoggerV2>, 
        diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
) -> SemanticAnalysisService{
        return SemanticAnalysisService {  
            doc_service,
            logger: logger,
            diag_collector,
        }
    }


    pub fn get_symbol_table_for_class_def_only(&self, class: &str) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let uri = self.doc_service.get_uri_for_class(class)?;
        return self.get_symbol_table_for_uri_def_only(&uri);
    }

    pub fn get_symbol_table_for_uri_def_only(&self, uri: &Url) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        // check sym table on doc info first
        let doc_info = self.doc_service.get_document_info(uri)?;
        if let Some(sym_table) = doc_info.read().unwrap().get_symbol_table(){
            self.logger.log(LogType::Info, LogLevel::Verbose, "[GET Symbol Table] Cached symbol table found");
            return Ok(sym_table);
        }
        // else analyze
        let doc: Arc<Mutex<Document>> = self.analyze_uri(&uri, 
            AnalyzeRequestOptions::default().set_only_def(true))?;
        let sym_table = doc.lock().unwrap().get_symbol_table().clone();
        match sym_table{
            Some(st) => {
                self.logger.log(LogType::Info, LogLevel::Verbose, "[GET Symbol Table] Returning analyzed symbol table");
                return Ok(st.clone())
            },
            _=> return Err(ProjectManagerError::new(format!("Unable to get Symbol table for {}",uri).as_str(), ErrorCode::InternalError))
        }
    }

    /// if no error occurs, annotated tree & symbol table is guranteed to be Some
    pub fn analyze_uri(&self, uri : &Url, options: AnalyzeRequestOptions) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{

        // self.logger.log_info(format!("[Req Analyze Uri:{}]{}", if !only_definitions {"Full"}else{"Light"},uri).as_str());
        
        let doc: Arc<Mutex<Document>> = if options.cache_result{
            self.doc_service.get_parsed_document(uri, true)?
        } else {
            self.doc_service.get_parsed_document_without_caching(uri, true)?
        };
        // is exist and only defs needed return existing,
        //  otherwise regenerate
        if doc.lock().unwrap().annotated_ast.is_some(){
            if options.only_definitions{
                // some means at least definitions defined
                self.logger.log(LogType::Info, LogLevel::Verbose, "[Analyze URI] Cached annotated symbol found");
                return Ok(doc);
            } else {
                // else have to check if full annotation
                if doc.lock().unwrap().only_definitions == options.only_definitions{
                    self.logger.log(LogType::Info, LogLevel::Verbose, "[Analyze URI] Cached annotated symbol found");
                    return Ok(doc);
                }
            }
        }
        let doc_info = self.doc_service.get_document_info(uri)?;
        // self.logger.log_info(format!("[Analyzing Uri]{}", uri).as_str());
        let analyzed_doc = self.analyze(doc, doc_info, options.only_definitions)?;
        // save to symtable to doc_info
        self.logger.log(LogType::Info, LogLevel::Verbose, "[Analyze URI] Analyzed doc");
        return Ok(analyzed_doc);
    }
    
    pub fn analyze(&self, doc: Arc<Mutex<Document>>, doc_info: Arc<RwLock<DocumentInfo>>, only_definitions: bool) -> 
    Result<Arc<Mutex<Document>>, ProjectManagerError>{
        
        // let mut semantic_analysis_service = SemanticAnalysisService::new(
        //     self.doc_service.clone(), 
        //     self.logger.clone(),
        //     self.diag_collector.clone(),
        //     Some(self.already_seen_classes.clone())
        // );
        let mut annotator = AstAnnotator::new(
            self.clone(), 
            self.diag_collector.clone(), 
            self.logger.clone_box_with_appended_prefix("Analysis Service"),
            only_definitions
        );
        let annotated_doc = annotator.annotate_doc(doc, doc_info)?;

        return Ok(annotated_doc);
    }
}


#[cfg(test)]
mod test{
    

    use crate::manager::test::{create_test_sem_service, create_test_doc_service, create_uri_from_path};

    #[test]
    fn test_get_symbol_table(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        let sem_service = create_test_sem_service(doc_service);

        let result = sem_service.get_symbol_table_for_class_def_only(&"aRootClass".to_string()).unwrap();
        let result = result.lock().unwrap();
        assert_eq!(result.iter_symbols().count(), 9);
    }
}