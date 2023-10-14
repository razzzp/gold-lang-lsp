use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstTypeDeclaration, AstProcedure, AstFunction, AstTypeBasic, AstGlobalVariableDeclaration, AstUses, AstParameterDeclaration, AstLocalVariableDeclaration}, analyzers::{IVisitor, AnalyzerDiagnostic}, utils::{DynamicChild, Range, IRange, OptionString, ILogger, IDiagnosticCollector, GenericDiagnosticCollector, ILoggerV2}, lexer::tokens::TokenType, unwrap_or_return};
use core::fmt::Debug;
use std::{collections::{HashMap, HashSet}, sync::{Mutex, Arc, RwLock, RwLockWriteGuard, MutexGuard, LockResult}, ops::{DerefMut, Deref}, result, f32::consts::E, vec::IntoIter};
use crate::utils::{OptionExt};
use super::{ProjectManager, annotated_node::{AnnotatedNode, EvalType, TypeInfo, Location, NativeType, self}, data_structs::{Document, ProjectManagerError}, document_service::DocumentService, type_resolver::TypeResolver, ast_annotator::AstAnnotator};
use crate::manager::symbol_table::{SymbolTable,SymbolInfo,SymbolType, ISymbolTable};
#[derive(Debug, Clone)]
pub struct SemanticAnalysisService {
    pub doc_service : Arc<RwLock<DocumentService>>,
    logger: Arc<dyn ILoggerV2>,
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
    already_seen_uri: Arc<Mutex<HashSet<String>>>,
}

impl SemanticAnalysisService {
    pub fn new(
        doc_service: Arc<RwLock<DocumentService>>, 
        logger: Arc<dyn ILoggerV2>, 
        diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
        already_seen_uri : Option<Arc<Mutex<HashSet<String>>>>
) -> SemanticAnalysisService{
        return SemanticAnalysisService {  
            doc_service,
            logger: logger,
            diag_collector,
            already_seen_uri: already_seen_uri.unwrap_or_default(),
        }
    }

    fn check_already_seen(&mut self, uri: &Url) -> Result<(),ProjectManagerError>{
        if self.already_seen_uri.lock().unwrap().contains(&uri.to_string()){
            return Err(ProjectManagerError::new(format!("{} already locked in request session",uri).as_str(), ErrorCode::RequestFailed));
        }
        self.already_seen_uri.lock().unwrap().insert(uri.to_string());
        return Ok(())
    }
    pub fn get_symbol_table_class_def_only(&mut self, class: &String) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let uri = self.doc_service.read().unwrap().get_uri_for_class(class)?;
        let doc: Arc<Mutex<Document>> = self.analyze_uri(&uri, true)?;
        let doc_lock = doc.lock().unwrap();
        match doc_lock.get_symbol_table(){
            Some(st) => return Ok(st.clone()),
            _=> return Err(ProjectManagerError::new(format!("Unable to get Symbol table for {}",class).as_str(), ErrorCode::InternalError))
        }
    }

    /// if no error occurs, annotated tree & symbol table is guranteed to be Some
    pub fn analyze_uri(&mut self, uri : &Url, only_definitions: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        self.check_already_seen(uri)?;
        self.logger.log_info(format!("[Req Analyze Uri:{}]{}", if !only_definitions {"Full"}else{"Light"},uri).as_str());
        eprintln!();
        let doc: Arc<Mutex<Document>> = self.doc_service.write().unwrap().get_parsed_document(uri, true)?;
        // is exist and only defs needed return existing,
        //  otherwise regenerate
        if doc.lock().unwrap().annotated_ast.is_some() && only_definitions{
            return Ok(doc);
        }
        self.logger.log_info(format!("[Analyzing Uri]{}", uri).as_str());
        return self.analyze(doc, only_definitions);
    }
    
    fn analyze(&mut self, doc: Arc<Mutex<Document>>, only_definitions: bool) -> 
    Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let root_node = doc.lock().unwrap().get_ast().clone();
        // let mut semantic_analysis_service = SemanticAnalysisService::new(
        //     self.doc_service.clone(), 
        //     self.logger.clone(),
        //     self.diag_collector.clone(),
        //     Some(self.already_seen_classes.clone())
        // );
        let mut annotator = AstAnnotator::new(
            self, 
            self.diag_collector.clone(), 
            self.logger.clone(),
            only_definitions
        );
        let annotated_tree = annotator.analyze(&root_node)?;
        doc.lock().unwrap().annotated_ast = Some(annotated_tree.clone());
        return Ok(doc);
    }
}


#[cfg(test)]
mod test{
    use std::sync::{Arc, RwLock};

    use crate::manager::test::{create_test_sem_service, create_test_doc_service, create_uri_from_path};

    #[test]
    fn test_get_symbol_table(){
        let root_uri = create_uri_from_path("./test/workspace");
        let mut doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        let doc_service = Arc::new(RwLock::new(doc_service));
        let mut sem_service = create_test_sem_service(doc_service);

        let result = sem_service.get_symbol_table_class_def_only(&"aRootClass".to_string()).unwrap();
        let result = result.lock().unwrap();
        assert_eq!(result.iter_symbols().count(), 8);
    }
}