use std::{sync::{Arc, Mutex, RwLock}, fmt::{Display, Debug}, error::Error};

use lsp_server::ErrorCode;



use crate::parser::{ast::IAstNode, ParserDiagnostic};

use super::annotated_node::AnnotatedNode;
use crate::manager::symbol_table::ISymbolTable;

#[derive(Debug)]
pub struct Document{
    pub ast: Arc<dyn IAstNode>,
    pub parser_diagnostics: Vec<ParserDiagnostic>,
    pub analyzer_diagnostics: Option<Arc<Vec<lsp_types::Diagnostic>>>,
    pub annotated_ast: Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>,
    pub only_definitions : bool
}
impl Document{
    pub fn new(ast: Arc<dyn IAstNode>, parser_diagnostics: Vec<ParserDiagnostic>) -> Document {
        return Document{
            ast,
            parser_diagnostics,
            analyzer_diagnostics:None,
            annotated_ast: None,
            only_definitions: false
        };
    }
    pub fn get_ast<'a>(&'a self) -> &'a Arc<dyn IAstNode>{
        &self.ast
    }
    pub fn get_symbol_table(&self)-> Option<Arc<Mutex<dyn ISymbolTable>>>{
        return self.annotated_ast.as_ref()?.read().unwrap().symbol_table.clone();
    }
    pub fn set_symbol_table(&mut self, symbol_table: Option<Arc<Mutex<dyn ISymbolTable>>>){
        match &self.annotated_ast{
            Some(ast) => {
                ast.write().unwrap().symbol_table = symbol_table;
            },
            _=> (),
        }
    }

    pub fn get_analyzer_diagnostics(&self)-> Option<Arc<Vec<lsp_types::Diagnostic>>>{
        match &self.analyzer_diagnostics {
            Some(syms) => return Some(syms.clone()),
            _=> None
        }
    }
    pub fn set_analyzer_diagnostics(&mut self, diagnostics: Option<Arc<Vec<lsp_types::Diagnostic>>>){
        self.analyzer_diagnostics = diagnostics;
    }
    pub fn get_parser_diagnostics(&self) -> &Vec<ParserDiagnostic>{
        &self.parser_diagnostics
    }
}

#[derive(Debug, Default)]
pub struct DocumentInfo{
    pub uri: String,
    pub file_path: String,
    saved: Option<Arc<Mutex<Document>>>,
    opened: Option<Arc<Mutex<Document>>>
}

impl DocumentInfo{
    pub fn new(uri: String, file_path: String) -> DocumentInfo {
        return DocumentInfo { 
            uri, 
            file_path, 
            saved: None, 
            opened: None
        }
    }
    pub fn get_saved_document(&self) -> Option<Arc<Mutex<Document>>> {
        if let Some(doc) = &self.saved {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn get_opened_document(&self) -> Option<Arc<Mutex<Document>>> {
        if let Some(doc) = &self.opened {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn set_saved_document(&mut self, doc: Option<Arc<Mutex<Document>>>){
        self.saved = doc;
    }
    pub fn set_opened_document(&mut self, doc: Option<Arc<Mutex<Document>>>){
        self.opened = doc;
    }
}

#[derive(Debug)]
pub struct ProjectManagerError{
    pub msg: String,
    pub error_code: ErrorCode,
}
impl ProjectManagerError {
    pub fn new(msg: &str, error_code : ErrorCode)->ProjectManagerError{
        ProjectManagerError { msg: msg.to_string(), error_code }
    }
}
impl Display for ProjectManagerError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error; code:{}; msg:{};", self.error_code as usize, self.msg)
    }
}
impl Error for ProjectManagerError{

}

