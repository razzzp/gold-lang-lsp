use std::{sync::{Arc, Mutex}, fmt::{Display, Debug}, error::Error};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, RelatedFullDocumentDiagnosticReport};

use crate::parser::{ast::IAstNode, ParserDiagnostic};

#[derive(Debug)]
pub struct GoldDocument{
    ast: Box<dyn IAstNode>,
    parser_diagnostics: Vec<ParserDiagnostic>,
    symbols: Option<Arc<Vec<DocumentSymbol>>>,
    analyzer_diagnostics: Option<Arc<Vec<lsp_types::Diagnostic>>>,
    diagnostic_report: Option<Arc<RelatedFullDocumentDiagnosticReport>>,
    symbol_table: Option<Box<dyn ISymbolTable>>
}
impl GoldDocument{
    pub fn new(ast: Box<dyn IAstNode>, parser_diagnostics: Vec<ParserDiagnostic>) -> GoldDocument {
        return GoldDocument{
            ast,
            parser_diagnostics,
            symbols: None,
            analyzer_diagnostics:None,
            diagnostic_report:None,
            symbol_table: None
        };
    }
    pub fn get_ast<'a>(&'a self) -> &'a dyn IAstNode{
        self.ast.as_ast_node()
    }
    pub fn get_symbols(&self)-> Option<Arc<Vec<DocumentSymbol>>>{
        match &self.symbols {
            Some(syms) => return Some(syms.clone()),
            _=> None
        }
    }
    pub fn set_symbols(&mut self, symbols: Option<Arc<Vec<DocumentSymbol>>>){
        self.symbols = symbols;
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
    pub fn get_diagnostic_report(&self)-> Option<Arc<RelatedFullDocumentDiagnosticReport>>{
        match &self.diagnostic_report {
            Some(diag_report) => Some(diag_report.clone()),
            _=> None
        }
    }
    pub fn set_diagnostic_report(&mut self, report: Option<Arc<RelatedFullDocumentDiagnosticReport>>){
        self.diagnostic_report = report;
    }

    pub fn get_parser_diagnostics(&self) -> &Vec<ParserDiagnostic>{
        &self.parser_diagnostics
    }
}

#[derive(Debug, Default)]
pub struct GoldDocumentInfo{
    pub uri: String,
    pub file_path: String,
    saved: Option<Arc<Mutex<GoldDocument>>>,
    opened: Option<Arc<Mutex<GoldDocument>>>
}

impl GoldDocumentInfo{
    pub fn new(uri: String, file_path: String) -> GoldDocumentInfo {
        return GoldDocumentInfo { 
            uri, 
            file_path, 
            saved: None, 
            opened: None
        }
    }
    pub fn get_saved_document(&self) -> Option<Arc<Mutex<GoldDocument>>> {
        if let Some(doc) = &self.saved {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn get_opened_document(&self) -> Option<Arc<Mutex<GoldDocument>>> {
        if let Some(doc) = &self.opened {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn set_saved_document(&mut self, doc: Option<Arc<Mutex<GoldDocument>>>){
        self.saved = doc;
    }
    pub fn set_opened_document(&mut self, doc: Option<Arc<Mutex<GoldDocument>>>){
        self.opened = doc;
    }
}

#[derive(Debug)]
pub struct GoldProjectManagerError{
    pub msg: String,
    pub error_code: ErrorCode,
}
impl Display for GoldProjectManagerError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error; code:{}; msg:{};", self.error_code as usize, self.msg)
    }
}
impl Error for GoldProjectManagerError{

}


pub trait ISymbolTable: Debug + Send{

}