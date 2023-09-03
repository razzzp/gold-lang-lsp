use std::{collections::HashMap, error::Error, fs::File, io::Read, ops::Deref, rc::Rc, alloc::GlobalAlloc, sync::{Arc, Mutex}};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{GoldDocumentError, parse_gold}, lexer::GoldLexer, utils::IRange};


// pub trait IDocument {
//     fn get_symbols(&self)-> Vec<&'static DocumentSymbol>;
// }
#[derive(Debug)]
pub struct GoldDocument{
    symbols: Vec<DocumentSymbol>,
    ast_nodes: Vec<Box<dyn IAstNode>>,
    errors: Vec<GoldDocumentError>,
    diagnostic_report: RelatedFullDocumentDiagnosticReport
}
impl GoldDocument{
    pub fn get_symbols(&self)-> Vec<DocumentSymbol>{
        self.symbols.iter().map(|s| s.clone()).collect()
    }
    pub fn get_diagnostic_report(&self)-> RelatedFullDocumentDiagnosticReport{
        self.diagnostic_report.clone()
    }
}

#[derive(Debug, Default)]
pub struct GoldDocumentInfo{
    uri: String,
    file_path: String,
    saved: Option<Arc<GoldDocument>>, 
    opened: Option<Arc<GoldDocument>>
}
impl GoldDocumentInfo{
    pub fn get_saved_document(&self) -> Option<Arc<GoldDocument>> {
        if let Some(doc) = &self.saved {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn get_opened_document(&self) -> Option<Arc<GoldDocument>> {
        if let Some(doc) = &self.opened {
            return Some(doc.clone())
        } else {
            return None
        }
    }
}

#[derive(Debug)]
pub struct GoldProjectManager{
    documents_map: HashMap<String, Rc<GoldDocument>>,
    open_documents_map: HashMap<String, Rc<GoldDocument>>,
    documents: HashMap<String, Arc<Mutex<GoldDocumentInfo>>>
}

#[derive(Debug)]
pub struct GoldProjectManagerError{
    pub msg: String,
    pub error_code: ErrorCode,
}

impl GoldProjectManager{
    pub fn new() -> GoldProjectManager{
        GoldProjectManager{
            documents_map: HashMap::new(),
            open_documents_map: HashMap::new(),
            documents: HashMap::new(),
        }
    }

    pub fn get_document_info(&mut self, uri: &Url) -> Result<Arc<Mutex<GoldDocumentInfo>>, GoldProjectManagerError>{
        let uri_string = uri.to_string();
        let doc_info =  self.documents.get(&uri_string);
        if doc_info.is_some() {
            return Ok(doc_info.unwrap().clone());
        } else {
            let file_path = match uri.to_file_path() {
                Ok(fp) => {
                    let fp = match fp.as_path().to_str() {
                        Some(s) => s.to_string(),
                        _ => return Err(GoldProjectManagerError{
                                msg: format!("cannot convert uri to file path:{}", uri),
                                error_code: ErrorCode::InvalidRequest,
                            })
                    };
                    fp
                },
                Err(_) => return Err(GoldProjectManagerError{
                        msg: format!("cannot convert uri to file path:{}", uri),
                        error_code: ErrorCode::InvalidRequest,
                    })
            };
            let new_doc_info = GoldDocumentInfo{
                uri: uri.to_string(),
                file_path: file_path,
                ..Default::default()
            };
            self.documents.insert(uri_string.clone(), Arc::new(Mutex::new(new_doc_info)));
            Ok(self.documents.get(&uri_string).unwrap().clone())     
        }
    }

    pub fn get_parsed_document(&mut self, uri: &Url) -> Result<Arc<GoldDocument>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        // TODO check document still valid
        // check opened document
        if doc_info.lock().unwrap().get_opened_document().is_some() {
            return Ok(doc_info.lock().unwrap().get_opened_document().unwrap());
        }
        // check last saved doc
        if doc_info.lock().unwrap().get_saved_document().is_some() {
            return Ok(doc_info.lock().unwrap().get_saved_document().unwrap());
        } else {
            // if none, read from file
            let new_doc = self.parse_document(doc_info.lock().unwrap().file_path.as_str())?;
            doc_info.lock().unwrap().saved = Some(Arc::new(new_doc));
            return Ok(doc_info.lock().unwrap().get_saved_document().unwrap());
        }
    }

    pub fn notify_document_saved(&mut self, uri: &Url) -> Result<Arc<GoldDocument>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        // TODO check document still valid
        let new_doc = self.parse_document(doc_info.lock().unwrap().file_path.as_str())?;
        doc_info.lock().unwrap().saved = Some(Arc::new(new_doc));
        doc_info.lock().unwrap().opened = None;
        return Ok(doc_info.lock().unwrap().saved.as_ref().unwrap().clone());
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String) -> Result<Arc<GoldDocument>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        // TODO check document still valid
        let new_doc = self.parse_content(full_file_content)?;
        doc_info.lock().unwrap().opened = Some(Arc::new(new_doc));
        return Ok(doc_info.lock().unwrap().opened.as_ref().unwrap().clone());
    }

    fn parse_document(&self, file_path: &str) -> Result<GoldDocument, GoldProjectManagerError>{
        // open file
        let mut file = match File::open(file_path){
            Ok(f) => f,
            Err(e) => return Err(GoldProjectManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        // read bytes first, then convert to handle invalid chars
        let mut contents = Vec::new();
        match file.read_to_end(&mut contents){
            Ok(_n)=> (),
            Err(e) => return Err(GoldProjectManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        // invalid chars replaced with �
        let contents_as_string = String::from_utf8_lossy(contents.as_slice()).to_string();
        return self.parse_content(&contents_as_string)
    }

    fn parse_content(&self, full_file_content: &String) -> Result<GoldDocument, GoldProjectManagerError> {
        // lexing
        let mut lexer = GoldLexer::new();
        let (tokens, lexer_errors) = lexer.lex(&full_file_content);
        // parse
        let (ast_nodes, mut doc_errors) = parse_gold(&tokens);
        // add lexer errors
        doc_errors.extend(lexer_errors.into_iter().map(|l_error|{
            GoldDocumentError { range: l_error.range, msg: l_error.msg }
        }));
        let symbols = self.generate_document_symbols(ast_nodes.1.as_ref())?;
        let diagnostic_report = self.generate_document_diagnostic_report(&doc_errors)?;

        return Ok(GoldDocument { 
            symbols: symbols, 
            ast_nodes: ast_nodes.1, 
            errors: doc_errors,
            diagnostic_report
        })
    }

    fn generate_document_symbols(&self, ast_nodes: &Vec<Box<dyn IAstNode>>) -> Result<Vec<DocumentSymbol>, GoldProjectManagerError>{
        let mut result = Vec::<DocumentSymbol, >::new();
        let mut class_symbol = self.find_and_generate_class_symbol(ast_nodes);
        for node in ast_nodes {
            let symbol = self.generate_symbol_for_node(node.as_ref());
            if symbol.is_some(){
                match &mut class_symbol {
                    Some(class_sym) => class_sym.children.as_mut().unwrap().push(symbol.unwrap()),
                    None => result.push(symbol.unwrap())
                };
            }
        }
        if class_symbol.is_some(){result.push(class_symbol.unwrap())}
        return Ok(result);
    }

    fn generate_document_diagnostic_report(&self, gold_doc_errors: &Vec<GoldDocumentError>)
    -> Result<RelatedFullDocumentDiagnosticReport, GoldProjectManagerError>{
        let diagnostics = self.generate_diagnostics(gold_doc_errors);
        return Ok(RelatedFullDocumentDiagnosticReport{
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport{
                result_id: None,
                items: diagnostics,
            },
        })
    }

    fn generate_diagnostics(&self, gold_doc_errors: &Vec<GoldDocumentError>) -> Vec<Diagnostic>{
        gold_doc_errors.iter()
            .map(|gold_error| {
                Diagnostic::new(
                    gold_error.get_range().as_lsp_type_range(),
                    Some(DiagnosticSeverity::ERROR), 
                    None, 
                    Some("gold".to_string()), 
                    gold_error.get_msg(), 
                    None, 
                    None)
            }).collect()
    }

    fn generate_symbol_for_node(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        let mut result : Option<DocumentSymbol>= None;
        match self.generate_constant_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_type_declaration_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_global_var_decl_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_proc_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_func_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        return result;
    }

    fn generate_constant_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstConstantDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: Some(n.value.value.as_ref().unwrap().to_string()), 
                    kind: SymbolKind::CONSTANT, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_type_declaration_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstTypeDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: None, 
                    kind: SymbolKind::PROPERTY, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_global_var_decl_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: None, 
                    kind: SymbolKind::FIELD, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_proc_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstProcedure>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: None, 
                    kind: SymbolKind::METHOD, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_func_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstFunction>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: None, 
                    kind: SymbolKind::FUNCTION, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn find_and_generate_class_symbol(&self, ast_nodes: &Vec<Box<dyn IAstNode>>) -> Option<DocumentSymbol>{
        let mut result: Option<DocumentSymbol> = None;
        for ast_node in ast_nodes {
            match ast_node.as_ref().as_any().downcast_ref::<AstClass>(){
                Some(n) => {
                    result = Some(DocumentSymbol { 
                        name: n.name.clone(), 
                        detail: Some(n.parent_class.clone()), 
                        kind: SymbolKind::CLASS, 
                        range: n.get_range().as_lsp_type_range(), 
                        selection_range: n.get_range().as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                }
                None => continue
            }
        }
        return result;
    }
}

#[cfg(test)]
mod test{
    use super::GoldProjectManager;


    #[test]
    fn test_gold_document_manager(){
        let doc_manager = GoldProjectManager::new();
        let doc = doc_manager.parse_document("test/aTestClass.god").unwrap();
        // println!("{:#?}",doc);
    }
}