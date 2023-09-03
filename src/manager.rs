use std::{collections::HashMap, error::Error, fs::File, io::Read, ops::Deref, rc::Rc, alloc::GlobalAlloc, sync::{Arc, Mutex}, cell::RefCell, panic::resume_unwind};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{ParserDiagnostic, parse_gold}, lexer::GoldLexer, utils::IRange, analyzer::{AnalyzerDiagnostic, ast_walker::AstWalker, IAstWalker, analyzers::UnusedVarAnalyzer}};

// pub trait IDocument {
//     fn get_symbols(&self)-> Vec<&'static DocumentSymbol>;
// }
#[derive(Debug)]
#[derive(Default)]
pub struct GoldDocument{
    ast_nodes: Vec<Box<dyn IAstNode>>,
    parser_diagnostics: Vec<ParserDiagnostic>,
    symbols: Option<Rc<Vec<DocumentSymbol>>>,
    analyzer_diagnostics: Option<Rc<Vec<lsp_types::Diagnostic>>>,
    diagnostic_report: Option<Rc<RelatedFullDocumentDiagnosticReport>>
}
impl GoldDocument{
    pub fn get_symbols(&self)-> Option<Rc<Vec<DocumentSymbol>>>{
        match &self.symbols {
            Some(syms) => return Some(syms.clone()),
            _=> None
        }
    }
    pub fn get_analyzer_diagnostics(&self)-> Option<Rc<Vec<lsp_types::Diagnostic>>>{
        match &self.analyzer_diagnostics {
            Some(syms) => return Some(syms.clone()),
            _=> None
        }
    }
    pub fn get_diagnostic_report(&self)-> Option<Rc<RelatedFullDocumentDiagnosticReport>>{
        match &self.diagnostic_report {
            Some(diag_report) => Some(diag_report.clone()),
            _=> None
        }
    }
}

#[derive(Debug, Default)]
pub struct GoldDocumentInfo{
    uri: String,
    file_path: String,
    saved: Option<Rc<RefCell<GoldDocument>>>,
    opened: Option<Rc<RefCell<GoldDocument>>>
}

impl GoldDocumentInfo{
    pub fn get_saved_document(&self) -> Option<Rc<RefCell<GoldDocument>>> {
        if let Some(doc) = &self.saved {
            return Some(doc.clone())
        } else {
            return None
        }
    }

    pub fn get_opened_document(&self) -> Option<Rc<RefCell<GoldDocument>>> {
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

    pub fn get_parsed_document(&mut self, uri: &Url) -> Result<Rc<RefCell<GoldDocument>>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
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
            doc_info.lock().unwrap().saved = Some(Rc::new(RefCell::new(new_doc)));
            return Ok(doc_info.lock().unwrap().get_saved_document().unwrap());
        }
    }

    pub fn get_document_symbols(&mut self, doc: Rc<RefCell<GoldDocument>>)-> Rc<Vec<DocumentSymbol>>{
        let syms = doc.borrow().get_symbols();
        if syms.is_some() {
            return syms.unwrap();
        } else {
            return self.generate_document_symbols(doc).unwrap();
        }
    }

    pub fn get_diagnostic_report(&mut self, doc: Rc<RefCell<GoldDocument>>) -> Rc<RelatedFullDocumentDiagnosticReport>{
        let diag_report = doc.borrow().get_diagnostic_report();
        if diag_report.is_some(){
            return diag_report.unwrap();
        } else {
            return self.generate_document_diagnostic_report(doc).unwrap();
        }
    }

    pub fn notify_document_saved(&mut self, uri: &Url) -> Result<Rc<RefCell<GoldDocument>>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_document(doc_info.lock().unwrap().file_path.as_str())?;
        doc_info.lock().unwrap().saved = Some(Rc::new(RefCell::new(new_doc)));
        doc_info.lock().unwrap().opened = None;
        return Ok(doc_info.lock().unwrap().saved.as_ref().unwrap().clone());
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String) -> Result<Rc<RefCell<GoldDocument>>, GoldProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_content(full_file_content)?;
        doc_info.lock().unwrap().opened = Some(Rc::new(RefCell::new(new_doc)));
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
        let (ast_nodes, mut parser_diagnostics) = parse_gold(&tokens);
        // add lexer errors
        parser_diagnostics.extend(lexer_errors.into_iter().map(|l_error|{
            ParserDiagnostic { range: l_error.range, msg: l_error.msg }
        }));
        return Ok(GoldDocument { 
            ast_nodes: ast_nodes.1,
            parser_diagnostics,
            ..Default::default()
        })
    }

    fn get_analyzer_diagnostics(&self, doc : Rc<RefCell<GoldDocument>>) -> Result<Rc<Vec<lsp_types::Diagnostic>>, GoldProjectManagerError>{
        let diags = doc.borrow().get_analyzer_diagnostics();
        if diags.is_some(){
            return Ok(diags.unwrap());
        } else {
            let diags = self.analyze_ast(&doc.borrow().ast_nodes);
            doc.borrow_mut().analyzer_diagnostics = Some(Rc::new(diags));
            return Ok(doc.borrow().analyzer_diagnostics.as_ref().unwrap().clone());
        }
    }

    fn analyze_ast(&self, ast_nodes : &Vec<Box<dyn IAstNode>>) -> Vec<lsp_types::Diagnostic>{
        // let result = Vec::new();
        let mut analyzer = AstWalker::new();
        analyzer.register_analyzer(Box::new(UnusedVarAnalyzer::new()));

        return analyzer.analyze(ast_nodes);
    }

    fn generate_document_symbols(&self, doc: Rc<RefCell<GoldDocument>>) -> Result<Rc<Vec<DocumentSymbol>>, GoldProjectManagerError>{
        let mut result = Vec::<DocumentSymbol>::new();
        let mut class_symbol = self.find_and_generate_class_symbol(&doc.borrow().ast_nodes);
        for node in &doc.borrow().ast_nodes {
            let symbol = self.generate_symbol_for_node(node.as_ref());
            if symbol.is_some(){
                match &mut class_symbol {
                    Some(class_sym) => class_sym.children.as_mut().unwrap().push(symbol.unwrap()),
                    None => result.push(symbol.unwrap())
                };
            }
        }
        if class_symbol.is_some(){result.push(class_symbol.unwrap())}
        doc.borrow_mut().symbols = Some(Rc::new(result));
        return Ok(doc.borrow().symbols.as_ref().unwrap().clone());
    }

    fn generate_document_diagnostic_report(&self, doc: Rc<RefCell<GoldDocument>>)
    -> Result<Rc<RelatedFullDocumentDiagnosticReport>, GoldProjectManagerError>{
        let mut diagnostics = self.get_diagnostics(doc)?;
        return Ok(Rc::new(RelatedFullDocumentDiagnosticReport{
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport{
                result_id: None,
                items: diagnostics,
            },
        }))
    }

    fn get_diagnostics(&self, doc: Rc<RefCell<GoldDocument>>) -> Result<Vec<Diagnostic>, GoldProjectManagerError>{
        let mut result: Vec<Diagnostic> = doc.borrow().parser_diagnostics.iter()
            .map(|gold_error| {
                Diagnostic::new(
                    gold_error.get_range().as_lsp_type_range(),
                    Some(DiagnosticSeverity::ERROR), 
                    None, 
                    Some("gold".to_string()), 
                    gold_error.get_msg(), 
                    None, 
                    None)
            }).collect();
        let analyzer_diags = self.get_analyzer_diagnostics(doc)?;
        analyzer_diags.iter().for_each(|d|{result.push(d.clone())});
        return Ok(result);
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