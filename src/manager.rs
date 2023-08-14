use std::{collections::HashMap, error::Error, fs::File, io::Read, ops::Deref, rc::Rc, alloc::GlobalAlloc};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport};

use crate::{ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{GoldDocumentError, parse_gold}, lexer::GoldLexer, utils::IRange};


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

#[derive(Debug)]
pub struct GoldDocumentManager{
    documents_map: HashMap<String, Rc<GoldDocument>>,
    open_documents_map: HashMap<String, Rc<GoldDocument>>
}

#[derive(Debug)]
pub struct GoldDocumentManagerError{
    pub msg: String,
    pub error_code: ErrorCode,
}

impl GoldDocumentManager{
    pub fn new() -> GoldDocumentManager{
        GoldDocumentManager{
            documents_map: HashMap::new(),
            open_documents_map: HashMap::new()
        }
    }

    /// gets parsed document from map, 
    ///  or parses it and adds to map if
    ///  not yet parsed
    pub fn get_document(&mut self, file_path: &str) -> Result<Rc<GoldDocument>, GoldDocumentManagerError>{
        let doc =  self.documents_map.get(file_path);
        if doc.is_some() {
            // TODO check whether document has changed
            return Ok(doc.unwrap().clone());
        } else {
            let new_doc = self.parse_document(file_path)?;
            self.documents_map.insert(file_path.to_string(), Rc::new(new_doc));
            Ok(self.documents_map.get(file_path).unwrap().clone())
        }
    }

    pub fn get_open_document(&mut self, file_path: &str, full_file_content: &String) ->Result<Rc<GoldDocument>, GoldDocumentManagerError>{
        // TODO check version 
        let new_doc = self.parse_file(full_file_content)?;
        self.open_documents_map.insert(file_path.to_string(), Rc::new(new_doc));
        Ok(self.open_documents_map.get(file_path).unwrap().clone())
    }

    fn parse_document(&self, file_path: &str) -> Result<GoldDocument, GoldDocumentManagerError>{
        // open file
        let mut file = match File::open(file_path){
            Ok(f) => f,
            Err(e) => return Err(GoldDocumentManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        let mut contents = String::new();
        match file.read_to_string(&mut contents){
            Ok(s)=> (),
            Err(e) => return Err(GoldDocumentManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        return self.parse_file(&contents)
    }

    fn parse_file(&self, full_file_content: &String) -> Result<GoldDocument, GoldDocumentManagerError> {
        // lexing
        let mut lexer = GoldLexer::new();
        let (tokens, lexer_errors) = lexer.lex(&full_file_content);
        // parse
        let (ast_nodes, doc_errors) = parse_gold(&tokens);
        let symbols = self.generate_document_symbols(ast_nodes.1.as_ref())?;
        let diagnostic_report = self.generate_document_diagnostic_report(&doc_errors)?;

        return Ok(GoldDocument { 
            symbols: symbols, 
            ast_nodes: ast_nodes.1, 
            errors: doc_errors,
            diagnostic_report
        })
    }

    fn generate_document_symbols(&self, ast_nodes: &Vec<Box<dyn IAstNode>>) -> Result<Vec<DocumentSymbol>, GoldDocumentManagerError>{
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
    -> Result<RelatedFullDocumentDiagnosticReport, GoldDocumentManagerError>{
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
    use super::GoldDocumentManager;


    #[test]
    fn test_gold_document_manager(){
        let mut doc_manager = GoldDocumentManager::new();
        let doc = doc_manager.get_document("test/aTestClass.god").unwrap();
        // println!("{:#?}",doc);
    }
}