use std::{collections::HashMap, error::Error, fs::File, io::Read, ops::Deref, rc::Rc};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind};

use crate::{ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{GoldDocumentError, parse_gold}, lexer::GoldLexer, utils::IRange};


// pub trait IDocument {
//     fn get_symbols(&self)-> Vec<&'static DocumentSymbol>;
// }
pub struct GoldDocument{
    symbols: Vec<DocumentSymbol>,
    ast_nodes: Vec<Box<dyn IAstNode>>,
    errors: Vec<GoldDocumentError>
}
impl GoldDocument{
    pub fn get_symbols(&self)-> Vec<DocumentSymbol>{
        self.symbols.iter().map(|s| s.clone()).collect()
    }
}

pub struct GoldDocumentManager{
    document_map: HashMap<String, Rc<GoldDocument>>,
}

pub struct GoldDocumentManagerError{
    pub msg: String,
    pub error_code: ErrorCode,
}

impl GoldDocumentManager{
    pub fn new() -> GoldDocumentManager{
        GoldDocumentManager{
            document_map: HashMap::<>::new()
        }
    }

    /// gets parsed document from map, 
    ///  or parses it and adds to map if
    ///  not yet parsed
    pub fn get_document(&mut self, uri: &str) -> Result<Rc<GoldDocument>, GoldDocumentManagerError>{
        let doc =  self.document_map.get(uri);
        if doc.is_some() {
            return Ok(doc.unwrap().clone());

        } else {
            let new_doc = self.parse_document(uri)?;
            self.document_map.insert(uri.to_string(), Rc::new(new_doc));
            Ok(self.document_map.get(uri).unwrap().clone())
        }
    }

    fn parse_document(&self, uri: &str) -> Result<GoldDocument, GoldDocumentManagerError>{
        // open file
        let mut file = match File::open("foo.txt"){
            Ok(f) => f,
            Err(e) => return Err(GoldDocumentManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        let mut contents = String::new();
        match file.read_to_string(&mut contents){
            Ok(s)=> (),
            Err(e) => return Err(GoldDocumentManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };

        // lexing
        let mut lexer = GoldLexer::new();
        let (tokens, lexer_errors) = lexer.lex(&contents);
        // parse
        let (ast_nodes, doc_errors) = parse_gold(&tokens);
        let symbols = self.generate_document_symbols(ast_nodes.1.as_ref())?;

        return Ok(GoldDocument { symbols: symbols, ast_nodes: ast_nodes.1, errors: doc_errors })
    }

    fn generate_document_symbols(&self, ast_nodes: &Vec<Box<dyn IAstNode>>) -> Result<Vec<DocumentSymbol>, GoldDocumentManagerError>{
        let mut result = Vec::<DocumentSymbol>::new();
        let class_symbol = self.find_and_generate_class_symbol(ast_nodes);
        for node in ast_nodes {
            let symbol = self.generate_symbol_for_node(node.as_ref());
            if symbol.is_some(){
                result.push(symbol.unwrap());
            }
        }
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
                        children: None
                    });
                    break;
                }
                None => continue
            }
        }
        return result;
    }
}
