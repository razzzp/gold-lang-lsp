#![allow(deprecated)]
use std::{sync::{Arc, Mutex}, ops::Deref};

use lsp_types::{DocumentSymbol, SymbolKind};

use crate::{utils::{OptionString, IRange}, parser::ast::{IAstNode, AstConstantDeclaration, AstTypeDeclaration, AstGlobalVariableDeclaration, AstProcedure, AstFunction, AstClass, AstModule}};

use super::symbol_table::{ISymbolTable, SymbolType, SymbolInfo};

pub struct DocumentSymbolGenerator{

}
impl DocumentSymbolGenerator{
    pub fn new() -> DocumentSymbolGenerator{
        return DocumentSymbolGenerator {  }
    }

    pub fn generate_symbols(&self, symbol_table: Arc<Mutex<dyn ISymbolTable>>)-> Vec<DocumentSymbol> {
        let symbol_table = symbol_table.lock().unwrap();
        let mut result = Vec::<DocumentSymbol>::new();
        let mut class_symbol = self.find_and_generate_class_symbol(&symbol_table);
        let mut class_children = match class_symbol.as_mut(){
            Some(s) => s.children.as_mut(),
            _=> None
        };
        for symbol in symbol_table.iter_symbols(){
            let symbol = self.generate_doc_symbol(symbol);
            if symbol.is_none() {continue;}

            match &mut class_children {
                Some(children) => children.push(symbol.unwrap()),
                None => result.push(symbol.unwrap())
            };
        }
        if class_symbol.is_some(){result.push(class_symbol.unwrap())}
        return result;
    }

    fn generate_doc_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        let result : Option<DocumentSymbol>;
        result = match symbol.sym_type {
            SymbolType::Constant => self.generate_constant_symbol(symbol),
            SymbolType::Field => self.generate_field_symbol(symbol),
            SymbolType::Type => self.generate_type_declaration_symbol(symbol),
            SymbolType::Func => self.generate_func_symbol(symbol),
            SymbolType::Proc => self.generate_proc_symbol(symbol),
            _=> None,
        };
        return result;
    }

    fn generate_constant_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
            name: symbol.id.clone(), 
            detail: symbol.type_str.unwrap_clone(), 
            kind: SymbolKind::CONSTANT, 
            range: symbol.range.as_lsp_type_range(), 
            selection_range: symbol.selection_range.as_lsp_type_range(), 
            tags: None,
            deprecated: None,
            children: None
        })
    }

    fn generate_type_declaration_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
            name: symbol.id.clone(), 
            detail: None, 
            kind: SymbolKind::PROPERTY, 
            range: symbol.range.as_lsp_type_range(), 
            selection_range: symbol.selection_range.as_lsp_type_range(), 
            tags: None,
            deprecated: None,
            children: None
        })
    }

    fn generate_field_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
            name: symbol.id.clone(), 
            detail: symbol.type_str.unwrap_clone(), 
            kind: SymbolKind::FIELD, 
            range: symbol.range.as_lsp_type_range(), 
            selection_range: symbol.selection_range.as_lsp_type_range(), 
            tags: None,
            deprecated: None,
            children: None
        })
    }

    fn generate_proc_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
                name: symbol.id.clone(), 
                detail: None, 
                kind: SymbolKind::METHOD, 
                range: symbol.range.as_lsp_type_range(), 
                selection_range: symbol.selection_range.as_lsp_type_range(), 
                tags: None,
                deprecated: None,
                children: None
        })
    }

    fn generate_func_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
            name: symbol.id.clone(), 
            detail: symbol.type_str.unwrap_clone(), 
            kind: SymbolKind::FUNCTION, 
            range: symbol.range.as_lsp_type_range(), 
            selection_range: symbol.selection_range.as_lsp_type_range(), 
            tags: None,
            deprecated: None,
            children: None
        })
    }

    fn find_and_generate_class_symbol(&self, symbol_table: &dyn Deref<Target = dyn ISymbolTable>) -> Option<DocumentSymbol>{
        let mut result: Option<DocumentSymbol> = None;
        for symbol in symbol_table.iter_symbols() {
            match symbol.sym_type{
                SymbolType::Class=> {
                    let detail = match symbol.parent.as_ref() {
                        Some(s) => Some(s.clone()),
                        _=> None
                    };
                    result = Some(DocumentSymbol { 
                        name: symbol.id.clone(), 
                        detail,
                        kind: SymbolKind::CLASS, 
                        range: symbol.range.as_lsp_type_range(), 
                        selection_range: symbol.selection_range.as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                },
                SymbolType::Module=> {
                    result = Some(DocumentSymbol { 
                        name: symbol.id.clone(), 
                        detail: None,
                        kind: SymbolKind::MODULE, 
                        range: symbol.range.as_lsp_type_range(), 
                        selection_range: symbol.selection_range.as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                }
                _ => continue
            }
        }
        return result;
    }
}


pub struct DocumentSymbolGeneratorFromAst{

}
impl DocumentSymbolGeneratorFromAst{
    pub fn new() -> DocumentSymbolGeneratorFromAst{
        return DocumentSymbolGeneratorFromAst {  }
    }

    pub fn generate_symbols(&self, ast: &dyn IAstNode)-> Vec<DocumentSymbol> {
        let mut result = Vec::<DocumentSymbol>::new();
        let mut class_symbol = self.find_and_generate_class_symbol(ast);
        for node in ast.get_children_ref().unwrap_or_default().iter() {
            let symbol = self.generate_symbol_for_node(node.as_ast_node());
            if symbol.is_some(){
                match &mut class_symbol {
                    Some(class_sym) => class_sym.children.as_mut().unwrap().push(symbol.unwrap()),
                    None => result.push(symbol.unwrap())
                };
            }
        }
        if class_symbol.is_some(){result.push(class_symbol.unwrap())}
        return result;
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
                    name: n.identifier.get_value_as_str().to_string(), 
                    detail: Some(n.value_token.get_value_as_str().to_string()), 
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
                    name: n.identifier.get_value_as_str().to_string(), 
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
                    name: n.identifier.get_value_as_str().to_string(), 
                    detail: Some(n.type_node.get_identifier().to_string()), 
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
                    name: n.identifier.get_identifier().to_string(), 
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
                    name: n.identifier.get_identifier().to_string(), 
                    detail: Some(n.return_type.get_identifier().to_string()), 
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

    fn find_and_generate_class_symbol(&self, ast: &dyn IAstNode) -> Option<DocumentSymbol>{
        let mut result: Option<DocumentSymbol> = None;
        for ast_node in ast.get_children_ref().unwrap_or_default().iter() {
            match ast_node.as_any().downcast_ref::<AstClass>(){
                Some(n) => {
                    let parent_class_name = match &n.parent_class{
                        Some(t) => Some(t.get_value_as_str()),
                        _=>None
                    };
                    result = Some(DocumentSymbol { 
                        name: n.identifier.get_value().to_string(), 
                        detail: parent_class_name.map(|name|{name.to_string()}), 
                        kind: SymbolKind::CLASS, 
                        range: n.get_range().as_lsp_type_range(), 
                        selection_range: n.get_range().as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                }
                None => ()
            }
            match ast_node.as_any().downcast_ref::<AstModule>(){
                Some(n) => {
                    result = Some(DocumentSymbol { 
                        name: n.get_identifier().to_string(), 
                        detail: None, 
                        kind: SymbolKind::MODULE, 
                        range: n.get_range().as_lsp_type_range(), 
                        selection_range: n.get_range().as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                }
                None => ()
            }
        }
        return result;
    }
}