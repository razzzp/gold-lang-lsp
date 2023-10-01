use lsp_types::{DocumentSymbol, SymbolKind};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstTypeDeclaration, AstProcedure, AstFunction, AstTypeBasic, AstGlobalVariableDeclaration}, analyzers::IVisitor, utils::{DynamicChild, Range, IRange, OptionString}, lexer::tokens::TokenType};
use core::fmt::Debug;
use std::{collections::HashMap, sync::{Mutex, Arc}, ops::{DerefMut, Deref}};
use crate::utils::{OptionExt};
use super::ProjectManager;

#[derive(Debug,Default)]
pub enum SymbolType{
    #[default]
    Class,
    Field,
    Type,
    Proc,
    Func,
    Variable,
    Constant
}

#[derive(Debug,Default)]
pub struct SymbolInfo {
    pub id: String,
    pub sym_type: SymbolType,
    pub eval_type: Option<String>,
    pub owner: Option<String>,
    pub in_class: Option<String>,
    pub parent: Option<String>,
    pub range: Range,
    pub selection_range: Range
}
impl SymbolInfo{
    pub fn new(id:String, sym_type: SymbolType)->SymbolInfo{
        return SymbolInfo{
            id,
            sym_type,
            ..Default::default()
        }
    }
}

pub trait ISymbolGenerator : IVisitor{
    fn take_symbol_table(&mut self) -> Option<Arc<Mutex<dyn ISymbolTable>>>;
}

pub trait ISymbolTable: Debug + Send{
    fn get_symbol_info(&self, id: String) -> Option<&SymbolInfo>;
    fn insert_symbol_info(& mut self, id : String, info: SymbolInfo) -> & SymbolInfo;
    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a SymbolInfo> +'a>;
}

#[derive(Debug)]
pub struct SymbolTable{
    // allows order to be preserved and access through key
    symbols_list: Vec<SymbolInfo>,
    hash_map : HashMap<String, usize>,
    // string_table: HashMap<String, Arc<Mutex<String>>>,
}
impl SymbolTable{
    pub fn new()-> SymbolTable{
        SymbolTable { 
            symbols_list: Vec::new(),
            hash_map: HashMap::new(),
            // string_table: HashMap::new()
        }
    }
}
impl ISymbolTable for SymbolTable {
    fn get_symbol_info(&self, id: String) -> Option<&SymbolInfo> {
        let idx = match self.hash_map.get(&id) {
            Some(i) => i,
            _=> return None,
        };
        return self.symbols_list.get(*idx)
    }

    fn insert_symbol_info(&mut self, id : String, info: SymbolInfo) -> &SymbolInfo {
        let idx = self.symbols_list.len();
        self.symbols_list.push(info);
        self.hash_map.insert(id, idx);
        return self.symbols_list.get(idx).unwrap()
    }

    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a SymbolInfo> + 'a > {
        let iter = self.symbols_list.iter();
        return Box::new(iter);
    }
}


#[derive(Debug)]
pub struct SymbolGenerator<'a> {
    symbol_table : Option<SymbolTable>,
    project_manager : &'a mut ProjectManager
}

// ensure generator is not used after this!
impl<'a> ISymbolGenerator for SymbolGenerator<'a>{
    fn take_symbol_table(&mut self) -> Option<Arc<Mutex<dyn ISymbolTable>>> {
        match self.symbol_table.take(){
            Some(t) => return Some(Arc::new(Mutex::new(t))),
            _=> return None
        }
    }
}

impl<'a> SymbolGenerator<'a> {
    pub fn new(project_manager: &'a mut ProjectManager) -> SymbolGenerator{
        return SymbolGenerator {  
            symbol_table: Some(SymbolTable::new()),
            project_manager
        }
    }

    fn handle_class(&mut self, node: &AstClass){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Class);
        sym_info.parent = Some(node.parent_class.clone());
        self.symbol_table.unwrap_mut().insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_constant_decl(&mut self, node: &AstConstantDeclaration){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Constant);
        sym_info.eval_type = match &node.value.token_type{
            TokenType::StringLiteral => Some("string".to_string()),
            TokenType::NumericLiteral => Some("numeric".to_string()),
            _=> Some("unknown".to_string())
        };
        self.symbol_table.unwrap_mut().insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_type_decl(&mut self, node: &AstTypeDeclaration){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Type);
        match node.type_node.as_any().downcast_ref::<AstTypeBasic>(){
            Some(n) => {sym_info.eval_type = Some(n.get_identifier())},
            _=> ()
        }
        self.symbol_table.unwrap_mut().insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_proc_decl(&mut self, node: &AstProcedure){
        let sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Proc);
        self.symbol_table.unwrap_mut().insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_func_decl(&mut self, node: &AstFunction){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Func);
        match node.return_type.as_any().downcast_ref::<AstTypeBasic>(){
            Some(n) => {sym_info.eval_type = Some(n.get_identifier())},
            _=> ()
        }
        self.symbol_table.unwrap_mut().insert_symbol_info(node.get_identifier(), sym_info);
    }
}
impl<'a> IVisitor for SymbolGenerator<'a>{
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>) {
        match node.data.as_any().downcast_ref::<AstClass>(){
            Some(n) => self.handle_class(n),
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstConstantDeclaration>(){
            Some(n) => self.handle_constant_decl(n),
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstTypeDeclaration>(){
            Some(n) => self.handle_type_decl(n),
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstProcedure>(){
            Some(n) => self.handle_proc_decl(n),
            _=> ()
        }
        match node.data.as_any().downcast_ref::<AstFunction>(){
            Some(n) => self.handle_func_decl(n),
            _=> ()
        }
    }

    fn notify_end(&mut self) {
        ()
    }

    fn as_visitor(&mut self) -> &dyn IVisitor {
        self
    }
}



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
            SymbolType::Field => self.generate_global_var_decl_symbol(symbol),
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
            detail: symbol.eval_type.unwrap_clone(), 
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

    fn generate_global_var_decl_symbol(&self, symbol: &SymbolInfo)-> Option<DocumentSymbol>{
        Some(DocumentSymbol { 
            name: symbol.id.clone(), 
            detail: symbol.eval_type.unwrap_clone(), 
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
            detail: symbol.eval_type.unwrap_clone(), 
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
                }
                _ => continue
            }
        }
        return result;
    }
}