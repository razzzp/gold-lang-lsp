use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstTypeDeclaration, AstProcedure, AstFunction, AstTypeBasic}, analyzers::IVisitor, utils::DynamicChild, lexer::tokens::TokenType};
use core::fmt::Debug;
use std::{collections::HashMap, sync::{Mutex, Arc}};

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
    pub parent: Option<String>
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

pub trait ISymbolTable: Debug + Send{
    fn get_symbol_info(&self, id: String) -> Option<&SymbolInfo>;
    fn insert_symbol_info(&mut self, id : String, info: SymbolInfo) -> &SymbolInfo;
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
}


#[derive(Debug)]
pub struct SymbolGenerator {
    symbol_table : Box<dyn ISymbolTable>,
    project_manager : Arc<Mutex<ProjectManager>>
}

impl SymbolGenerator {
    pub fn new(project_manager: Arc<Mutex<ProjectManager>>) -> SymbolGenerator{
        return SymbolGenerator {  
            symbol_table: Box::new(SymbolTable::new()),
            project_manager
        }
    }

    fn handle_class(&mut self, node: &AstClass){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Class);
        sym_info.parent = Some(node.parent_class.clone());
        self.symbol_table.insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_constant_decl(&mut self, node: &AstConstantDeclaration){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Constant);
        sym_info.eval_type = match &node.value.token_type{
            TokenType::StringLiteral => Some("string".to_string()),
            TokenType::NumericLiteral => Some("numeric".to_string()),
            _=> Some("unknown".to_string())
        };
        self.symbol_table.insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_type_decl(&mut self, node: &AstTypeDeclaration){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Type);
        match node.type_node.as_any().downcast_ref::<AstTypeBasic>(){
            Some(n) => {sym_info.eval_type = Some(n.get_identifier())},
            _=> ()
        }
        self.symbol_table.insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_proc_decl(&mut self, node: &AstProcedure){
        let sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Proc);
        self.symbol_table.insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_func_decl(&mut self, node: &AstFunction){
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Type);
        match node.return_type.as_any().downcast_ref::<AstTypeBasic>(){
            Some(n) => {sym_info.eval_type = Some(n.get_identifier())},
            _=> ()
        }
        self.symbol_table.insert_symbol_info(node.get_identifier(), sym_info);
    }
}
impl IVisitor for SymbolGenerator{
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

