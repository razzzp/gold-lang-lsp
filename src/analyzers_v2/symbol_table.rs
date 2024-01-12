
use std::collections::HashMap;

use core::fmt::Debug;

use std::collections::HashSet;
use std::sync::Mutex;

use std::sync::Arc;

use crate::utils::OptionExt;
use crate::utils::OptionString;
use crate::utils::Range;

use crate::analyzers_v2::annotated_node::EvalType;

#[derive(Debug, Default, Clone)]
pub enum SymbolType {
    #[default]
    Class,
    Field,
    Type,
    Proc,
    Func,
    Variable,
    Constant,
    Module
}

#[derive(Debug, Default, Clone)]
pub struct SymbolInfo {
    pub id: String,
    pub sym_type: SymbolType,
    pub type_str: Option<String>,
    pub eval_type: Option<EvalType>,
    pub owner: Option<String>,
    pub in_class: Option<String>,
    pub parent: Option<String>,
    pub range: Range,
    pub selection_range: Range,
}

impl SymbolInfo {
    pub fn new(id: String, sym_type: SymbolType) -> SymbolInfo {
        return SymbolInfo {
            id,
            sym_type,
            ..Default::default()
        };
    }
}

pub trait ISymbolTableGenerator {
    fn take_symbol_table(&mut self) -> Option<Arc<Mutex<dyn ISymbolTable>>>;
}

pub trait ISymbolTable: Debug + Send {
    fn get_symbol_info(&mut self, id: &str) -> Option<Arc<SymbolInfo>>;
    fn search_symbol_info_wparent(&self, id: &str) -> Option<(String, Arc<SymbolInfo>)>;
    fn search_symbol_info(&self, id: &str) -> Option<(String, Arc<SymbolInfo>)>;
    fn insert_symbol_info(&mut self, id: &str, info: SymbolInfo) -> &SymbolInfo;
    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Arc<SymbolInfo>> + 'a>;
    fn get_parent_symbol_table(&self) -> Option<Arc<Mutex<dyn ISymbolTable>>>;
    fn identifier_exists(&mut self, id: &str) -> bool;
    fn as_isymbol_table_mut(&mut self) -> &mut dyn ISymbolTable;
    fn add_uses_entity(&mut self, entity_name: &str);
    fn get_list_of_uses(&self) -> Vec<String>;
    // for debug only
    fn print_all_symbols(&self);
    fn get_class(&self) -> Option<String>;
    fn search_all_symbol_info(&self, id: &str) -> Vec<(String, Arc<SymbolInfo>)>;
    fn collect_unique_symbols_w_parents(&self) -> Vec<Arc<SymbolInfo>>;
}

#[derive(Debug)]
pub struct SymbolTable {
    pub(crate) parent_symbol_table: Option<Arc<Mutex<dyn ISymbolTable>>>,
    // allows order to be preserved and access through key
    pub(crate) symbols_list: Vec<Arc<SymbolInfo>>,
    pub(crate) hash_map: HashMap<String, usize>,
    pub(crate) uses_entities: Vec<String>,
    // populated with symbol tables from the uses_entities
    pub(crate) uses_symbol_table: Vec<Arc<Mutex<dyn ISymbolTable>>>,
    // string_table: HashMap<String, Arc<Mutex<String>>>,
    pub for_class_or_module: Option<String>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            parent_symbol_table: None,
            symbols_list: Vec::new(),
            hash_map: HashMap::new(),
            uses_symbol_table: Vec::new(),
            uses_entities: Vec::new(),
            // string_table: HashMap::new(),
            for_class_or_module: None,
        }
    }

    pub fn set_parent_symbol_table(&mut self, symbol_table: Arc<Mutex<dyn ISymbolTable>>) {
        self.parent_symbol_table = Some(symbol_table)
    }

    pub fn add_uses_symbol_table(&mut self, symbol_table: Arc<Mutex<dyn ISymbolTable>>) {
        self.uses_symbol_table.push(symbol_table);
    }
}

impl ISymbolTable for SymbolTable {
    fn get_symbol_info(&mut self, id: &str) -> Option<Arc<SymbolInfo>> {
        // search cur st first
        let mut result = match self.hash_map.get(&id.to_uppercase()) {
            Some(i) => self.symbols_list.get(*i).cloned(),
            _ => None,
        };
        // if not found search parent st
        if result.is_none() && self.parent_symbol_table.is_some() {
            let parent_st = self.parent_symbol_table.unwrap_ref();
            // don't need to search uses of parent
            result = parent_st.lock().unwrap().get_symbol_info(id);
        }
        return result;
    }

    fn search_symbol_info_wparent(&self, id: &str) -> Option<(String, Arc<SymbolInfo>)> {
        // search cur st first
        let mut result = match self.hash_map.get(&id.to_uppercase()) {
            Some(i) => {
                return Some((
                    self.for_class_or_module.unwrap_clone_or_empty_string(),
                    self.symbols_list.get(*i).cloned().unwrap(),
                ))
            }
            _ => None,
        };
        // if not found search parent st
        if result.is_none() && self.parent_symbol_table.is_some() {
            let parent_st = self.parent_symbol_table.unwrap_ref();
            // don't need to search uses of parent
            result = parent_st.lock().unwrap().search_symbol_info_wparent(id);
        }
        return result;
    }

    /// returns list of syminfo matching id, searches through parent
    fn search_all_symbol_info(&self, id: &str) -> Vec<(String, Arc<SymbolInfo>)> {
        // search cur st first
        let mut result = Vec::new();
        match self.hash_map.get(&id.to_uppercase()) {
            Some(i) => {
                result.push((
                    self.for_class_or_module.unwrap_clone_or_empty_string(),
                    self.symbols_list.get(*i).cloned().unwrap(),
                ));
            }
            _ => (),
        };

        match self.get_parent_symbol_table(){
            Some(st) => {
                result.extend(st.lock().unwrap().search_all_symbol_info(id))
            }
            _=>()
        }
        return result;
    }

    fn get_list_of_uses(&self) -> Vec<String> {
        self.uses_entities.iter().map(|s| s.clone()).collect()
    }

    fn insert_symbol_info(&mut self, id: &str, symbol_info: SymbolInfo) -> &SymbolInfo {
        let idx = self.symbols_list.len();
        self.symbols_list.push(Arc::new(symbol_info));
        // uppercase keys
        self.hash_map.insert(id.to_uppercase(), idx);
        return self.symbols_list.get(idx).unwrap();
    }

    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Arc<SymbolInfo>> + 'a> {
        let iter = self.symbols_list.iter();
        return Box::new(iter);
    }

    fn get_parent_symbol_table(&self) -> Option<Arc<Mutex<dyn ISymbolTable>>> {
        self.parent_symbol_table.clone()
    }

    // for debug only
    fn print_all_symbols(&self) {
        match self.parent_symbol_table.as_ref() {
            Some(st) => st.lock().unwrap().print_all_symbols(),
            _ => (),
        }
        self.iter_symbols().for_each(|s| eprintln!("{}", s.id))
    }

    fn identifier_exists(&mut self, id: &str) -> bool {
        return self.get_symbol_info(id).is_some();
    }

    fn as_isymbol_table_mut(&mut self) -> &mut dyn ISymbolTable {
        self
    }
    fn add_uses_entity(&mut self, entity_name: &str) {
        self.uses_entities.push(entity_name.to_string())
    }

    fn get_class(&self) -> Option<String> {
        self.for_class_or_module.clone()
    }

    fn collect_unique_symbols_w_parents(&self) -> Vec<Arc<SymbolInfo>> {
        let mut result = Vec::new();
        let mut seen = HashSet::new();
        result.extend(
            self.iter_symbols().map(|s|{
                seen.insert(&s.id);
                s.clone()
            })
        );
        match &self.parent_symbol_table{
            Some(parent_st) => {
                parent_st.lock().unwrap()
                    .collect_unique_symbols_w_parents()
                    .into_iter()
                    .filter(|s|{
                        return !seen.contains(&s.id);
                    })
                    .for_each(|s|{
                        result.push(s.clone())
                    })
            }
            _=> ()
        }
        return result;
    }

    fn search_symbol_info(&self, id: &str) -> Option<(String, Arc<SymbolInfo>)> {
        // search cur st first
        match self.hash_map.get(&id.to_uppercase()) {
            Some(i) => {
                return Some((
                    self.for_class_or_module.unwrap_clone_or_empty_string(),
                    self.symbols_list.get(*i).cloned().unwrap(),
                ))
            }
            _ => return None,
        }
    }
}
