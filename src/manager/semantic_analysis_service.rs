use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstTypeDeclaration, AstProcedure, AstFunction, AstTypeBasic, AstGlobalVariableDeclaration, AstUses, AstParameterDeclaration, AstLocalVariableDeclaration}, analyzers::{IVisitor, AnalyzerDiagnostic}, utils::{DynamicChild, Range, IRange, OptionString, ILogger, IDiagnosticCollector}, lexer::tokens::TokenType};
use core::fmt::Debug;
use std::{collections::{HashMap, HashSet}, sync::{Mutex, Arc, RwLock, RwLockWriteGuard}, ops::{DerefMut, Deref}, result, f32::consts::E};
use crate::utils::{OptionExt};
use super::{ProjectManager, SymbolTableRequestOptions, annotated_node::{AnnotatedNode, EvalType, TypeInfo, Location, NativeType}, data_structs::{Document, ProjectManagerError}};

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
    pub type_str: Option<String>,
    pub eval_type: Option<EvalType>,
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

pub trait ISymbolTableGenerator{
    fn take_symbol_table(&mut self) -> Option<Arc<Mutex<dyn ISymbolTable>>>;
}

pub trait ISymbolTable: Debug + Send{
    fn get_symbol_info(&mut self, id: &String, search_uses: bool) -> Option<Arc<SymbolInfo>>;
    fn insert_symbol_info(& mut self, id : String, info: SymbolInfo) -> & SymbolInfo;
    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Arc<SymbolInfo>> + 'a >;
    fn get_parent_symbol_table(&self) -> Option<Arc<Mutex<dyn ISymbolTable>>>;
    fn identifier_exists(&mut self, id : &String, search_uses: bool) -> bool;
    // for debug only
    fn print_all_symbols(&self);
}

#[derive(Debug)]
pub struct SymbolTable{
    parent_symbol_table: Option<Arc<Mutex<dyn ISymbolTable>>>,
    // allows order to be preserved and access through key
    symbols_list: Vec<Arc<SymbolInfo>>,
    hash_map : HashMap<String, usize>,
    uses_entities: Vec<String>,
    // populated with symbol tables from the uses_entities
    uses_symbol_table: Vec<Arc<Mutex<dyn ISymbolTable>>>,
    // string_table: HashMap<String, Arc<Mutex<String>>>,
}
impl SymbolTable{
    pub fn new()-> SymbolTable{
        SymbolTable { 
            parent_symbol_table: None,
            symbols_list: Vec::new(),
            hash_map: HashMap::new(),
            uses_symbol_table: Vec::new(),
            uses_entities: Vec::new()
            // string_table: HashMap::new()
        }
    }

    pub fn set_parent_symbol_table(&mut self, symbol_table: Arc<Mutex<dyn ISymbolTable>>) {
        self.parent_symbol_table = Some(symbol_table)
    }

    pub fn add_uses_symbol_table(&mut self, symbol_table: Arc<Mutex<dyn ISymbolTable>>) {
        self.uses_symbol_table.push(symbol_table); 
    }

    fn iter_uses_symbol_table<'a>(&'a mut self) -> Vec<&'a Arc<Mutex<dyn ISymbolTable>>>{
        self.uses_symbol_table.iter().collect()
    }
    pub fn add_uses_entity(&mut self, entity_name:&String){
        self.uses_entities.push(entity_name.clone())
    }
}
impl ISymbolTable for SymbolTable {
    fn get_symbol_info(&mut self, id: &String, search_uses: bool) -> Option<Arc<SymbolInfo>> {
        let idx = match self.hash_map.get(id) {
            Some(i) => i,
            _=> return None,
        };
        let mut result =  self.symbols_list.get(*idx).cloned();
        if result.is_none() && self.parent_symbol_table.is_some(){
            let parent_st= self.parent_symbol_table.unwrap_ref();
            // don't need to search uses of parent
            result = parent_st.lock().unwrap().get_symbol_info(id, false);
        }
        if search_uses && result.is_none(){
            for st in self.iter_uses_symbol_table(){
                // don't need to search uses of uses
                result = st.lock().unwrap().get_symbol_info(&id, false);
                if result.is_some() {break}
            }
        }
        return result;
    }

    fn insert_symbol_info(&mut self, id : String, symbol_info: SymbolInfo) -> &SymbolInfo {
        let idx = self.symbols_list.len();
        self.symbols_list.push(Arc::new(symbol_info));
        self.hash_map.insert(id, idx);
        return self.symbols_list.get(idx).unwrap()
    }

    fn iter_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Arc<SymbolInfo>> + 'a > {
        let iter = self.symbols_list.iter();
        return Box::new(iter);
    }

    fn get_parent_symbol_table(&self) -> Option<Arc<Mutex<dyn ISymbolTable>>> {
        self.parent_symbol_table.clone()
    }

    // for debug only
    fn print_all_symbols(&self){
        match self.parent_symbol_table.as_ref(){
            Some(st) => {st.lock().unwrap().print_all_symbols()}
            _=> ()
        }
        self.iter_symbols().for_each(|s|{
            eprintln!("{}", s.id)
        })
    }

    fn identifier_exists(&mut self, id : &String, search_uses: bool) -> bool {
        return self.get_symbol_info(id, search_uses).is_some();
    }
}


#[derive(Debug)]
pub struct SemanticAnalysisService<'a> {
    root_symbol_table : Option<SymbolTable>,
    symbol_table_stack: Vec<SymbolTable>,
    project_manager : &'a mut ProjectManager,
    logger: Arc<Mutex<dyn ILogger>>,
    diag_collector: Box<dyn IDiagnosticCollector<AnalyzerDiagnostic>>,
    already_seen_classes: HashSet<String>,
    cur_method_node : Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
}

// ensure generator is not used after this!
impl<'a> ISymbolTableGenerator for SemanticAnalysisService<'a>{
    fn take_symbol_table(&mut self) -> Option<Arc<Mutex<dyn ISymbolTable>>> {
        match self.root_symbol_table.take(){
            Some(t) => return Some(Arc::new(Mutex::new(t))),
            _=> return None
        }
    }
}

impl<'a> SemanticAnalysisService<'a> {
    pub fn new(
        project_manager: &'a mut ProjectManager, 
        logger: Arc<Mutex<dyn ILogger>>, 
        diag_collector: Box<dyn IDiagnosticCollector<AnalyzerDiagnostic>>,
        already_seen_classes : Option<HashSet<String>>
) -> SemanticAnalysisService{
        return SemanticAnalysisService {  
            root_symbol_table: Some(SymbolTable::new()),
            symbol_table_stack: Vec::new(),
            project_manager,
            logger: logger,
            diag_collector,
            already_seen_classes: already_seen_classes.unwrap_or_default(),
            cur_method_node:None
        }
    }
    fn get_symbol_table_for_class(&mut self, class: &String) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        if self.already_seen_classes.contains(class){
            return Err(ProjectManagerError::new(format!("{} already locked in request session",class).as_str(), ErrorCode::RequestFailed));
        }
        return self.project_manager.get_symbol_table_for_class(&class, Some(self.already_seen_classes.clone()));
    }

    pub fn analyze_uri(&mut self, uri : &Url) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc: Arc<Mutex<Document>> = self.project_manager.get_parsed_document(uri, true)?;
        return self.analyze(doc);
    }
    
    fn analyze(&mut self, doc: Arc<Mutex<Document>>) -> 
    Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let root_node = doc.lock().unwrap().get_ast().clone();
        let annotated_tree = self.generate_annotated_tree(&root_node);
        self.walk_tree(&annotated_tree);
        let sym_table = Arc::new(Mutex::new(self.root_symbol_table.take().unwrap()));
        doc.lock().unwrap().symbol_table = Some(sym_table.clone());
        doc.lock().unwrap().annotated_ast = Some(annotated_tree.clone());
        return Ok(doc);
    }

    fn generate_annotated_tree<'b>(&self, root_node: &Arc<dyn IAstNode>) -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>{
        let new_annotated_node = Arc::new(RwLock::new(AnnotatedNode::new(root_node, None)));
        let children = match root_node.get_children_arc(){
            Some(c) => c,
            _=> return new_annotated_node
        };
        for child in children {
            let new_child_node = self.generate_annotated_tree(child);
            new_child_node.write().unwrap().parent = Some(Arc::downgrade(&new_annotated_node.clone()));
            new_annotated_node.write().unwrap().children.push(new_child_node.clone())
        }
        return new_annotated_node;
    }

    fn walk_tree(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        self.visit(node);
        for child in &node.read().unwrap().children {
            self.walk_tree(child)
        }
    }

    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let write_lock = node.write().unwrap();
        self.handle_class(&write_lock);
        self.handle_constant_decl(&write_lock);
        self.handle_type_decl(&write_lock);
        self.handle_proc_decl(&write_lock, node);
        self.handle_func_decl(&write_lock, node);
        self.handle_field_decl(&write_lock);
        self.handle_uses(&write_lock);
        self.handle_param_decl(&write_lock);
        self.handle_var_decl(&write_lock);
    }

    fn notify_new_scope(&mut self){
        self.symbol_table_stack.push(SymbolTable::new())
    }

    fn pop_last_scope(&mut self) -> Option<SymbolTable>{
        return self.symbol_table_stack.pop();
    }

    /// Inserts the symbol to the current scope, last in stack/root
    fn get_cur_sym_table(&mut self) -> &mut SymbolTable{
        let cur_st = match self.symbol_table_stack.last_mut(){
            Some(st) => st,
            _=> self.root_symbol_table.unwrap_mut()
        };
        return cur_st;
    }

    fn check_identifier_already_defined(&mut self, id: &String, range: Range) -> bool{
        // do we need to check uses?
        match self.get_cur_sym_table().get_symbol_info(id, false){
            Some(sym)=>{
                self.diag_collector.add_diagnostic(
                    AnalyzerDiagnostic::new(
                        format!("Identifier already defined. In {}", sym.in_class.unwrap_clone_or_empty_string()).as_str(), 
                        range
                    )
                );
                return true;
            }
            _=> {return false;}
        }
    } 

    /// Inserts the symbol to the current scope, last in stack/root
    fn insert_symbol_info(&mut self, id: String, symbol: SymbolInfo) -> &SymbolInfo{
        let cur_st = self.get_cur_sym_table();
        cur_st.insert_symbol_info(id, symbol)
    }

    fn get_eval_type(&mut self, type_node : &Arc<dyn IAstNode>)-> EvalType{
        let mut result = EvalType::Unknown;
        result = match type_node.as_any().downcast_ref::<AstTypeBasic>(){
            Some(t) =>{
                match t.get_identifier().to_uppercase().as_str() {
                    "INT1" | "INT2" | "INT4" | "INT8" => EvalType::Native(NativeType::Int),
                    "NUMERIC" => EvalType::Native(NativeType::Numeric),
                    "STRING" =>  EvalType::Native(NativeType::String),
                    "CSTRING" =>  EvalType::Native(NativeType::CString),
                    _=> EvalType::Unknown
                }
            }
            _=> result,
        };
        return result;
    }

    // fn lock_and_cast<'b,T:IAstNode>(&self, node : Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) -> 
    // Option<(RwLockWriteGuard<'b, AnnotatedNode<dyn IAstNode>>, &'b T)>{
    //     let w_lock = node.write().unwrap();
    //     let downcast = match w_lock.data.as_any().downcast_ref::<T>(){
    //         Some(d) => d,
    //         _=> return None
    //     };
    //     return Some((w_lock, downcast))
    // }

    fn handle_class(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let ori_node = match node.data.as_any().downcast_ref::<AstClass>(){
            Some(node) => node,
            _=> return
        };
        let class_name = ori_node.get_identifier();
        let mut sym_info = SymbolInfo::new(class_name.clone(), SymbolType::Class);
        // TODO there is probably a better place to put this
        self.already_seen_classes.insert(class_name.clone());

        if let Some(parent_class) = &ori_node.parent_class{
            let parent_class_name = parent_class.get_value();
            if parent_class_name == class_name{
                // parent class can't be itself
                self.diag_collector.add_diagnostic(
                    AnalyzerDiagnostic::new("Parent class cannot be itself", parent_class.get_range())
                );
            }
            sym_info.parent = Some(parent_class_name.clone());
            let parent_symbol_table = self.get_symbol_table_for_class(&parent_class_name);
            match parent_symbol_table{
                Ok(st) => {
                    // set parent symbol table
                    self.root_symbol_table.unwrap_mut().set_parent_symbol_table(st);
                },
                Err(e)=> {
                    // parent class not found/cannot be parsed
                    self.diag_collector.add_diagnostic(
                        AnalyzerDiagnostic::new(format!("Parent class not defined; {}", e).as_str(), parent_class.get_range())
                    );
                }
            }
        }
        sym_info.range = ori_node.get_range();
        sym_info.selection_range = ori_node.identifier.get_range();
        self.insert_symbol_info(ori_node.get_identifier(), sym_info);
    }

    fn handle_constant_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstConstantDeclaration>(){
            Some(node) => node,
            _=> return
        };
        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());
        
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Constant);
        sym_info.eval_type = match &node.value.token_type{
            TokenType::StringLiteral => Some(EvalType::Native(NativeType::String)),
            TokenType::NumericLiteral => Some(EvalType::Native(NativeType::Numeric)),
            _=> Some(EvalType::Unknown)
        };
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);
    }

    fn handle_type_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstTypeDeclaration>(){
            Some(node) => node,
            _=> return
        };
        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());
        
        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Type);
        sym_info.type_str = Some(node.type_node.get_identifier());
        // set eval type
        sym_info.eval_type = Some(self.get_eval_type(&node.type_node));
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);

    }

    fn handle_proc_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>, arc_node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node = match node.data.as_any().downcast_ref::<AstProcedure>(){
            Some(node) => node,
            _=> return
        };
        // set sym table to last method
        if let Some(sym_table) = self.pop_last_scope(){
            // should always be Some
            self.cur_method_node.as_ref().unwrap().write().unwrap().symbol_table = Some(Box::new(sym_table))
        }

        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());

        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Proc);
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);

        //set cur method
        self.cur_method_node = Some(arc_node.clone());
        self.notify_new_scope();
    }

    fn handle_func_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>, arc_node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node = match node.data.as_any().downcast_ref::<AstFunction>(){
            Some(node) => node,
            _=> return
        };
        // set sym table to last method
        if let Some(sym_table) = self.pop_last_scope(){
            // should always be Some
            self.cur_method_node.as_ref().unwrap().write().unwrap().symbol_table = Some(Box::new(sym_table))
        }

        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());

        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Func);
        sym_info.type_str = Some(node.return_type.get_identifier());
        // set eval type
        sym_info.eval_type = Some(self.get_eval_type(&node.return_type));
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);

        // start symbol table for method 
        self.notify_new_scope();
        //set cur method
        self.cur_method_node = Some(arc_node.clone());
    }

    

    fn handle_field_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            Some(node) => node,
            _=> return
        };
        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());

        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Field);
        sym_info.type_str = Some(node.type_node.get_identifier());
        sym_info.eval_type = Some(self.get_eval_type(&node.type_node));
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);
    }

    fn handle_uses(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstUses>(){
            Some(node) => node,
            _=> return
        };
        for uses in &node.list_of_uses{
            self.get_cur_sym_table().add_uses_entity(&uses.get_value());

            // get st for uses
            let uses_sym_table = match self.get_symbol_table_for_class(&uses.get_value()) {
                Ok(st) => st,
                _=> continue
            };
            self.get_cur_sym_table().add_uses_symbol_table(uses_sym_table);
        }
    }

    fn handle_param_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstParameterDeclaration>(){
            Some(node) => node,
            _=> return
        };
        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());

        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Field);
        match &node.type_node{
            Some(t) => {
                sym_info.type_str = Some(t.get_identifier());
                sym_info.eval_type = Some(self.get_eval_type(&t));
            },
            _=> ()
        }
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);
    }
    fn handle_var_decl(&mut self, node: &RwLockWriteGuard<'_, AnnotatedNode<dyn IAstNode>>){
        let node = match node.data.as_any().downcast_ref::<AstLocalVariableDeclaration>(){
            Some(node) => node,
            _=> return
        };
        self.check_identifier_already_defined(&node.get_identifier(), node.get_range());

        let mut sym_info = SymbolInfo::new(node.get_identifier(), SymbolType::Field);
        sym_info.type_str = Some(node.type_node.get_identifier());
        sym_info.eval_type = Some(self.get_eval_type(&node.type_node));
        sym_info.range = node.get_range();
        sym_info.selection_range = node.identifier.get_range();
        self.insert_symbol_info(node.get_identifier(), sym_info);
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
                }
                _ => continue
            }
        }
        return result;
    }
}