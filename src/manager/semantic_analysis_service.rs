use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstTypeDeclaration, AstProcedure, AstFunction, AstTypeBasic, AstGlobalVariableDeclaration, AstUses, AstParameterDeclaration, AstLocalVariableDeclaration}, analyzers::{IVisitor, AnalyzerDiagnostic}, utils::{DynamicChild, Range, IRange, OptionString, ILogger, IDiagnosticCollector, GenericDiagnosticCollector, ILoggerV2}, lexer::tokens::TokenType, unwrap_or_return};
use core::fmt::Debug;
use std::{collections::{HashMap, HashSet}, sync::{Mutex, Arc, RwLock, RwLockWriteGuard, MutexGuard, LockResult}, ops::{DerefMut, Deref}, result, f32::consts::E, vec::IntoIter};
use crate::utils::{OptionExt};
use super::{ProjectManager, annotated_node::{AnnotatedNode, EvalType, TypeInfo, Location, NativeType, self}, data_structs::{Document, ProjectManagerError}, document_service::DocumentService, type_resolver::TypeResolver};
use crate::manager::symbol_table::{SymbolTable,SymbolInfo,SymbolType, ISymbolTable};
#[derive(Debug, Clone)]
pub struct SemanticAnalysisService {
    pub doc_service : Arc<RwLock<DocumentService>>,
    logger: Arc<dyn ILoggerV2>,
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
    already_seen_uri: Arc<Mutex<HashSet<String>>>,
}

impl SemanticAnalysisService {
    pub fn new(
        doc_service: Arc<RwLock<DocumentService>>, 
        logger: Arc<dyn ILoggerV2>, 
        diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
        already_seen_uri : Option<Arc<Mutex<HashSet<String>>>>
) -> SemanticAnalysisService{
        return SemanticAnalysisService {  
            doc_service,
            logger: logger,
            diag_collector,
            already_seen_uri: already_seen_uri.unwrap_or_default(),
        }
    }

    fn check_already_seen(&mut self, uri: &Url) -> Result<(),ProjectManagerError>{
        if self.already_seen_uri.lock().unwrap().contains(&uri.to_string()){
            return Err(ProjectManagerError::new(format!("{} already locked in request session",uri).as_str(), ErrorCode::RequestFailed));
        }
        self.already_seen_uri.lock().unwrap().insert(uri.to_string());
        return Ok(())
    }
    pub fn get_symbol_table_class_def_only(&mut self, class: &String) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let uri = self.doc_service.read().unwrap().get_uri_for_class(class)?;
        let doc: Arc<Mutex<Document>> = self.analyze_uri(&uri, true)?;
        let doc_lock = doc.lock().unwrap();
        match &doc_lock.symbol_table{
            Some(st) => return Ok(st.clone()),
            _=> return Err(ProjectManagerError::new(format!("Unable to get Symbol table for {}",class).as_str(), ErrorCode::InternalError))
        }
    }

    /// if no error occurs, annotated tree & symbol table is guranteed to be Some
    pub fn analyze_uri(&mut self, uri : &Url, only_definitions: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        self.check_already_seen(uri)?;
        self.logger.log_info(format!("[Req Analyze Uri:{}]{}", if !only_definitions {"Full"}else{"Light"},uri).as_str());
        eprintln!();
        let doc: Arc<Mutex<Document>> = self.doc_service.write().unwrap().get_parsed_document(uri, true)?;
        // is exist and only defs needed return existing,
        //  otherwise regenerate
        if doc.lock().unwrap().annotated_ast.is_some() && only_definitions{
            return Ok(doc);
        }
        self.logger.log_info(format!("[Analyzing Uri]{}", uri).as_str());
        return self.analyze(doc, only_definitions);
    }
    
    fn analyze(&mut self, doc: Arc<Mutex<Document>>, only_definitions: bool) -> 
    Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let root_node = doc.lock().unwrap().get_ast().clone();
        // let mut semantic_analysis_service = SemanticAnalysisService::new(
        //     self.doc_service.clone(), 
        //     self.logger.clone(),
        //     self.diag_collector.clone(),
        //     Some(self.already_seen_classes.clone())
        // );
        let mut annotator = AstAnnotator::new(
            self, 
            self.diag_collector.clone(), 
            self.logger.clone(),
            only_definitions
        );
        let (annotated_tree, sym_table) = annotator.analyze(&root_node)?;
        doc.lock().unwrap().symbol_table = Some(sym_table.clone());
        doc.lock().unwrap().annotated_ast = Some(annotated_tree.clone());
        return Ok(doc);
    }
}


pub struct AstAnnotator<'a> {
    root_symbol_table : Option<Arc<Mutex<SymbolTable>>>,
    // need to wrap in arc, to provide consistent api with root sym table
    symbol_table_stack: Vec<Arc<Mutex<SymbolTable>>>,

    logger: Arc<dyn ILoggerV2>,
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
    semantic_analysis_service: &'a mut SemanticAnalysisService,
    cur_method_node : Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>,
    only_definitions: bool
}
impl<'a> AstAnnotator<'a>{
    pub fn new (
        semantic_analysis_service: &'a mut SemanticAnalysisService,
        diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
        logger: Arc<dyn ILoggerV2>,
        // to prevent large branching when only definition is needed
        only_definitions: bool
    )->AstAnnotator<'a>{
        return AstAnnotator{
            root_symbol_table: Some(Arc::new(Mutex::new(SymbolTable::new()))),
            symbol_table_stack: Vec::new(),
            logger: logger,
            diag_collector,
            semantic_analysis_service,
            cur_method_node:None,
            only_definitions,
        }      
    }

    fn analyze(&mut self, root_node: &Arc<dyn IAstNode>) -> 
    Result<(Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, Arc<Mutex<SymbolTable>>), ProjectManagerError>{
        let annotated_tree = self.generate_annotated_tree(&root_node);
        self.walk_tree(&annotated_tree);
        self.notify_end_method();
        let sym_table = self.root_symbol_table.take().unwrap();
        return Ok((annotated_tree, sym_table));
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
            if self.only_definitions{
                self.visit(child)
            }else{
                self.walk_tree(child)
            }
        }
    }

    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        self.handle_class(&node);
        self.handle_constant_decl(&node);
        self.handle_type_decl(&node);
        self.handle_proc_decl(&node);
        self.handle_func_decl(&node);
        self.handle_field_decl(&node);
        self.handle_uses(&node);
        self.handle_param_decl(&node);
        self.handle_var_decl(&node);
    }

    fn notify_new_scope(&mut self){
        let mut new_sym_table = SymbolTable::new();
        new_sym_table.set_parent_symbol_table(self.root_symbol_table.unwrap_ref().clone());
        // append uses to new scope
        new_sym_table.for_class = self.root_symbol_table.unwrap_ref().lock().unwrap().for_class.clone();
        new_sym_table.uses_entities.extend(
            self.root_symbol_table.unwrap_ref().lock().unwrap()
            .uses_entities
            .iter()
            .map(|s|{s.to_string()})
        );
        self.symbol_table_stack.push(Arc::new(Mutex::new(new_sym_table)));
    }

    fn notify_end_method(&mut self){
        // sets last sym table for method
        // set sym table to last method
        if let Some(sym_table) = self.pop_last_scope(){
            // should always be Some
            self.cur_method_node.as_ref().unwrap().write().unwrap().symbol_table = Some(sym_table);
        }
    }

    fn pop_last_scope(&mut self) -> Option<Arc<Mutex<SymbolTable>>>{
        return self.symbol_table_stack.pop();
    }

    /// Inserts the symbol to the current scope, last in stack/root
    fn get_cur_sym_table<'b>(&'b mut self) -> Arc<Mutex<SymbolTable>>{
        let cur_st = match self.symbol_table_stack.last_mut(){
            Some(st) => st.clone(),
            _=> self.root_symbol_table.unwrap_ref().clone()
        };
        return cur_st;
    }

    fn check_identifier_already_defined(&mut self, id: &String, range: Range) -> bool{
        // do we need to check uses?
        match self.get_cur_sym_table().lock().unwrap().get_symbol_info(id){
            Some(sym)=>{
                self.diag_collector.lock().unwrap().add_diagnostic(
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
    fn insert_symbol_info(&mut self, id: String, symbol: SymbolInfo){
        let cur_st = self.get_cur_sym_table();
        cur_st.lock().unwrap().insert_symbol_info(id, symbol);
    }

    fn get_eval_type(&mut self, node : &Arc<dyn IAstNode>)-> EvalType{
        let mut type_resolver = TypeResolver::new(self.semantic_analysis_service.clone());
        let st : Arc<Mutex<dyn ISymbolTable>> = self.get_cur_sym_table();
        return type_resolver.resolve_node_type(node, &st);
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



    fn handle_class(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let ori_node = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstClass>());

        let class_name = ori_node.get_identifier();
        self.root_symbol_table.unwrap_ref().lock().unwrap().for_class = Some(class_name.clone());
        let mut sym_info = SymbolInfo::new(class_name.clone(), SymbolType::Class);
        sym_info.eval_type = Some(EvalType::Class(class_name.clone()));

        if let Some(parent_class) = &ori_node.parent_class{
            let parent_class_name = parent_class.get_value();
            if parent_class_name == class_name{
                // parent class can't be itself
                self.diag_collector.lock().unwrap().add_diagnostic(
                    AnalyzerDiagnostic::new("Parent class cannot be itself", parent_class.get_range())
                );
            }
            sym_info.parent = Some(parent_class_name.clone());
            let parent_symbol_table = self.semantic_analysis_service.get_symbol_table_class_def_only(&parent_class_name);
            match parent_symbol_table{
                Ok(st) => {
                    // set parent symbol table
                    self.root_symbol_table.unwrap_ref().lock().unwrap().set_parent_symbol_table(st);
                },
                Err(e)=> {
                    // parent class not found/cannot be parsed
                    self.diag_collector.lock().unwrap().add_diagnostic(
                        AnalyzerDiagnostic::new(format!("Parent class not defined; {}", e).as_str(), parent_class.get_range())
                    );
                }
            }
        }
        sym_info.range = ori_node.get_range();
        sym_info.selection_range = ori_node.identifier.get_range();
        self.insert_symbol_info(ori_node.get_identifier(), sym_info.clone());
        // add self to sym table
        let mut self_sym = sym_info;
        self_sym.id = "Self".to_string();
        self.insert_symbol_info("self".to_string(), self_sym);
    }

    fn handle_constant_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let cst_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstConstantDeclaration>());

        self.check_identifier_already_defined(&cst_decl.get_identifier(), cst_decl.get_range());
        
        let mut sym_info = SymbolInfo::new(cst_decl.get_identifier(), SymbolType::Constant);
        sym_info.eval_type = match &cst_decl.value.token_type{
            TokenType::StringLiteral => Some(EvalType::Native(NativeType::String)),
            TokenType::NumericLiteral => Some(EvalType::Native(NativeType::Num)),
            _=> Some(EvalType::Unknown)
        };
        sym_info.range = cst_decl.get_range();
        sym_info.selection_range = cst_decl.identifier.get_range();
        self.insert_symbol_info(cst_decl.get_identifier(), sym_info);
    }

    fn handle_type_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let type_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstTypeDeclaration>());

        self.check_identifier_already_defined(&type_decl.get_identifier(), type_decl.get_range());
        
        let mut sym_info = SymbolInfo::new(type_decl.get_identifier(), SymbolType::Type);
        sym_info.type_str = Some(type_decl.type_node.get_identifier());
        // set eval type
        sym_info.eval_type = Some(self.get_eval_type(&type_decl.type_node));
        sym_info.range = type_decl.get_range();
        sym_info.selection_range = type_decl.identifier.get_range();
        self.insert_symbol_info(type_decl.get_identifier(), sym_info);

    }

    fn handle_proc_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let proc_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstProcedure>());
        // set sym table to last method
        self.notify_end_method();

        self.check_identifier_already_defined(&proc_decl.get_identifier(), proc_decl.get_range());

        let mut sym_info = SymbolInfo::new(proc_decl.get_identifier(), SymbolType::Proc);
        sym_info.range = proc_decl.get_range();
        sym_info.selection_range = proc_decl.identifier.get_range();
        self.insert_symbol_info(proc_decl.get_identifier(), sym_info);

        //set cur method
        self.cur_method_node = Some(node.clone());
        self.notify_new_scope();
    }

    fn handle_func_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let func_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstFunction>());
        // set sym table to last method
        self.notify_end_method();

        self.check_identifier_already_defined(&func_decl.get_identifier(), func_decl.get_range());

        let mut sym_info = SymbolInfo::new(func_decl.get_identifier(), SymbolType::Func);
        sym_info.type_str = Some(func_decl.return_type.get_identifier());
        // set eval type
        sym_info.eval_type = Some(self.get_eval_type(&func_decl.return_type));
        sym_info.range = func_decl.get_range();
        sym_info.selection_range = func_decl.identifier.get_range();
        self.insert_symbol_info(func_decl.get_identifier(), sym_info);

        // start symbol table for method 
        self.notify_new_scope();
        //set cur method
        self.cur_method_node = Some(node.clone());
    }

    

    fn handle_field_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let field_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstGlobalVariableDeclaration>());

        self.check_identifier_already_defined(&field_decl.get_identifier(), field_decl.get_range());

        let mut sym_info = SymbolInfo::new(field_decl.get_identifier(), SymbolType::Field);
        sym_info.type_str = Some(field_decl.type_node.get_identifier());
        sym_info.eval_type = Some(self.get_eval_type(&field_decl.type_node));
        sym_info.range = field_decl.get_range();
        sym_info.selection_range = field_decl.identifier.get_range();
        self.insert_symbol_info(field_decl.get_identifier(), sym_info);
    }

    fn handle_uses(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let uses_node = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstUses>());

        for uses in &uses_node.list_of_uses{
            self.get_cur_sym_table().lock().unwrap().add_uses_entity(&uses.get_value());

            // get st for uses, causes stack overflow :)
            // let uses_sym_table = match self.get_symbol_table_for_class(&uses.get_value()) {
            //     Ok(st) => st,
            //     _=> continue
            // };
            // self.get_cur_sym_table().add_uses_symbol_table(uses_sym_table);
        }
    }

    fn handle_param_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let param_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstParameterDeclaration>());

        self.check_identifier_already_defined(&param_decl.get_identifier(), param_decl.get_range());

        let mut sym_info = SymbolInfo::new(param_decl.get_identifier(), SymbolType::Variable);
        match &param_decl.type_node{
            Some(t) => {
                sym_info.type_str = Some(t.get_identifier());
                sym_info.eval_type = Some(self.get_eval_type(&t));
            },
            _=> ()
        }
        sym_info.range = param_decl.get_range();
        sym_info.selection_range = param_decl.identifier.get_range();
        self.insert_symbol_info(param_decl.get_identifier(), sym_info);
    }

    fn handle_var_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let var_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstLocalVariableDeclaration>());

        self.check_identifier_already_defined(&var_decl.get_identifier(), var_decl.get_range());

        let mut sym_info = SymbolInfo::new(var_decl.get_identifier(), SymbolType::Variable);
        sym_info.type_str = Some(var_decl.type_node.get_identifier());
        sym_info.eval_type = Some(self.get_eval_type(&var_decl.type_node));
        sym_info.range = var_decl.get_range();
        sym_info.selection_range = var_decl.identifier.get_range();
        self.insert_symbol_info(var_decl.get_identifier(), sym_info);
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

#[cfg(test)]
mod test{
    use std::sync::{Arc, RwLock};

    use crate::manager::test::{create_test_sem_service, create_test_doc_service, create_uri_from_path};

    #[test]
    fn test_get_symbol_table(){
        let root_uri = create_uri_from_path("./test/workspace");
        let mut doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        let doc_service = Arc::new(RwLock::new(doc_service));
        let mut sem_service = create_test_sem_service(doc_service);

        let result = sem_service.get_symbol_table_class_def_only(&"aRootClass".to_string()).unwrap();
        let result = result.lock().unwrap();
        assert_eq!(result.iter_symbols().count(), 8);
    }
}