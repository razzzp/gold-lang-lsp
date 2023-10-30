use std::sync::{Arc, Mutex, RwLock};

use crate::{
    utils::{IDiagnosticCollector, ILoggerV2, Range, OptionString, OptionExt, IRange}, 
    analyzers::AnalyzerDiagnostic, 
    parser::ast::*, unwrap_or_return, lexer::tokens::TokenType};

use super::{
    symbol_table::*, 
    semantic_analysis_service::SemanticAnalysisService, 
    annotated_node::*, 
    data_structs::{ProjectManagerError, Document, DocumentInfo}, 
    type_resolver::TypeResolver
};




pub struct AstAnnotator{
    root_symbol_table : Option<Arc<Mutex<SymbolTable>>>,
    // need to wrap in arc, to provide consistent api with root sym table
    symbol_table_stack: Vec<Arc<Mutex<SymbolTable>>>,

    logger: Arc<dyn ILoggerV2>,
    diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
    semantic_analysis_service: SemanticAnalysisService,
    cur_method_node : Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>,
    only_definitions: bool,
    type_resolver: TypeResolver,
}
impl AstAnnotator{
    pub fn new (
        semantic_analysis_service: SemanticAnalysisService,
        diag_collector: Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>,
        logger: Arc<dyn ILoggerV2>,
        // to prevent large branching when only definition is needed
        only_definitions: bool,
    )->AstAnnotator{
        return AstAnnotator{
            root_symbol_table: Some(Arc::new(Mutex::new(SymbolTable::new()))),
            symbol_table_stack: Vec::new(),
            logger: logger,
            diag_collector,
            type_resolver : TypeResolver::new(semantic_analysis_service.clone()),
            semantic_analysis_service,
            cur_method_node:None,
            only_definitions,
        }      
    }


    pub fn annotate_doc(&mut self, doc: Arc<Mutex<Document>>, doc_info: Arc<RwLock<DocumentInfo>>) -> 
    Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let root_node = doc.lock().unwrap().get_ast().clone();
        // lock flag to let other services know, annotation is still in progress
        let annotation_done_flag = doc.lock().unwrap().annotation_done.clone();
        // try lock, if fail, already locked by previous call, then do nothing
        let flag_lock = match annotation_done_flag.try_lock(){
            Ok(lock) => Some(lock),
            _=> None
        };

        let annotated_tree = self.generate_annotated_tree(&root_node);

        let st = self.root_symbol_table.unwrap_ref().clone();
        // assign root st to root node
        annotated_tree.write().unwrap().symbol_table = Some(st.clone());
        // assign to doc, so that in the case of circular
        // dependency, analyzing the same doc again won't regenerate and cause
        // infinite loop
        doc.lock().unwrap().annotated_ast = Some(annotated_tree.clone());
        doc.lock().unwrap().only_definitions = self.only_definitions;
        // also assign root sym_table to doc_info to prevent, stack overflow
        doc_info.write().unwrap().set_symbol_table(Some(st));
        // walk tree and analyze
        self.walk_tree(&annotated_tree);
        self.notify_end_method();

        // is this needed?
        if let Some(lock)= flag_lock{
            drop(lock);
        }
        
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
            // first level are definitions, so visit first (preorder)
            self.visit(child);
            if !self.only_definitions {
                // everything under, travel postorder
                // (process children first)
                for inner_child in &child.read().unwrap().children{
                    self.walk_tree_postorder(inner_child)
                }
            }
        }
    }

    fn walk_tree_postorder(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        for child in &node.read().unwrap().children {
                self.walk_tree_postorder(child)
        }
        self.visit(node);
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
        self.handle_terminal_node(&node);
        self.handle_bin_op_node(&node);
        self.handle_method_call(&node);
        self.handle_module(&node);
    }

    fn notify_new_scope(&mut self){
        let mut new_sym_table = SymbolTable::new();
        new_sym_table.set_parent_symbol_table(self.root_symbol_table.unwrap_ref().clone());
        // append uses to new scope
        new_sym_table.for_class_or_module = self.root_symbol_table.unwrap_ref().lock().unwrap().for_class_or_module.clone();
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

    fn check_identifier_already_defined(&mut self, id: &str, range: Range) -> bool{
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
    fn insert_symbol_info(&mut self, id: &str, symbol: SymbolInfo){
        let cur_st = self.get_cur_sym_table();
        cur_st.lock().unwrap().insert_symbol_info(id, symbol);
    }

    fn get_eval_type(&mut self, node : &Arc<dyn IAstNode>)-> EvalType{
        let st : Arc<Mutex<dyn ISymbolTable>> = self.get_cur_sym_table();
        return self.type_resolver.resolve_node_type(node, &st);
    }


    fn handle_class(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let ori_node = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstClass>());

        let class_name = ori_node.get_identifier();
        let class_name_arc = ori_node.identifier.get_value();
        self.root_symbol_table.unwrap_ref().lock().unwrap().for_class_or_module = Some(class_name.to_string());
        let mut sym_info = SymbolInfo::new(class_name.to_string(), SymbolType::Class);
        sym_info.eval_type = Some(EvalType::Class(class_name_arc));

        if let Some(parent_class) = &ori_node.parent_class{
            let parent_class_name = parent_class.get_value_as_str();
            if parent_class_name == class_name{
                // parent class can't be itself
                self.diag_collector.lock().unwrap().add_diagnostic(
                    AnalyzerDiagnostic::new("Parent class cannot be itself", parent_class.get_range())
                );
            }
            sym_info.parent = Some(parent_class_name.to_string());
            let parent_symbol_table = self.semantic_analysis_service.get_symbol_table_for_class_def_only(&parent_class_name.to_string());
            match parent_symbol_table{
                Ok(st) => {
                    // set parent symbol table
                    self.root_symbol_table.unwrap_ref().lock().unwrap().set_parent_symbol_table(st);
                },
                Err(e)=> {
                    // parent class not found/cannot be parsed
                    // or class already visited in current session
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
        self_sym.id = "self".to_string();
        self.insert_symbol_info("self", self_sym);
    }


    fn handle_module(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let ori_node = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstModule>());

        let module_name = ori_node.get_identifier();
        let module_name_arc = ori_node.id.get_value();
        self.root_symbol_table.unwrap_ref().lock().unwrap().for_class_or_module = Some(module_name.to_string());
        let mut sym_info = SymbolInfo::new(module_name.to_string(), SymbolType::Module);
        sym_info.eval_type = Some(EvalType::Module(module_name_arc));

        sym_info.range = ori_node.get_range();
        sym_info.selection_range = ori_node.id.get_range();
        self.insert_symbol_info(ori_node.get_identifier(), sym_info);
    }

    fn handle_constant_decl(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let cst_decl = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstConstantDeclaration>());

        self.check_identifier_already_defined(&cst_decl.get_identifier().to_string(), cst_decl.get_range());
        
        let mut sym_info = SymbolInfo::new(cst_decl.get_identifier().to_string(), SymbolType::Constant);
        sym_info.eval_type = match &cst_decl.value_token.token_type{
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
        
        let mut sym_info = SymbolInfo::new(type_decl.get_identifier().to_string(), SymbolType::Type);
        sym_info.type_str = Some(type_decl.type_node.get_identifier().to_string());
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

        let mut sym_info = SymbolInfo::new(proc_decl.get_identifier().to_string(), SymbolType::Proc);
        sym_info.range = proc_decl.get_range();
        sym_info.eval_type = Some(EvalType::Proc);
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

        let mut sym_info = SymbolInfo::new(func_decl.get_identifier().to_string(), SymbolType::Func);
        sym_info.type_str = Some(func_decl.return_type.get_identifier().to_string());
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

        let mut sym_info = SymbolInfo::new(field_decl.get_identifier().to_string(), SymbolType::Field);
        sym_info.type_str = Some(field_decl.type_node.get_identifier().to_string());
        sym_info.eval_type = Some(self.get_eval_type(&field_decl.type_node));
        sym_info.range = field_decl.get_range();
        sym_info.selection_range = field_decl.identifier.get_range();
        self.insert_symbol_info(field_decl.get_identifier(), sym_info);
    }

    fn handle_uses(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        let node_lock = node.write().unwrap();
        let uses_node = unwrap_or_return!(node_lock.data.as_any().downcast_ref::<AstUses>());

        for uses in &uses_node.list_of_uses{
            self.get_cur_sym_table().lock().unwrap().add_uses_entity(&uses.get_value_as_str());

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

        let mut sym_info = SymbolInfo::new(param_decl.get_identifier().to_string(), SymbolType::Variable);
        match &param_decl.type_node{
            Some(t) => {
                sym_info.type_str = Some(t.get_identifier().to_string());
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

        let mut sym_info = SymbolInfo::new(var_decl.get_identifier().to_string(), SymbolType::Variable);
        sym_info.type_str = Some(var_decl.type_node.get_identifier().to_string());
        sym_info.eval_type = Some(self.get_eval_type(&var_decl.type_node));
        sym_info.range = var_decl.get_range();
        sym_info.selection_range = var_decl.identifier.get_range();
        self.insert_symbol_info(var_decl.get_identifier(), sym_info);
    }


    fn check_parent_dot_ops(&self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>)
    ->Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
    {
        let parent_node = node.read().unwrap().parent.as_ref()?.upgrade()?;
        let lock = parent_node.read().unwrap();
        if let Some(bin_op) = lock.data.as_any().downcast_ref::<AstBinaryOp>(){
            if bin_op.op_token.token_type == TokenType::Dot{
                return Some(parent_node.clone());
            } else {return  None;}
        } else {
            return None;
        }
    }

    fn eval_right_hand_of_entity(&mut self, left_entity_name : &str, right_id: &str) -> EvalType{
        // search right node in class
        // if class is self don't call sem service, because it will fail
        let class_sym_table = if left_entity_name.to_string() == self.root_symbol_table.as_ref().unwrap().lock().unwrap().for_class_or_module.unwrap_clone_or_empty_string() {
            self.get_cur_sym_table()
        } else {
            match self.semantic_analysis_service.get_symbol_table_for_class_def_only(&left_entity_name){
                Ok(st) => st,
                _=> return EvalType::default()
            }
        };
        let (_class, sym_info) = match self.type_resolver.search_sym_info_w_class(right_id, &class_sym_table, false){
            Some(r) => r,
            _=> return EvalType::default()
        };
        return sym_info.eval_type.clone().unwrap_or_default();
    }

    fn resolve_termninal_node(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) -> EvalType{
        if let Some(dot_op_parent) = self.check_parent_dot_ops(&node){
            let parent_lock = dot_op_parent.read().unwrap();
            // should be safe to unwrap
            let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            let left_node = &bin_op_node.left_node;
            if Arc::ptr_eq(left_node, &node.read().unwrap().data){
                // if left node, just resolve normally
                let inner = &node.read().unwrap().data.clone();
                return self.get_eval_type(&inner);
            } else {
                // if right node, check left node type first
                let left_node_type =match &parent_lock.children[0].read().unwrap().eval_type{
                    Some(t) => t.clone(),
                    _=> return EvalType::default()
                };
                match left_node_type{
                    EvalType::Class(class_name)=>{
                        // search right node in class
                        // if class is self don't call sem service, because it will fail
                        return self.eval_right_hand_of_entity(&class_name, &bin_op_node.right_node.get_identifier());
                    },
                    EvalType::Module(module_name)=>{
                        // search right node in class
                        // if class is self don't call sem service, because it will fail
                        return self.eval_right_hand_of_entity(&module_name, &bin_op_node.right_node.get_identifier());
                    },
                    _=> return EvalType::default(),
                }
            }
        } else {
            let inner = &node.read().unwrap().data.clone();
            return self.get_eval_type(&inner);
        }
    }

    fn handle_terminal_node(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        if !self.can_cast::<AstTerminal>(node) {return}
        
        let eval_type = self.resolve_termninal_node(node);
        node.write().unwrap().eval_type = Some(eval_type);
    }

    fn resolve_bin_op_node(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>)-> EvalType{
        // should be safe to unwarp
        let node_lock = node.read().unwrap();
        let bin_op = node_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap();
        match &bin_op.op_token.token_type {
            TokenType::Dot => {
                // child types should already be resolved,
                // resulting will be same as right node
                // println!("{:#?}",node_lock.children[1].read().unwrap().eval_type.clone().unwrap_or_default());
                return node_lock.children[1].read().unwrap().eval_type.clone().unwrap_or_default();
            },
            _=> return EvalType::Unknown
        };

    }

    fn handle_bin_op_node(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        if !self.can_cast::<AstBinaryOp>(node) {return}
        
        let eval_type = self.resolve_bin_op_node(node);
        node.write().unwrap().eval_type = Some(eval_type);
    }

    fn resolve_method_call(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>)-> EvalType{
        // TODO can probably refactor with terminal node resolution
        let node_lock = node.read().unwrap();
        let _method_node = node_lock.data.as_any().downcast_ref::<AstMethodCall>().unwrap();

        if let Some(dot_op_parent) = self.check_parent_dot_ops(node){
            let parent_lock = dot_op_parent.read().unwrap();
            // should be safe to unwrap
            let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            let left_node = &bin_op_node.left_node;
            if Arc::ptr_eq(left_node, &node_lock.data){
                // if left node, just resolve normally
                return self.get_eval_type(&node_lock.data);
            } else {
                // if right node, check left node type first
                let left_node_type =match &parent_lock.children[0].read().unwrap().eval_type{
                    Some(t) => t.clone(),
                    _=> return EvalType::default()
                };
                match left_node_type{
                    EvalType::Class(class_name)=>{
                        // search right node in class
                        // if class is self don't call sem service, because it will fail
                        let class_sym_table = if class_name.to_string() == self.root_symbol_table.as_ref().unwrap().lock().unwrap().for_class_or_module.unwrap_clone_or_empty_string() {
                            self.get_cur_sym_table()
                        } else {
                            match self.semantic_analysis_service.get_symbol_table_for_class_def_only(&class_name){
                                Ok(st) => st,
                                _=> return EvalType::default()
                            }
                        };
                        let (_class, sym_info) = match self.type_resolver.search_sym_info_w_class(&bin_op_node.right_node.get_identifier(), &class_sym_table, false){
                            Some(r) => r,
                            _=> return EvalType::default()
                        };
                        return sym_info.eval_type.clone().unwrap_or_default();
                    },
                    _=> return EvalType::default(),
                }
            }
        } else {
            // just search using cur sym table
            return self.get_eval_type(&node_lock.data);
        }
    }

    fn handle_method_call(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        if !self.can_cast::<AstMethodCall>(node) {return}
        
        let eval_type = self.resolve_method_call(node);
        node.write().unwrap().eval_type = Some(eval_type);
    }

    fn can_cast<T: 'static>(&self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>) -> bool{
        let node_lock = node.read().unwrap();
        match node_lock.data.as_any().downcast_ref::<T>(){
            Some(_) => return true,
            _=> return false
        }
    }
}

#[cfg(test)]
mod test{
    use std::sync::{Mutex, Arc, RwLock};

    use crate::{manager::{test::{create_test_project_manager, create_test_sem_service, create_test_logger}, utils::search_encasing_node, annotated_node::{EvalType, NativeType}, data_structs::DocumentInfo}, utils::Position};

    #[test]
    fn test_bin_op(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = 
"
class aObject
var1 : aObject
proc Test
    var obj : aObject
    ;
    obj.var1.var1
endproc
        ".to_string();
        let doc = proj_manager.doc_service.parse_content(&test_input).unwrap();
        let doc = Arc::new(Mutex::new(doc));
        let sem_service = create_test_sem_service(proj_manager.doc_service.clone());
        let doc_info = Arc::new(RwLock::new(DocumentInfo::new("".to_string(), "".to_string())));
        let doc = sem_service.analyze(doc, doc_info, false).unwrap();
        let root =doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let logger= create_test_logger();
        // check obj
        let node = search_encasing_node(&root, &Position::new(6, 5), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
        // check first right
        let node = search_encasing_node(&root, &Position::new(6, 8), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
        // check second right
        let node = search_encasing_node(&root, &Position::new(6, 13), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
    }

    #[test]
    fn test_bin_op_refto(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = 
"
class aObject
var1 : refto aObject
proc Test
    var obj : aObject
    ;
    writeln(obj.var1.var1)
endproc
        ".to_string();
        let doc = proj_manager.doc_service.parse_content(&test_input).unwrap();
        let doc = Arc::new(Mutex::new(doc));
        let sem_service = create_test_sem_service(proj_manager.doc_service.clone());
        let doc_info = Arc::new(RwLock::new(DocumentInfo::new("".to_string(), "".to_string())));
        let doc = sem_service.analyze(doc, doc_info, false).unwrap();
        let root =doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let logger= create_test_logger();
        // check obj
        let node = search_encasing_node(&root, &Position::new(6, 15), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
        // check first right
        let node = search_encasing_node(&root, &Position::new(6, 19), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
        // check second right
        let node = search_encasing_node(&root, &Position::new(6, 25), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
    }


    #[test]
    fn test_method_call(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = 
"
class aObject
var1 : CString

proc ObjectProc(input: Int4)
endProc

func ObjectFunc(input: Int4) return aObject
endfunc

proc Test
    var obj : aObject
    ;
    writeln(obj)
    self.ObjectProc(10)
    self.ObjectFunc(10)
    self.ObjectFunc(10).var1
endproc
        ".to_string();
        let doc = proj_manager.doc_service.parse_content(&test_input).unwrap();
        let doc = Arc::new(Mutex::new(doc));
        let sem_service = create_test_sem_service(proj_manager.doc_service.clone());
        let doc_info = Arc::new(RwLock::new(DocumentInfo::new("".to_string(), "".to_string())));
        let doc = sem_service.analyze(doc, doc_info, false).unwrap();
        let root =doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let logger= create_test_logger();
        // native proc
        let node = search_encasing_node(&root, &Position::new(13, 9), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Proc);

        // class proc
        let node = search_encasing_node(&root, &Position::new(14, 15), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Proc);
        
        // class func
        let node = search_encasing_node(&root, &Position::new(15, 15), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class(Arc::from("aObject")));
        
        // class func deref
        let node = search_encasing_node(&root, &Position::new(16, 28), &logger);
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Native(NativeType::CString));
    }
}