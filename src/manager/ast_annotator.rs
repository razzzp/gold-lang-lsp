use std::sync::{Arc, Mutex, RwLock};

use crate::{
    utils::{IDiagnosticCollector, ILoggerV2, Range, OptionString, OptionExt, IRange}, 
    analyzers::AnalyzerDiagnostic, 
    parser::ast::*, unwrap_or_return, lexer::tokens::TokenType};

use super::{
    symbol_table::*, 
    semantic_analysis_service::SemanticAnalysisService, 
    annotated_node::*, 
    data_structs::ProjectManagerError, 
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

    pub fn analyze(&mut self, root_node: &Arc<dyn IAstNode>) -> 
    Result<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, ProjectManagerError>{
        let annotated_tree = self.generate_annotated_tree(&root_node);
        self.walk_tree(&annotated_tree);
        self.notify_end_method();
        //add sym table to root of tree
        annotated_tree.write().unwrap().symbol_table = Some(self.root_symbol_table.take().unwrap());
        return Ok(annotated_tree);
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
            // first level are definitions, so visit first
            // preorder
            self.visit(child);
            if !self.only_definitions {
                // everything under, travel postorder,
                // process children first
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
        let st : Arc<Mutex<dyn ISymbolTable>> = self.get_cur_sym_table();
        return self.type_resolver.resolve_node_type(node, &st);
    }


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
                        let class_sym_table = if class_name == self.root_symbol_table.as_ref().unwrap().lock().unwrap().for_class.unwrap_clone_or_empty_string() {
                            self.get_cur_sym_table()
                        } else {
                            match self.semantic_analysis_service.get_symbol_table_class_def_only(&class_name){
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
                // take child types should be resolved,
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
    use std::sync::{Mutex, Arc};

    use crate::{manager::{test::{create_test_project_manager, create_uri_from_path, create_test_def_service, create_test_sem_service}, utils::search_encasing_node, annotated_node::EvalType}, utils::Position};

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
        let doc = proj_manager.doc_service.read().unwrap().parse_content(&test_input).unwrap();
        let doc = Arc::new(Mutex::new(doc));
        let mut sem_service = create_test_sem_service(proj_manager.doc_service.clone());
        let doc = sem_service.analyze(doc, false).unwrap();
        let root =doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        // check obj
        let node = search_encasing_node(&root, &Position::new(6, 5));
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class("aObject".to_string()));
        // check first right
        let node = search_encasing_node(&root, &Position::new(6, 8));
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class("aObject".to_string()));
        // check second right
        let node = search_encasing_node(&root, &Position::new(6, 13));
        // println!("{:#?}", node.read().unwrap().as_annotated_node());
        assert_eq!(node.read().unwrap().eval_type.as_ref().clone().unwrap(), &EvalType::Class("aObject".to_string()));
    }
}