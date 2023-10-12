

use std::{sync::{Arc, RwLock, Mutex, RwLockWriteGuard, RwLockReadGuard, Weak}, any::Any};

use lsp_server::ErrorCode;
use lsp_types::{LocationLink, Url};

use crate::{parser::ast::{IAstNode, AstTerminal, AstBinaryOp, AstTypeBasic, AstClass}, utils::{Position, IRange, ILoggerV2}, lexer::tokens::TokenType, manager::type_resolver};

use super::{ProjectManager, data_structs::{ProjectManagerError, Document}, annotated_node::{AnnotatedNode, EvalType}, semantic_analysis_service::{ISymbolTable, SemanticAnalysisService}, type_resolver::TypeResolver, document_service::DocumentService};



pub struct DefinitionService{
    doc_service : Arc<RwLock<DocumentService>>,
    semantic_analysis_service: SemanticAnalysisService,
    type_resolver: TypeResolver,
    source_uri: Url,
    logger: Arc<dyn ILoggerV2>,
}
impl DefinitionService{
    pub fn new(
        doc_service : Arc<RwLock<DocumentService>>,
        semantic_analysis_service: SemanticAnalysisService, 
        logger: Arc<dyn ILoggerV2>,
        source_uri: &Url
    )->DefinitionService{
        return DefinitionService { 
            doc_service,
            type_resolver: TypeResolver::new(semantic_analysis_service.clone()),
            semantic_analysis_service: semantic_analysis_service,
            logger,
            source_uri: source_uri.clone(),
        }
    }

    fn search_encasing_node(&self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
    {
        let r_node = node.read().unwrap();
        for child in &r_node.children{
            // self.logger.log_info(format!("[Req Definition] Cur Node {}",child.read().unwrap().data.to_string_type_pos()).as_str());
            if child.read().unwrap().data.get_range().contains_pos(pos){
                return self.search_encasing_node(child, pos)
            }
        }
        return node.clone()
    }

    fn is_bin_op_dot_ops(&self, node : &Option<Weak<RwLock<AnnotatedNode<dyn IAstNode>>>>)->bool{
        if node.is_none(){
            return false;
        }
        let node = match node.as_ref().unwrap().upgrade(){
            Some(n) => n,
            _=> return false
        };
        let node = node.read().unwrap();
        if let Some(bin_op) = node.data.as_any().downcast_ref::<AstBinaryOp>(){
            if bin_op.op_token.token_type == TokenType::Dot{
                return true
            } else {return  false;}
        } else {
            return false;
        }
    }

    fn generate_loc_link(&self, node:  &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, st: &Arc<Mutex<dyn ISymbolTable>>)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let mut st_lock =st.lock().unwrap();
        match st_lock.get_symbol_info(&node.data.get_identifier()){
            Some(s) =>{
                let mut result = Vec::new();
                result.push(LocationLink{
                    origin_selection_range: Some(node.data.get_range().as_lsp_type_range()),
                    target_range: s.range.as_lsp_type_range(),
                    target_selection_range: s.selection_range.as_lsp_type_range(),
                    target_uri: self.source_uri.clone()
                });
                return  Ok(result);
            },
            _=> Err(ProjectManagerError::new("Cannot find symbol definition", lsp_server::ErrorCode::RequestFailed))
        }
    }

    fn handle_terminal(
        &mut self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        if let Some(term_node) = node.data.as_any().downcast_ref::<AstTerminal>(){
            // if part of dot ops, may need to check another class
            if self.is_bin_op_dot_ops(&node.parent){
                let bin_op_parent = match node.parent.as_ref().unwrap().upgrade(){
                    Some(p) => p,
                    None=> return Some(Err(ProjectManagerError::new("Failed to upgrade wek ref", ErrorCode::InternalError)))
                };
                let parent_lock = bin_op_parent.read().unwrap();
                // should be safe to unwrap
                let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
                let left_node = &bin_op_node.left_node;
                if Arc::ptr_eq(left_node, &node.data){
                    // if left node, just search sym table
                    return Some(self.generate_loc_link(node, st))
                } else {
                    // if right node, check left node type first
                    let mut type_resolver = TypeResolver::new(self.semantic_analysis_service.clone());
                    let left_node_type = type_resolver.resolve_annotated_node_lock_type(&parent_lock.children[0].read().unwrap(), st);
                    match left_node_type{
                        EvalType::Class(class_name)=>{
                            // search right node in class
                            let uri = match self.doc_service.read().unwrap().get_uri_for_class(&class_name){
                                Ok(u) => u,
                                _=> return None,
                            };
                            // if class is self don't call sem service, because it will fail
                            let class_sym_table = if uri == self.source_uri{
                                st.clone()
                            } else {
                                match self.semantic_analysis_service.get_symbol_table_class_def_only(&class_name){
                                    Ok(st) => st,
                                    _=> return None
                                }
                            };
                         
                            let (class, sym_info) = self.type_resolver.search_sym_info_w_class(&bin_op_node.right_node.get_identifier(), &class_sym_table, false)?;
                            let target_uri = match self.doc_service.read().unwrap().get_uri_for_class(&class){
                                Ok(u) => u,
                                _=> return None,
                            };
                            let mut result = Vec::new();
                            result.push(LocationLink{
                                origin_selection_range: Some(node.data.get_range().as_lsp_type_range()),
                                target_uri,
                                target_selection_range: sym_info.selection_range.as_lsp_type_range(),
                                target_range: sym_info.range.as_lsp_type_range()
                            });
                            return Some(Ok(result));
                           
                        },
                        _=> return None,
                    }
                }
            } else {
                // enough to check symbol table in current doc
                return Some(self.generate_loc_link(node, st))
            }
        } else {return None}
    }

    fn handle_type_basic(
        &mut self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        let type_node = match node.data.as_any().downcast_ref::<AstTypeBasic>(){
            Some(n) => n,
            _=> return None,
        };

        if let Some((class,sym)) = self.type_resolver.search_sym_info_w_class(&type_node.get_identifier(), st, true){
            let uri = match self.doc_service.read().unwrap().get_uri_for_class(&class){
                Ok(u) => u,
                _=> return None
            };
            let mut result = Vec::new();
            result.push(LocationLink{
                origin_selection_range: Some(node.data.get_range().as_lsp_type_range()),
                target_range: sym.range.as_lsp_type_range(),
                target_selection_range: sym.selection_range.as_lsp_type_range(),
                target_uri: uri
            });
            return  Some(Ok(result));
        } else {return None}
    }

    fn handle_class(
        &mut self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        let class_node = match node.data.as_any().downcast_ref::<AstClass>(){
            Some(n) => n,
            _=> return None,
        };
        let id;
        let origin_range;
        match class_node.parent_class.as_ref() {
            Some(t) =>{
                if t.get_range().contains_pos(pos){
                    id = t.get_value();
                    origin_range= t.get_range();
                } else {
                    return None
                }
            },
            _=>{
                return None
            }
        }
        if let Some((class, sym)) = self.type_resolver.search_sym_info_w_class(&id, st, false){
            let uri = match self.doc_service.read().unwrap().get_uri_for_class(&class){
                Ok(u) => u,
                _=> return None,
            };
            let mut result = Vec::new();
            result.push(LocationLink{
                origin_selection_range: Some(origin_range.as_lsp_type_range()),
                target_range: sym.range.as_lsp_type_range(),
                target_selection_range: sym.selection_range.as_lsp_type_range(),
                target_uri: uri
            });
            return  Some(Ok(result));
        } else {return None}
    }

    fn handle_node(
        &mut self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    )-> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>{
        let r_node = node.read().unwrap();
        if let Some(r)= self.handle_type_basic(&r_node, st){
            return  r;
        }
        if let Some(r)= self.handle_terminal(&r_node, st){
            return  r;
        }
        if let Some(r)= self.handle_class(&r_node, st, pos){
            return  r;
        }
        return Ok(Vec::new());
    }

    pub fn get_definition(&mut self, pos : &Position)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri, false)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let root_st = doc.lock().unwrap().symbol_table.as_ref().unwrap().clone();
        
        let enc_node =self.search_encasing_node(&ast, &pos);
        self.logger.log_info(format!("[Req Definition] Node: {}", enc_node.read().unwrap().data.get_identifier()).as_str());
        let st = TypeResolver::get_nearest_symbol_table(&enc_node.read().unwrap(), &root_st);

        return self.handle_node(&enc_node, &st, pos)
    }
}

#[cfg(test)]
mod test{
    use crate::{manager::test::{create_test_project_manager, create_test_type_resolver, create_uri_from_path, create_test_sem_service, create_test_def_service}, utils::Position};


    #[test]
    fn test_resolve_local_variable(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(21, 12);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        assert_eq!(loc.target_uri, test_input);
    }

    #[test]
    fn test_resolve_local_variable_left_bin(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(23, 15);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        assert_eq!(loc.target_uri, test_input);
    }

    #[test]
    fn test_resolve_local_variable_right_bin(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(23, 35);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aSecondClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }


    #[test]
    fn test_resolve_self(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(24, 14);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_resolve_self_field(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(24, 21);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_resolve_type(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(17, 29);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aSecondClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }


    #[test]
    fn test_parent_field(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aSecondClass.god");
        // pos input to test, local var
        let pos_input = Position::new(7, 13);

        let mut def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }
}