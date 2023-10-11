

use std::{sync::{Arc, RwLock, Mutex, RwLockWriteGuard, RwLockReadGuard, Weak}, any::Any};

use lsp_server::ErrorCode;
use lsp_types::{LocationLink, Url};

use crate::{parser::ast::{IAstNode, AstTerminal, AstBinaryOp}, utils::Position, lexer::tokens::TokenType, manager::type_resolver};

use super::{ProjectManager, data_structs::{ProjectManagerError, Document}, annotated_node::{AnnotatedNode, EvalType}, semantic_analysis_service::{ISymbolTable, SemanticAnalysisService}, type_resolver::TypeResolver, document_service::DocumentService};



pub struct DefinitionService{
    doc_service : Arc<RwLock<DocumentService>>,
    semantic_analysis_service: SemanticAnalysisService,
    type_resolver: TypeResolver,
    source_uri: Url
}
impl DefinitionService{
    pub fn new(doc_service : Arc<RwLock<DocumentService>>,semantic_analysis_service: SemanticAnalysisService, source_uri: &Url)->DefinitionService{
        DefinitionService { 
            doc_service,
            type_resolver: TypeResolver::new(semantic_analysis_service.clone()),
            semantic_analysis_service: semantic_analysis_service,
            source_uri: source_uri.clone()
        }
    }

    fn search_encasing_node(&self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, pos : &Position)
    -> Arc<RwLock<AnnotatedNode<dyn IAstNode>>>
    {
        let r_node = node.read().unwrap();
        for child in &r_node.children{
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

    fn generate_loc_link(&self, node:  &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, root_st: &Arc<Mutex<dyn ISymbolTable>>)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        // enough to check symbol table in current doc
        let st = TypeResolver::get_nearest_symbol_table(node, root_st);
        let mut st_lock =st.lock().unwrap();
        match st_lock.get_symbol_info(&node.data.get_identifier().to_uppercase()){
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
        &self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        root_st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        if let Some(term_node) = node.data.as_any().downcast_ref::<AstTerminal>(){
            // if part of dot ops, may need to check another class
            if self.is_bin_op_dot_ops(&node.parent){
                let bin_op_parent = match node.parent.as_ref().unwrap().upgrade(){
                    Some(p) => p,
                    None=> return Some(Err(ProjectManagerError::new("Failed to upgrade wek ref", ErrorCode::InternalError)))
                };
                let parent_lock = bin_op_parent.read().unwrap();
                // should be safe to unlock
                let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
                let left_node = &bin_op_node.left_node;
                if Arc::ptr_eq(left_node, &node.data){
                    // if left node, just search sym table
                    return Some(self.generate_loc_link(node, root_st))
                } else {
                    // if right node, check left node type first
                    let type_resolver = TypeResolver::new(self.semantic_analysis_service.clone());
                    let left_node_type = type_resolver.resolve_annotated_node_lock_type(&parent_lock.children[0].read().unwrap(), root_st);
                    match left_node_type{
                        EvalType::Class(class_name)=>{
                            // search right node in class
                            let uri = match self.doc_service.read().unwrap().get_uri_for_class(&class_name){
                                Ok(u) => u,
                                _=> return None,
                            };
                            let class_sym_table = match self.semantic_analysis_service.get_symbol_table_for_class(&class_name){
                                Ok(st) => st,
                                _=> return None
                            };
                            let sym_info = class_sym_table.lock().unwrap().get_symbol_info(&bin_op_node.right_node.get_identifier());

                            match sym_info {
                                Some(s) => {
                                    let mut result = Vec::new();
                                    result.push(LocationLink{
                                        origin_selection_range: Some(node.data.get_range().as_lsp_type_range()),
                                        target_uri: uri,
                                        target_selection_range: s.selection_range.as_lsp_type_range(),
                                        target_range: s.range.as_lsp_type_range()
                                    });
                                    return Some(Ok(result));
                                },
                                _=> return None
                            }
                        },
                        _=> return None,
                    }
                }
            } else {
                // enough to check symbol table in current doc
                return Some(self.generate_loc_link(node, root_st))
            }
        } else {return None}
    }

    fn handle_node(
        &self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, 
        root_st: &Arc<Mutex<dyn ISymbolTable>>
    )-> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>{
        let r_node = node.read().unwrap();
        if let Some(r)= self.handle_terminal(&r_node, root_st){
            return  r;
        }
        return Ok(Vec::new());
    }

    pub fn get_definition(&self, pos : &Position)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri)?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();
        let root_st = doc.lock().unwrap().symbol_table.as_ref().unwrap().clone();
        let enc_node =self.search_encasing_node(&ast, &pos);

        return self.handle_node(&enc_node, &root_st)
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
        let pos_input = Position::new(19, 15);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
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
        let pos_input = Position::new(21, 15);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
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
        let pos_input = Position::new(21, 35);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aSecondClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }
}