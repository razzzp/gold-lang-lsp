

use std::sync::{Arc, RwLock, Mutex, RwLockReadGuard};

use lsp_server::ErrorCode;
use lsp_types::{LocationLink, Url};

use crate::{parser::ast::{IAstNode, AstTerminal, AstBinaryOp, AstTypeBasic, AstClass, AstTypeReference, AstMethodCall, AstProcedure, AstFunction, AstGlobalVariableDeclaration}, utils::{Position, IRange, ILoggerV2}, lexer::tokens::TokenType};

use super::{data_structs::ProjectManagerError, semantic_analysis_service::{SemanticAnalysisService, AnalyzeRequestOptions}, document_service::DocumentService, utils::search_encasing_node};
use crate::analyzers_v2::{
    annotated_node::{AnnotatedNode, EvalType},
    type_resolver::TypeResolver,
    symbol_table::ISymbolTable
};

pub struct DefinitionService{
    doc_service : DocumentService,
    semantic_analysis_service: SemanticAnalysisService,
    type_resolver: TypeResolver,
    source_uri: Url,
    logger: Arc<dyn ILoggerV2>,
}
impl DefinitionService{
    pub fn new(
        doc_service : DocumentService,
        semantic_analysis_service: SemanticAnalysisService, 
        logger: Arc<dyn ILoggerV2>,
        source_uri: &Url
    )->DefinitionService{
        return DefinitionService { 
            doc_service,
            // type_resovler should have its own session
            type_resolver: TypeResolver::new(semantic_analysis_service.clone()),
            semantic_analysis_service: semantic_analysis_service,
            logger,
            source_uri: source_uri.clone(),
        }
    }

    fn check_parent_dot_ops(&self, node : &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>)
    ->Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
    {
        let parent_node = node.parent.as_ref()?.upgrade()?;
        let lock = parent_node.read().unwrap();
        if let Some(bin_op) = lock.data.as_any().downcast_ref::<AstBinaryOp>(){
            if bin_op.op_token.token_type == TokenType::Dot{
                return Some(parent_node.clone());
            } else {return  None;}
        } else {
            return None;
        }
    }

    fn check_parent_method_decl(&self, node : &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>)
    ->Option<Arc<RwLock<AnnotatedNode<dyn IAstNode>>>>
    {
        let parent_node = node.parent.as_ref()?.upgrade()?;
        let lock = parent_node.read().unwrap();
        if let Some(_) = lock.data.as_any().downcast_ref::<AstProcedure>(){
            return Some(parent_node.clone());
        } else  if let Some(_) = lock.data.as_any().downcast_ref::<AstFunction>(){
            return Some(parent_node.clone());
        } else{
            return None
        }
    }

    fn check_is_gvar_decl(&self, node : &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>) -> bool
    {
        if let Some(_) = node.data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            return true
        } else{
            return false
        }
    }

    fn get_origin_selection_range(&self, node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, pos: &Position) -> Option<lsp_types::Range>{
        if let Some(node) = node.data.as_any().downcast_ref::<AstTerminal>(){
            return Some(node.get_range().as_lsp_type_range());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstTypeBasic>(){
            return Some(node.get_range().as_lsp_type_range());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstTypeReference>(){
            return Some(node.ident_token.get_range().as_lsp_type_range());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstClass>(){
            if node.parent_class.as_ref()?.get_range().contains_pos(pos){
                return Some(node.parent_class.as_ref()?.get_range().as_lsp_type_range());
            }
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstMethodCall>(){
            return Some(node.identifier.get_range().as_lsp_type_range());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            if node.identifier.get_range().contains_pos(pos){
                return Some(node.identifier.get_range().as_lsp_type_range());
            }
        }
        return None
    }

    fn get_id<'a>(&'a self, node: &'a RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, pos: &Position) -> Option<&'a str>{
        if let Some(node) = node.data.as_any().downcast_ref::<AstTerminal>(){
            return Some(node.get_identifier());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstTypeBasic>(){
            return Some(node.get_identifier());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstTypeReference>(){
            return Some(node.get_identifier());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstClass>(){
            if node.parent_class.as_ref()?.get_range().contains_pos(pos){
                return Some(node.parent_class.as_ref()?.get_value_as_str());
            }
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstMethodCall>(){
            return Some(node.get_identifier());
        }
        if let Some(node) = node.data.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            if node.identifier.get_range().contains_pos(pos){
                return Some(node.identifier.get_value_as_str());
            }
        }
        return None
    }

    fn generate_loc_link_single(
        &self, 
        node:  &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos : &Position,
        search_uses: bool
    )
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let origin_selection_range = self.get_origin_selection_range(node, pos);
        let id = match self.get_id(node, pos){
            Some(id) => id,
            _=> return Err(ProjectManagerError::new("Cannot get id for node", lsp_server::ErrorCode::RequestFailed))
        };
        let (in_class, sym_info) = match self.type_resolver.search_sym_info_w_class(
            &id, 
            st, 
            search_uses
        ){
            Some(r) => r,
            _=> return Err(ProjectManagerError::new("Cannot find symbol definition", lsp_server::ErrorCode::RequestFailed))
        };

        let uri = match self.doc_service.get_uri_for_class(&in_class){
            Ok(u) => u,
            _=> return Err(ProjectManagerError::new("Cannot get uri for class", lsp_server::ErrorCode::RequestFailed)),
        };

        let mut result = Vec::new();
        result.push(LocationLink{
            origin_selection_range,
            target_range: sym_info.range.as_lsp_type_range(),
            target_selection_range: sym_info.selection_range.as_lsp_type_range(),
            target_uri: uri
        });
        return  Ok(result);

    }


    fn generate_loc_link_all(
        &self, 
        node:  &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos : &Position,
    )
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let origin_selection_range = self.get_origin_selection_range(node, pos);
        let id = match self.get_id(node, pos){
            Some(id) => id,
            _=> return Err(ProjectManagerError::new("Cannot get id for node", lsp_server::ErrorCode::RequestFailed))
        };
        let sym_infos= self.type_resolver.search_sym_info_through_parent(&id, st);

        let mut result = Vec::new();
        for (class, sym) in sym_infos{
            let uri = match self.doc_service.get_uri_for_class(&class){
                Ok(u) => u,
                _=> return Err(ProjectManagerError::new("Cannot get uri for class", lsp_server::ErrorCode::RequestFailed)),
            };
            result.push(LocationLink{
                origin_selection_range,
                target_range: sym.range.as_lsp_type_range(),
                target_selection_range: sym.selection_range.as_lsp_type_range(),
                target_uri: uri
            });
        }
        return  Ok(result);

    }

    fn generate_right_hand_of_entity(
        &self,
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        entity: &str,
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position,
    ) -> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>
    {
        // search right node in class
        let uri = match self.doc_service.get_uri_for_class(&entity){
            Ok(u) => u,
            _=> return None,
        };
        // if class is self don't call sem service, because it will fail
        let class_sym_table = if uri == self.source_uri{
            st.clone()
        } else {
            match self.semantic_analysis_service.get_symbol_table_for_class_def_only(&entity){
                Ok(st) => st,
                _=> return None
            }
        };
        // don't search uses because the member should be in the st itself
        return Some(self.generate_loc_link_all(node, &class_sym_table, pos))
    }

    fn handle_generic(
        &self, 
        node: &RwLockReadGuard<'_, AnnotatedNode<dyn IAstNode>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    )-> Option<Result<Vec<lsp_types::LocationLink>, ProjectManagerError>>{
        // if part of dot ops, may need to check another class
        if let Some(bin_op_parent) = self.check_parent_dot_ops(node){

            let parent_lock = bin_op_parent.read().unwrap();
            // should be safe to unwrap
            let bin_op_node = parent_lock.data.as_any().downcast_ref::<AstBinaryOp>().unwrap(); 
            let left_node = &bin_op_node.left_node;
            if Arc::ptr_eq(left_node, &node.data){
                // if left node, just search sym table
                return Some(self.generate_loc_link_single(node, st, pos, true))
            } 
            else {
                // if right node, check left node type first
                let left_node_type = parent_lock.children[0].read().unwrap().eval_type.clone().unwrap_or_default();
                match left_node_type{
                    EvalType::Class(class_name)=>{
                        // search right node in class
                        return self.generate_right_hand_of_entity(node, &class_name, st, pos);
                    },
                    EvalType::Module(module_name)=>{
                        // search right node in class
                        return self.generate_right_hand_of_entity(node, &module_name, st, pos);
                    },
                    _=> return None,
                }
            }
        } 
        else if let Some(_) = self.check_parent_method_decl(node){
            // handle like rhs, i.e. search through parent symbol tables
            return Some(self.generate_loc_link_all(node, st, pos));
        } 
        else if self.check_is_gvar_decl(node){
            return Some(self.generate_loc_link_all(node, st, pos));
        }
        else {
            // enough to check symbol table in current doc
            return Some(self.generate_loc_link_single(node, st, pos, true))
        }
    }

    fn handle_node(
        &self, 
        node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>, 
        st: &Arc<Mutex<dyn ISymbolTable>>,
        pos: &Position
    )-> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>{
        let r_node = node.read().unwrap();
        if let Some(r)= self.handle_generic(&r_node, st, pos){
            match r {
                Ok(r) => return Ok(r),
                Err(e) => {
                    self.logger.log_error(&e.to_string());
                }
            }
        }
        return Ok(Vec::new());
    }

    pub fn get_definition(&self, pos : &Position)
    -> Result<Vec<lsp_types::LocationLink>, ProjectManagerError>
    {
        let doc = self.semantic_analysis_service.analyze_uri(&self.source_uri, AnalyzeRequestOptions::default().set_cache(true))?;
        let ast = doc.lock().unwrap().annotated_ast.as_ref().unwrap().clone();

        self.logger.log_info(format!("[Req Definition] Searching encasing node").as_str());
        let enc_node = search_encasing_node(&ast, &pos, &self.logger);
        self.logger.log_info(format!("[Req Definition] Node: {}", enc_node.read().unwrap().data.get_identifier()).as_str());
        let st = match TypeResolver::get_nearest_symbol_table(&enc_node.read().unwrap()){
            Some(st) => st,
            _=> return Err(ProjectManagerError::new("Cannot find symbol table", ErrorCode::InternalError))
        };

        return self.handle_node(&enc_node, &st, pos)
    }
}

#[cfg(test)]
mod test{
    use crate::{manager::test::{create_test_project_manager, create_uri_from_path, create_test_def_service}, utils::Position};


    #[test]
    fn test_resolve_local_variable(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(21, 12);

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
        let pos_input = Position::new(23, 15);

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
        let pos_input = Position::new(23, 35);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aSecondClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_resolve_right_bin_twice(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(36, 40);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }


    #[test]
    fn test_resolve_self(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(24, 14);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
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

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
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

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
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

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_field_refto(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aRootClass.god");
        // pos input to test, local var
        let pos_input = Position::new(12, 25);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_method_calls(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aThirdClass.god");
        // pos input to test, local var
        let third_class_uri = create_uri_from_path("./test/workspace/aThirdClass.god");
        let root_class_uri = create_uri_from_path("./test/workspace/aRootClass.god");
        let pos_input = Position::new(10, 15);
        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        assert_eq!(loc.target_uri, third_class_uri);

        let pos_input = Position::new(11, 15);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        assert_eq!(loc.target_uri, third_class_uri);

        let pos_input = Position::new(11, 29);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        assert_eq!(loc.target_uri, root_class_uri);
    }

    #[test]
    fn test_module(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aFourthClass.god");
        // pos input to test, local var
        let pos_input = Position::new(9, 8);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aModule.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_module_member(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aFourthClass.god");
        // pos input to test, local var
        let pos_input = Position::new(9, 19);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aModule.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_multiple_def(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aFifthClass.god");
        // pos input to test, local var
        let pos_input = Position::new(9, 20);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 2);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFourthClass.god");
        assert_eq!(loc.target_uri, target_uri);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_proc_decl(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aFifthClass.god");
        // pos input to test, local var
        let pos_input = Position::new(7, 18);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_proc_decl_override(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let test_input = create_uri_from_path("./test/workspace/aFifthClass.god");
        // pos input to test, local var
        let pos_input = Position::new(14, 19);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 2);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFourthClass.god");
        assert_eq!(loc.target_uri, target_uri);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }

    #[test]
    fn test_other_calls(){
        let mut proj_manager = create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        
        let test_input = create_uri_from_path("./test/workspace/aFifthClass.god");
        let _ = proj_manager.generate_document_diagnostic_report(&test_input);
        // pos input to test, local var
        let pos_input = Position::new(7, 18);

        let def_service = create_test_def_service(proj_manager.doc_service.clone(), &test_input);
        let mut result = def_service.get_definition(&pos_input).unwrap();
        assert_eq!(result.len(), 1);
        let loc = result.pop().unwrap();
        let target_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        assert_eq!(loc.target_uri, target_uri);
    }
}