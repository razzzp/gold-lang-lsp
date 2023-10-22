use std::sync::Arc;

use lsp_types::{Url, TypeHierarchyItem, SymbolKind};

use crate::{utils::{ILoggerV2, Position}, parser::ast::AstClass};

use super::{semantic_analysis_service::SemanticAnalysisService, entity_tree_service::EntityTreeService, data_structs::ProjectManagerError, utils::{search_encasing_node, search_sym_info_for_node}, annotated_node::EvalType};



#[derive(Debug, Clone)]
pub struct TypeHierarchyService{
    sem_service: SemanticAnalysisService,
    entity_tree_service: EntityTreeService,
    logger : Arc<dyn ILoggerV2>,
}
impl TypeHierarchyService{
    pub fn new(
        sem_service: SemanticAnalysisService,
        entity_tree_service : EntityTreeService,
        logger: Arc<dyn ILoggerV2>
    )-> TypeHierarchyService{
        return TypeHierarchyService { 
            sem_service, 
            entity_tree_service, 
            logger 
        }
    }

    pub fn prepare_type_hierarchy(&self, uri : &Url, pos: &Position)
    -> Result<Option<Vec<TypeHierarchyItem>>, ProjectManagerError>{
        let doc = self.sem_service.analyze_uri(uri, false)?;
        let root_ast = &doc
            .lock().unwrap()
            .annotated_ast.as_ref()
            .ok_or(ProjectManagerError::new("Annotated Root AST is None", lsp_server::ErrorCode::InternalError))?
            .clone();
        let enc_node = search_encasing_node(&root_ast, pos);
        let (_class, sym_info) = match search_sym_info_for_node(&enc_node, &self.sem_service){
            Some(r) => r,
            _=> return Ok(None)
        };
        let eval_type = match &sym_info.eval_type{
            Some(e) => e,
            _=> return Ok(None)
        };
        match eval_type{
            EvalType::Class(s) => {
                let type_h_item= TypeHierarchyItem{
                    name: s.to_string(),
                    kind: lsp_types::SymbolKind::CLASS,
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range: sym_info.range.as_lsp_type_range(),
                    selection_range: sym_info.selection_range.as_lsp_type_range(),
                    data:None
                };
                return Ok(Some(vec![type_h_item]));
            }
            _=> return Ok(None)
        }
    }
}

#[cfg(test)]
mod test{
    use crate::manager::document_service::DocumentService;
    use crate::manager::test::{create_test_sem_service, create_test_logger, create_test_doc_service, create_uri_from_path};
    use crate::manager::entity_tree_service::test::create_test_entity_tree_service;
    use crate::utils::Position;
    use super::TypeHierarchyService;


    pub fn create_test_type_hierarchy_service(doc_service: DocumentService)->TypeHierarchyService{
        return TypeHierarchyService::new(
            create_test_sem_service(doc_service), 
            create_test_entity_tree_service(),
            create_test_logger()
        )
    }

    #[test]
    fn test_class_type_hierarchy(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        doc_service.index_files();
        let th_service = create_test_type_hierarchy_service(doc_service.clone());

        let test_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        let test_pos = Position::new(0, 13);
        let result = th_service.prepare_type_hierarchy(&test_uri, &test_pos).unwrap().unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aFifthClass");
    }
}