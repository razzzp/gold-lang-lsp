use std::sync::{Arc, Mutex};

use lsp_server::ErrorCode;
use lsp_types::{Url, TypeHierarchyItem, SymbolKind};

use crate::utils::{ILoggerV2, Position};

use super::{semantic_analysis_service::SemanticAnalysisService, entity_tree_service::{EntityTreeService, EntityInfoNode}, data_structs::ProjectManagerError, utils::{search_encasing_node, search_sym_info_for_node}, symbol_table::SymbolType};



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
    -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>{
        let doc = self.sem_service.analyze_uri(uri, false)?;
        let root_ast = &doc
            .lock().unwrap()
            .annotated_ast.as_ref()
            .ok_or(ProjectManagerError::new("Annotated Root AST is None", lsp_server::ErrorCode::InternalError))?
            .clone();
        let enc_node = search_encasing_node(&root_ast, pos);
        let (class, sym_info) = match search_sym_info_for_node(&enc_node, &self.sem_service){
            Some(r) => r,
            _=> return Ok(Vec::new())
        };
        match &sym_info.sym_type{
            SymbolType::Class => {
                let type_h_item= TypeHierarchyItem{
                    name: sym_info.id.to_string(),
                    kind: lsp_types::SymbolKind::CLASS,
                    tags: None,
                    detail: None,
                    uri: uri.clone(),
                    range: sym_info.range.as_lsp_type_range(),
                    selection_range: sym_info.selection_range.as_lsp_type_range(),
                    data:None
                };
                return Ok(vec![type_h_item]);
            }
            SymbolType::Func|SymbolType::Proc => {
                let func_def_uri = self.sem_service.doc_service.get_uri_for_class(&class)?;
                let type_h_item= TypeHierarchyItem{
                    name: sym_info.id.to_string(),
                    kind: lsp_types::SymbolKind::FUNCTION,
                    tags: None,
                    detail: Some(class),
                    uri: func_def_uri,
                    range: sym_info.range.as_lsp_type_range(),
                    selection_range: sym_info.selection_range.as_lsp_type_range(),
                    data:None
                };
                return Ok(vec![type_h_item]);
            }
            _=> return Ok(Vec::new())
        }
    }


    fn generate_entity_type_hierarchy_item(&self, entity_info: &Arc<Mutex<EntityInfoNode>>) -> Option<TypeHierarchyItem>{
        let lock = entity_info.lock().unwrap();
        let sym_table = self.sem_service.get_symbol_table_for_class_def_only(&lock.id).ok()?;
        let uri = self.sem_service.doc_service.get_uri_for_class(&lock.id).ok()?;
        let entity_sym_info = sym_table.lock().unwrap().get_symbol_info(&lock.id)?;
        return Some(TypeHierarchyItem { 
            name: lock.id.clone(), 
            kind: SymbolKind::CLASS, 
            tags: None, 
            detail: None, 
            uri, 
            range: entity_sym_info.range.as_lsp_type_range(), 
            selection_range: entity_sym_info.selection_range.as_lsp_type_range(), 
            data: None 
        });
    }

    fn generate_method_subtypes_for_entity(&self, entity_info: &Arc<Mutex<EntityInfoNode>>, id: &String)
    -> Option<Vec<TypeHierarchyItem>>{
        let sym_table = self.sem_service.get_symbol_table_for_class_def_only(&entity_info.lock().unwrap().id).ok()?;
        let mut result = Vec::new();
        // search symbol only at one level
        if let Some((class, found_sym)) = sym_table.lock().unwrap().search_symbol_info(id) {
            // if found push to result
            let uri = self.sem_service.doc_service.get_uri_for_class(&class).ok()?;
            result.push(TypeHierarchyItem{
                name: found_sym.id.to_string(),
                uri,
                detail: Some(class.clone()),
                kind: SymbolKind::FUNCTION,
                range: found_sym.range.as_lsp_type_range(),
                selection_range: found_sym.selection_range.as_lsp_type_range(),
                tags: None,
                data: None,
            });
            return Some(result);
        }
        // if not found in current level check children
        for child in &entity_info.lock().unwrap().children{
            match self.generate_method_subtypes_for_entity(&child, id) {
                Some(items) => {result.extend(items);},
                _=> ()
            }
        }
        return Some(result);
    }

    fn generate_method_subtypes(&self, uri : &Url, id: &String)
    -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>{
        let sym_table = self.sem_service.get_symbol_table_for_uri_def_only(uri)?;
        let class_id = sym_table.lock().unwrap().get_class()
            .ok_or(ProjectManagerError::new(format!("Cannot find class for uri {}", uri).as_str(), ErrorCode::InvalidRequest))?;
        let entity_info = match self.entity_tree_service.get_entity(&class_id){
            Some(e) => e,
            _=> return Ok(Vec::new())
        };
        let mut result = Vec::new();
        let children = entity_info.lock().unwrap().children.clone();
        for child in &children {
            match self.generate_method_subtypes_for_entity(&child, id){
                Some(items) => {result.extend(items);},
                _=> ()
            }
        }
        return Ok(result)
    }

    pub fn type_hierarchy_subtypes(&self, item : &TypeHierarchyItem)
    -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>{
        if item.kind == SymbolKind::CLASS{
            let entity_info = match self.entity_tree_service.get_entity(&item.name){
                Some(e) => e,
                _=> return Ok(Vec::new())
            };
            let mut result = Vec::new();
            let children = entity_info.lock().unwrap().children.clone();
            for child in &children{
                match self.generate_entity_type_hierarchy_item(child){
                    Some(item) => result.push(item),
                    _=> ()
                }
            }
            return Ok(result);
        } else if item.kind == SymbolKind::FUNCTION{
            return self.generate_method_subtypes(&item.uri, &item.name);
        } else {
            return Ok(Vec::new());
        }
    }


    fn generate_method_supertypes_for_entity(&self, entity_info: &Arc<Mutex<EntityInfoNode>>, id: &String)
    -> Option<TypeHierarchyItem>{
        let sym_table = self.sem_service.get_symbol_table_for_class_def_only(&entity_info.lock().unwrap().id).ok()?;
        // search symbol only at one level
        if let Some((class, found_sym)) = sym_table.lock().unwrap().search_symbol_info(id) {
            // if found push to result
            let uri = self.sem_service.doc_service.get_uri_for_class(&class).ok()?;
            return Some(TypeHierarchyItem{
                name: found_sym.id.to_string(),
                uri,
                detail: Some(class.clone()),
                kind: SymbolKind::FUNCTION,
                range: found_sym.range.as_lsp_type_range(),
                selection_range: found_sym.selection_range.as_lsp_type_range(),
                tags: None,
                data: None,
            });
        } 

        // if not found in current level check parent
        let parent = match &entity_info.lock().unwrap().parent {
            Some(p) => p.upgrade()?,
            _=> return None
        };
        return self.generate_method_supertypes_for_entity(&parent, id);
    }


    fn generate_method_supertypes(&self, uri : &Url, id: &String)
    -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>{
        let sym_table = self.sem_service.get_symbol_table_for_uri_def_only(uri)?;
        let class_id = sym_table.lock().unwrap().get_class()
            .ok_or(ProjectManagerError::new(format!("Cannot find class for uri {}", uri).as_str(), ErrorCode::InvalidRequest))?;
        let entity_info = match self.entity_tree_service.get_entity(&class_id){
            Some(e) => e,
            _=> return Ok(Vec::new())
        };
        let mut result = Vec::new();
        let parent = match &entity_info.lock().unwrap().parent {
            Some(p) => p.upgrade()
                .ok_or(ProjectManagerError::new(format!("Cannot upgrade parent for {}", class_id).as_str(), ErrorCode::InternalError))?,
            _=> return Ok(Vec::new())
        };
        match self.generate_method_supertypes_for_entity(&parent, id){
            Some(item) => {result.push(item)},
            _=> ()
        }
        
        return Ok(result)
    }

    pub fn type_hierarchy_supertypes(&self, item : &TypeHierarchyItem)
    -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>{
        if item.kind == SymbolKind::CLASS{
            let entity_info = match self.entity_tree_service.get_entity(&item.name){
                Some(e) => e,
                _=> return Ok(Vec::new())
            };
            let mut result = Vec::new();
            let parent = entity_info.lock().unwrap().parent.clone();
            if let Some(parent) = parent {
                let parent = parent.upgrade()
                .ok_or(ProjectManagerError::new(format!("Failed to get parent {}", &item.name).as_str(), ErrorCode::InternalError))?;
            
                match self.generate_entity_type_hierarchy_item(&parent){
                    Some(item) => result.push(item),
                    _=> ()
                }
            }
            return Ok(result);
        } else if item.kind == SymbolKind::FUNCTION{
            return self.generate_method_supertypes(&item.uri, &item.name);
        } else {
            return Ok(Vec::new());
        }
    }
}

#[cfg(test)]
mod test{
    use lsp_types::{TypeHierarchyItem, SymbolKind};

    use crate::manager::document_service::DocumentService;
    use crate::manager::entity_tree_service::EntityTreeService;
    use crate::manager::test::{create_test_sem_service, create_test_logger, create_test_doc_service, create_uri_from_path};
    use crate::manager::entity_tree_service::test::create_test_entity_tree_service;
    use crate::utils::Position;
    use super::TypeHierarchyService;


    pub fn create_test_type_hierarchy_service(
        doc_service: DocumentService, 
        tree_service: EntityTreeService
    )->TypeHierarchyService{
        return TypeHierarchyService::new(
            create_test_sem_service(doc_service), 
            tree_service,
            create_test_logger()
        )
    }

    #[test]
    fn test_class_type_hierarchy(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        let tree_service = create_test_entity_tree_service();
        doc_service.index_files();
        tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), tree_service);

        let test_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        let test_pos = Position::new(0, 13);
        let result = th_service.prepare_type_hierarchy(&test_uri, &test_pos).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aFifthClass");
    }

    #[test]
    fn test_class_type_hierarchy_subtypes(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        doc_service.index_files();
        let entity_tree_service = create_test_entity_tree_service();
        entity_tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), entity_tree_service);

        let test_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        let test_item = TypeHierarchyItem{
            name:"aFifthClass".to_string(),
            kind: SymbolKind::CLASS,
            uri: test_uri,
            range: lsp_types::Range::default(),
            selection_range: lsp_types::Range::default(),
            tags: None,
            detail: None,
            data: None,
        };
        let result = th_service.type_hierarchy_subtypes(&test_item).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aCompletionTest");

        
    }

    #[test]
    fn test_class_type_hierarchy_supertypes(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        doc_service.index_files();
        let entity_tree_service = create_test_entity_tree_service();
        entity_tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), entity_tree_service);

        let test_uri = create_uri_from_path("./test/workspace/aFifthClass.god");
        let test_item = TypeHierarchyItem{
            name:"aFifthClass".to_string(),
            kind: SymbolKind::CLASS,
            uri: test_uri,
            range: lsp_types::Range::default(),
            selection_range: lsp_types::Range::default(),
            tags: None,
            detail: None,
            data: None,
        };
        let result = th_service.type_hierarchy_supertypes(&test_item).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aFourthClass");
    }


    #[test]
    fn test_method_type_hierarchy_prepare(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        let tree_service = create_test_entity_tree_service();
        doc_service.index_files();
        tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), tree_service);

        let test_uri = create_uri_from_path("./test/workspace/TypeHierarchyTest/aFirstLevelClass1.god");
        let test_pos = Position::new(2, 18);
        let result = th_service.prepare_type_hierarchy(&test_uri, &test_pos).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aFirstLevelClass1Proc");
    }

    #[test]
    fn test_method_type_hierarchy_subtypes(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        let tree_service = create_test_entity_tree_service();
        doc_service.index_files();
        tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), tree_service);

        let test_uri = create_uri_from_path("./test/workspace/TypeHierarchyTest/aFirstLevelClass1.god");
        let test_item = TypeHierarchyItem{
            name:"aFirstLevelClass1Proc".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: test_uri,
            range: lsp_types::Range::default(),
            selection_range: lsp_types::Range::default(),
            tags: None,
            detail: None,
            data: None,
        };

        let result = th_service.type_hierarchy_subtypes(&test_item).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].name.as_str(), "aFirstLevelClass1Proc");
    }

    #[test]
    fn test_method_type_hierarchy_supertypes(){
        let uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(uri));
        let tree_service = create_test_entity_tree_service();
        doc_service.index_files();
        tree_service.build_tree(&doc_service);
        let th_service = create_test_type_hierarchy_service(doc_service.clone(), tree_service);

        let test_uri = create_uri_from_path("./test/workspace/TypeHierarchyTest/aSecondLevelClass1.god");
        let test_item = TypeHierarchyItem{
            name:"aFirstLevelClass1Proc".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: test_uri,
            range: lsp_types::Range::default(),
            selection_range: lsp_types::Range::default(),
            tags: None,
            detail: None,
            data: None,
        };

        let result = th_service.type_hierarchy_supertypes(&test_item).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name.as_str(), "aFirstLevelClass1Proc");
    }
}