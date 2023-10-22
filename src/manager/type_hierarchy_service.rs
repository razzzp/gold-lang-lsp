use std::sync::Arc;

use lsp_types::{Url, TypeHierarchyItem};

use crate::utils::{ILoggerV2, Position};

use super::{semantic_analysis_service::SemanticAnalysisService, entity_tree_service::EntityTreeService, data_structs::ProjectManagerError};



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
        return Ok(None);
    }
}