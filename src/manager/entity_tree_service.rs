use std::{collections::HashMap, sync::{Mutex, Weak, Arc, RwLock, RwLockWriteGuard}, str::FromStr};

use lsp_types::Url;

use crate::{threadpool::ThreadPool, utils::{ILoggerV2, LogLevel, LogType}};

use super::{data_structs::DocumentInfo, document_service::{DocumentService, GetParsedDocumentOptions}};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum EntityType {
    Class,
    Module
}

#[derive(Debug, Clone)]
pub struct EntityInfoNode{
    pub id: String,
    pub parent: Option<Weak<Mutex<EntityInfoNode>>>,
    pub children: Vec<Arc<Mutex<EntityInfoNode>>>,
    pub entity_type: EntityType,
}
impl EntityInfoNode{
    pub fn new(id: &str, entity_type : EntityType) -> EntityInfoNode {
        return EntityInfoNode { id: id.to_string(), parent: None, children: Vec::new(), entity_type }
    }
}

#[derive(Debug)]
pub struct EntityTreeService{
    class_module_map : Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfoNode>>>>>,
    logger : Box<dyn ILoggerV2>,
    num_of_chunks: usize
}
impl Clone for EntityTreeService{
    fn clone(&self) -> Self {
        Self { class_module_map: self.class_module_map.clone(), logger: self.logger.clone_box(), num_of_chunks: self.num_of_chunks }
    }
}
impl EntityTreeService{
    pub fn new(num_of_chunks: usize, logger: Box<dyn ILoggerV2>) -> EntityTreeService{
        return EntityTreeService { 
            class_module_map: Arc::new(RwLock::new(HashMap::new())),
            logger,
            num_of_chunks
        }
    }

    pub fn get_class_module_map(&self) -> &Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfoNode>>>>>{
        &self.class_module_map
    }

    fn get_or_create_entity(
        id: &str, 
        map_lock: &mut RwLockWriteGuard<'_, HashMap<String, Arc<Mutex<EntityInfoNode>>>>
    ) ->  Arc<Mutex<EntityInfoNode>> {
        let id_upper = id.to_uppercase();
        if let Some(entity) = map_lock.get(&id_upper){
            return entity.clone()
        } else {
            let new_entity = Arc::new(Mutex::new(EntityInfoNode::new(id, EntityType::Class)));
            map_lock.insert(id_upper.to_uppercase(), new_entity.clone());
            return new_entity
        };
    }

    fn get_or_create_entity_2(
        id: &str, 
        map: &Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfoNode>>>>>
    ) ->  Arc<Mutex<EntityInfoNode>> {
        let id_upper = id.to_uppercase();
        if let Some(entity) = map.read().unwrap().get(&id_upper){
            return entity.clone()
        } 

        let new_entity = Arc::new(Mutex::new(EntityInfoNode::new(id, EntityType::Class)));
        map.write().unwrap().insert(id_upper.to_uppercase(), new_entity.clone());
        return new_entity
    }

    pub fn build_tree_parallel(&self, doc_service: &DocumentService, threadpool: &ThreadPool){
        
        let timer = std::time::Instant::now();
        self.logger.log_info("Building Class tree");

        self.class_module_map.write().unwrap().clear();

        let files_to_process : Vec<String> = doc_service
            .get_doc_info_mapping()
            .read().unwrap()
            .keys().cloned().collect();

        // calculate chunk size 
        let chunk_size = files_to_process.len() / self.num_of_chunks;
        let chunks: Vec<Vec<_>> = files_to_process.chunks(chunk_size).map(|s| s.into()).collect();

        for chunk in chunks {
            let map= self.class_module_map.clone();
            let logger = self.logger.clone_box();
            let doc_service = doc_service.clone();
            threadpool.execute(move ||{
                EntityTreeService::build_tree(&doc_service, &chunk, &map, &logger);
                logger.log_info(format!("Building class tree {:#?}; Processed {} entities", timer.elapsed(), map.read().unwrap().len()).as_str());
            });
        }
    }

    pub fn build_tree(
        doc_service: &DocumentService, 
        file_uris: &Vec<String>,
        map : &Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfoNode>>>>>,
        logger: &Box<dyn ILoggerV2>
    ){
        file_uris.iter().for_each( |path: &String| {
            let uri = match Url::from_file_path(path){
                Ok(uri) => uri,
                _=> {
                    logger.log(LogType::Warning, LogLevel::Verbose, 
                        format!("Failed to create uri from path {}", path).as_str());
                    return;
                }
            };
            
            // don't cache result
            if let Ok(doc) = doc_service.get_parsed_document(
                &uri, 
                GetParsedDocumentOptions::default().set_wait_on_lock(true)
            ){
                // generate from entity info
                if let Some(e_info) = &doc.lock().unwrap().entity_info {
                    let entity = EntityTreeService::get_or_create_entity_2(&e_info.id, &map);      
                    if let Some(parent_class) = &e_info.parent{
                        // if parent doesn't exist
                        let parent_entity  = EntityTreeService::get_or_create_entity_2(parent_class, &map);
                        // set parent
                        entity.lock().unwrap().parent = Some(Arc::downgrade(&parent_entity));
                        // add as child
                        parent_entity.lock().unwrap().children.push(entity.clone());
                    }
                } else {
                    // self.logger.log_warning(format!("Can't find entity info for {}", doc_info.read().unwrap().uri).as_str())
                }
                // free memory
                drop(doc);
                return;
            } 
        });
    }

    pub fn get_root_class(&self) -> Option<Arc<Mutex<EntityInfoNode>>>{
        let map_lock =self.class_module_map.read().unwrap();
        let result = map_lock.values().find(|e|{
            // root class is one without parent
            let lock = e.lock().unwrap();
            return (lock.entity_type == EntityType::Class) && lock.parent.is_none() && lock.children.len()>0;
        }).clone();
        return result.map(|v| {v.clone()})
    }

    pub fn get_entity(&self, id: &str) -> Option<Arc<Mutex<EntityInfoNode>>>{
        return self.class_module_map.read().unwrap().get(&id.to_uppercase()).cloned();
    }
}

#[cfg(test)]
pub mod test{
    use crate::{manager::{document_service::GetParsedDocumentOptions, test::{create_test_doc_service, create_test_logger, create_uri_from_path}}, threadpool::ThreadPool, utils::Logger};

    use super::EntityTreeService;


    pub fn create_test_entity_tree_service() -> EntityTreeService{
        return EntityTreeService::new(4, create_test_logger())
    }

    #[test]
    fn test_generate_tree(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let entity_tree_service = create_test_entity_tree_service();
        let logger = create_test_logger();
        let file_uris = doc_service.get_doc_info_mapping().read().unwrap().keys().cloned().collect();
        EntityTreeService::build_tree(&doc_service, &file_uris, &entity_tree_service.class_module_map, &logger);

        let root_class = entity_tree_service.get_root_class().unwrap();
        assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }

    #[test]
    fn test_generate_tree_parsed(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        let _ = doc_service.get_parsed_document_for_class("aRootClass", 
        GetParsedDocumentOptions::default().set_cache_result(true).set_wait_on_lock(true));
        
        let entity_tree_service = create_test_entity_tree_service();
        let logger = create_test_logger();
        let file_uris = doc_service.get_doc_info_mapping().read().unwrap().keys().cloned().collect();
        EntityTreeService::build_tree(&doc_service, &file_uris, &entity_tree_service.class_module_map, &logger);

        let root_class = entity_tree_service.get_root_class().unwrap();
        assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }

    #[ignore = "long running time"]
    #[test]
    fn test_generate_tree_large(){
        let root_uri = create_uri_from_path("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let entity_tree_service = create_test_entity_tree_service();
        let logger = create_test_logger();
        let file_uris = doc_service.get_doc_info_mapping().read().unwrap().keys().cloned().collect();
        EntityTreeService::build_tree(&doc_service, &file_uris, &entity_tree_service.class_module_map, &logger);

        let _root_class = entity_tree_service.get_root_class().unwrap();
        // assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }

    #[ignore = "long running time"]
    #[test]
    fn test_generate_tree_large_parallel(){
        let root_uri = create_uri_from_path("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let threads = ThreadPool::new(4, create_test_logger());
        let class_service = create_test_entity_tree_service();

        class_service.build_tree_parallel(&doc_service, &threads);

        drop(threads);
        let _root_class = class_service.get_root_class().unwrap();
        // assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }
}