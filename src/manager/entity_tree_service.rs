use std::{collections::HashMap, sync::{Mutex, Weak, Arc, RwLock, RwLockWriteGuard}, fs::File, io::{BufReader, BufRead}, str::FromStr};

use lsp_types::Url;
use regex::Regex;

use crate::{utils::ILoggerV2, threadpool::ThreadPool};

use super::{document_service::DocumentService, data_structs::DocumentInfo};

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

#[derive(Debug, Clone)]
pub struct EntityTreeService{
    class_module_map : Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfoNode>>>>>,
    logger : Arc<dyn ILoggerV2>,
    chunk_size: usize
}
impl EntityTreeService{
    pub fn new(chunk_size: usize, logger: Arc<dyn ILoggerV2>) -> EntityTreeService{
        return EntityTreeService { 
            class_module_map: Arc::new(RwLock::new(HashMap::new())),
            logger,
            chunk_size
        }
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

        let files_to_process : Vec<(String, Arc<RwLock<DocumentInfo>>)> = doc_service
            .get_doc_info_mapping()
            .read().unwrap()
            .iter().map(|r| (r.0.clone(),r.1.clone())).collect();

        let chunks: Vec<Vec<_>> = files_to_process.chunks(self.chunk_size).map(|s| s.into()).collect();

        for chunk in chunks {
            let map= self.class_module_map.clone();
            let logger = self.logger.clone();
            let doc_service = doc_service.clone();
            threadpool.execute(move ||{
                chunk.iter().for_each( |pair: &(String, Arc<RwLock<DocumentInfo>>)| {
                    let (uri, _doc_info) = pair;
    
                    let uri = match Url::from_str(uri.as_str()){
                        Ok(uri) => uri,
                        _=> return
                    };
                    // self.logger.log_info(format!("Processing no.{} {}", count, uri).as_str());
                    if let Ok(doc) = doc_service.get_parsed_document(&uri, true){
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
                        return;
                    } 
                });
                logger.log_info(format!("Class tree processed in {:#?}; Found {} entities", timer.elapsed(), map.read().unwrap().len()).as_str());
            });
        }
    }

    pub fn build_tree(&self, doc_service: &DocumentService){
        
        let timer = std::time::Instant::now();
        self.logger.log_info("Building Class tree");

        // precompile regex
        let class_regx= Regex::new(r"(?i)^\s*\bclass\s*\b(\w+)\s*(?:\(\s*(\w+)\s*\))?").unwrap();
        let module_regx = Regex::new(r"(?i)^\s*\bmodule\s*\b(\w+)").unwrap();
        let mut map_lock = self.class_module_map.write().unwrap();
        map_lock.clear();
        let mut count : usize = 0;
        let files_to_process : Vec<(String, Arc<RwLock<DocumentInfo>>)> = doc_service
            .get_doc_info_mapping()
            .read().unwrap()
            .iter().map(|r| (r.0.clone(),r.1.clone())).collect();
        files_to_process.iter().for_each( |pair: &(String, Arc<RwLock<DocumentInfo>>)| {
            
            let (uri, doc_info) = pair;
            count += 1;
            let uri = match Url::from_str(uri.as_str()){
                Ok(uri) => uri,
                _=> return
            };
            // self.logger.log_info(format!("Processing no.{} {}", count, uri).as_str());
            if let Ok(doc) = doc_service.get_parsed_document(&uri, true){
                // generate from entity info
                if let Some(e_info) = &doc.lock().unwrap().entity_info {
                    let entity = EntityTreeService::get_or_create_entity(&e_info.id, &mut map_lock);      
                    if let Some(parent_class) = &e_info.parent{
                        // if parent doesn't exist
                        let parent_entity  = EntityTreeService::get_or_create_entity(parent_class, &mut map_lock);
                        // set parent
                        entity.lock().unwrap().parent = Some(Arc::downgrade(&parent_entity));
                        // add as child
                        parent_entity.lock().unwrap().children.push(entity.clone());
                    }
                } else {
                    // self.logger.log_warning(format!("Can't find entity info for {}", doc_info.read().unwrap().uri).as_str())
                }
                return;
            } 
        });
        self.logger.log_info(format!("Class tree built in {:#?}; Found {} entities", timer.elapsed(), map_lock.len()).as_str());
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
    use crate::{manager::test::{create_test_doc_service, create_test_logger, create_uri_from_path}, threadpool::ThreadPool};

    use super::EntityTreeService;


    pub fn create_test_entity_tree_service() -> EntityTreeService{
        return EntityTreeService::new(15_000, create_test_logger())
    }

    #[test]
    fn test_generate_tree(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let class_service = create_test_entity_tree_service();

        class_service.build_tree(&doc_service);

        let root_class = class_service.get_root_class().unwrap();
        assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }

    #[test]
    fn test_generate_tree_parsed(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        let _ = doc_service.get_parsed_document_for_class("aRootClass", true);
        
        let class_service = create_test_entity_tree_service();

        class_service.build_tree(&doc_service);

        let root_class = class_service.get_root_class().unwrap();
        assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }

    #[ignore = "long running time"]
    #[test]
    fn test_generate_tree_large(){
        let root_uri = create_uri_from_path("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let class_service = create_test_entity_tree_service();

        class_service.build_tree(&doc_service);

        let root_class = class_service.get_root_class().unwrap();
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
        let root_class = class_service.get_root_class().unwrap();
        // assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }
}