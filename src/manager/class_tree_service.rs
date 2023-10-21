use std::{collections::HashMap, sync::{Mutex, Weak, Arc, RwLock, RwLockWriteGuard}, fs::File, io::{BufReader, BufRead}};

use regex::Regex;

use crate::utils::ILoggerV2;

use super::{document_service::DocumentService, data_structs::DocumentInfo};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum EntityType {
    Class,
    Module
}

#[derive(Debug, Clone)]
pub struct EntityInfo{
    pub id: String,
    pub parent: Option<Weak<Mutex<EntityInfo>>>,
    pub children: Vec<Arc<Mutex<EntityInfo>>>,
    pub entity_type: EntityType,
}
impl EntityInfo{
    pub fn new(id: &str, entity_type : EntityType) -> EntityInfo {
        return EntityInfo { id: id.to_string(), parent: None, children: Vec::new(), entity_type }
    }
}

#[derive(Debug, Clone)]
pub struct ClassModuleTreeService{
    class_module_map : Arc<RwLock<HashMap<String, Arc<Mutex<EntityInfo>>>>>,
    logger : Arc<dyn ILoggerV2>
}
impl ClassModuleTreeService{
    pub fn new(logger: Arc<dyn ILoggerV2>) -> ClassModuleTreeService{
        return ClassModuleTreeService { 
            class_module_map: Arc::new(RwLock::new(HashMap::new())),
            logger
        }
    }

    fn get_or_create_entity(
        id: &str, 
        map_lock: &mut RwLockWriteGuard<'_, HashMap<String, Arc<Mutex<EntityInfo>>>>
    ) ->  Arc<Mutex<EntityInfo>> {
        let id_upper = id.to_uppercase();
        if let Some(entity) = map_lock.get(&id_upper){
            return entity.clone()
        } else {
            let new_entity = Arc::new(Mutex::new(EntityInfo::new(id, EntityType::Class)));
            map_lock.insert(id_upper.to_uppercase(), new_entity.clone());
            return new_entity
        };
    }

    pub fn build_tree(&self, doc_service: &DocumentService){
        
        self.logger.log_info("Building Class tree");

        // precompile regex
        let class_regx= Regex::new(r"(?i)^\s*\bclass\s*\b(\w+)\s*(?:\(\s*(\w+)\s*\))?").unwrap();
        let module_regx = Regex::new(r"(?i)^\s*\bmodule\s*\b(\w+)").unwrap();
        let mut map_lock = self.class_module_map.write().unwrap();
        map_lock.clear();

        doc_service.for_each_uri_docinfo( |pair: (&String, &Arc<RwLock<DocumentInfo>>)| {
            
            let (_uri, doc_info) = pair;
            if let Some(_doc) = doc_info.read().unwrap().get_document(){
                // TODO store class and parent in doc
                todo!()
            } else {
                // parse until class info found
                let path = &doc_info.read().unwrap().file_path;
                let file = match File::open(path){
                    Ok(f) => f,
                    Err(e) =>{
                        self.logger.log_error(format!("Failed to open file; {}", e).as_str());
                        return
                    }
                };
                let mut reader = BufReader::new(file);
                let mut buf = String::new();
                
                loop {
                    buf.clear();
                    // read line and try to match with class/module regex
                    let _read_size = match reader.read_line(&mut buf){
                        Ok(size) => {
                            // EOF
                            if size == 0 {return;}
                        },
                        Err(e) => {
                            self.logger.log_error(format!("Error reading line; {}", e).as_str());
                            return;
                        }
                    };
                    // regex in rust does not support lookaround...
                    // take left side of comments if any
                    let str_to_match = match buf.split_once(';'){
                        // take left side
                        Some(r)=> r.0,
                        // else use buf
                        _=> &buf
                    };

                    // try match class
                    if let Some(class_match)= class_regx.captures(&str_to_match){
                        let class_name = match class_match.get(1){
                            Some(m) => m.as_str(),
                            _=> {
                                self.logger.log_error("Internal error; regex capture none");
                                return;
                            }
                        };

                        let entity = ClassModuleTreeService::get_or_create_entity(class_name, &mut map_lock);
                        
                        if let Some(parent_class) = class_match.get(2).map(|m|{m.as_str()}){
                            // if parent doesn't exist
                            let parent_entity  = ClassModuleTreeService::get_or_create_entity(parent_class, &mut map_lock);
                            // set parent
                            entity.lock().unwrap().parent = Some(Arc::downgrade(&parent_entity));
                            // add as child
                            parent_entity.lock().unwrap().children.push(entity.clone());
                        }
                        return
                    }

                    // try match module
                    if let Some(module_match) = module_regx.captures(&str_to_match){
                        let module_name = match module_match.get(1){
                            Some(m) => m.as_str(),
                            _=> {
                                self.logger.log_error("Internal error; regex capture none");
                                return;
                            }
                        };
                        let new_entity = Arc::new(Mutex::new(EntityInfo::new(module_name, EntityType::Module)));
                        // add module info
                        map_lock.insert(module_name.to_uppercase(), new_entity);
                        return
                    }
                }
            }
        });
        self.logger.log_info("Class tree built");
    }

    pub fn get_root_class(&self) -> Option<Arc<Mutex<EntityInfo>>>{
        let map_lock =self.class_module_map.read().unwrap();
        let result = map_lock.values().find(|e|{
            // root class is one without parent
            let lock = e.lock().unwrap();
            return (lock.entity_type == EntityType::Class) && lock.parent.is_none();
        }).clone();
        return result.map(|v| {v.clone()})
    }
}

#[cfg(test)]
mod test{
    use crate::manager::test::{create_test_doc_service, create_test_logger, create_uri_from_path};

    use super::ClassModuleTreeService;


    pub fn create_test_class_tree_service() -> ClassModuleTreeService{
        return ClassModuleTreeService::new(create_test_logger())
    }

    #[test]
    fn test_generate_tree(){
        let root_uri = create_uri_from_path("./test/workspace");
        let doc_service = create_test_doc_service(Some(root_uri));
        doc_service.index_files();
        
        let class_service = create_test_class_tree_service();

        class_service.build_tree(&doc_service);

        let root_class = class_service.get_root_class().unwrap();
        assert_eq!(root_class.lock().unwrap().id.as_str(), "aRootClass");
        // println!("{:#?}", root_class)
    }
}