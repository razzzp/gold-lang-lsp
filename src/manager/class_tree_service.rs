use std::{collections::HashMap, sync::{Mutex, Weak, Arc, RwLock}, fs::File, io::{BufReader, BufRead}};

use regex::Regex;

use crate::utils::ILoggerV2;

use super::{document_service::{self, DocumentService}, data_structs::DocumentInfo};

#[derive(Debug, Clone)]
pub struct EntityInfo{
    parent: Option<Weak<Mutex<EntityInfo>>>,
    pub children: Vec<Arc<Mutex<EntityInfo>>>
}
impl EntityInfo{
    pub fn new() -> EntityInfo {
        return EntityInfo { parent: None, children: Vec::new() }
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

    pub fn build_tree(&self, doc_service: DocumentService){
        // precompile regex
        let class_regx= Regex::new(r"(?i)^([^:]+):([0-9]+):(.+)$").unwrap();
        let module_regx = Regex::new("") 

        doc_service.for_each_uri_docinfo(|pair: (&String, &Arc<RwLock<DocumentInfo>>)|{
            let (uri, doc_info) = pair;
            if let Some(doc) = doc_info.read().unwrap().get_document(){
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
                    // read line and try to match with class/module regex
                    let read_size = match reader.read_line(&mut buf){
                        Ok(size) => size,
                        Err(e) => {
                            self.logger.log_error(format!("Error reading line; {}", e).as_str());
                            return;
                        }
                    };

                }
            }
        });
    }
}