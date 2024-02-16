use std::{collections::HashMap, fs::File, io::Read, path::PathBuf, str::FromStr, sync::{Arc, Mutex, RwLock}};

use lsp_server::ErrorCode;
use lsp_types::Url;


use crate::{lexer::GoldLexer, parser::{ast::{AstClass, AstModule}, parse_gold, ParserDiagnostic}, utils::{ILoggerV2, LogLevel, LogType}};

use super::{data_structs::{DocumentInfo, ProjectManagerError, Document}, entity_tree_service::EntityType};

#[derive(Debug, Clone)]
pub struct EntityInfo{
    pub id: Arc<str>,
    pub parent: Option<Arc<str>>,
    pub entity_type: EntityType,
}
impl EntityInfo{
    pub fn new(id: Arc<str>, entity_type : EntityType, parent: Option<Arc<str>>) -> EntityInfo {
        return EntityInfo { 
            id, 
            parent,
            entity_type 
        }
    }
}

#[derive(Debug,Default,Clone,Copy)]
pub struct GetParsedDocumentOptions{
    cache_result: bool,
    wait_on_lock: bool
}
impl GetParsedDocumentOptions{
    pub fn set_cache_result(mut self, val : bool) -> GetParsedDocumentOptions{
        self.cache_result = val;
        self
    } 
    pub fn set_wait_on_lock(mut self, val : bool) -> GetParsedDocumentOptions{
        self.wait_on_lock = val;
        self
    } 
}

#[derive(Debug)]
/// provides caching and parsing of documents in
/// workspace
pub struct DocumentService {
    path_docinfo_map: Arc<RwLock<HashMap<String, Arc<RwLock<DocumentInfo>>>>>,
    class_uri_map: Arc<RwLock<HashMap<String, Url>>>,
    root_path: Option<String>,
    logger: Box<dyn ILoggerV2>,
}
impl Clone for DocumentService{
    fn clone(&self) -> Self {
        Self { path_docinfo_map: self.path_docinfo_map.clone(), class_uri_map: self.class_uri_map.clone(), root_path: self.root_path.clone(), logger: self.logger.clone_box() }
    }
}
impl DocumentService {
    pub fn new(
        root_uri : Option<Url>, 
        logger: Box<dyn ILoggerV2>
    ) -> Result<DocumentService, ProjectManagerError>{
        let mut root_path : Option<String> = None;

        match root_uri {
            Some(uri) => {
                root_path = match uri.to_file_path() {
                    Ok(fp) => {
                        let fp = match fp.as_path().to_str() {
                            Some(s) => s.to_string(),
                            _ => return Err(ProjectManagerError{
                                    msg: format!("cannot convert uri to file path:{}", uri),
                                    error_code: ErrorCode::InvalidRequest,
                                })
                        };
                        Some(fp)
                    },
                    Err(_) => return Err(ProjectManagerError{
                            msg: format!("cannot convert uri to file path:{}", uri),
                            error_code: ErrorCode::InvalidRequest,
                    })
                };
            },
            _=>()
        }

        Ok(DocumentService{
            path_docinfo_map: Arc::new(RwLock::new(HashMap::new())),
            root_path,
            class_uri_map: Arc::new(RwLock::new(HashMap::new())),
            logger
        })
    }

    fn get_key_for_path(&self, path: &PathBuf) -> String {
        std::fs::canonicalize(path).unwrap().to_string_lossy().to_string()
    }

    fn get_key_for_uri(&self, url: &Url) -> Result<String, ProjectManagerError>{
        let path = url
            .to_file_path()
            .map_err(|_| ProjectManagerError::new(
                format!("Failed to conver url to path: {}",url).as_str(),
                ErrorCode::InvalidParams))?;
        return Ok(self.get_key_for_path(&path));
    }

    pub fn index_files(&self){
        if self.root_path.is_none() {return};

        self.logger.log_info("Indexing files");
        let timer = std::time::Instant::now();
        let root_path = PathBuf::from_str(self.root_path.as_ref().unwrap()).unwrap();
        let root_path = match std::fs::canonicalize(&root_path){
            Ok(p) => p,
            Err(_) => {
                self.logger.log_error("Failed to canonicalize root path:");
                return;
            }
        };

        let path_docinfo_map = self.path_docinfo_map.clone();
        let class_uri_map = self.class_uri_map.clone();
        // self.threadpool.execute(move ||{
            let mut stack = Vec::new();
            // stack for pushing dirs
            
            stack.push(root_path);
            // lock here and keep locked until all files indexed
            let mut path_docinfo_map = path_docinfo_map.write().unwrap();
            // let mut class_uri_map = class_uri_map.write().unwrap();
            while !stack.is_empty(){
                let path = stack.pop().unwrap();
                for entry in std::fs::read_dir(&path).unwrap(){
                    // skip if err
                    if let Err(e)= entry { self.logger.log_error(format!("{}",e).as_str());continue;}
    
                    let entry = entry.unwrap();
                    
                    if let Ok(file_type) = entry.file_type(){
                        let entry_path = entry.path();
                        // if dir push to stack, and continue
                        if file_type.is_dir() {stack.push(entry_path.clone()); continue;}
                        
                        let is_god_ext = entry_path.extension().map_or(false, |ext| ext == "god");
                        // if file and .god, add to hash
                        if file_type.is_file() && is_god_ext {
                            let uri = Url::from_file_path(entry_path.as_path()).unwrap();
                            // don't need to canonicalize since root is already
                            //  in that form
                            let key = entry_path.to_string_lossy().to_string();
                            // only add if it doesn't exist
                            //  Use path() to ensure it is normalized to percent encoded  ASCII
                            if !path_docinfo_map.contains_key(&key){
                                
                                let new_doc_info = DocumentInfo::new(
                                    uri.to_string(),
                                    key.clone()
                                );
                                let new_doc_info = Arc::new(RwLock::new(new_doc_info));
                                path_docinfo_map.insert(key.clone(), new_doc_info);
                                // index class name to file uri
                                match entry_path.as_path().file_stem(){
                                    Some(p) => {
                                        match p.to_str(){
                                            Some(s) => {
                                                class_uri_map.write().unwrap().insert(s.to_string().to_uppercase(), uri.clone());
                                                self.logger.log(LogType::Info, LogLevel::Verbose, format!(
                                                    "found file {}; uri:{}", key, uri.to_string()).as_str());
                                            },
                                            _=> ()
                                        }
                                    }
                                    _=> ()
                                }
                            }
                        }
                    }
                }
            }

            self.logger.log_info(
                format!(
                    "Indexing files done in {:#?}; {} files found", 
                    timer.elapsed(), 
                    class_uri_map.read().unwrap().len()).as_str());
            return
        // });
    }

    pub fn get_uri_for_class(&self, class_name: &str)-> Result<Url, ProjectManagerError>{
        match self.class_uri_map.read().unwrap().get(&class_name.to_uppercase()){
            Some(uri) => Ok(uri.clone()),
            _=> Err(ProjectManagerError::new(format!("Cannot get uri for class {}", class_name).as_str(), ErrorCode::InternalError))
        }
    }

    pub fn get_document_info(&self, uri: &Url) -> Result<Arc<RwLock<DocumentInfo>>, ProjectManagerError>{
        self.logger.log(LogType::Info, LogLevel::Verbose, format!("Get doc info {}", uri).as_str());
        let key = self.get_key_for_uri(uri)?;
        self.logger.log(LogType::Info, LogLevel::Verbose, format!("Key: {}", key).as_str());
        let doc_info =  self.path_docinfo_map.read().unwrap().get(&key).cloned();

        if doc_info.is_some() {
            self.logger.log(LogType::Info, LogLevel::Verbose, "Found doc info");
            return Ok(doc_info.unwrap().clone());
        }

        let file_path = match uri.to_file_path() {
            Ok(fp) => {
                let fp = match fp.as_path().to_str() {
                    Some(s) => s.to_string(),
                    _ => return Err(ProjectManagerError{
                            msg: format!("cannot convert uri to file path:{}", uri),
                            error_code: ErrorCode::InvalidRequest,
                        })
                };
                fp
            },
            Err(_) => return Err(ProjectManagerError{
                    msg: format!("cannot convert uri to file path:{}", uri),
                    error_code: ErrorCode::InvalidRequest,
                })
        };
        let new_doc_info = DocumentInfo::new(
            uri.to_string(),
            file_path,
        );
        let new_doc_info = Arc::new(RwLock::new(new_doc_info));
        self.path_docinfo_map.write().unwrap().insert(key.clone(), new_doc_info.clone());
        Ok(new_doc_info)
    }

    pub fn get_parsed_document_for_class(&self, class: &str, options: GetParsedDocumentOptions) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let path = self.get_uri_for_class(class)?;
        return self.get_parsed_document(&path, options);
    }

    pub fn get_parsed_document(&self, uri: &Url , options: GetParsedDocumentOptions) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        self.logger.log(LogType::Info, LogLevel::Verbose, format!("Get parsed doc for {}", uri).as_str());
        let doc_info = self.get_document_info(uri)?;
        let read_doc_info = match doc_info.try_read(){
            Ok(rw_lock) => rw_lock,
            Err(_) => {
                if options.wait_on_lock {doc_info.read().unwrap()}
                else {return Err(ProjectManagerError::new(format!("doc info locked,{}", uri).as_str(), ErrorCode::RequestFailed))}
            }
        };
        // check opened document
        if read_doc_info.get_opened_document().is_some() {
            self.logger.log(LogType::Info, LogLevel::Verbose, "Found opened doc");
            return Ok(read_doc_info.get_opened_document().unwrap());
        }
        // check last saved doc
        if read_doc_info.get_saved_document().is_some() {
            self.logger.log(LogType::Info, LogLevel::Verbose, "Found saved doc");
            return Ok(read_doc_info.get_saved_document().unwrap());
        } 
        // drop read lock
        drop(read_doc_info);
        let mut write_doc_info = match doc_info.try_write(){
            Ok(wr_lock) => wr_lock,
            Err(_) => {
                if options.wait_on_lock {doc_info.write().unwrap()}
                else {return Err(ProjectManagerError::new(format!("doc info locked,{}", uri).as_str(), ErrorCode::RequestFailed))}
            }
        };
        
        // if none, read from file
        self.logger.log(LogType::Info, LogLevel::Verbose, "Parsing doc");
        let new_doc = Arc::new(Mutex::new(self.parse_document(write_doc_info.file_path.as_str())?));
        if options.cache_result{
            write_doc_info.set_saved_document(Some(new_doc.clone()));
        }
        return Ok(new_doc);
    }

    pub fn notify_document_closed(&self, uri: &Url){
        // drop docs
        if let Ok(doc_info) = self.get_document_info(uri){
            doc_info.write().unwrap().set_saved_document(None);
            doc_info.write().unwrap().set_opened_document(None);
        }
    }

    fn parse_document(&self, file_path: &str) -> Result<Document, ProjectManagerError>{
        // open file
        let mut file = match File::open(file_path){
            Ok(f) => f,
            Err(e) => return Err(ProjectManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        // read bytes first, then convert to handle invalid chars
        let mut contents = Vec::new();
        match file.read_to_end(&mut contents){
            Ok(_n)=> (),
            Err(e) => return Err(ProjectManagerError{msg:e.to_string(), error_code: ErrorCode::InternalError})
        };
        // invalid chars replaced with �
        let contents_as_string = String::from_utf8_lossy(contents.as_slice()).to_string();
        return self.parse_content(&contents_as_string)
    }

    pub fn parse_content(&self, full_file_content: &String) -> Result<Document, ProjectManagerError> {
        // lexing
        let mut lexer = GoldLexer::new();
        let (tokens, lexer_errors) = lexer.lex(&full_file_content);
        // parse
        let ((_remainder ,ast_nodes), mut parser_diagnostics) = parse_gold(&tokens);
        // add lexer errors
        parser_diagnostics.extend(lexer_errors.into_iter().map(|l_error|{
            ParserDiagnostic { range: l_error.range, msg: l_error.msg }
        }));
        // get entity info (class/module)
        let mut entity_info = None;
        if let Some(children) = ast_nodes.get_children_ref(){
            for child in children{
                match child.as_any().downcast_ref::<AstClass>(){
                    Some(n) => {
                        entity_info = Some(EntityInfo::new(
                            n.identifier.get_value(), 
                            EntityType::Class, 
                            n.parent_class.as_ref().map(|t|{t.get_value()})
                        ));
                        break;
                    }
                    _=>()
                }
                match child.as_any().downcast_ref::<AstModule>(){
                    Some(n) => {
                        entity_info = Some(EntityInfo::new(
                            n.id.get_value(), 
                            EntityType::Module, 
                            None
                        ));
                        break;
                    }
                    _=>()
                }
            }
        }

        let new_doc =Document::new( 
            ast_nodes,
            parser_diagnostics,
            entity_info,
        );
        return Ok(new_doc)
    }

    pub fn count_files(&self) -> usize{
        return self.path_docinfo_map.read().unwrap().len();
    }

    pub fn partition_files(&self, partition_size: usize)->Vec<Vec<String>> {
        if partition_size == 0 {panic!("partition size cannot be 0")}

        let mut result = Vec::new();
        let mut cur_partition =  Vec::new();
        for uri in self.path_docinfo_map.read().unwrap().keys(){
            if cur_partition.len() >= partition_size{
                result.push(cur_partition);
                cur_partition = Vec::new();
            }
            cur_partition.push(uri.clone())
        }
        return result;
    }

    pub fn for_each_uri_docinfo(&self, process_func : impl FnMut((&String, &Arc<RwLock<DocumentInfo>>))){
        self.path_docinfo_map.read().unwrap().iter().for_each(process_func);
    }

    pub fn get_doc_info_mapping(&self) -> Arc<RwLock<HashMap<String, Arc<RwLock<DocumentInfo>>>>>{
        return self.path_docinfo_map.clone();
    }
}

#[cfg(test)]
mod test{
    use std::{path::PathBuf, fs};

    use lsp_types::Url;

    use crate::manager::test::create_test_logger;

    use super::DocumentService;

    #[test]
    fn test_gold_document_manager(){
        
        let doc_service = DocumentService::new(None, create_test_logger()).unwrap();
        let _doc = doc_service.parse_document("test/aTestClass.god",).unwrap();
        // println!("{:#?}",doc);
    }

    #[test]
    fn test_index_file(){
        let path = PathBuf::from("./test/workspace/TestIndexFiles");
        let path =  fs::canonicalize(&path).unwrap().to_str().unwrap().to_string();
        let uri = Url::from_file_path(path.clone()).unwrap();

        let doc_manager = DocumentService::new(Some(uri), create_test_logger()).unwrap();
        doc_manager.index_files();
        
        assert_eq!(doc_manager.path_docinfo_map.read().unwrap().len(), 2);
        // ensure not locked
        drop(doc_manager.path_docinfo_map.try_write().unwrap());
        drop(doc_manager.class_uri_map.try_write().unwrap());
    }

    #[ignore = "long running time"]
    #[test]
    fn test_file_outside_workspace_after_index(){
        // skip if dir doesn't exist
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let path =  match fs::canonicalize(&path){
            Ok(p) => p.to_str().unwrap().to_string(),
            _=> return
        };
        let uri = Url::from_file_path(path.clone()).unwrap();
       
        let doc_manager = DocumentService::new(Some(uri), create_test_logger()).unwrap();
        doc_manager.index_files();
        
        println!("num of files: {}",doc_manager.path_docinfo_map.read().unwrap().len());
        // ensure not locked
        drop(doc_manager.path_docinfo_map.try_write().unwrap());
        drop(doc_manager.class_uri_map.try_write().unwrap());
    }
}