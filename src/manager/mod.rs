use std::{collections::{HashMap, HashSet}, fs::File, io::Read, rc::Rc, sync::{Arc, Mutex, RwLock}, cell::RefCell, error::Error, fmt::Display};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{ParserDiagnostic, parse_gold}, lexer::GoldLexer, utils::{IRange, ILogger, GenericDiagnosticCollector}, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IAnalyzer, IVisitor}, threadpool::ThreadPool, manager::semantic_analysis_service::ISymbolTableGenerator};
use data_structs::*;

use self::semantic_analysis_service::{ISymbolTable, SemanticAnalysisService, DocumentSymbolGenerator};

pub mod data_structs;
pub mod semantic_analysis_service;
pub mod annotated_node;
pub mod definition_service;

#[derive(Debug,Default,Clone,Copy)]
pub struct SymbolTableRequestOptions{
    wait_on_lock: bool
}

#[derive(Debug)]
pub struct ProjectManager{
    uri_docinfo_map : Arc<RwLock<HashMap<String, Arc<RwLock<DocumentInfo>>>>>,
    class_uri_map: Arc<RwLock<HashMap<String, Url>>>,
    root_path: Option<String>,
    logger: Arc<Mutex<dyn ILogger>>,
}
impl ProjectManager{
    pub fn new(root_uri : Option<Url>, logger: Arc<Mutex<dyn ILogger>>) -> Result<ProjectManager, ProjectManagerError>{
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

        Ok(ProjectManager{
            uri_docinfo_map: Arc::new(RwLock::new(HashMap::new())),
            root_path,
            class_uri_map: Arc::new(RwLock::new(HashMap::new())),
            logger
        })
    }

    pub fn index_files(&mut self){
        if self.root_path.is_none() {return};

        let root_path = self.root_path.as_ref().unwrap().clone();
        let uri_docinfo_map = self.uri_docinfo_map.clone();
        let class_uri_map = self.class_uri_map.clone();
        // self.threadpool.execute(move ||{
            let mut stack = Vec::new();
            // stack for pushing dirs
            
            stack.push(root_path);
            // lock here and keep locked until all files indexed
            let mut uri_docinfo_map = uri_docinfo_map.write().unwrap();
            // let mut class_uri_map = class_uri_map.write().unwrap();
            while !stack.is_empty(){
                let path = stack.pop().unwrap();
                for entry in std::fs::read_dir(path).unwrap(){
                    // skip if err
                    if let Err(e)= entry { eprint!("{}",e);continue;}
    
                    let entry = entry.unwrap();
                    if let Ok(file_type) = entry.file_type(){
                        let path = entry.path().clone().to_str().unwrap().to_string();
                        // if dir push to stack
                        if file_type.is_dir() {stack.push(path.clone())}
                        // if file and .god, add to hash
                        if file_type.is_file() && path.ends_with(".god") {
                            let uri = Url::from_file_path(&path).unwrap();
                            let new_doc_info = DocumentInfo::new(
                                uri.to_string(),
                                path
                            );
                            let new_doc_info = Arc::new(RwLock::new(new_doc_info));
                            uri_docinfo_map.insert(uri.to_string(), new_doc_info);
                            // index class name to file uri
                            match entry.path().as_path().file_stem(){
                                Some(p) => {
                                    match p.to_str(){
                                        Some(s) => {
                                            class_uri_map.write().unwrap().insert(s.to_string(), uri.clone());
                                            // eprint!("found file {}; uri:{}",s.to_string(), uri.to_string());
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
            return
        // });
    }

    pub fn get_document_info(&mut self, uri: &Url) -> Result<Arc<RwLock<DocumentInfo>>, ProjectManagerError>{
        let uri_string = uri.to_string();
        let doc_info =  self.uri_docinfo_map.read().unwrap().get(&uri_string).cloned();
        if doc_info.is_some() {
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
        self.uri_docinfo_map.write().unwrap().insert(uri_string.clone(), new_doc_info.clone());
        Ok(new_doc_info)
    }

    pub fn get_parsed_document_for_class(&mut self, class: &String, wait_on_lock: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let uri = self.get_uri_for_class(class)?;
        return self.get_parsed_document(&uri, wait_on_lock);
    }

    pub fn get_parsed_document(&mut self, uri: &Url, wait_on_lock: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let read_doc_info = match doc_info.try_read(){
            Ok(rw_lock) => rw_lock,
            Err(_) => {
                if wait_on_lock {doc_info.read().unwrap()}
                else {return Err(ProjectManagerError::new(format!("doc info locked,{}", uri).as_str(), ErrorCode::RequestFailed))}
            }
        };
        // check opened document
        if read_doc_info.get_opened_document().is_some() {
            return Ok(read_doc_info.get_opened_document().unwrap());
        }
        // check last saved doc
        if read_doc_info.get_saved_document().is_some() {
            return Ok(read_doc_info.get_saved_document().unwrap());
        } 
        // drop read lock
        drop(read_doc_info);
        let mut write_doc_info = match doc_info.try_write(){
            Ok(wr_lock) => wr_lock,
            Err(_) => {
                if wait_on_lock {doc_info.write().unwrap()}
                else {return Err(ProjectManagerError::new(format!("doc info locked,{}", uri).as_str(), ErrorCode::RequestFailed))}
            }
        };
        
        // if none, read from file
        let new_doc = self.parse_document(write_doc_info.file_path.as_str())?;
        write_doc_info.set_saved_document(Some(Arc::new(Mutex::new(new_doc))));
        return Ok(write_doc_info.get_saved_document().unwrap());
        
    }

    pub fn notify_document_saved(&mut self, uri: &Url) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_document(doc_info.write().unwrap().file_path.as_str())?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.write().unwrap().set_saved_document(Some(new_doc.clone()));
        doc_info.write().unwrap().set_opened_document(None);
        return Ok(new_doc);
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_content(full_file_content)?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.write().unwrap().set_opened_document(Some(new_doc.clone()));
        return Ok(new_doc);
    }

    pub fn get_uri_for_class(& self, class_name: &String)-> Result<Url, ProjectManagerError>{
        match self.class_uri_map.read().unwrap().get(class_name){
            Some(uri) => Ok(uri.clone()),
            _=> Err(ProjectManagerError::new("no uri found for class", ErrorCode::InternalError))
        }
    }

    pub fn analyze_doc(&mut self, uri : &Url, already_seen_classes: Option<HashSet<String>>) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let mut semantic_analysis_service = SemanticAnalysisService::new(
            self, 
            self.logger.clone(),
            Box::new(GenericDiagnosticCollector::new()),
            already_seen_classes
        );
        let doc = semantic_analysis_service.analyze_uri(uri)?;
        return Ok(doc);
    }

    pub fn get_symbol_table_for_class(&mut self, class: &String, already_seen_classes: Option<HashSet<String>>) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let uri = self.get_uri_for_class(&class)?;
        return self.get_symbol_table_for_uri(&uri, already_seen_classes);
    }

    pub fn get_symbol_table_for_uri(&mut self, uri : &Url, already_seen_classes: Option<HashSet<String>>) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let doc = self.analyze_doc(uri, already_seen_classes)?;
        let _ = match &doc.lock().unwrap().symbol_table{
            Some(st) => return Ok(st.clone()),
            _=> return Err(ProjectManagerError::new("Unable to generate symbol table", ErrorCode::InternalError))
        };
    }

    pub fn generate_document_symbols(&mut self, uri : &Url) -> Result<Vec<DocumentSymbol>, ProjectManagerError>{
        let sym_table = self.get_symbol_table_for_uri(uri, None)?;
        let sym_gen = DocumentSymbolGenerator {};
        return Ok(sym_gen.generate_symbols(sym_table))
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

    fn parse_content(&self, full_file_content: &String) -> Result<Document, ProjectManagerError> {
        // lexing
        let mut lexer = GoldLexer::new();
        let (tokens, lexer_errors) = lexer.lex(&full_file_content);
        // parse
        let (ast_nodes, mut parser_diagnostics) = parse_gold(&tokens);
        // add lexer errors
        parser_diagnostics.extend(lexer_errors.into_iter().map(|l_error|{
            ParserDiagnostic { range: l_error.range, msg: l_error.msg }
        }));
        let new_doc =Document::new( 
            ast_nodes.1,
            parser_diagnostics,
        );
        return Ok(new_doc)
    }

    fn get_analyzer_diagnostics(&self, doc : Arc<Mutex<Document>>) -> Result<Arc<Vec<lsp_types::Diagnostic>>, ProjectManagerError>{
        let diags = doc.lock().unwrap().get_analyzer_diagnostics();
        if diags.is_some(){
            return Ok(diags.unwrap());
        } else {
            let diags = self.analyze_ast(doc.lock().unwrap().get_ast());
            let diags = Arc::new(diags);
            doc.lock().unwrap().set_analyzer_diagnostics(Some(diags.clone()));
            return Ok(diags);
        }
    }

    fn collect_diagnostics(&self, analyzers: &Vec<Rc<RefCell<dyn IAnalyzer>>>) -> Vec<lsp_types::Diagnostic>{
        let mut result = Vec::new();
        for analyzer in analyzers{
            analyzer.as_ref().borrow().append_diagnostics(&mut result);
        }
        return result;
    }

    fn analyze_ast(&self, ast : &Arc<dyn IAstNode>) -> Vec<lsp_types::Diagnostic>{
        // let result = Vec::new();
        let mut ast_walker: AstWalker<dyn IAnalyzer> = AstWalker::new(true);
        let analyzers: Vec<Rc<RefCell<dyn IAnalyzer>>> = vec![
            Rc::new(RefCell::new(UnusedVarAnalyzer::new())),
            Rc::new(RefCell::new(InoutParamChecker::new())),
            Rc::new(RefCell::new(FunctionReturnTypeChecker::new()))
        ];
        ast_walker.register_visitors(&analyzers);
        ast_walker.run(ast);
        
        return self.collect_diagnostics(&analyzers)
    }

    pub fn generate_document_diagnostic_report(&mut self, uri : &Url)
    -> Result<Arc<RelatedFullDocumentDiagnosticReport>, ProjectManagerError>{
        let doc = self.get_parsed_document(uri, true)?;
        let diagnostics = self.get_diagnostics(doc)?;
        return Ok(Arc::new(RelatedFullDocumentDiagnosticReport{
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport{
                result_id: None,
                items: diagnostics,
            },
        }))
    }

    fn get_diagnostics(&self, doc: Arc<Mutex<Document>>) -> Result<Vec<Diagnostic>, ProjectManagerError>{
        let mut result: Vec<Diagnostic> = doc.lock().unwrap().get_parser_diagnostics().iter()
            .map(|gold_error| {
                Diagnostic::new(
                    gold_error.get_range().as_lsp_type_range(),
                    Some(DiagnosticSeverity::ERROR), 
                    None, 
                    Some("gold".to_string()), 
                    gold_error.get_msg(), 
                    None, 
                    None)
            }).collect();
        let analyzer_diags = self.get_analyzer_diagnostics(doc)?;
        analyzer_diags.iter().for_each(|d|{result.push(d.clone())});
        return Ok(result);
    }
}


#[cfg(test)]
mod test{
    use std::{fs::{File, self}, io::Read, path::{PathBuf, Path}, time, thread, rc::Rc, cell::RefCell, sync::{Mutex, Arc}, str::FromStr};

    use lsp_types::Url;

    use crate::{lexer::{GoldLexer}, parser::{parse_gold, ast::IAstNode, ParserDiagnostic}, utils::{ast_to_string_brief_recursive, ILogger, ConsoleLogger}, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IVisitor, IAnalyzer}, manager::SymbolTableRequestOptions};

    use super::ProjectManager;

    fn parse_and_analyze(file_path: &str) -> (Arc<dyn IAstNode>, Vec<ParserDiagnostic>){
        let  mut f = File::open(file_path).expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        // println!("{file_contents}");
        let mut lexer = GoldLexer::new();
        let lex_result = lexer.lex(&file_contents);
        // println!("{:#?}", tokens);
        assert_eq!(lex_result.1.len(), 0);
        let ast = parse_gold(&lex_result.0);
        assert_eq!(ast.1.len(), 0);
        return (ast.0.1, ast.1);
    }

    fn create_test_ast_walker<T:IVisitor+?Sized>()-> AstWalker<T>{
        let result = AstWalker::<T>::new(true);
        return result;
    }

    fn create_test_logger()-> Arc<Mutex<dyn ILogger>>{
        Arc::new(Mutex::new(ConsoleLogger::new("[LSP Server]")))
    }

    fn create_uri_from_path(path:&str)-> Url{
        let path = PathBuf::from_str(path).unwrap();
        let path = std::fs::canonicalize(path).unwrap();
        return Url::from_file_path(path).unwrap();
    }

    fn create_test_project_manager(root: &str) -> ProjectManager{
        let uri = create_uri_from_path(root);
        return ProjectManager::new(Some(uri), create_test_logger()).unwrap()
    }

    #[test]
    fn test_gold_document_manager(){
        
        let doc_manager = ProjectManager::new(None, create_test_logger()).unwrap();
        let _doc = doc_manager.parse_document("test/aTestClass.god").unwrap();
        // println!("{:#?}",doc);
    }

    #[test]
    fn test_read_file() {
        let  mut f = File::open("./test/aTestClass.god").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        // println!("{file_contents}");
        let mut lexer = GoldLexer::new();
        let tokens = lexer.lex(&file_contents).0;
        // println!("{:#?}", tokens);
        let ast = parse_gold(&tokens);
        // println!("{:#?}", ast.0.0);
        for node in ast.0.1.get_children_ref().unwrap_or_default().iter(){
            println!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        }
        // println!("{:#?}", ast.1.len());
    }

    #[test]
    fn test_read_file_proc1() {
        let  mut f = File::open("./test/aTestProc1.god").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        // println!("{file_contents}");
        let mut lexer = GoldLexer::new();
        let lex_result = lexer.lex(&file_contents);
        // println!("{:#?}", tokens);
        assert_eq!(lex_result.1.len(), 0);
        let ast = parse_gold(&lex_result.0);

        assert_eq!(ast.1.len(), 0);
        // println!("{:#?}", ast.0.0);
        for node in ast.0.1.get_children_ref().unwrap_or_default().iter(){
            println!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        }
        // println!("{:#?}", ast.1.len());
    }

    #[test]
    fn test_read_file_proc_call() {
        let  mut f = File::open("./test/aTestProcCall.god").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        // println!("{file_contents}");
        let mut lexer = GoldLexer::new();
        let lex_result = lexer.lex(&file_contents);
        // println!("{:#?}", tokens);
        assert_eq!(lex_result.1.len(), 0);
        let ast = parse_gold(&lex_result.0);

        assert_eq!(ast.1.len(), 0);
        // println!("{:#?}", ast.0.0);
        for node in ast.0.1.get_children_ref().unwrap_or_default().iter(){
            println!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        }
        // println!("{:#?}", ast.1.len());
    }

    #[test]
    fn test_unused_var_file() {
        let (asts, _) = parse_and_analyze("./test/aTestUnusedVar.god");
        let mut walker = create_test_ast_walker::<dyn IAnalyzer>();
        let mut analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(UnusedVarAnalyzer::new()));
        walker.register_visitor(&analyzer);
        walker.run(&asts);
        // for diag in diags{
        //     print!("{:#?}",diag);
        // }
        assert_eq!(analyzer.borrow().get_diagnostics_count(), 1);
    }

    #[test]
    fn test_varbytearray_param_checker_file() {
        let (asts, _) = parse_and_analyze("./test/aTestVarByteArrayParamChecker.god");
        let mut walker = create_test_ast_walker::<dyn IAnalyzer>();
        let mut analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(InoutParamChecker::new()));
        walker.register_visitor(&analyzer);
        let diags = walker.run(&asts);
        // for diag in diags{
        //     print!("{:#?}",diag);
        // }
        assert_eq!(analyzer.borrow().get_diagnostics_count(), 2);
    }

    #[test]
    fn test_func_return_type_checker_param_file() {
        let (asts, _) = parse_and_analyze("./test/aTestFunctionReturnTypeChecker.god");
        let mut walker = create_test_ast_walker::<dyn IAnalyzer>();
        let analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(FunctionReturnTypeChecker::new()));
        walker.register_visitor(&analyzer);
        let diags = walker.run(&asts);
        // for diag in diags{
        //     print!("{:#?}",diag);
        // }
        assert_eq!(analyzer.borrow().get_diagnostics_count(), 3);
    }

    #[test]
    fn test_read_file_event_method() {
        let (_, parser_diags) = parse_and_analyze("./test/aTestEventMethod.god");
        assert_eq!(parser_diags.len(), 0);
    }

    #[test]
    fn test_read_file_oql() {
        let (_, parser_diags) = parse_and_analyze("./test/aTestOQL.god");
        assert_eq!(parser_diags.len(), 0);
    }


    #[test]
    fn test_index_file(){
        let path = PathBuf::from("./test/workspace");
        let path =  fs::canonicalize(&path).unwrap().to_str().unwrap().to_string();
        let uri = Url::from_file_path(path.clone()).unwrap();
       
        let file_path= PathBuf::from("./test/workspace/Bundle2/aClass5.god");
        let file_path =  fs::canonicalize(&file_path).unwrap().to_str().unwrap().to_string();
        let file_uri = Url::from_file_path(file_path.clone()).unwrap();

        let mut doc_manager = ProjectManager::new(Some(uri), create_test_logger()).unwrap();
        doc_manager.index_files();
        
        assert_eq!(doc_manager.uri_docinfo_map.read().unwrap().len(), 5);
        // ensure not locked
        drop(doc_manager.uri_docinfo_map.try_write().unwrap());
        drop(doc_manager.class_uri_map.try_write().unwrap());
        // try request
        let _ = doc_manager.generate_document_symbols(&file_uri).unwrap();
    }

    #[test]
    fn test_file_outside_workspace_after_index(){
        // skip if dir doesn't exist
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let path =  match fs::canonicalize(&path){
            Ok(p) => p.to_str().unwrap().to_string(),
            _=> return
        };
        let uri = Url::from_file_path(path.clone()).unwrap();
       
        let mut doc_manager = ProjectManager::new(Some(uri), create_test_logger()).unwrap();
        doc_manager.index_files();
        
        println!("num of files: {}",doc_manager.uri_docinfo_map.read().unwrap().len());
        // ensure not locked
        drop(doc_manager.uri_docinfo_map.try_write().unwrap());
        drop(doc_manager.class_uri_map.try_write().unwrap());
        // test file outside workspace
        let _ = doc_manager.generate_document_symbols(&Url::parse("file:///c%3A/Users/muhampra/dev/ewam_src/37.63.03/OcsMobCardsApps/Images/aOcsMobileICCardImageARRViewer.god").unwrap()).unwrap();
    }

    #[test]
    fn test_get_symbol_table(){
        let mut proj_manager= create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        let result = proj_manager.get_symbol_table_for_class(&"aRootClass".to_string(), None).unwrap();
        let result = result.lock().unwrap();
        assert_eq!(result.iter_symbols().count(), 6);
    }

    #[test]
    fn test_get_document_symbols(){
        let mut proj_manager= create_test_project_manager("./test/workspace");
        let input = create_uri_from_path("./test/workspace/aRootClass.god");
        let result = proj_manager.generate_document_symbols(&input).unwrap();
        assert_eq!(result.len(), 1);
    }
}