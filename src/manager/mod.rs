use std::{collections::{HashMap, HashSet}, fs::File, io::Read, rc::Rc, sync::{Arc, Mutex, RwLock}, cell::RefCell, error::Error, fmt::Display, str::FromStr};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url, LocationLink};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{ParserDiagnostic, parse_gold}, lexer::GoldLexer, utils::{IRange, ILogger, GenericDiagnosticCollector, IDiagnosticCollector, Position, ILoggerV2}, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IAnalyzer, IVisitor, AnalyzerDiagnostic}, threadpool::ThreadPool};
use data_structs::*;

use self::{semantic_analysis_service::{SemanticAnalysisService}, document_service::DocumentService,  definition_service::DefinitionService, doc_symbol_generator::DocumentSymbolGeneratorFromAst};
use crate::manager::doc_symbol_generator::DocumentSymbolGenerator;

pub mod data_structs;
pub mod semantic_analysis_service;
pub mod annotated_node;
pub mod definition_service;
pub mod type_resolver;
pub mod document_service;
pub mod symbol_table;
pub mod ast_annotator;
pub mod doc_symbol_generator;
pub mod utils;

#[derive(Debug, Clone)]
pub struct ProjectManager{
    pub doc_service: Arc<RwLock<DocumentService>>,
    logger: Arc<dyn ILoggerV2>,
}
impl ProjectManager{
    pub fn new(root_uri : Option<Url>, logger: Arc<dyn ILoggerV2>) -> Result<ProjectManager, ProjectManagerError>{
        let doc_service = Arc::new(RwLock::new(DocumentService::new(root_uri.clone(), logger.clone())?));

        Ok(ProjectManager{
            doc_service,
            logger
        })
    }

    pub fn index_files(&mut self){
        return self.doc_service.write().unwrap().index_files();
    }

    pub fn analyze_files(&mut self, thread_pool : &ThreadPool){
        let partitions = self.doc_service.read().unwrap().partition_files(1000);
        for partition in partitions.into_iter(){
            let mut sem_service = self.create_sem_service();
            thread_pool.execute(move ||{
                for uri_string in partition{
                    let uri = match Url::from_str(&uri_string.as_str()){
                        Ok(uri) => uri,
                        _=> continue
                    };

                    // ignore result any errors should be logged inside
                    let _ = sem_service.analyze_uri(&uri, true);
                }
            });
        }
    }

    pub fn generate_definitions(&mut self){
        return self.doc_service.write().unwrap().index_files();
    }

    pub fn get_document_info(&mut self, uri: &Url) -> Result<Arc<RwLock<DocumentInfo>>, ProjectManagerError>{
        return self.doc_service.write().unwrap().get_document_info(uri);
    }

    pub fn get_parsed_document_for_class(&mut self, class: &String, wait_on_lock: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        return self.doc_service.write().unwrap().get_parsed_document_for_class(class, wait_on_lock);
    }

    pub fn get_parsed_document(&mut self, uri: &Url, wait_on_lock: bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        return self.doc_service.write().unwrap().get_parsed_document(uri, wait_on_lock);
    }

    pub fn notify_document_saved(&mut self, uri: &Url) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        return self.doc_service.write().unwrap().notify_document_saved(uri);
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String, threadpool: &ThreadPool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.doc_service.write().unwrap().get_document_info(uri)?;
        // discard old doc, and analyze new content
        doc_info.write().unwrap().set_opened_document(None);
        let new_doc = self.doc_service.write().unwrap().parse_content(full_file_content)?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.write().unwrap().set_opened_document(Some(new_doc.clone()));
        let mut sem_service = self.create_sem_service();
        let new_doc_2 = new_doc.clone();
        threadpool.execute(move ||{
            let _ = sem_service.analyze(new_doc, false);
        });
        return Ok(new_doc_2);
    }

    pub fn notify_document_opened(&mut self, uri: &Url, threadpool: &ThreadPool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc = self.doc_service.write().unwrap().get_parsed_document(uri, true)?;
        // discard old doc, and analyze new content
        let doc_2 = doc.clone();
        let mut sem_service = self.create_sem_service();
        threadpool.execute(move ||{
            let _ = sem_service.analyze(doc, false);
        });
        return Ok(doc_2);
    }

    pub fn get_uri_for_class(& self, class_name: &String)-> Result<Url, ProjectManagerError>{
        return self.doc_service.read().unwrap().get_uri_for_class(class_name);
    }

    fn create_sem_service(&self) -> SemanticAnalysisService{
        return SemanticAnalysisService::new(
            self.doc_service.clone(),
            self.logger.clone(),
            Arc::new(Mutex::new(GenericDiagnosticCollector::new())),
            None
        );
    }

    pub fn analyze_doc(&mut self, uri : &Url, only_definitions:bool) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let mut semantic_analysis_service = self.create_sem_service();
        let doc = semantic_analysis_service.analyze_uri(uri, only_definitions)?;
        return Ok(doc);
    }

    pub fn generate_document_symbols(&mut self, uri : &Url) -> Result<Vec<DocumentSymbol>, ProjectManagerError>{
        // use lighter sym generator if doc not analyzed yet
        let doc = self.doc_service.write().unwrap().get_parsed_document(uri, true)?;
        if doc.lock().unwrap().annotated_ast.is_none(){
            let sym_gen = DocumentSymbolGeneratorFromAst::new();
            let ast = doc.lock().unwrap().ast.clone();
            return Ok(sym_gen.generate_symbols(ast.as_ast_node()))
        }
        // otherwise use symbol table to generate
        let sym_table = match doc.lock().unwrap().get_symbol_table(){
            Some(st) => st.clone(),
            _=> return Err(ProjectManagerError::new("Unable to generate symbol table", ErrorCode::InternalError))
        };
        if sym_table.try_lock().is_err() || sym_table.lock().unwrap().iter_symbols().count() == 0{
            // TODO improve
            // st locked by analysis so fallback to ast
            let sym_gen = DocumentSymbolGeneratorFromAst::new();
            let ast = doc.lock().unwrap().ast.clone();
            return Ok(sym_gen.generate_symbols(ast.as_ast_node()))
        }

        // can generate using st
        let sym_gen = DocumentSymbolGenerator {};
        return Ok(sym_gen.generate_symbols(sym_table))
    }

    pub fn generate_goto_definitions(&mut self, uri : &Url, pos: &Position) -> Result<Vec<LocationLink>, ProjectManagerError>{
        let sem_service = self.create_sem_service();
        let mut def_service = DefinitionService::new(
            self.doc_service.clone(), 
            sem_service, 
            self.logger.clone(),
            uri,
        );
        return def_service.get_definition(&pos)
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
/// tests at this module level are more towards integration testing
pub mod test{
    use std::{fs::{File, self}, io::Read, path::{PathBuf, Path}, time, thread, rc::Rc, cell::RefCell, sync::{Mutex, Arc, RwLock}, str::FromStr};

    use lsp_types::Url;

    use crate::{lexer::{GoldLexer}, parser::{parse_gold, ast::IAstNode, ParserDiagnostic}, utils::{ast_to_string_brief_recursive, ILogger, ConsoleLogger, IDiagnosticCollector, GenericDiagnosticCollector, StdErrLogger, ILoggerV2, StdOutLogger}, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IVisitor, IAnalyzer, AnalyzerDiagnostic}, threadpool::ThreadPool};

    use super::{ProjectManager, document_service::DocumentService, type_resolver::TypeResolver, semantic_analysis_service::SemanticAnalysisService, definition_service::DefinitionService};

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

    pub fn create_test_doc_service(uri : Option<Url>) -> DocumentService{
        return DocumentService::new(uri, create_test_logger()).unwrap();
    }

    fn create_test_ast_walker<T:IVisitor+?Sized>()-> AstWalker<T>{
        let result = AstWalker::<T>::new(true);
        return result;
    }

    pub fn create_test_logger()-> Arc<dyn ILoggerV2>{
        Arc::new(StdOutLogger::new("[LSP Server]"))
    }

    pub fn create_uri_from_path(path:&str)-> Url{
        let path = PathBuf::from_str(path).unwrap();
        let path = std::fs::canonicalize(path).unwrap();
        return Url::from_file_path(path).unwrap();
    }

    pub fn create_test_project_manager(root: &str) -> ProjectManager{
        let uri = create_uri_from_path(root);
        return ProjectManager::new(Some(uri), create_test_logger()).unwrap()
    }

    pub fn create_test_diag_collector()->Arc<Mutex<dyn IDiagnosticCollector<AnalyzerDiagnostic>>>{
        return Arc::new(Mutex::new(GenericDiagnosticCollector::new()))
    }

    pub fn create_test_sem_service(doc_service: Arc<RwLock<DocumentService>>)-> SemanticAnalysisService{
        return SemanticAnalysisService::new(
            doc_service,
            create_test_logger(),
            create_test_diag_collector(),
            None
        )
    }

    pub fn create_test_type_resolver(doc_service: Arc<RwLock<DocumentService>>) -> TypeResolver{
        let sem_analysis_service = create_test_sem_service(doc_service);
        return TypeResolver::new(sem_analysis_service);
    }

    pub fn create_test_def_service(doc_service: Arc<RwLock<DocumentService>>, uri: &Url) -> DefinitionService{
        let sem_analysis_service = create_test_sem_service(doc_service.clone());
        return DefinitionService::new(
            doc_service,
            sem_analysis_service,
            create_test_logger(),
            &uri)
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
        // try request
        let _ = doc_manager.generate_document_symbols(&file_uri).unwrap();
    }

    #[test]
    fn test_index_file_large(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        proj_manager.index_files();
        assert!(proj_manager.doc_service.read().unwrap().count_files() > 54000);
    }

    #[ignore="long running time"]
    #[test]
    fn test_analyze_files_large(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let threadpool = ThreadPool::new(5,proj_manager.logger.clone());
        proj_manager.index_files();
        proj_manager.analyze_files(&threadpool);
        drop(threadpool);
        assert!(proj_manager.doc_service.read().unwrap().count_files() > 54000);
    }

    #[test]
    fn test_file_outside_workspace_after_index(){
        // skip if dir doesn't exist
        let uri = create_uri_from_path("./test/aTestClass.god");
       
        let mut proj_manager= create_test_project_manager("./test/workspace");
        proj_manager.index_files();
        // test file outside workspace
        let _ = proj_manager.generate_document_symbols(&uri).unwrap();
    }

    #[test]
    fn test_get_document_symbols(){
        let mut proj_manager= create_test_project_manager("./test/workspace");
        let input = create_uri_from_path("./test/workspace/aRootClass.god");
        let result = proj_manager.generate_document_symbols(&input).unwrap();
        assert_eq!(result.len(), 1);
    }

    /// 
    #[ignore = "takes too long"]
    #[test]
    fn test_generate_doc_sym_large(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        proj_manager.index_files();
        let input_uri = create_uri_from_path("C:\\Users\\muhampra\\dev\\ewam_src\\37.63.03\\OcsMobileCards\\USIM 20\\aOcsUsimV20ICCardImage.god");
        // let _result = proj_manager.generate_document_diagnostic_report(&input_uri).unwrap();
        let result = proj_manager.generate_document_symbols(&input_uri).unwrap();
        assert_eq!(result.len(), 1);
    }
    

    /// 
    #[ignore = "long runing time"]
    #[test]
    fn test_generate_doc_sym_module(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        proj_manager.index_files();
        let input_uri = create_uri_from_path("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev\\src\\System\\SystemModules\\wGraph.god");
        // let _result = proj_manager.generate_document_diagnostic_report(&input_uri).unwrap();
        let result = proj_manager.generate_document_symbols(&input_uri).unwrap();
        assert_eq!(result.len(), 219);
    }

    #[ignore = "long running time"]
    #[test]
    fn test_analyze_doc_large(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        proj_manager.index_files();
        let input_uri = proj_manager.get_uri_for_class(&"aOcsUSIMV20ICCardImage".to_string()).unwrap();
        // let _result = proj_manager.generate_document_diagnostic_report(&input_uri).unwrap();
        let result = proj_manager.analyze_doc(&input_uri, false).unwrap();
        assert!(result.lock().unwrap().annotated_ast.is_some());
    }

    #[ignore = "for benchmarking"]
    #[test]
    fn test_parsed_50_000(){
        let path = PathBuf::from("./test/aNormalSizedClass.god");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let file_content = std::fs::read_to_string(path).unwrap();
        let doc_service= create_test_doc_service(None);
        
        let now = std::time::Instant::now();
        let num_iter = 50_000;
        for _i in 0..num_iter{
            let mut doc = doc_service.parse_content(&file_content).unwrap();
            doc.only_definitions = true
        }
        let elapse = now.elapsed();
        print!("Ran {num_iter} in {:.2?}", elapse);
    }
}