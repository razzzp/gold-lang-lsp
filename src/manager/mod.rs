use std::{rc::Rc, sync::{Arc, Mutex}, cell::RefCell, str::FromStr};

use lsp_types::{DocumentSymbol, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url, LocationLink, CompletionItem, TypeHierarchyItem};
use regex::Regex;

use crate::{parser::ast::IAstNode, utils::{IRange, GenericDiagnosticCollector, Position, ILoggerV2, IDiagnosticCollector}, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, function_return_type_checker::FunctionReturnTypeChecker, IAnalyzer}, threadpool::ThreadPool, analyzers_v2::inherited_checker::{InheritedChecker}};
use data_structs::*;

use self::{
    semantic_analysis_service::{SemanticAnalysisService, AnalyzeRequestOptions}, 
    document_service::DocumentService,  definition_service::DefinitionService, 
    completion_service::CompletionService, 
    entity_tree_service::EntityTreeService, 
    type_hierarchy_service::TypeHierarchyService};

use crate::analyzers_v2::{
    annotated_ast_walker::{AnnotatedAstWalkerPreOrder, IAnnotatedNodeVisitor}, 
    naming_convention_checker::NamingConventionChecker,
    unpurged_varbytearray_checker::UnpurgedVarByteArrayChecker,
    doc_symbol_generator::DocumentSymbolGeneratorFromAst,
};

pub mod data_structs;
pub mod semantic_analysis_service;
pub mod definition_service;
pub mod document_service;
pub mod utils;
pub mod completion_service;
pub mod entity_tree_service;
pub mod type_hierarchy_service;

#[derive(Debug)]
pub struct ProjectManager{
    pub doc_service: DocumentService,
    pub entity_tree_service: EntityTreeService,
    logger: Box<dyn ILoggerV2>,
}
impl Clone for ProjectManager{
    fn clone(&self) -> Self {
        Self { doc_service: self.doc_service.clone(), entity_tree_service: self.entity_tree_service.clone(), logger: self.logger.clone_box() }
    }
}
impl ProjectManager{
    pub fn new(root_uri : Option<Url>, logger: Box<dyn ILoggerV2>) -> Result<ProjectManager, ProjectManagerError>{
        let mut doc_service_logger = logger.clone_box();
        doc_service_logger.append_prefix("[Doc Service]");
        let doc_service = DocumentService::new(root_uri.clone(), doc_service_logger)?;

        let mut tree_service_logger = logger.clone_box();
        tree_service_logger.append_prefix("[Entity Tree Service]");
        let class_module_tree_service = EntityTreeService::new(15_000, tree_service_logger);
        Ok(ProjectManager{
            doc_service,
            logger,
            entity_tree_service: class_module_tree_service,
        })
    }

    pub fn index_files(&mut self){
        return self.doc_service.index_files();
    }

    pub fn analyze_core_files(&mut self){

        let map = self.doc_service.get_doc_info_mapping();
        let sem_service = self.create_sem_service();
        // collect WAM and WF files
        let regx= Regex::new(r"/(WAM|WF)\w*?/").unwrap();
        let map_lock = map.read().unwrap();
        let files_to_process : Vec<Url> = map_lock
            .values()
            .filter_map(|v|{
                let lock = v.read().unwrap();
                if regx.is_match(&lock.uri)
                {return Url::from_str(&lock.uri).ok()} else {return None};
            })
            .collect();
        drop(map_lock);

        let timer  = std::time::SystemTime::now();
        self.logger.log_info(format!("Analyzing core files; {} files", &files_to_process.len()).as_str());

        for uri in &files_to_process{
            // don't need to cache, just generate top level symbol level
            let _ = sem_service.analyze_uri(uri, AnalyzeRequestOptions::default().set_cache(false).set_only_def(true));
        }
        self.logger.log_info(
            format!("Finished analyzing core files in {:#?}; {} files", 
            timer.elapsed().unwrap_or_default(), 
            files_to_process.len()).as_str());
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String, _threadpool: &ThreadPool) -> Result<(), ProjectManagerError>{
        let doc_info = self.doc_service.get_document_info(uri)?;
        // discard old opened doc
        doc_info.write().unwrap().reset_transient_data();

        let new_doc = self.doc_service.parse_content(full_file_content)?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.write().unwrap().set_opened_document(Some(new_doc.clone()));

        return Ok(());
    }

    pub fn notify_document_opened(&mut self, uri: &Url, _threadpool: &ThreadPool) -> Result<(), ProjectManagerError>{
        let _doc_info = self.doc_service.get_document_info(uri)?;
        // do nothing? client should already call get diagnostics, etc.
        // doc_info.write().unwrap().set_opened_document(None);
        return Ok(());
    }

    pub fn notify_document_saved(&mut self, uri: &Url, _threadpool: &ThreadPool) -> Result<(), ProjectManagerError>{
        let doc_info = self.doc_service.get_document_info(uri)?;
        // purge parsed data, so next req will reparse everything
        doc_info.write().unwrap().reset_all_data();
        //re-index files
        self.doc_service.index_files();
        return Ok(());
    }

    fn create_sem_service(&self) -> SemanticAnalysisService{
        return SemanticAnalysisService::new(
            self.doc_service.clone(),
            self.logger.clone_box_with_appended_prefix("[Sem Service]"),
            Arc::new(Mutex::new(GenericDiagnosticCollector::new()))
        );
    }

    pub fn analyze_doc(&mut self, uri : &Url, options:AnalyzeRequestOptions) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let semantic_analysis_service = self.create_sem_service();
        let doc = semantic_analysis_service.analyze_uri(uri, options)?;
        return Ok(doc);
    }

    pub fn generate_document_symbols(&mut self, uri : &Url) -> Result<Vec<DocumentSymbol>, ProjectManagerError>{
        // use lighter sym generator if doc not analyzed yet
        let doc = self.doc_service.get_parsed_document(uri, true)?;
        
        let sym_gen = DocumentSymbolGeneratorFromAst::new();
        let ast = doc.lock().unwrap().ast.clone();
        return Ok(sym_gen.generate_symbols(ast.as_ast_node()))

        // annotated node may be present but not complete, so just generate from ast
        // // otherwise use symbol table to generate
        // let sym_table = match doc.lock().unwrap().get_symbol_table(){
        //     Some(st) => st.clone(),
        //     _=> return Err(ProjectManagerError::new("Unable to generate symbol table", ErrorCode::InternalError))
        // };
        // if sym_table.try_lock().is_err() || sym_table.lock().unwrap().iter_symbols().count() == 0{
        //     // TODO improve
        //     // st locked by analysis so fallback to ast
        //     let sym_gen = DocumentSymbolGeneratorFromAst::new();
        //     let ast = doc.lock().unwrap().ast.clone();
        //     return Ok(sym_gen.generate_symbols(ast.as_ast_node()))
        // }

        // // can generate using st
        // let sym_gen = DocumentSymbolGenerator {};
        // return Ok(sym_gen.generate_symbols(sym_table))
    }

    pub fn generate_goto_definitions(&mut self, uri : &Url, pos: &Position) -> Result<Vec<LocationLink>, ProjectManagerError>{
        let sem_service = self.create_sem_service();
        let def_service = DefinitionService::new(
            self.doc_service.clone(), 
            sem_service, 
            self.logger.clone_box_with_appended_prefix("Definition Service"),
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
            // Rc::new(RefCell::new(InoutParamChecker::new())),
            Rc::new(RefCell::new(FunctionReturnTypeChecker::new()))
        ];
        ast_walker.register_visitors(&analyzers);
        ast_walker.run(ast);
        
        return self.collect_diagnostics(&analyzers)
    }

    pub fn generate_document_diagnostic_report(&mut self, uri : &Url)
    -> Result<Arc<RelatedFullDocumentDiagnosticReport>, ProjectManagerError>{
        let diagnostics = self.generate_diagnostics(uri)?;
        return Ok(Arc::new(RelatedFullDocumentDiagnosticReport{
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport{
                result_id: None,
                items: diagnostics,
            },
        }))
    }

    /// analyzes doc and generates diags
    fn generate_diags_on_annotated_ast(&self, uri : &Url) -> Option<Vec<Diagnostic>>{

        let sem_service  = self.create_sem_service();
        let doc = sem_service.analyze_uri(uri, AnalyzeRequestOptions::default().set_cache(true)).ok()?;
        let annotated_ast = doc.lock().unwrap().annotated_ast.as_ref()?.clone();
        // wait for annonation to finish if locked
        let annotation_done_flag = doc.lock().unwrap().annotation_done.clone();
        drop(annotation_done_flag.lock().unwrap());

        // prepare analyzers
        let diag_collector = Arc::new(Mutex::new(GenericDiagnosticCollector::<Diagnostic>::new()));
        let unpurged_checker: Box<dyn IAnnotatedNodeVisitor> = 
            Box::new(UnpurgedVarByteArrayChecker::new(diag_collector.clone()));
        let name_checker: Box<dyn IAnnotatedNodeVisitor> = 
            Box::new(NamingConventionChecker::new(diag_collector.clone()));
        let inherited_checker : Box<dyn IAnnotatedNodeVisitor> = 
            Box::new(InheritedChecker::new(diag_collector.clone()));
        // analyze
        let mut walker = AnnotatedAstWalkerPreOrder::new();
        walker.register_visitor(unpurged_checker);
        walker.register_visitor(name_checker);
        walker.register_visitor(inherited_checker);
        walker.walk(&annotated_ast);

        let diags = diag_collector.lock().unwrap().take_diagnostics();
        return Some(diags)
    }

    fn generate_diagnostics(&self, uri : &Url) -> Result<Vec<Diagnostic>, ProjectManagerError>{
        let doc = self.doc_service.get_parsed_document(uri, true)?;
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
        // analyzers on IAstNode
        let analyzer_diags = self.get_analyzer_diagnostics(doc.clone())?;
        analyzer_diags.iter().for_each(|d|{result.push(d.clone())});
        // analyzers on annotated node
        result.extend(self.generate_diags_on_annotated_ast(uri).unwrap_or_default());

        return Ok(result);
    }

    pub fn generate_completion_proposals(
        &mut self,
        uri : &Url, 
        pos : &Position,
    ) -> Result<Vec<CompletionItem>, ProjectManagerError>
    {   
        let completion_service = CompletionService::new(
            self.doc_service.clone(), 
            self.create_sem_service(), 
            uri.clone(),
            self.logger.clone_box_with_appended_prefix("Completion Service"));
        return completion_service.generate_completion_proposals(pos);
    }

    pub fn prepare_type_hierarchy(
        &mut self,
        uri : &Url, 
        pos : &Position,
    ) -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>
    {   
        let type_hierarchy_service = TypeHierarchyService::new(
            self.create_sem_service(),
            self.entity_tree_service.clone(),
            self.logger.clone_box_with_appended_prefix("Type Hierarchy Service"));
        return type_hierarchy_service.prepare_type_hierarchy(uri, pos);
    }

    pub fn type_hierarchy_subtypes(
        &mut self,
        item : &TypeHierarchyItem
    ) -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>
    {   
        let type_hierarchy_service = TypeHierarchyService::new(
            self.create_sem_service(),
            self.entity_tree_service.clone(),
            self.logger.clone_box_with_appended_prefix("Type Hierarchy Service")
        );
        return type_hierarchy_service.type_hierarchy_subtypes(item);
    }

    pub fn type_hierarchy_supertypes(
        &mut self,
        item: &TypeHierarchyItem
    ) -> Result<Vec<TypeHierarchyItem>, ProjectManagerError>
    {   
        let type_hierarchy_service = TypeHierarchyService::new(
            self.create_sem_service(),
            self.entity_tree_service.clone(),
            self.logger.clone_box_with_appended_prefix("Type Hierarchy Service")
        );
        return type_hierarchy_service.type_hierarchy_supertypes(item);
    }
}



#[cfg(test)]
/// tests at this module level are more towards integration testing
pub mod test{
    use std::{fs::{File, self}, io::Read, path::PathBuf, rc::Rc, cell::RefCell, sync::{Arc}, str::FromStr};

    use lsp_types::Url;

    use crate::{analyzers::{ast_walker::AstWalker, function_return_type_checker::FunctionReturnTypeChecker, inout_param_checker::InoutParamChecker, unused_var_analyzer::UnusedVarAnalyzer, IAnalyzer, IVisitor}, lexer::GoldLexer, manager::semantic_analysis_service::AnalyzeRequestOptions, parser::{ast::IAstNode, parse_gold, ParserDiagnostic}, utils::{ast_to_string_brief_recursive, test_utils::create_test_diag_collector, ILoggerV2, LogLevel, StdOutLogger}};

    use super::{
        ProjectManager, 
        document_service::DocumentService,
        semantic_analysis_service::SemanticAnalysisService,
        definition_service::DefinitionService
    };
    use crate::analyzers_v2::type_resolver::TypeResolver;

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

    pub fn create_test_logger()-> Box<dyn ILoggerV2>{
        Box::new(StdOutLogger::new("[LSP Server]", LogLevel::Verbose))
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

    pub fn create_test_sem_service(doc_service: DocumentService)-> SemanticAnalysisService{
        return SemanticAnalysisService::new(
            doc_service,
            create_test_logger(),
            create_test_diag_collector()
        )
    }

    pub fn create_test_type_resolver(doc_service: DocumentService) -> TypeResolver{
        let sem_analysis_service = create_test_sem_service(doc_service);
        return TypeResolver::new(sem_analysis_service);
    }

    pub fn create_test_def_service(doc_service: DocumentService, uri: &Url) -> DefinitionService{
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
        let analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(UnusedVarAnalyzer::new()));
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
        let analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(InoutParamChecker::new()));
        walker.register_visitor(&analyzer);
        let _diags = walker.run(&asts);
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
        let _diags = walker.run(&asts);
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
        assert!(proj_manager.doc_service.count_files() > 54000);
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
        let input_uri = proj_manager.doc_service.get_uri_for_class(&"aOcsUSIMV20ICCardImage".to_string()).unwrap();
        // let _result = proj_manager.generate_document_diagnostic_report(&input_uri).unwrap();
        let result = proj_manager.analyze_doc(&input_uri, AnalyzeRequestOptions::default().set_cache(true)).unwrap();
        assert!(result.lock().unwrap().annotated_ast.is_some());
    }


    #[ignore = "need repo to test"]
    #[test]
    fn test_analyze_core_files(){
        let path = PathBuf::from("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        let _ =  match fs::metadata(&path){
            Ok(_) => (),
            _=> return
        };
        let mut proj_manager= create_test_project_manager("C:\\Users\\muhampra\\dev\\projects\\razifp\\cps-dev");
        proj_manager.index_files();
        proj_manager.analyze_core_files();
    }
}