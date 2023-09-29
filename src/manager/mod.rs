use std::{collections::HashMap, fs::File, io::Read, rc::Rc, sync::{Arc, Mutex, RwLock}, cell::RefCell, error::Error, fmt::Display, borrow::BorrowMut};

use lsp_server::ErrorCode;
use lsp_types::{DocumentSymbol, SymbolKind, Diagnostic, RelatedFullDocumentDiagnosticReport, DiagnosticSeverity, FullDocumentDiagnosticReport, Url};

use crate::{parser::ast::{IAstNode, AstClass, AstConstantDeclaration, AstProcedure, AstGlobalVariableDeclaration, AstTypeDeclaration, AstFunction}, parser::{ParserDiagnostic, parse_gold}, lexer::GoldLexer, utils::IRange, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IAnalyzer, IVisitor}, threadpool::ThreadPool};
use data_structs::*;

use self::symbol_generator::ISymbolTable;

pub mod data_structs;
pub mod symbol_generator;

#[derive(Debug)]
pub struct ProjectManager{
    threadpool : ThreadPool,
    uri_docinfo_map : Arc<RwLock<HashMap<String, Arc<Mutex<DocumentInfo>>>>>,
    class_uri_map: Arc<RwLock<HashMap<String, String>>>,
    root_path: Option<String>,
}
impl ProjectManager{
    pub fn new(root_uri : Option<Url>) -> Result<ProjectManager, ProjectManagerError>{
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
            threadpool: ThreadPool::new(5),
            class_uri_map: Arc::new(RwLock::new(HashMap::new()))
        })
    }

    pub fn index_files(&mut self){
        if self.root_path.is_none() {return};

        let root_path = self.root_path.as_ref().unwrap().clone();
        let uri_docinfo_map = self.uri_docinfo_map.clone();
        let class_uri_map = self.class_uri_map.clone();
        self.threadpool.execute(move ||{
            let mut stack = Vec::new();
            // stack for pushing dirs
            
            stack.push(root_path);
            // lock here and keep locked until all files indexed
            let mut uri_docinfo_map = uri_docinfo_map.write().unwrap();
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
                            let new_doc_info = Arc::new(Mutex::new(new_doc_info));
                            uri_docinfo_map.insert(uri.to_string(), new_doc_info);
                            // index class name to file uri
                            match entry.path().as_path().file_stem(){
                                Some(p) => {
                                    match p.to_str(){
                                        Some(s) => {class_uri_map.write().unwrap().insert(s.to_string(), uri.to_string());},
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
        });
    }

    pub fn get_document_info(&mut self, uri: &Url) -> Result<Arc<Mutex<DocumentInfo>>, ProjectManagerError>{
        let uri_string = uri.to_string();
        let proj_manager=  self.uri_docinfo_map.read().unwrap();
        let doc_info =  proj_manager.get(&uri_string);
        if doc_info.is_some() {
            return Ok(doc_info.unwrap().clone());
        } else {
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
            let new_doc_info = Arc::new(Mutex::new(new_doc_info));
            self.uri_docinfo_map.write().unwrap().insert(uri_string.clone(), new_doc_info.clone());
            Ok(new_doc_info)     
        }
    }

    pub fn get_parsed_document(&mut self, uri: &Url) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        // check opened document
        if doc_info.lock().unwrap().get_opened_document().is_some() {
            return Ok(doc_info.lock().unwrap().get_opened_document().unwrap());
        }
        // check last saved doc
        if doc_info.lock().unwrap().get_saved_document().is_some() {
            return Ok(doc_info.lock().unwrap().get_saved_document().unwrap());
        } else {
            // if none, read from file
            let new_doc = self.parse_document(doc_info.lock().unwrap().file_path.as_str())?;
            doc_info.lock().unwrap().set_saved_document(Some(Arc::new(Mutex::new(new_doc))));
            return Ok(doc_info.lock().unwrap().get_saved_document().unwrap());
        }
    }

    pub fn get_document_symbols(&mut self, doc: Arc<Mutex<Document>>)-> Arc<Vec<DocumentSymbol>>{
        let syms = doc.lock().unwrap().get_document_symbols();
        if syms.is_some() {
            return syms.unwrap();
        } else {
            return self.generate_document_symbols(doc).unwrap();
        }
    }

    pub fn get_diagnostic_report(&mut self, doc: Arc<Mutex<Document>>) -> Arc<RelatedFullDocumentDiagnosticReport>{
        let diag_report = doc.lock().unwrap().get_diagnostic_report();
        if diag_report.is_some(){
            return diag_report.unwrap();
        } else {
            return self.generate_document_diagnostic_report(doc).unwrap();
        }
    }

    pub fn notify_document_saved(&mut self, uri: &Url) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_document(doc_info.lock().unwrap().file_path.as_str())?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.lock().unwrap().set_saved_document(Some(new_doc.clone()));
        doc_info.lock().unwrap().set_opened_document(None);
        return Ok(new_doc);
    }

    pub fn notify_document_changed(&mut self, uri: &Url, full_file_content: &String) -> Result<Arc<Mutex<Document>>, ProjectManagerError>{
        let doc_info = self.get_document_info(uri)?;
        let new_doc = self.parse_content(full_file_content)?;
        let new_doc = Arc::new(Mutex::new(new_doc));
        doc_info.lock().unwrap().set_opened_document(Some(new_doc.clone()));
        return Ok(new_doc);
    }

    pub fn get_uri_for_class(&self, class_name: &String)-> Result<String, ProjectManagerError>{
        match self.class_uri_map.read().unwrap().get(class_name){
            Some(uri) => Ok(uri.clone()),
            _=> Err(ProjectManagerError::new("no uri found for class", ErrorCode::InternalError))
        }
    }

    pub fn get_symbol_table(&mut self, class: String) -> Result<Arc<Mutex<dyn ISymbolTable>>, ProjectManagerError>{
        let uri = self.get_uri_for_class(&class)?;
        let doc: Arc<Mutex<Document>> = self.get_parsed_document(&Url::parse(uri.as_str()).unwrap())?;
        return self.get_or_generate_symbols(doc);
    }

    fn get_or_generate_symbols(&self, doc:Arc<Mutex<Document>>) -> Result<Arc<Mutex<dyn ISymbolTable>>,ProjectManagerError>{
        if doc.lock().unwrap().get_symbol_table().is_some(){
            return Ok(doc.lock().unwrap().get_symbol_table().unwrap());
        } 

        let sym_table = self.generate_symbol_table(doc.clone())?;
        doc.lock().unwrap().set_symbol_table(Some(sym_table.clone()));
        return Ok(sym_table)
    }

    fn generate_symbol_table(&self, doc: Arc<Mutex<Document>>)->Result<Arc<Mutex<dyn ISymbolTable>>,ProjectManagerError>{
        todo!()
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

    fn analyze_ast<'a>(&self, ast : &'a dyn IAstNode) -> Vec<lsp_types::Diagnostic>{
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

    fn generate_document_symbols(&self, doc: Arc<Mutex<Document>>) -> Result<Arc<Vec<DocumentSymbol>>, ProjectManagerError>{
        let symbol_generator = DocumentSymbolGenerator::new();
        let symbols = symbol_generator.generate_symbols(doc.lock().unwrap().get_ast());
        let symbols = Arc::new(symbols);
        doc.lock().unwrap().set_document_symbols(Some(symbols.clone()));
        return Ok(symbols);
    }

    fn generate_document_diagnostic_report(&self, doc: Arc<Mutex<Document>>)
    -> Result<Arc<RelatedFullDocumentDiagnosticReport>, ProjectManagerError>{
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

struct DocumentSymbolGenerator{

}
impl DocumentSymbolGenerator{
    pub fn new() -> DocumentSymbolGenerator{
        return DocumentSymbolGenerator {  }
    }

    pub fn generate_symbols(&self, ast: &dyn IAstNode)-> Vec<DocumentSymbol> {
        let mut result = Vec::<DocumentSymbol>::new();
        let mut class_symbol = self.find_and_generate_class_symbol(ast);
        for node in ast.get_children().unwrap_or_default().iter() {
            let symbol = self.generate_symbol_for_node(node.as_ast_node());
            if symbol.is_some(){
                match &mut class_symbol {
                    Some(class_sym) => class_sym.children.as_mut().unwrap().push(symbol.unwrap()),
                    None => result.push(symbol.unwrap())
                };
            }
        }
        if class_symbol.is_some(){result.push(class_symbol.unwrap())}
        return result;
    }

    fn generate_symbol_for_node(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        let mut result : Option<DocumentSymbol>= None;
        match self.generate_constant_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_type_declaration_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_global_var_decl_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_proc_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        match self.generate_func_symbol(ast_node) {
            Some(s) => {result = Some(s);},
            None => ()
        };
        return result;
    }

    fn generate_constant_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstConstantDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: Some(n.value.value.as_ref().unwrap().to_string()), 
                    kind: SymbolKind::CONSTANT, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_type_declaration_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstTypeDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: None, 
                    kind: SymbolKind::PROPERTY, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_global_var_decl_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstGlobalVariableDeclaration>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.value.as_ref().unwrap().to_string(), 
                    detail: Some(n.type_node.get_identifier()), 
                    kind: SymbolKind::FIELD, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_proc_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstProcedure>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.get_identifier(), 
                    detail: None, 
                    kind: SymbolKind::METHOD, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn generate_func_symbol(&self, ast_node: &dyn IAstNode)-> Option<DocumentSymbol>{
        match ast_node.as_any().downcast_ref::<AstFunction>(){
            Some(n) => Some(DocumentSymbol { 
                    name: n.identifier.get_identifier(), 
                    detail: Some(n.return_type.get_identifier()), 
                    kind: SymbolKind::FUNCTION, 
                    range: n.get_range().as_lsp_type_range(), 
                    selection_range: n.identifier.get_range().as_lsp_type_range(), 
                    tags: None,
                    deprecated: None,
                    children: None
            }),
            None => None
        }
    }

    fn find_and_generate_class_symbol(&self, ast: &dyn IAstNode) -> Option<DocumentSymbol>{
        let mut result: Option<DocumentSymbol> = None;
        for ast_node in ast.get_children().unwrap_or_default().iter() {
            match ast_node.as_any().downcast_ref::<AstClass>(){
                Some(n) => {
                    result = Some(DocumentSymbol { 
                        name: n.name.clone(), 
                        detail: Some(n.parent_class.clone()), 
                        kind: SymbolKind::CLASS, 
                        range: n.get_range().as_lsp_type_range(), 
                        selection_range: n.get_range().as_lsp_type_range(), 
                        tags: None,
                        deprecated: None,
                        children: Some(Vec::new())
                    });
                    break;
                }
                None => continue
            }
        }
        return result;
    }
}

#[cfg(test)]
mod test{
    use std::{fs::{File, self}, io::Read, path::PathBuf, time, thread, rc::Rc, cell::RefCell};

    use lsp_types::Url;

    use crate::{lexer::{GoldLexer}, parser::{parse_gold, ast::IAstNode, ParserDiagnostic}, utils::ast_to_string_brief_recursive, analyzers::{ast_walker::AstWalker, unused_var_analyzer::UnusedVarAnalyzer, inout_param_checker::InoutParamChecker, function_return_type_checker::FunctionReturnTypeChecker, IVisitor, IAnalyzer}};

    use super::ProjectManager;

    fn parse_and_analyze(file_path: &str) -> (Box<dyn IAstNode>, Vec<ParserDiagnostic>){
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

    fn create_ast_walker<T:IVisitor+?Sized>()-> AstWalker<T>{
        let result = AstWalker::<T>::new(true);
        return result;
    }

    fn collect_diagnostics(analyzers: &Vec<Box<dyn IAnalyzer>>) -> Vec<lsp_types::Diagnostic>{
        let mut result = Vec::new();
        for analyzer in analyzers{
            analyzer.append_diagnostics(&mut result);
        }
        return result;
    }

    #[test]
    fn test_gold_document_manager(){
        let doc_manager = ProjectManager::new(None).unwrap();
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
        for node in ast.0.1.get_children().unwrap_or_default().iter(){
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
        for node in ast.0.1.get_children().unwrap_or_default().iter(){
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
        for node in ast.0.1.get_children().unwrap_or_default().iter(){
            println!("{}",ast_to_string_brief_recursive(node.as_ast_node()));
        }
        // println!("{:#?}", ast.1.len());
    }

    #[test]
    fn test_unused_var_file() {
        let (asts, _) = parse_and_analyze("./test/aTestUnusedVar.god");
        let mut walker = create_ast_walker::<dyn IAnalyzer>();
        let mut analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(UnusedVarAnalyzer::new()));
        walker.register_visitor(&analyzer);
        walker.run(asts.as_ast_node());
        // for diag in diags{
        //     print!("{:#?}",diag);
        // }
        assert_eq!(analyzer.borrow().get_diagnostics_count(), 1);
    }

    #[test]
    fn test_varbytearray_param_checker_file() {
        let (asts, _) = parse_and_analyze("./test/aTestVarByteArrayParamChecker.god");
        let mut walker = create_ast_walker::<dyn IAnalyzer>();
        let mut analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(InoutParamChecker::new()));
        walker.register_visitor(&analyzer);
        let diags = walker.run(asts.as_ast_node());
        // for diag in diags{
        //     print!("{:#?}",diag);
        // }
        assert_eq!(analyzer.borrow().get_diagnostics_count(), 2);
    }

    #[test]
    fn test_func_return_type_checker_param_file() {
        let (asts, _) = parse_and_analyze("./test/aTestFunctionReturnTypeChecker.god");
        let mut walker = create_ast_walker::<dyn IAnalyzer>();
        let analyzer:Rc<RefCell<dyn IAnalyzer>> = Rc::new(RefCell::new(FunctionReturnTypeChecker::new()));
        walker.register_visitor(&analyzer);
        let diags = walker.run(asts.as_ast_node());
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
       
        let mut doc_manager = ProjectManager::new(Some(uri)).unwrap();
        doc_manager.index_files();

        // to prevent termination before index has a chance to run
        let ten_millis = time::Duration::from_millis(500);
        thread::sleep(ten_millis);
        
        let map = doc_manager.uri_docinfo_map.clone();
        let map = map.read().unwrap();
        assert_eq!(map.len(), 4);
    }
}