use std::net::ToSocketAddrs;
use std::{io::Read, fs::File};

use crate::manager::GoldProjectManager;
use crate::{lexer::GoldLexer, parser::parse_gold};
use std::error::Error;

use lsp_types::notification::{DidChangeTextDocument, PublishDiagnostics, DidSaveTextDocument};
use lsp_types::{OneOf, DocumentSymbolResponse, DocumentSymbol, SymbolKind, Range, Url, DocumentSymbolParams, DiagnosticOptions, DiagnosticServerCapabilities, DocumentDiagnosticParams, DocumentDiagnosticReport, TextDocumentSyncKind, TextDocumentSyncCapability, DidChangeTextDocumentParams, PublishDiagnosticsParams, lsp_notification, DidSaveTextDocumentParams};
use lsp_types::request::{DocumentSymbolRequest, DocumentDiagnosticRequest};
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response, ResponseError, ErrorCode, Notification,};
use nom::error;
use parser::GoldDocumentError;

pub mod lexer;
pub mod parser;
pub mod utils;
pub mod manager;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");
    // determine which transport to use
    let args: Vec<String>= std::env::args().collect();
    let transport_arg = args.iter().fold(String::new(),
        |mut acc, cur| match cur.as_str() {
            "--socket" | "--stdio" | "--pipe" => if acc.is_empty() {acc.push_str(cur.as_str()); acc} else {acc},
            _ => acc
        }
    );
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    // TODO don't hardcode port
    let mut addrs_iter = "localhost:5001".to_socket_addrs().unwrap();
    let (connection, io_threads) = match transport_arg.as_str() {
        "--socket" => Connection::connect(addrs_iter.next().unwrap())?,
        "--stdio" => Connection::stdio(),
        _=> Connection::connect(addrs_iter.next().unwrap())?,
    };
    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions::default())),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    }).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    
    let mut doc_manager = GoldProjectManager::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                let req = match cast_req::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        match handle_document_symbol_request(&mut doc_manager, id.clone(), params){
                            Ok(resp) => connection.sender.send(resp)?,
                            Err(e) => send_error(&connection, id, e.0, e.1)?
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast_req::<DocumentDiagnosticRequest>(req) {
                    Ok((id, params)) => {
                        match handle_document_diagnostics_request(&mut doc_manager, id.clone(), params){
                            Ok(resp) => connection.sender.send(resp)?,
                            Err(e) => send_error(&connection, id, e.0, e.1)?
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                
                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
                let not = match cast_not::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_change_notification(&mut doc_manager, params){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => eprintln!("error: code({}) {}", e.0, e.1)
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast_not::<DidSaveTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_save_notification(&mut doc_manager, params){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => eprintln!("error: code({}) {}", e.0, e.1)
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            }
        }
    }
    Ok(())
}

fn handle_document_symbol_request(
    doc_manager: &mut GoldProjectManager, 
    id: RequestId, 
    params: DocumentSymbolParams)
    -> Result<Message, (i32, String)>
{
    eprintln!("got Document Symbol request #{id}: {params:?}");
    let doc = match doc_manager.get_parsed_document(&params.text_document.uri) {
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let symbols = doc.get_symbols();
    let result = Some(DocumentSymbolResponse::Nested(symbols));
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_document_diagnostics_request(
    doc_manager: &mut GoldProjectManager, 
    id: RequestId, 
    params: DocumentDiagnosticParams)
    -> Result<Message, (i32, String)>
{
    eprintln!("got Document Diagnostics request #{id}: {params:?}");
    let doc = match doc_manager.get_parsed_document(&params.text_document.uri) {
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = doc.get_diagnostic_report();
    let result = DocumentDiagnosticReport::Full(diag_report);
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_did_change_notification(
    doc_manager: &mut GoldProjectManager, 
    params: DidChangeTextDocumentParams)
    -> Result<Vec<Message>, (i32, String)>
{
    eprintln!("got Did Change notification {params:?}");
    // get full file content
    let full_file_content = match params.content_changes.last(){
        Some(text_doc_change_event) => {
            if text_doc_change_event.range.is_some(){
                return Err((ErrorCode::InvalidParams as i32, "Incremental did change event not supported".to_string()))
            }
            &text_doc_change_event.text
        },
        None=> return Err((ErrorCode::InvalidParams as i32, "Incremental did change event not supported".to_string()))
    };
    let doc = match doc_manager.notify_document_changed(&params.text_document.uri, full_file_content){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = doc.get_diagnostic_report();
    let pub_diag_params = PublishDiagnosticsParams::new(
        params.text_document.uri, 
        diag_report.full_document_diagnostic_report.items.clone(), 
        Some(params.text_document.version));
    let params_serialized = serde_json::to_value(&pub_diag_params).unwrap();
    let publish_diag = Notification{
        method: "textDocument/publishDiagnostics".to_string(),
        params: params_serialized
    };
    let mut result = Vec::<Message>::new();
    result.push(Message::Notification(publish_diag));
    return Ok(result)
}

fn handle_did_save_notification(
    doc_manager: &mut GoldProjectManager, 
    params: DidSaveTextDocumentParams)
    -> Result<Vec<Message>, (i32, String)>
{
    eprintln!("got Did Change notification {params:?}");
    // get full file content
    let doc = match doc_manager.notify_document_saved(&params.text_document.uri){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = doc.get_diagnostic_report();
    let pub_diag_params = PublishDiagnosticsParams::new(
        params.text_document.uri, 
        diag_report.full_document_diagnostic_report.items.clone(), 
        None
    );
    let params_serialized = serde_json::to_value(&pub_diag_params).unwrap();
    let publish_diag = Notification{
        method: "textDocument/publishDiagnostics".to_string(),
        params: params_serialized
    };
    let mut result = Vec::<Message>::new();
    result.push(Message::Notification(publish_diag));
    return Ok(result)
}

fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_not<R>(not: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    not.extract(R::METHOD)
}

fn send_error(connection: &Connection, id: RequestId, code: i32, message: String)
-> Result<(), Box<dyn Error + Sync + Send>> {
    let error = Some(ResponseError {
        code,
        message,
        data: None
    });
    let resp = Response { id, result: None, error: error };
    Ok(connection.sender.send(Message::Response(resp))?)
}

fn convert_uri_to_file_path_str(uri : &Url) -> Result<String, String>{
    let req_doc_uri = match uri.to_file_path(){
        Ok(r) => r,
        _ => return Err("invalid uri".to_string())
    };
    match req_doc_uri.as_path().to_str() {
        Some(p) => return Ok(p.to_string()),
        _ => return Err("invalid uri".to_string())
    };
}


#[cfg(test)]
mod test {
    use std::fs::File;
    use std::io::Read;

    use lsp_types::Url;

    use crate::parser::ast::{AstClass, AstUses};
    use crate::convert_uri_to_file_path_str;
    use crate::lexer::GoldLexer;
    use crate::parser;
    use crate::parser::parse_gold;
    use crate::utils::print_ast_brief_recursive;

    #[test]
    fn test_class_everything(){
        let mut lexer = GoldLexer::new();
        let input = String::from
        ("class aTestClass   (aParentClass)\r\n\n
uses aFirstClass, aSecondClass\n
const cStringConstant = 'test constant' multiLang\n
const cNumericConstant = 18.5
memory GlobalVariable : refTo [P,A] SomeType
SecondGlobalVariable : listOf [T] AnotherType
IntGlobalVariable : int4
CStringVar : cstring

procedure FirstProcedure(FirstParam: FirstParamType, SecondParam: SecondParamType) private override 
    external 'DLL.Method' forward

proc SecondProc(inout FirstParam)
    ; method body
endproc

func FirstFunc return SomeType
    ; method body
    ; bla bla
    if LeftExpr = RightExpr
        if True
        endif
    endif
endfunc
        ");
        let tokens = lexer.lex(&input).0;
        let ast = parser::parse_gold(&tokens);

        // assert input left is empty
        assert_eq!(ast.0.0.len(), 0);
        // assert no errors
        assert_eq!(ast.1.len(), 0);
        let nodes = ast.0.1;
        // assert first node is class
        let class = &nodes[0].as_any().downcast_ref::<AstClass>().unwrap();
        assert_eq!(class.name.as_str(), "aTestClass");
        assert_eq!(class.parent_class.as_str(), "aParentClass");
        assert_eq!(class.raw_pos, 0);

        // assert second node is uses
        let uses = &nodes[1].as_any().downcast_ref::<AstUses>().unwrap();
        assert_eq!(uses.raw_pos, 37);
        // assert uses list
        let uses_ident = &uses.list_of_uses[0];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aFirstClass");
        let uses_ident = &uses.list_of_uses[1];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aSecondClass");

        for node in nodes{
            println!("{}", print_ast_brief_recursive(node.as_ast_node()));
        }  
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
        for node in ast.0.1{
            println!("{}",print_ast_brief_recursive(node.as_ref()));
        }
        // println!("{:#?}", ast.1.len());
    }

    #[test]
    fn test_convert_uri_to_file_path(){
        // TODO not valid in different pcs
        let uri = Url::parse("file:///home/razzzp/dev/gold-lang-lsp/test/aTestClass.god").unwrap();
        let path = convert_uri_to_file_path_str(&uri).unwrap();
        assert_eq!(path, "/home/razzzp/dev/gold-lang-lsp/test/aTestClass.god");
    }
}
