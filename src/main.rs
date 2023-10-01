use std::net::ToSocketAddrs;
use std::ops::Deref;
use std::sync::{Mutex, Arc};



use crate::manager::ProjectManager;
use crate::threadpool::ThreadPool;
use crate::utils::ConsoleLogger;

use std::error::Error;

use lsp_types::notification::{DidChangeTextDocument, DidSaveTextDocument};
use lsp_types::{OneOf, DocumentSymbolResponse, Url, DocumentSymbolParams, DiagnosticOptions, DiagnosticServerCapabilities, DocumentDiagnosticParams, DocumentDiagnosticReport, TextDocumentSyncKind, TextDocumentSyncCapability, DidChangeTextDocumentParams, PublishDiagnosticsParams, DidSaveTextDocumentParams};
use lsp_types::request::{DocumentSymbolRequest, DocumentDiagnosticRequest};
use lsp_types::{
    InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response, ResponseError, ErrorCode, Notification,};
use utils::ILogger;



pub mod lexer;
pub mod parser;
pub mod utils;
pub mod manager;
pub mod analyzers;
pub mod threadpool;

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
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    
    //TODO implem multithreading
    let mut threadpool = ThreadPool::new(5);
    let mut logger = ConsoleLogger::new("[LSP Server]");
    // TODO use same logger in server and project manager
    let mut doc_manager = match ProjectManager::new(
        params.root_uri,
        Arc::new(Mutex::new(logger.clone()))
    ){
        Ok(r) => r,
        Err(e) => {
            return Err(Box::new(e));
        }
    };
    doc_manager.index_files();

    for msg in &connection.receiver {
        // eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                logger.log(format!("got request; #{}; method:{}", req.id, req.method).as_str());
                let req = match cast_req::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        match handle_document_symbol_request(&mut doc_manager, id.clone(), params, logger.as_logger_mut()){
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
                        match handle_document_diagnostics_request(&mut doc_manager, id.clone(), params, logger.as_logger_mut()){
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
                eprintln!("got response: #{}",resp.id);
            }
            Message::Notification(not) => {
                logger.log(format!("got notification; method:{}", not.method).as_str());
                let not = match cast_not::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_change_notification(&mut doc_manager, params, logger.as_logger_mut()){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => logger.log(format!("error: code({}) {}", e.0, e.1).as_str())
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast_not::<DidSaveTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_save_notification(&mut doc_manager, params, logger.as_logger_mut()){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => logger.log(format!("error: code({}) {}", e.0, e.1).as_str())
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
    doc_manager: &mut ProjectManager, 
    id: RequestId, 
    params: DocumentSymbolParams,
    logger: &mut dyn ILogger,
)
    -> Result<Message, (i32, String)>
{
    logger.log(format!("handling Document Symbol request #{id}").as_str());
    let symbols = match doc_manager.get_document_symbols(&params.text_document.uri){
        Ok(syms) => syms,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
    let result = Some(DocumentSymbolResponse::Nested(symbols));
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_document_diagnostics_request(
    doc_manager: &mut ProjectManager, 
    id: RequestId, 
    params: DocumentDiagnosticParams,
    logger: &mut dyn ILogger,
)
    -> Result<Message, (i32, String)>
{
    logger.log(format!("handling Document Diagnostics request #{id}").as_str());
    let doc = match doc_manager.get_parsed_document(&params.text_document.uri) {
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = doc_manager.get_diagnostic_report(doc.clone());
    let result = DocumentDiagnosticReport::Full(diag_report.deref().clone());
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_did_change_notification(
    doc_manager: &mut ProjectManager, 
    params: DidChangeTextDocumentParams,
    logger: &mut dyn ILogger,)
    -> Result<Vec<Message>, (i32, String)>
{
    logger.log(format!("handling Did Change notification").as_str());
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
    let diag_report = doc_manager.get_diagnostic_report(doc.clone());
    let pub_diag_params = PublishDiagnosticsParams::new(
        params.text_document.uri, 
        diag_report.full_document_diagnostic_report.items.clone(), 
        Some(params.text_document.version));
    let params_serialized = serde_json::to_value(&pub_diag_params).unwrap();
    let _publish_diag = Notification{
        method: "textDocument/publishDiagnostics".to_string(),
        params: params_serialized
    };
    let result = Vec::<Message>::new();
    // TODO do we need to publsh? looks like client automatically 
    //  requests diag report when doc change, but previously it didn't update automatically?
    // result.push(Message::Notification(publish_diag));
    return Ok(result)
}

fn handle_did_save_notification(
    doc_manager: &mut ProjectManager, 
    params: DidSaveTextDocumentParams,
    logger: &mut dyn ILogger,
)
    -> Result<Vec<Message>, (i32, String)>
{
    logger.log(format!("handling Did Save notification").as_str());
    // get full file content
    let doc = match doc_manager.notify_document_saved(&params.text_document.uri){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = doc_manager.get_diagnostic_report(doc.clone());
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


#[cfg(test)]
mod test {

}
