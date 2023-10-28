use std::net::ToSocketAddrs;
use std::ops::Deref;
use std::sync::Arc;



use crate::manager::ProjectManager;
use crate::threadpool::ThreadPool;


use std::error::Error;

use crossbeam_channel::Sender;
use lsp_types::notification::{DidChangeTextDocument, DidSaveTextDocument, DidOpenTextDocument};
use lsp_types::{OneOf, DocumentSymbolResponse, DocumentSymbolParams, DiagnosticOptions, DiagnosticServerCapabilities, DocumentDiagnosticParams, DocumentDiagnosticReport, TextDocumentSyncKind, TextDocumentSyncCapability, DidChangeTextDocumentParams, PublishDiagnosticsParams, DidSaveTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, DidOpenTextDocumentParams, CompletionParams, CompletionResponse, CompletionOptions, TypeHierarchyOptions, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams};
use lsp_types::request::{
    DocumentSymbolRequest, 
    DocumentDiagnosticRequest, 
    GotoDefinition, 
    Completion, 
    TypeHierarchyPrepare,
    TypeHierarchySubtypes,
    TypeHierarchySupertypes,
};

use lsp_types::{
    InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response, ResponseError, ErrorCode, Notification,};
use utils::{StdErrLogger, ILoggerV2};



pub mod lexer;
pub mod parser;
pub mod utils;
pub mod manager;
pub mod analyzers;
pub mod threadpool;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    let logger : Arc<dyn ILoggerV2>= Arc::new(StdErrLogger::new("[Gold Lang LSP]"));
    logger.log_info("Starting Gold Lang LSP server");
    // server capabilities
    let mut server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions::default())),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions{
            ..Default::default()
        }),
        ..Default::default()
    }).unwrap();
    // add type hierarchy options
    if let Some(map) = server_capabilities.as_object_mut(){
        let type_hierarchy_provider = serde_json::to_value(&TypeHierarchyOptions::default()).unwrap();
        // println!("{:#?}", type_hierarchy_provider);
        map.insert("typeHierarchyProvider".to_string(),type_hierarchy_provider);
        // println!("{:#?}", map);
    }
    logger.log_info(format!("Server Capabilities {:#?}", server_capabilities).as_str());
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

    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params,logger)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
    logger: Arc<dyn ILoggerV2>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    
    let threadpool = ThreadPool::new(7, logger.clone());
    let mut proj_manager = match ProjectManager::new(
        params.root_uri,
        logger.clone()
    ){
        Ok(r) => r,
        Err(e) => {
            return Err(Box::new(e));
        }
    };
    // creates uri & file path mapping for all .god files
    proj_manager.index_files();

    // builds class tree, and tracks modules
    let entity_tree_service = proj_manager.entity_tree_service.clone();
    let doc_service = proj_manager.doc_service.clone();
    entity_tree_service.build_tree_parallel(&doc_service, &threadpool);


    // analyze core files (WAM, WF)
    let mut proj_manager_clone = proj_manager.clone();
    threadpool.execute(move ||{
        proj_manager_clone.analyze_core_files();
    });

    for msg in &connection.receiver {
        // eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                logger.log_info(format!("got request; #{}; method:{}", req.id, req.method).as_str());
                let req = match cast_req::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        match handle_document_symbol_request(&mut proj_manager, id.clone(), params, &logger){
                            Ok(resp) => connection.sender.send(resp)?,
                            Err(e) => send_error(&connection.sender, id, e.0, e.1)?
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                let req = match cast_req::<DocumentDiagnosticRequest>(req) {
                    Ok((id, params)) => {
                        match handle_document_diagnostics_request(&mut proj_manager, id.clone(), params, &logger){
                            Ok(resp) => connection.sender.send(resp)?,
                            Err(e) => send_error(&connection.sender, id, e.0, e.1)?
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_req::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        // heavy call to analyze doc, move to separate thread
                        let sender = connection.sender.clone();
                        let mut proj_manager = proj_manager.clone();
                        let logger = logger.clone();
                        threadpool.execute(move ||{
                            match handle_goto_definition_request(&mut proj_manager, id.clone(), params, &logger){
                                Ok(resp) => {let _ = sender.send(resp);},
                                Err(e) => {let _  = send_error(&sender, id, e.0, e.1);}
                            };
                        });
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_req::<Completion>(req) {
                    Ok((id, params)) => {
                        // heavy call to analyze doc, move to separate thread
                        let sender = connection.sender.clone();
                        let mut proj_manager = proj_manager.clone();
                        let logger = logger.clone();
                        threadpool.execute(move ||{
                            match handle_completion_request(&mut proj_manager, id.clone(), params, &logger){
                                Ok(resp) => {let _ = sender.send(resp);},
                                Err(e) => {let _  = send_error(&sender, id, e.0, e.1);}
                            };
                        });
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_req::<TypeHierarchyPrepare>(req) {
                    Ok((id, params)) => {
                        let sender = connection.sender.clone();
                        let mut proj_manager = proj_manager.clone();
                        let logger = logger.clone();
                        threadpool.execute(move ||{
                            match handle_type_hierarchy_prepare_request(&mut proj_manager, id.clone(), params, &logger){
                                Ok(resp) => {let _ = sender.send(resp);},
                                Err(e) => {let _  = send_error(&sender, id, e.0, e.1);}
                            };
                        });
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_req::<TypeHierarchySubtypes>(req) {
                    Ok((id, params)) => {
                        let sender = connection.sender.clone();
                        let mut proj_manager = proj_manager.clone();
                        let logger = logger.clone();
                        threadpool.execute(move ||{
                            let _ = handle_result(
                                handle_type_hierarchy_subtypes_request(&mut proj_manager, id.clone(), params, &logger), 
                                id, 
                                &sender);
                        });
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let _req = match cast_req::<TypeHierarchySupertypes>(req) {
                    Ok((id, params)) => {
                        let sender = connection.sender.clone();
                        let mut proj_manager = proj_manager.clone();
                        let logger = logger.clone();
                        threadpool.execute(move ||{
                            let _ = handle_result(
                                handle_type_hierarchy_supertypes_request(&mut proj_manager, id.clone(), params, &logger), 
                                id, 
                                &sender);
                        });
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
                logger.log_info(format!("got notification; method:{}", not.method).as_str());
                let not = match cast_not::<DidChangeTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_change_notification(&mut proj_manager, params, &logger, &threadpool){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => logger.log_error(format!("error: code({}) {}", e.0, e.1).as_str())
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                let not = match cast_not::<DidSaveTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_save_notification(&mut proj_manager, params, &logger){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => logger.log_error(format!("error: code({}) {}", e.0, e.1).as_str())
                        }
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast_not::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        match handle_did_open_notification(&mut proj_manager, params, &logger, &threadpool){
                            Ok(msgs) => {
                                for msg in msgs {
                                    connection.sender.send(msg)?;
                                }
                            },
                            Err(e) => logger.log_error(format!("error: code({}) {}", e.0, e.1).as_str())
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
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: DocumentSymbolParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("handling Document Symbol request #{id}").as_str());
    let symbols = match proj_manager.generate_document_symbols(&params.text_document.uri){
        Ok(syms) => syms,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
    let result = Some(DocumentSymbolResponse::Nested(symbols));
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_document_diagnostics_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: DocumentDiagnosticParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("handling Document Diagnostics request #{id}").as_str());
    let diag_report = match proj_manager.generate_document_diagnostic_report(&params.text_document.uri){
        Ok(diag_report) => diag_report,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
    let result = DocumentDiagnosticReport::Full(diag_report.deref().clone());
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_goto_definition_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: GotoDefinitionParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("handling Document Diagnostics request #{id}").as_str());
    let loc_links = match proj_manager.generate_goto_definitions(
        &params.text_document_position_params.text_document.uri, 
        &params.text_document_position_params.position.into()){
        Ok(location_links) => location_links,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
    let result = GotoDefinitionResponse::Link(loc_links);
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_did_change_notification(
    proj_manager: &mut ProjectManager, 
    params: DidChangeTextDocumentParams,
    logger: &Arc<dyn ILoggerV2>,
    threadpool: &ThreadPool
)
    -> Result<Vec<Message>, (i32, String)>
{
    logger.log_info(format!("handling Did Change notification").as_str());
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
    let _ = match proj_manager.notify_document_changed(&params.text_document.uri, full_file_content, threadpool){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = match proj_manager.generate_document_diagnostic_report(&params.text_document.uri){
        Ok(diag_report) => diag_report,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
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
    proj_manager: &mut ProjectManager, 
    params: DidSaveTextDocumentParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Vec<Message>, (i32, String)>
{
    logger.log_info(format!("handling Did Save notification").as_str());
    // get full file content
    let _ = match proj_manager.doc_service.notify_document_saved(&params.text_document.uri){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    let diag_report = match proj_manager.generate_document_diagnostic_report(&params.text_document.uri){
        Ok(diag_report) => diag_report,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
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

fn handle_did_open_notification(
    proj_manager: &mut ProjectManager, 
    params: DidOpenTextDocumentParams,
    logger: &Arc<dyn ILoggerV2>,
    threadpool: &ThreadPool
)
    -> Result<Vec<Message>, (i32, String)>
{
    logger.log_info(format!("handling Did Save notification").as_str());
    // get full file content
    let result= Vec::new();
    if params.text_document.language_id.as_str() != "gold"{
        return Ok(result)
    }
    let _ = match proj_manager.notify_document_opened(&params.text_document.uri, threadpool){
        Ok(d) => d,
        Err(e) =>{
            return Err((e.error_code as i32, e.msg));
        }
    };
    
    return Ok(result)
}

fn handle_completion_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: CompletionParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("handling Completion request #{id}").as_str());
    let completion_items = match proj_manager.generate_completion_proposals(
        &params.text_document_position.text_document.uri, 
        &params.text_document_position.position.into())
    {
        Ok(items) => items,
        Err(e) => return Err((e.error_code as i32, e.msg))
    };
    
    let result = CompletionResponse::Array(completion_items);
    let result = serde_json::to_value(&result).unwrap();
           
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_type_hierarchy_prepare_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: TypeHierarchyPrepareParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("Handling Type Hierarchy Prepare request #{id}").as_str());
    let type_hierarchy_items = proj_manager.prepare_type_hierarchy(
        &params.text_document_position_params.text_document.uri,
        &params.text_document_position_params.position.into()
    ).map_err(|e|{return (e.error_code as i32, e.msg);})?;


    let result = serde_json::to_value(type_hierarchy_items).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_type_hierarchy_subtypes_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: TypeHierarchySubtypesParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("Handling Type Hierarchy Subtypes request #{id}").as_str());
    let type_hierarchy_items = proj_manager.type_hierarchy_subtypes(
        &params.item
    ).map_err(|e|{return (e.error_code as i32, e.msg);})?;


    let result = serde_json::to_value(type_hierarchy_items).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
}

fn handle_type_hierarchy_supertypes_request(
    proj_manager: &mut ProjectManager, 
    id: RequestId, 
    params: TypeHierarchySupertypesParams,
    logger: &Arc<dyn ILoggerV2>,
)
    -> Result<Message, (i32, String)>
{
    logger.log_info(format!("Handling Type Hierarchy Supertypes request #{id}").as_str());
    let type_hierarchy_items = proj_manager.type_hierarchy_supertypes(
        &params.item
    ).map_err(|e|{return (e.error_code as i32, e.msg);})?;

    let result = serde_json::to_value(type_hierarchy_items).unwrap();
    let resp = Response { id, result: Some(result), error: None };
    return Ok(Message::Response(resp));
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

fn send_error(sender: &Sender<Message>, id: RequestId, code: i32, message: String)
-> Result<(), Box<dyn Error + Sync + Send>> {
    let error = Some(ResponseError {
        code,
        message,
        data: None
    });
    let resp = Response { id, result: None, error: error };
    Ok(sender.send(Message::Response(resp))?)
}

fn handle_result(result : Result<Message, (i32, String)>, id: RequestId, sender: &Sender<Message>)
-> Result<(), Box<dyn Error + Sync + Send>>{
    match  result {
        Ok(resp) => sender.send(resp)?,
        Err(e) => send_error(sender, id, e.0, e.1)?
    };
    Ok(())
}


#[cfg(test)]
mod test {

}
