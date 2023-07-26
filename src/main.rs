use std::net::ToSocketAddrs;
use std::{io::Read, fs::File};

use crate::manager::GoldDocumentManager;
use crate::{lexer::GoldLexer, parser::parse_gold};
use std::error::Error;

use lsp_types::{OneOf, DocumentSymbolResponse, DocumentSymbol, SymbolKind, Range};
use lsp_types::request::DocumentSymbolRequest;
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response, ResponseError, ErrorCode};
use nom::error;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod utils;
pub mod manager;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let mut addrs_iter = "localhost:5001".to_socket_addrs().unwrap();
    let (connection, io_threads) = Connection::listen(addrs_iter.next().unwrap())?;

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();
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
    
    let mut doc_manager = GoldDocumentManager::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                match cast::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        eprintln!("got Document Symbol request #{id}: {params:?}");

                        let req_doc_uri = params.text_document.uri;
                        let doc = match doc_manager.get_document(req_doc_uri.as_str()){
                            Ok(d) => d,
                            Err(e) =>{
                                let error = Some(ResponseError {
                                    code: e.error_code as i32,
                                    message: e.msg,
                                    data: None
                                });
                                let resp = Response { id, result: None, error: error };
                                connection.sender.send(Message::Response(resp))?;
                                continue;
                            }
                        };
                        let symbols = doc.get_symbols();
                        let result = Some(DocumentSymbolResponse::Nested(symbols));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response { id, result: Some(result), error: None };
                        connection.sender.send(Message::Response(resp))?;
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
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}


#[cfg(test)]
mod test {
    use std::fs::File;
    use std::io::Read;

    use crate::ast::AstClass;
    use crate::ast::AstUses;
    use crate::lexer::GoldLexer;
    use crate::parser;
    use crate::parser::parse_gold;

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

        // println!("{:#?}", nodes);
    }

    fn test_read_file() {
        let  mut f = File::open("./test_inputs/aTestClass.god").expect("file not found");
        let mut file_contents = String::new();
        match f.read_to_string(&mut file_contents){
            Ok(_)=>(),
            Err(msg) => panic!("{msg}")
        };
        println!("{file_contents}");
        let mut lexer = GoldLexer::new();
        let tokens = lexer.lex(&file_contents).0;
        println!("{:#?}", tokens);
        let ast = parse_gold(&tokens);
        println!("{:#?}", ast.0.0);
        println!("{:#?}", ast.0.1);
        println!("{:#?}", ast.1.len());
    }
}
