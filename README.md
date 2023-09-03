# Archi
Currently the lsp is split into 4 parts:
- Lexer
- Parser
- ProjectManager
- LSP Server

## LSP Server
The server references code from the scaffolding used by rust-analyzer, the LSP used for the official Rust VSCode Extension: https://github.com/rust-lang/rust-analyzer/tree/master/lib/lsp-server

To Do:
- Maybe worthwhile to consider using Tower-LSP instead. An async Tokio-based library for LSPs with less boilerplate needed. https://github.com/ebkalderon/tower-lsp

## Project Manager
Stil in progress, but the idea is that this component will index the project folder and control when the files are read, parsed and analysed. It will also act as a controller to handle the LSP Server request

## Lexer
Tokenize the text file. Output will be fed to the Parser

## Parser
Reads the Token stream and constructs as AST. Output will be fed to the Analyzer

## Analyzer (To Do)
This module would:
- Perform Type Checking on AST (attach types? or generate new tree?)
- Provide Semantic Diagnostics
- Perform static code analysis, should be able to easily add new rules. E.g. rules:
    - Check unused vars
    - Check undisposed objects?
    - Check unpurged tVarByteArrays?