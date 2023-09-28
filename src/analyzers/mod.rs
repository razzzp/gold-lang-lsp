use crate::{parser::ast::IAstNode, utils::DynamicChild};

pub mod ast_walker;
pub mod unused_var_analyzer;
pub mod inout_param_checker;
pub mod function_return_type_checker;

pub trait IAstWalker{
    fn analyze(& mut self, ast_nodes: &dyn IAstNode) -> Vec<lsp_types::Diagnostic>;
    fn register_analyzer(&mut self, analyzer: Box<dyn IAnalyzer>);
}

#[derive(Debug)]
pub struct AnalyzerDiagnostic{

}

pub trait IAnalyzer{
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>);
    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>);
    fn notify_end(&mut self);
}