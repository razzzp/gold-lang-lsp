use lsp_types::lsif::Item;

use crate::{parser::ast::IAstNode, utils::DynamicChild};

pub mod ast_walker;
pub mod unused_var_analyzer;
pub mod inout_param_checker;
pub mod function_return_type_checker;


#[macro_export]
macro_rules! implem_as_ivisitor {
    () => {
        fn as_visitor(&mut self) -> &dyn IVisitor {
            self
        }
    };
}

// pub trait IAstWalker<'a, V, I>
// where 
//     V: IVisitor + ?Sized + 'a,
//     I: Iterator<Item = &'a mut V> + ?Sized
// {
//     fn run(&'a mut self, ast_nodes: &'a dyn IAstNode);
//     fn register_visitor(&'a mut self, visitor: &'a mut V);
//     fn register_visitors(&'a mut self, visitors: &'a mut I);
// }

#[derive(Debug)]
pub struct AnalyzerDiagnostic{

}

pub trait IAnalyzer: IVisitor{
    fn append_diagnostics(&self, result : &mut Vec<lsp_types::Diagnostic>);
    fn get_diagnostics_count(&self) ->  usize;  
}

pub trait IVisitor: {
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>);
    fn notify_end(&mut self);
    fn as_visitor(&mut self) -> &dyn IVisitor;
}
