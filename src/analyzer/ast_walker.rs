use crate::parser::ast::IAstNode;
use crate::utils::DynamicChild;

use super::{IAstWalker, IAnalyzer};
use super::AnalyzerDiagnostic;

pub struct AstWalker{
    analyzers: Vec<Box<dyn IAnalyzer>>,
}
impl AstWalker{
    pub fn new() -> AstWalker{
        return AstWalker{
            analyzers: Vec::new()
        };
    }
    
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>){
        for analyzer in &mut self.analyzers{
            analyzer.visit(node);
        }
        match node.data.get_children_dynamic() {
            Some(children) =>{
                children.into_iter().for_each(|dyn_child|{
                    self.visit(&dyn_child);
                });
            }
            _=>()
        }
    }
}

impl IAstWalker for AstWalker{
    fn analyze(&mut self, ast_nodes: &Vec<Box<dyn IAstNode>>) -> Vec<lsp_types::Diagnostic> {
        let dyn_nodes : Vec<_> = ast_nodes.iter().map(|node|{
            return DynamicChild{
                data: node.as_ast_node(),
                parent: None
            }
        }).collect();
        for dyn_node in &dyn_nodes{
            self.visit(dyn_node);
        }
        let mut result = Vec::<lsp_types::Diagnostic>::new();
        self.analyzers.iter().for_each(|analyzer|{
            analyzer.append_diagnostics(&mut result);
        });
        return result;
    }

    fn register_analyzer(&mut self, analyzer: Box<dyn IAnalyzer>) {
        self.analyzers.push(analyzer);
    }
}