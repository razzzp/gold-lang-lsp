use crate::parser::ast::IAstNode;
use crate::utils::DynamicChild;

use super::{IAstWalker, IAnalyzer};

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
                for dyn_child in children{
                    self.visit(&dyn_child);
                }   
            }
            
            _=>()
        }
    }
    fn notify_end(&mut self){
        for analyzer in &mut self.analyzers{
            analyzer.notify_end();
        }
    }
}

impl IAstWalker for AstWalker{
    fn analyze(& mut self, ast: &Box<dyn IAstNode>) -> Vec<lsp_types::Diagnostic> {
        for dyn_node in ast.get_children_dynamic().unwrap_or_default().iter(){
            self.visit(dyn_node);
        }
        self.notify_end();
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