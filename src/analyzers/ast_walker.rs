use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use crate::parser::ast::IAstNode;
use crate::utils::DynamicChild;

use super::IVisitor;

pub struct AstWalker<V>
where V: IVisitor + ?Sized
{
    recursive: bool,
    visitors: Vec<Rc<RefCell<V>>>,
}
impl<V> AstWalker<V>
where V: IVisitor + ?Sized
{
    pub fn new(recursive: bool) -> AstWalker<V>{
        return AstWalker{
            recursive,
            visitors: Vec::new()
        };
    }
    
    fn visit(&mut self, node: &DynamicChild<dyn IAstNode>){
        for analyzer in self.visitors.iter_mut(){
            analyzer.borrow_mut().visit(node);
        }
        if self.recursive{
            match node.data.get_children_ref_dynamic() {
                Some(children) =>{
                    for dyn_child in children{
                        self.visit(&dyn_child);
                    }   
                }
                
                _=>()
            }
        }
    }
    fn notify_end(&mut self){
        for analyzer in &self.visitors{
            analyzer.borrow_mut().notify_end();
        }
    }

    pub fn run(&mut self, ast: &Arc<dyn IAstNode>){
        for dyn_node in ast.get_children_ref_dynamic().unwrap_or_default().iter(){
            self.visit(dyn_node);
        }
        self.notify_end();
    }

    pub fn register_visitor(&mut self, visitor: &Rc<RefCell<V>>) {
        self.visitors.push(visitor.clone());
    }

    pub fn register_visitors(&mut self, visitors: &Vec<Rc<RefCell<V>>>) {
        for visitor in visitors{
            self.visitors.push(visitor.clone());
        } 
    }
}

