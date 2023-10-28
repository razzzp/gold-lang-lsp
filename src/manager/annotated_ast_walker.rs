
use std::sync::{
    Arc,
    RwLock,
};
use crate::manager::annotated_node::AnnotatedNode;
use crate::parser::ast::IAstNode;

pub trait IAnnotatedNodeVisitor: {
    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>);
    fn notify_end(&mut self);
}


pub struct AnnotatedAstWalker<V>
where V: IAnnotatedNodeVisitor + ?Sized
{
    visitors: Vec<Box<V>>,
}
impl<V> AnnotatedAstWalker<V>
where V: IAnnotatedNodeVisitor + ?Sized
{
    pub fn new() -> AnnotatedAstWalker<V>{
        return AnnotatedAstWalker{
            visitors: Vec::new()
        };
    }

    fn walk_tree(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        self.visit(node);
        for child in &node.read().unwrap().children {
            // first level are definitions, so visit first (preorder)
            self.visit(child);

            // everything under, travel postorder
            // (process children first)
            for inner_child in &child.read().unwrap().children{
                self.walk_tree_postorder(inner_child)
            }
        }
    }

    fn walk_tree_postorder(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        for child in &node.read().unwrap().children {
                self.walk_tree_postorder(child)
        }
        self.visit(node);
    }
    
    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        for visitor in self.visitors.iter_mut(){
            visitor.visit(node)
        }
    }

    fn notify_end(&mut self){
        for analyzer in self.visitors.iter_mut(){
            analyzer.notify_end();
        }
    }

    pub fn walk(&mut self, ast: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // separate getting children to prevent parent being locked,
        //  when visiting children
        self.walk_tree(ast);
        self.notify_end();
    }

    pub fn register_visitor(&mut self, visitor: Box<V>) {
        self.visitors.push(visitor);
    }

    pub fn register_visitors(&mut self, visitors: Vec<Box<V>>) {
        for visitor in visitors{
            self.visitors.push(visitor);
        } 
    }
}