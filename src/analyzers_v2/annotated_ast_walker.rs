
use std::sync::{
    Arc,
    RwLock,
};
use crate::analyzers_v2::annotated_node::AnnotatedNode;
use crate::parser::ast::{IAstNode, AstClass, AstModule, AstProcedure, AstFunction};

pub type AnnotatedAstNodeArx =Arc<RwLock<AnnotatedNode<dyn IAstNode>>>;

pub trait IAnnotatedNodeVisitor: {
    fn visit(&mut self, node : &AnnotatedAstNodeArx);
    fn visit_w_context(&mut self, node : &AnnotatedAstNodeArx, context: &dyn IAnnotatedAstWalkerContext);
    fn notify_end(&mut self);
}


pub trait IAnnotatedAstWalkerContext{
    fn get_current_class_or_module(&self) -> &Option<AnnotatedAstNodeArx>;
    fn get_current_method(&self) -> &Option<AnnotatedAstNodeArx>;
}


/// Use to keep track of context of the node being visited.
/// Idea is that analyzers will probably need common functionalities
/// like what method it is currently in, the class, etc...
/// 
#[derive(Debug, Default)]
pub struct AnnotatedAstWalkerContext{
    current_class_or_module: Option<AnnotatedAstNodeArx>,
    current_method : Option<AnnotatedAstNodeArx>
}
impl IAnnotatedAstWalkerContext for AnnotatedAstWalkerContext{
    fn get_current_class_or_module(&self) ->  &Option<AnnotatedAstNodeArx>{
        &self.current_class_or_module
    }

    fn get_current_method(&self) -> &Option<AnnotatedAstNodeArx>{
        &self.current_method
    }
}
impl AnnotatedAstWalkerContext{
    pub fn new() -> AnnotatedAstWalkerContext{
        return AnnotatedAstWalkerContext::default()
    }

    pub fn notify_visit_node(&mut self, node : &AnnotatedAstNodeArx){
        if node.read().unwrap().data.as_any().downcast_ref::<AstClass>().is_some(){
            self.current_class_or_module = Some(node.clone())
        }
        if node.read().unwrap().data.as_any().downcast_ref::<AstModule>().is_some(){
            self.current_class_or_module = Some(node.clone())
        }
        if node.read().unwrap().data.as_any().downcast_ref::<AstProcedure>().is_some(){
            self.current_method = Some(node.clone())
        }
        if node.read().unwrap().data.as_any().downcast_ref::<AstFunction>().is_some(){
            self.current_method = Some(node.clone())
        }
    }
}


pub struct AnnotatedAstWalkerPreOrder<V>
where V: IAnnotatedNodeVisitor + ?Sized
{
    visitors: Vec<Box<V>>,
    context: AnnotatedAstWalkerContext
}
impl<V> AnnotatedAstWalkerPreOrder<V>
where V: IAnnotatedNodeVisitor + ?Sized
{
    pub fn new() -> AnnotatedAstWalkerPreOrder<V>{
        return AnnotatedAstWalkerPreOrder{
            visitors: Vec::new(),
            context: AnnotatedAstWalkerContext::new()
        };
    }

    fn walk_tree(&mut self, node: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // separate getting children to prevent parent being locked,
        //  when visiting children
        self.visit(node);
        for child in &node.read().unwrap().children {
            self.walk_tree(child);
        }
    }

    fn visit(&mut self, node : &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
        // notify context tracker first
        self.context.notify_visit_node(node);

        for visitor in self.visitors.iter_mut(){
            visitor.visit_w_context(node, &self.context)
        }
    }

    fn notify_end(&mut self){
        for analyzer in self.visitors.iter_mut(){
            analyzer.notify_end();
        }
    }

    pub fn walk(&mut self, ast: &Arc<RwLock<AnnotatedNode<dyn IAstNode>>>){
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