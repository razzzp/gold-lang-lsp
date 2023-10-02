use crate::parser::ast::IAstNode;



pub struct AnnotatedNode<'a, T: IAstNode + 'a>{
    pub data: &'a T,
    pub parent: Option<&'a T>,
}
impl<'a,T: IAstNode + 'a> AnnotatedNode<'a, T>{
    pub fn new(data: &'a T, parent: Option<&'a T>) -> AnnotatedNode<'a,T> {
        AnnotatedNode { data: data, parent: parent}
    }
}
