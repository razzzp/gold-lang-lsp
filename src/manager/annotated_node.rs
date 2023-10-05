use std::sync::{Arc, RwLock, Weak};

use crate::parser::ast::IAstNode;

use super::symbol_generator::ISymbolTable;

#[derive(Debug)]
pub struct TypeInfo{
    name: String,
    location: lsp_types::Location
}

#[derive(Debug)]
pub enum NativeType{
    Int,
    Numeric,
    String,

}

#[derive(Debug)]
pub enum EvalType{
    Natve(NativeType),
    Defined(TypeInfo),
    Class(TypeInfo)
}

#[derive(Debug)]
pub struct AnnotatedNode<T: IAstNode+ ?Sized>{
    pub data: Arc<T>,
    pub parent: Option<Weak<RwLock<AnnotatedNode<T>>>>,
    pub children: Vec<Arc<RwLock<AnnotatedNode<T>>>>,
    pub symbol_table: Option<Box<dyn ISymbolTable>>,
    pub eval_type: Option<EvalType>

}
impl<T: IAstNode+ ?Sized> AnnotatedNode<T>{
    pub fn new(data: &Arc<T>, parent: Option<Weak<RwLock<AnnotatedNode<T>>>>) -> AnnotatedNode<T> {
        AnnotatedNode { 
            data: data.clone(), 
            parent: parent,
            symbol_table: None,
            eval_type: None,
            children: Vec::new()
            }
    }
}
