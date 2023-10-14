
use std::sync::{Arc, RwLock, Weak, Mutex};

use crate::parser::ast::IAstNode;

use crate::manager::symbol_table::ISymbolTable;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo{
    pub id: String,
    pub definition: Location
}
impl TypeInfo {
    pub fn new(id: &String, definition: Location) -> TypeInfo{
        TypeInfo{
            id: id.clone(),
            definition
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NativeType{
    Int,
    Num,
    Decimal,
    String,
    CString,
    Text,
    Boolean,
    Char
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location{
    pub in_doc: String,
}
impl Location{
    pub fn new(in_doc: &String) -> Location{
        Location { in_doc: in_doc.clone(), }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum EvalType{
    #[default]
    Unknown,
    Native(NativeType),
    Defined(TypeInfo),
    Class(String),
    // contains id of unresolved type
    Unresolved(String),
}

#[derive(Debug)]
pub struct AnnotatedNode<T: IAstNode+ ?Sized>{
    pub data: Arc<T>,
    pub parent: Option<Weak<RwLock<AnnotatedNode<T>>>>,
    pub children: Vec<Arc<RwLock<AnnotatedNode<T>>>>,
    pub symbol_table: Option<Arc<Mutex<dyn ISymbolTable>>>,
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
    pub fn as_annotated_node(&self)->&AnnotatedNode<T>{
        self
    }
}
