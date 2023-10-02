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
pub struct AnnotatedNode<'a, T: IAstNode + 'a>{
    pub data: &'a T,
    pub parent: Option<&'a T>,
    pub symbol_table: Option<Box<dyn ISymbolTable>>,
    pub eval_type: Option<EvalType>

}
impl<'a,T: IAstNode + 'a> AnnotatedNode<'a, T>{
    pub fn new(data: &'a T, parent: Option<&'a T>) -> AnnotatedNode<'a,T> {
        AnnotatedNode { 
            data: data, 
            parent: parent,
            symbol_table: None,
            eval_type: None
            }
    }
}
