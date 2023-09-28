use crate::{parser::ast::IAstNode};

use super::data_structs::{SymbolTable, ISymbolTable};


#[derive(Debug)]
pub struct SymbolGenerator {
    symbol_table : Box<dyn ISymbolTable>    

}

impl SymbolGenerator {
    pub fn new() -> SymbolGenerator{
        return SymbolGenerator {  
            symbol_table: Box::new(SymbolTable::new())
        }
    }

    pub fn generate(&mut self, node : &dyn IAstNode){
        for node in node.get_children_dynamic().unwrap_or_default(){
            // node.
        }
    }
}

