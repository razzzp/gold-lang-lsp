use std::sync::{Arc, RwLock};

use crate::parser::ast::IAstNode;

use self::annotated_node::AnnotatedNode;



pub mod annotated_node;
pub mod type_resolver;
pub mod symbol_table;
pub mod ast_annotator;
pub mod doc_symbol_generator;
pub mod annotated_ast_walker;
pub mod unpurged_varbytearray_checker;
pub mod naming_convention_checker;
pub mod inherited_checker;

pub type AnnotatedAstNodeArx =Arc<RwLock<AnnotatedNode<dyn IAstNode>>>;