use std::sync::{Arc, RwLock, Mutex};

use crate::{parser::ast::IAstNode, utils::IDiagnosticCollector};

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
pub type IDiagnosticCollectorArx  = Arc<Mutex<dyn IDiagnosticCollector<lsp_types::Diagnostic>>>;

#[cfg(test)]
pub mod test_utils{
   use std::sync::{Arc, Mutex};

   use lsp_types::Diagnostic;

use crate::{utils::{IDiagnosticCollector, GenericDiagnosticCollector, test_utils::create_test_diag_collector}, parser::ast::IAstNode, manager::test::{create_test_sem_service, create_test_doc_service, create_test_logger}};

   use super::{AnnotatedAstNodeArx, ast_annotator::AstAnnotator, annotated_ast_walker::IAnnotatedNodeVisitor};

   pub fn annotate_ast(node: &Arc<dyn IAstNode>) -> AnnotatedAstNodeArx{
      let test_logger = create_test_logger();
      let annotator = AstAnnotator::new(
         create_test_sem_service(
            create_test_doc_service(None)), 
            create_test_diag_collector(), test_logger, false);
      return annotator.generate_annotated_tree(&node)
   }
}