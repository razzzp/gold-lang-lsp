use super::annotated_ast_walker::IAnnotatedNodeVisitor;

use crate::analyzers_v2::annotated_ast_walker::{AnnotatedAstNodeArx, IAnnotatedAstWalkerContext};

/// Checks that inherited is called for certain functions
#[derive(Debug)]
pub struct InheritedChecker{

}
impl IAnnotatedNodeVisitor for InheritedChecker{
    fn visit(&mut self, node : &AnnotatedAstNodeArx) {
        todo!()
    }

    fn visit_w_context(&mut self, node : &AnnotatedAstNodeArx, context: &dyn IAnnotatedAstWalkerContext) {
        todo!()
    }

    fn notify_end(&mut self) {
        todo!()
    }
}