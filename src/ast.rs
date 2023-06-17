use crate::lexer::tokens::Token;

#[derive(Debug)]
pub struct AstTerminal{
   pub token: Token,
}

#[derive(Debug)]
pub struct AstClass{
   pub pos: usize,
   pub name: String,
   pub parent_class: String
}

#[derive(Debug)]
pub struct AstUses {
   pub pos: usize,
   pub list_of_uses: Vec<AstNode>
}

#[derive(Debug)]
pub enum AstNode {
   None,
   Class(AstClass),
   Terminal(AstTerminal),
   Uses(AstUses)
}

impl AstNode {
   pub fn type_as_str(&self) -> &'static str{
      match self {
         AstNode::None => "None",
         AstNode::Class(_) => "Class",
         AstNode::Terminal(_) => "Terminal",
         AstNode::Uses(_) => "Uses"
      }
   }
}