use std::any::Any;

use crate::lexer::tokens::Token;

pub trait IAstNode : std::fmt::Debug {

   fn get_type(&self) -> &'static str;
   fn get_pos(&self) -> usize;
   fn as_any(&self) -> &dyn Any;
   // fn get_token(&self) -> Token;
   // fn eval() -> ();
}

#[derive(Debug)]
pub struct AstTerminal{
   pub token: Token,
} 
impl IAstNode for AstTerminal{
    fn get_type(&self) -> &'static str {
        return "Terminal";
    }
    fn get_pos(&self) -> usize {
        return self.token.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct AstClass{
   pub pos: usize,
   pub name: String,
   pub parent_class: String
}
impl IAstNode for AstClass{
   fn get_type(&self) -> &'static str {
       return "Class";
   }
   fn get_pos(&self) -> usize {
       return self.pos;
   }
   fn as_any(&self) -> &dyn Any {
      self
  }
}

#[derive(Debug)]
pub struct AstUses {
   pub pos: usize,
   pub list_of_uses: Vec<Token>
}
impl IAstNode for AstUses{
   fn get_type(&self) -> &'static str {
       return "Uses";
   }
   fn get_pos(&self) -> usize {
       return self.pos;
   }
   fn as_any(&self) -> &dyn Any {
      self
  }
}

#[derive(Debug)]
pub struct AstTypePrimitiveFixedSize {
   pub pos: usize,
   pub type_token: Token
}
impl IAstNode for AstTypePrimitiveFixedSize{
   fn get_type(&self) -> &'static str {
       return "Type Primitive Unsized";
   }
   fn get_pos(&self) -> usize {
       return self.pos;
   }
   fn as_any(&self) -> &dyn Any {
      self
  }
}
#[derive(Debug)]
pub struct AstTypePrimitiveDynamicSize{
   pub pos: usize,
   pub type_token: Token
}
impl IAstNode for AstTypePrimitiveDynamicSize{
   fn get_type(&self) -> &'static str {
       return "Type Primitive Unsized";
   }
   fn get_pos(&self) -> usize {
       return self.pos;
   }
   fn as_any(&self) -> &dyn Any {
      self
  }
}

#[derive(Debug)]
pub struct AstEmpty {
}
impl IAstNode for AstEmpty{
   fn get_type(&self) -> &'static str {
       return "";
   }
   fn get_pos(&self) -> usize {
       return 0;
   }
   fn as_any(&self) -> &dyn Any {
      self
  }
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