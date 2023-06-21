use std::any::Any;

use crate::lexer::tokens::{Position, Range, Token};

pub trait IAstNode: std::fmt::Debug {
    fn get_type(&self) -> &'static str;
    fn get_raw_pos(&self) -> usize;
    fn get_pos(&self) -> Position;
    fn get_range(&self) -> Range;
    fn as_any(&self) -> &dyn Any;
    // fn get_token(&self) -> Token;
    // fn eval() -> ();
}

#[derive(Debug)]
pub struct AstTerminal {
    pub token: Token,
}
impl IAstNode for AstTerminal {
    fn get_type(&self) -> &'static str {
        return "Terminal";
    }
    fn get_raw_pos(&self) -> usize {
        return self.token.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_pos(&self) -> Position {
        self.token.pos.clone()
    }
    fn get_range(&self) -> Range {
        todo!()
    }
}

#[derive(Debug)]
pub struct AstClass {
    pub raw_pos: usize,
    pub name: String,
    pub parent_class: String,
}
impl IAstNode for AstClass {
    fn get_type(&self) -> &'static str {
        return "Class";
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_pos(&self) -> Position {
        todo!();
    }

    fn get_range(&self) -> Range {
        todo!();
    }
}

#[derive(Debug)]
pub struct AstUses {
    pub raw_pos: usize,
    pub list_of_uses: Vec<Token>,
}
impl IAstNode for AstUses {
    fn get_type(&self) -> &'static str {
        return "Uses";
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_pos(&self) -> Position {
        todo!()
    }

    fn get_range(&self) -> Range {
        todo!()
    }
}

#[derive(Debug)]
pub struct AstTypeBasicFixedSize {
    pub raw_pos: usize,
    pub type_token: Token,
}
impl IAstNode for AstTypeBasicFixedSize {
    fn get_type(&self) -> &'static str {
        return "Type Primitive Unsized";
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_pos(&self) -> Position {
        todo!()
    }

    fn get_range(&self) -> Range {
        todo!()
    }
}
#[derive(Debug)]
pub struct AstTypeBasicDynamicSize {
    pub raw_pos: usize,
    pub type_token: Token,
}
impl IAstNode for AstTypeBasicDynamicSize {
    fn get_type(&self) -> &'static str {
        return "Type Primitive Unsized";
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_pos(&self) -> Position {
        todo!()
    }

    fn get_range(&self) -> Range {
        todo!()
    }
}

#[derive(Debug)]
pub struct AstEmpty {}
impl IAstNode for AstEmpty {
    fn get_type(&self) -> &'static str {
        return "";
    }
    fn get_raw_pos(&self) -> usize {
        return 0;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_pos(&self) -> Position {
        todo!()
    }

    fn get_range(&self) -> Range {
        todo!()
    }
}

#[derive(Debug)]
pub struct AstTypeEnum{
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub variants: Vec<Token>
}
impl IAstNode for AstTypeEnum {
    fn get_type(&self) -> &'static str {
        return "Enum Type"
    }

    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }

    fn get_pos(&self) -> Position {
        return self.pos.clone();
    }

    fn get_range(&self) -> Range {
        return self.range.clone();
    }

    fn as_any(&self) -> &dyn Any {
        self 
    }
}

#[derive(Debug)]
pub struct AstTypeReference {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub options: Vec<Token>   
}
impl IAstNode for AstTypeReference {
    fn get_type(&self) -> &'static str {
        return "Type Reference"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }

    fn get_range(&self) -> Range {
        self.range.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}