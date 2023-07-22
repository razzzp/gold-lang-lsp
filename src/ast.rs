use std::any::Any;

use crate::lexer::tokens::Token;
use crate::utils::{Position, Range, IRange};

pub trait IAstNode: std::fmt::Debug + IRange {
    fn get_type(&self) -> &'static str;
    fn get_raw_pos(&self) -> usize;
    fn get_pos(&self) -> Position;
    // fn get_range(&self) -> Range;
    fn as_any(&self) -> &dyn Any;
    fn as_range(&self) -> &dyn IRange;
    // fn get_token(&self) -> Token;
    // fn eval() -> ();
}


#[derive(Debug)]
pub struct AstTerminal {
    pub token: Token,
}
impl IRange for AstTerminal {
    fn get_range(&self) -> Range {
        self.token.range.clone()
    }
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
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstClass {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub name: String,
    pub parent_class: String,
}
impl IRange for AstClass {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
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
        self.pos.clone()
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstUses {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub list_of_uses: Vec<Token>,
}
impl IRange for AstUses {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
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
        self.pos.clone()
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstTypeBasic {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub type_token: Token,
}
impl IRange for AstTypeBasic {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstTypeBasic {
    fn get_type(&self) -> &'static str {
        return "Type Basic Fixed";
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstEmpty {}
impl IRange for AstEmpty {
    fn get_range(&self) -> Range {
        todo!()
    }
}
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
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstTypeEnum{
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub variants: Vec<Token>
}
impl IRange for AstTypeEnum {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstTypeEnum {
    fn get_type(&self) -> &'static str {
        return "Type Enum"
    }

    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }

    fn get_pos(&self) -> Position {
        return self.pos.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self 
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstTypeReference {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub ref_type: Token,
    pub options: Vec<Token>   
}
impl IRange for AstTypeReference {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
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
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstTypeDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub type_node: Box<dyn IAstNode>
}
impl IRange for AstTypeDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstTypeDeclaration {
    fn get_type(&self) -> &'static str {
        return "Type Declaration"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstConstantDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub value: Token,
    pub is_multi_lang : bool
}
impl IRange for AstConstantDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstConstantDeclaration {
    fn get_type(&self) -> &'static str {
        return "Constant Declaration"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstGlobalVariableDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub type_node: Box<dyn IAstNode>,
    pub is_memory: bool
}
impl IRange for AstGlobalVariableDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstGlobalVariableDeclaration {
    fn get_type(&self) -> &'static str {
        return "Variable Declaration"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstProcedure {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub parameter_list: Option<AstParameterDeclarationList>,
    pub modifiers: Option<AstMethodModifiers>,
    pub body: Option<AstMethodBody>
}
impl IRange for AstProcedure {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstFunction {
    fn get_type(&self) -> &'static str {
        "Procedure Declaration"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}


#[derive(Debug)]
pub struct AstFunction {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub parameter_list: Option<AstParameterDeclarationList>,
    pub return_type: Box<dyn IAstNode>,
    pub modifiers: Option<AstMethodModifiers>,
    pub body: Option<AstMethodBody>
}
impl IRange for AstFunction {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstProcedure {
    fn get_type(&self) -> &'static str {
        "Function Declaration"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstParameterDeclarationList {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub parameter_list: Vec<Box<dyn IAstNode>>
}
impl IRange for AstParameterDeclarationList {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstParameterDeclarationList {
    fn get_type(&self) -> &'static str {
        "Parameter Declaration List"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstParameterDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub type_node: Option<Box<dyn IAstNode>>,
    pub modifier: Option<Token>
}
impl IRange for AstParameterDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstParameterDeclaration {
    fn get_type(&self) -> &'static str {
        "Procedure"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug,Default)]
pub struct AstMethodModifiers {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub is_private: bool,
    pub is_protected: bool,
    pub is_final: bool,
    pub is_override: bool,
    pub external_dll_name: Option<String>,
    pub is_forward: bool
}
impl IRange for AstMethodModifiers { 
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstMethodModifiers {
    fn get_type(&self) -> &'static str {
        "Method Modifiers"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstMethodBody {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub statements: Vec<Box<dyn IAstNode>>,
    pub end_token: Token,
}
impl IRange for AstMethodBody {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstMethodBody {
    fn get_type(&self) -> &'static str {
        "Method Body"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

#[derive(Debug)]
pub struct AstComment {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub comment: String,
}
impl IRange for AstComment {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
}
impl IAstNode for AstComment {
    fn get_type(&self) -> &'static str {
        "Comment"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.pos.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}



