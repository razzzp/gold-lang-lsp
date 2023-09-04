use std::any::Any;

use crate::lexer::tokens::Token;
use crate::utils::{Position, Range, IRange, DynamicChild};

macro_rules! implem_irange {
    ($struct_name:ident) => {
        impl IRange for $struct_name {
            fn get_range(&self) -> Range {
                self.range.clone()
            }
            fn set_range(&mut self, new_range: Range) {
                self.range=new_range
            }
            fn as_range(&self) -> &dyn IRange {
                self
            }
        }
    };
}

macro_rules! implem_iastnode_common {
    ($struct_name:ident, $string_type:literal) => {
        fn get_type(&self) -> &'static str {
            return stringify!($struct_name);
        }
        fn get_raw_pos(&self) -> usize {
            return self.raw_pos;
        }
        fn as_any(&self) -> &dyn Any {
            self
        }
        fn as_ast_node(&self) -> &dyn IAstNode{
            self
        }
        fn to_string_type(&self) -> String {
            $string_type.to_string()
        }
    };
}

pub trait IAstNode: std::fmt::Debug + IRange {
    /// returns node type, for display?
    fn get_type(&self) -> &'static str;
    fn get_raw_pos(&self) -> usize;
    fn get_pos(&self) -> Position{
        self.get_range().start.clone()
    }
    // fn get_range(&self) -> Range;
    fn as_any(&self) -> &dyn Any;
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>>{
        None
    }
    /// gets children wrapped in DynamicChild object, to provide parent node
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>>{
        if let Some(children) = self.get_children() {
            return Some(children.into_iter().map(|child| {
                DynamicChild::new(child, Some(self.as_ast_node()))
            }).collect())
        } else {
            return None
        }
    }
    /// main identifier of the node, if none exist, return the pos as string
    fn get_identifier(&self) -> String{
        todo!()
    }
    fn to_string_type_pos(&self) -> String {
        format!("{}:{}", self.to_string_type(), self.get_pos().to_string_brief())
    }
    fn to_string_ident_pos(&self) -> String {
        format!("{}:{}", self.get_identifier(), self.get_pos().to_string_brief())
    }
    fn to_string_type(&self) -> String {
        todo!()
    }
    fn as_ast_node(&self) -> &dyn IAstNode;
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
    fn set_range(&mut self, _new_range: Range) {
        // TODO hmm what todo here?
        ()
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTerminal {
    fn get_type(&self) -> &'static str {
        return "Terminal";
    }

    fn get_identifier(&self) -> String {
        self.token.value.as_ref().unwrap().to_string()
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "terminal".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.name.clone()
    }
    fn to_string_type(&self) -> String {
        "class".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.pos.to_string()
    }
    fn to_string_type(&self) -> String {
        "uses".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeBasic {
    fn get_type(&self) -> &'static str {
        return "Type Basic Fixed";
    }
    fn get_identifier(&self) -> String {
        return self.type_token.get_value()
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "type_basic".to_string()
    }
}

#[derive(Debug)]
pub struct AstEmpty {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
}
impl AstEmpty{
    pub fn new(raw_pos:usize, pos: Position, range: Range)-> AstEmpty{
        AstEmpty{raw_pos,pos,range}
    }
    pub fn default()-> AstEmpty{
        AstEmpty{raw_pos:0,pos:Position::default(),range:Range::default()}
    }
}
impl IRange for AstEmpty {
    fn get_range(&self) -> Range {
        Range::default()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
        Position::default()
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        "empty_node".to_string()
    }
    fn to_string_type(&self) -> String {
        "empty".to_string()
    }
}

#[derive(Debug)]
pub struct AstEnumVariant{
    pub raw_pos: usize,
    pub range: Range,
    pub identifier: Token
}
impl IRange for AstEnumVariant {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstEnumVariant {
    fn get_type(&self) -> &'static str {
        return "Enum Variant"
    }

    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self 
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "enum_variant".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeEnum{
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub variants: Vec<Box<AstEnumVariant>>,
}
impl IRange for AstTypeEnum {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeEnum {
    fn get_type(&self) -> &'static str {
        return "Type Enum"
    }

    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(
            self.variants.iter().map(|n| n.as_ast_node()).collect()
        )
    }
    fn get_pos(&self) -> Position {
        return self.pos.clone();
    }
    fn as_any(&self) -> &dyn Any {
        self 
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.pos.to_string()
    }
    fn to_string_type(&self) -> String {
        "type_enum".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeReference {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub ref_type: Token,
    pub options: Vec<Token>,
    pub ident_token: Token,
    pub inverse_var_token: Option<Token>, 
}
impl IRange for AstTypeReference {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.ref_type.get_value()
    }
    fn to_string_type(&self) -> String {
        "type_ref".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeSet {
    pub raw_pos: usize,
    pub range: Range,
    pub set_type: Box<dyn IAstNode>,
}
impl IRange for AstTypeSet {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeSet {
    fn get_type(&self) -> &'static str {
        return "Type Set"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.get_range().start.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.set_type.get_identifier()
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.set_type.as_ref());
        return Some(result);
    }
    fn to_string_type(&self) -> String {
        "type_set".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.type_node.as_ref(), Some(self)));
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "type_decl".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "const_decl".to_string()
    }
}

#[derive(Debug)]
pub struct AstGlobalVariableDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub type_node: Box<dyn IAstNode>,
    pub modifiers: Option<Box<AstMemberModifiers>>,
    pub is_memory: bool
}
impl IRange for AstGlobalVariableDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstGlobalVariableDeclaration {
    fn get_type(&self) -> &'static str {
        return "Global Variable Declaration"
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.type_node.as_ref(), Some(self)));
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "gvar_decl".to_string()
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
    pub body: Option<AstMethodBody>,
    pub end_token: Option<Token>,
}
impl IRange for AstProcedure {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstProcedure {
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        if self.modifiers.is_some() {result.push(self.modifiers.as_ref().unwrap().as_ast_node());}
        if self.body.is_some() {result.push(self.body.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(DynamicChild::new(self.parameter_list.as_ref().unwrap().as_ast_node(), Some(self)));}
        if self.modifiers.is_some() {result.push(DynamicChild::new(self.modifiers.as_ref().unwrap().as_ast_node(), Some(self)));}
        if self.body.is_some() {result.push(DynamicChild::new(self.body.as_ref().unwrap().as_ast_node(), Some(self)));}
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "proc_decl".to_string()
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
    pub body: Option<AstMethodBody>,
    pub end_token: Option<Token>,
}
impl IRange for AstFunction {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstFunction {
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        result.push(self.return_type.as_ref().as_ast_node());
        if self.modifiers.is_some() {result.push(self.modifiers.as_ref().unwrap().as_ast_node());}
        if self.body.is_some() {result.push(self.body.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(DynamicChild::new(self.parameter_list.as_ref().unwrap().as_ast_node(), Some(self)));}
        result.push(DynamicChild::new(self.return_type.as_ref().as_ast_node(), Some(self)));
        if self.modifiers.is_some() {result.push(DynamicChild::new(self.modifiers.as_ref().unwrap().as_ast_node(), Some(self)));}
        if self.body.is_some() {result.push(DynamicChild::new(self.body.as_ref().unwrap().as_ast_node(), Some(self)));}
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "fun_decl".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(self.parameter_list.iter().map(|node| {node.as_ref()}).collect());
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        return Some(self.parameter_list.iter().map(|node| {
            DynamicChild::new(node.as_ref(), Some(self))        
        }).collect());
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "param_decls".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstParameterDeclaration {
    fn get_type(&self) -> &'static str {
        "Param Declaration"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.type_node.is_some() {result.push(self.type_node.as_ref().unwrap().as_ref())}
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        if self.type_node.is_some() {result.push(DynamicChild::new(self.type_node.as_ref().unwrap().as_ref(), Some(self)));}
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "param_decl".to_string()
    }
}


#[derive(Debug,Default)]
pub struct AstMemberModifiers {
    pub raw_pos: usize,
    pub range: Range,
    pub is_private: bool,
    pub is_protected: bool,
    pub is_final: bool,
    pub is_override: bool,
}
impl IRange for AstMemberModifiers { 
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstMemberModifiers {
    fn get_type(&self) -> &'static str {
        "Member Modifiers"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.get_range().start.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "member_mod".to_string()
    }
}
#[derive(Debug,Default)]
pub struct AstMethodModifiers {
    pub raw_pos: usize,
    pub range: Range,
    pub modifiers: Option<Box<AstMemberModifiers>>,
    pub external_dll_name: Option<String>,
    pub is_forward: bool
}
impl IRange for AstMethodModifiers { 
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
        self.get_range().start.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if let Some(modifers_node) = self.modifiers.as_ref() {
            result.push(modifers_node.as_ast_node());
        }
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "method_mod".to_string()
    }
}

#[derive(Debug)]
pub struct AstMethodBody {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub statements: Vec<Box<dyn IAstNode>>,
}
impl IRange for AstMethodBody {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(self.statements.iter().map(|node| {node.as_ref()}).collect());
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        return Some(self.statements.iter().map(|node| {
            DynamicChild{data:node.as_ref(),parent:Some(self)} 
        }).collect());
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.pos.to_string()
    }
    fn to_string_type(&self) -> String {
        "method_body".to_string()
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
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.pos.to_string()
    }
    fn to_string_type(&self) -> String {
        "comment".to_string()
    }
}

#[derive(Debug)]
pub struct AstBinaryOp {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub op_token: Token,
    pub left_node: Box<dyn IAstNode>,
    pub right_node: Box<dyn IAstNode>
}
impl IRange for AstBinaryOp {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range = new_range;
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstBinaryOp {
    fn get_type(&self) -> &'static str {
        "Binary Op"
    }

    fn get_identifier(&self) -> String {
        self.op_token.get_value()
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.left_node.as_ref());
        result.push(self.right_node.as_ref());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.left_node.as_ref(), Some(self)));
        result.push(DynamicChild::new(self.right_node.as_ref(),Some(self)));
        return Some(result);
    }
        
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "bin_op".to_string()
    }
}

#[derive(Debug)]
pub struct AstCast {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub type_node: Box<dyn IAstNode>,
    pub expr_node: Box<dyn IAstNode>
}
impl IRange for AstCast {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstCast {
    fn get_type(&self) -> &'static str {
        "Cast"
    }

    fn get_identifier(&self) -> String {
        format!("{}", self.type_node.get_identifier())
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        result.push(self.expr_node.as_ref());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.type_node.as_ref(), Some(self)));
        result.push(DynamicChild::new(self.expr_node.as_ref(),Some(self)));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "cast".to_string()
    }
}

#[derive(Debug)]
pub struct AstUnaryOp {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub op_token: Token,
    pub expr_node: Box<dyn IAstNode>
}
impl IRange for AstUnaryOp {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range = new_range;
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstUnaryOp {
    fn get_type(&self) -> &'static str {
        "Unary Op"
    }

    fn get_identifier(&self) -> String {
        self.op_token.get_value()
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.expr_node.as_ref());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.expr_node.as_ref(),Some(self)));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "unary_op".to_string()
    }
}

#[derive(Debug)]
pub struct AstMethodCall {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub parameter_list: Vec<Box<dyn IAstNode>>
}
impl IRange for AstMethodCall {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstMethodCall {
    fn get_type(&self) -> &'static str {
        "Method Call"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(self.parameter_list.iter().map(|node| {node.as_ref()}).collect());
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        return Some(self.parameter_list.iter().map(|node| {
            DynamicChild::new(node.as_ref(), Some(self))        
        }).collect());
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "method_call".to_string()
    }
}

#[derive(Debug)]
pub struct AstConditionalBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub condition: Option<Box<dyn IAstNode>>,
    pub statements: Vec<Box<dyn IAstNode>>,
}
impl AstConditionalBlock{
    pub fn append_node(&mut self, node: Box<dyn IAstNode>) {
        self.statements.push(node);
    }
}
impl IRange for AstConditionalBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstConditionalBlock {
    fn get_type(&self) -> &'static str {
        "Conditional Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.condition.is_some() {result.push(self.condition.as_ref().unwrap().as_ast_node());}
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "cond_block".to_string()
    }
}

#[derive(Debug)]
pub struct AstIfBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub if_block: Box<AstConditionalBlock>,
    // else block also goes here, with conditional node being an empty node
    pub else_if_blocks: Option<Vec<Box<AstConditionalBlock>>>,
    pub end_token: Option<Token>
}
impl AstIfBlock {
    pub fn add_else_if_block(&mut self, block: Box<AstConditionalBlock>){
        if self.else_if_blocks.is_none() {
            self.else_if_blocks = Some(Vec::new());
        }
        self.else_if_blocks.as_mut().unwrap().push(block)
    }
}
impl IRange for AstIfBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstIfBlock {
    fn get_type(&self) -> &'static str {
        "If Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.if_block.as_ast_node());
        match self.else_if_blocks.as_ref() {
            Some(else_if_block) =>{
                result.extend(else_if_block.iter().map(|n| {
                    n.as_ast_node()
                }))
            }
            _=> ()
        }
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.if_block.as_ast_node(), Some(self)));
        match self.else_if_blocks.as_ref() {
            Some(else_if_block) =>{
                result.extend(else_if_block.iter().map(|n| {
                    DynamicChild::new(n.as_ast_node(), Some(self))
                }))
            }
            _=> ()
        }
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "if".to_string()
    }
}

#[derive(Debug)]
pub struct AstForBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub counter_token: Token,
    pub range_node: Box<dyn IAstNode>,
    pub step_node: Option<Box<dyn IAstNode>>,
    pub statements: Option<Vec<Box<dyn IAstNode>>>,
    pub end_token: Option<Token>
}
impl AstForBlock {
    
}
impl IRange for AstForBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstForBlock {
    fn get_type(&self) -> &'static str {
        "For Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.range_node.as_ast_node());
        match &self.step_node {
            Some(n)=> {result.push(n.as_ast_node())}
            _ => ()
        };
        match self.statements.as_ref() {
            Some(statements) =>{
                result.extend(statements.iter().map(|n| {
                    n.as_ast_node()
                }))
            }
            _=> ()
        }
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.range_node.as_ast_node(), Some(self)));
        match &self.step_node {
            Some(n)=> {result.push(DynamicChild::new(n.as_ast_node(),Some(self)))}
            _ => ()
        };
        match self.statements.as_ref() {
            Some(statements) =>{
                result.extend(statements.iter().map(|n| {
                    DynamicChild::new(n.as_ast_node(), Some(self))
                }))
            }
            _=> ()
        }
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "for".to_string()
    }
}

#[derive(Debug)]
pub struct AstForEachBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub in_expr_node: Box<dyn IAstNode>,
    pub using_var: Option<Box<dyn IAstNode>>,
    pub statements: Option<Vec<Box<dyn IAstNode>>>,
    pub end_token: Option<Token>
}
impl AstForEachBlock {
    
}
impl IRange for AstForEachBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstForEachBlock {
    fn get_type(&self) -> &'static str {
        "Foreach Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.in_expr_node.as_ast_node());
        if let Some(using_var) = self.using_var.as_ref() {
            result.push(using_var.as_ast_node());
        }
        match self.statements.as_ref() {
            Some(statements) =>{
                result.extend(statements.iter().map(|n| {
                    n.as_ast_node()
                }))
            }
            _=> ()
        }
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "foreach".to_string()
    }
}

#[derive(Debug)]
pub struct AstWhileBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub cond_block: Box<AstConditionalBlock>,
    pub end_token: Option<Token>
}
impl AstWhileBlock {
    
}
impl IRange for AstWhileBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstWhileBlock {
    fn get_type(&self) -> &'static str {
        "Loop Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.cond_block.as_ast_node());
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(DynamicChild::new(self.cond_block.as_ast_node(), Some(self)));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "while".to_string()
    }
}

#[derive(Debug)]
pub struct AstLoopBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub statements: Vec<Box<dyn IAstNode>>,
    pub end_token: Option<Token>
}
impl AstLoopBlock {
    
}
impl IRange for AstLoopBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstLoopBlock {
    fn get_type(&self) -> &'static str {
        "While Block"
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
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.statements.iter().map(|n| {
            DynamicChild::new(n.as_ast_node(), Some(self))
        }));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "loop".to_string()
    }
}

#[derive(Debug)]
pub struct AstLocalVariableDeclaration {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub identifier: Token,
    pub type_node: Box<dyn IAstNode>,
    pub absolute: Option<Box<dyn IAstNode>>,
}
impl IRange for AstLocalVariableDeclaration {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstLocalVariableDeclaration {
    fn get_type(&self) -> &'static str {
        return "Local Variable Declaration"
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        if let Some(node) = &self.absolute {
            result.push(node.as_ref().as_ast_node()); 
        }
        return Some(result);
    }
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {

        if let Some(children) = self.get_children() {
            return Some(children.into_iter().map(|child| {
                DynamicChild::new(child, Some(self.as_ast_node()))
            }).collect())
        } else {
            return None
        }
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "lvar_decl".to_string()
    }
}

#[derive(Debug)]
pub struct AstReturnNode {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub return_expr: Box<dyn IAstNode>,
}
impl IRange for AstReturnNode {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstReturnNode {
    fn get_type(&self) -> &'static str {
        return "Return Statement"
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
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.return_expr.as_ref().as_ast_node());
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "return".to_string()
    }
}

#[derive(Debug)]
pub struct AstSetLiteral {
    pub raw_pos: usize,
    pub range: Range,
    pub set_items: Vec<Box<dyn IAstNode>>,
}
impl IRange for AstSetLiteral {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstSetLiteral {
    fn get_type(&self) -> &'static str {
        return "Set Literal"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }

    fn get_pos(&self) -> Position {
        self.get_range().start.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(
            self.set_items.iter().map(|n| n.as_ast_node()).collect()
        );
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "set_lit".to_string()
    }
}


#[derive(Debug)]
pub struct AstWhenBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub when_expr: Option<Box<dyn IAstNode>>,
    pub statements: Vec<Box<dyn IAstNode>>,
}
impl IRange for AstWhenBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstWhenBlock {
    fn get_type(&self) -> &'static str {
        "When Block"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.when_expr.is_some() {result.push(self.when_expr.as_ref().unwrap().as_ast_node());}
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "when_block".to_string()
    }
}

#[derive(Debug)]
pub struct AstSwitchBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub switch_expr: Box<dyn IAstNode>,
    // else block also goes here, with conditional node being an empty node
    pub when_blocks: Vec<Box<AstWhenBlock>>,
    pub end_token: Option<Token>
}
impl AstSwitchBlock {
    pub fn add_case_block(&mut self, block: Box<AstWhenBlock>){
        self.when_blocks.push(block);
    }
}
impl IRange for AstSwitchBlock {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstSwitchBlock {
    fn get_type(&self) -> &'static str {
        "Switch Block"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.switch_expr.as_ast_node());
        result.extend(self.when_blocks.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "switch".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeRecordField{
    pub raw_pos: usize,
    pub range: Range,
    pub identifier : Token,
    pub type_node: Box<dyn IAstNode>
}
impl IRange for AstTypeRecordField {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeRecordField {
    fn get_type(&self) -> &'static str {
        "Type Record Field"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.identifier.get_value()
    }
    fn to_string_type(&self) -> String {
        "type_record_field".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeRecord{
    pub raw_pos: usize,
    pub range: Range,
    pub fields : Vec<Box<dyn IAstNode>>
}
impl IRange for AstTypeRecord {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeRecord {
    fn get_type(&self) -> &'static str {
        "Type Record"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        self.fields.iter().for_each(|field| {
            result.push(field.as_ref());
        });
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_record".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypePointer{
    pub raw_pos: usize,
    pub range: Range,
    pub type_node : Box<dyn IAstNode>
}
impl IRange for AstTypePointer {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypePointer {
    fn get_type(&self) -> &'static str {
        "Type Pointer"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_pointer".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeArray{
    pub raw_pos: usize,
    pub range: Range,
    pub array_seq_token: Token,
    pub index_nodes : Vec<Box<dyn IAstNode>>,
    pub object_type : Box<dyn IAstNode>
}
impl IRange for AstTypeArray {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeArray {
    fn get_type(&self) -> &'static str {
        "Type Array/Seq"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        self.index_nodes.iter().for_each(|node|{
            result.push(node.as_ast_node());
        });
        result.push(self.object_type.as_ref());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_array".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeRange{
    pub raw_pos: usize,
    pub range: Range,
    pub from: Box<dyn IAstNode>,
    pub to: Box<dyn IAstNode>,
}
impl IRange for AstTypeRange {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeRange {
    fn get_type(&self) -> &'static str {
        "Type Range"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.from.as_ref());
        result.push(self.to.as_ref());
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_range".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeProcedure {
    pub raw_pos: usize,
    pub range: Range,
    pub parameter_list: Option<AstParameterDeclarationList>,
}
impl IRange for AstTypeProcedure {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeProcedure {
    fn get_type(&self) -> &'static str {
        "Type Procedure"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_proc".to_string()
    }
}


#[derive(Debug)]
pub struct AstTypeFunction {
    pub raw_pos: usize,
    pub range: Range,
    pub parameter_list: Option<AstParameterDeclarationList>,
    pub return_type: Box<dyn IAstNode>
}
impl IRange for AstTypeFunction {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeFunction {
    fn get_type(&self) -> &'static str {
        "Type Function"
    }

    fn get_raw_pos(&self) -> usize {
        self.raw_pos
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        result.push(self.return_type.as_ast_node());
        return Some(result);
    }
    fn get_identifier(&self) -> String {
        self.to_string_type_pos()
    }
    fn to_string_type(&self) -> String {
        "type_func".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeInstanceOf {
    pub raw_pos: usize,
    pub range: Range,
    pub instance_type: Box<dyn IAstNode>,
}
impl IRange for AstTypeInstanceOf {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstTypeInstanceOf {
    fn get_type(&self) -> &'static str {
        return "Type InstanceOf";
    }
    fn get_identifier(&self) -> String {
        return self.instance_type.get_identifier()
    }
    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.instance_type.as_ast_node());
        return Some(result);
    }
    fn to_string_type(&self) -> String {
        "type_instanceof".to_string()
    }
}

#[derive(Debug)]
pub struct AstArrayAccess {
    pub raw_pos: usize,
    pub range: Range,
    pub left_node: Box<dyn IAstNode>,
    pub index_node: Box<dyn IAstNode>
}
implem_irange!(AstArrayAccess);
impl IAstNode for AstArrayAccess {
    implem_iastnode_common!(AstArrayAccess, "array_access");
    fn get_identifier(&self) -> String {
        return self.left_node.get_identifier()
    }
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.left_node.as_ast_node());
        result.push(self.index_node.as_ast_node());
        return Some(result);
    }
}