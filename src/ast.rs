use std::any::Any;

use crate::lexer::tokens::Token;
use crate::utils::{Position, Range, IRange, DynamicChild};

pub trait IAstNode: std::fmt::Debug + IRange {
    /// returns node type, for display?
    fn get_type(&self) -> &'static str;
    fn get_raw_pos(&self) -> usize;
    fn get_pos(&self) -> Position;
    // fn get_range(&self) -> Range;
    fn as_any(&self) -> &dyn Any;
    fn get_children(&self) -> Option<Vec<&dyn IAstNode>>{
        None
    }
    /// gets children wrapped in DynamicChild object, to provide parent node
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>>{
        None
    }
    /// main identifier of the node, if none exist, return the pos as string
    fn get_identifier(&self) -> String{
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
    fn as_range(&self) -> &dyn IRange {
        self
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
        self.pos.to_string()
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
    fn as_range(&self) -> &dyn IRange {
        self
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
}

#[derive(Debug)]
pub struct AstMethodBody {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub statements: Vec<Box<dyn IAstNode>>,
    pub end_token: Option<Token>,
}
impl IRange for AstMethodBody {
    fn get_range(&self) -> Range {
        self.range.clone()
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
        self.op_token.value.as_ref().unwrap().to_string()
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
    fn get_children_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>> {
        let mut result = Vec::new();
        if self.condition.is_some() {result.push(DynamicChild::new(self.condition.as_ref().unwrap().as_ast_node(), Some(self)));}
        result.extend(self.statements.iter().map(|n| {
            DynamicChild::new(n.as_ast_node(), Some(self))
        }));
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> String {
        self.pos.to_string()
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
        self.pos.to_string()
    }
}