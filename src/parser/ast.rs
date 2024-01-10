use std::any::Any;
use std::sync::Arc;

use crate::lexer::tokens::Token;
use crate::utils::{Position, Range, IRange, DynamicChild, OptionExt};

#[macro_export]
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

pub trait IAstNode: std::fmt::Debug + IRange + Send + Sync{
    /// returns node type, for display?
    fn get_type(&self) -> &'static str;
    fn get_raw_pos(&self) -> usize;
    fn get_pos(&self) -> Position{
        self.get_range().start.clone()
    }
    // fn get_range(&self) -> Range;
    fn as_any(&self) -> &dyn Any;
    fn get_children_ref<'a>(&'a self) -> Option<Vec<&dyn IAstNode>>{
        None
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>>{
        None
    }
    /// gets children wrapped in DynamicChild object, to provide parent node
    fn get_children_ref_dynamic(&self) -> Option<Vec<DynamicChild<dyn IAstNode>>>{
        if let Some(children) = self.get_children_ref() {
            return Some(children.into_iter().map(|child| {
                DynamicChild::new(child, Some(self.as_ast_node()))
            }).collect())
        } else {
            return None
        }
    }
    /// main identifier of the node, if none exist, return the pos as string
    fn get_identifier(&self) -> &str{
        todo!()
    }
    fn to_string_type_pos(&self) -> String {
        format!("{}:{}", self.to_string_type(), self.get_pos().to_string_brief())
    }
    fn to_string_ident_pos(&self) -> String {
        format!("{}:{}", self.get_identifier(), self.get_pos().to_string_brief())

    }
    fn to_string_type_range(&self) -> String {
        format!("{}:{}", self.to_string_type(), self.get_range().to_string_brief())

    }
    fn to_string_type(&self) -> String {
        todo!()
    }
    fn as_ast_node(&self) -> &dyn IAstNode;
    // fn get_token(&self) -> Token;
    // fn eval() -> ();
}

#[derive(Debug)]
pub struct AstRoot{
    pub statements: Vec<Arc<dyn IAstNode>>,
}
impl AstRoot {
    pub fn new(statements: Vec<Arc<dyn IAstNode>>) -> AstRoot{
        return AstRoot{
            statements,
        }
    }
}
impl IRange for AstRoot{
    fn get_range(&self) -> Range {
        Range::default()
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}
impl IAstNode for AstRoot{
    fn get_type(&self) -> &'static str {
        return "AstRoot";
    }
    fn get_identifier(&self) -> &str {
        ""
    }
    fn get_raw_pos(&self) -> usize {
        0
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn to_string_type(&self) -> String {
        "root".to_string()
    }
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        self.statements.iter().for_each(|n|{
            result.push(n.as_ast_node())
        });
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.statements.iter().collect());
    }
    
}

#[derive(Debug)]
pub struct AstTerminal {
    pub token: Token,
}
impl AstTerminal{
    pub fn new(token: Token)->AstTerminal{
        return AstTerminal { token, }
    }
}
impl IRange for AstTerminal {
    fn get_range(&self) -> Range {
        self.token.get_range()
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

    fn get_identifier(&self) -> &str {
        self.token.get_value_as_str()
    }

    fn get_raw_pos(&self) -> usize {
        return self.token.raw_pos;
    }
    fn as_any(&self) -> &dyn Any {
        self
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
    pub range: Range,
    pub identifier: Token,
    pub parent_class: Option<Token>,
}
implem_irange!(AstClass);
impl IAstNode for AstClass {
    implem_iastnode_common!(AstClass, "class");
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstModule {
    pub raw_pos: usize,
    pub range: Range,
    pub id: Token,
}
implem_irange!(AstModule);
impl IAstNode for AstModule {
    implem_iastnode_common!(AstModule, "module");
    fn get_identifier(&self) -> &str {
        self.id.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstUses {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub list_of_uses: Vec<Token>,
}
implem_irange!(AstUses);
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
    fn get_identifier(&self) -> &str {
        "uses"
    }
    fn to_string_type(&self) -> String {
        "uses".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeBasic {
    pub raw_pos: usize,
    pub range: Range,
    pub id_token: Token,
}
implem_irange!(AstTypeBasic);
impl IAstNode for AstTypeBasic {
    implem_iastnode_common!(AstTypeBasic, "type_basic");
    fn get_identifier(&self) -> &str {
        return self.id_token.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstTypeSized {
    pub raw_pos: usize,
    pub range: Range,
    pub type_token: Token,
    pub size_token: Token,
}
implem_irange!(AstTypeSized);
impl IAstNode for AstTypeSized {
    implem_iastnode_common!(AstTypeSized, "type_sized");
    fn get_identifier(&self) -> &str {
        return self.type_token.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstEmpty {
    pub raw_pos: usize,
    pub range: Range,
}
impl AstEmpty{
    pub fn new(raw_pos:usize,  range: Range)-> AstEmpty{
        AstEmpty{raw_pos,range}
    }
    pub fn default()-> AstEmpty{
        AstEmpty{raw_pos:0,range:Range::default()}
    }
}
implem_irange!(AstEmpty);
impl IAstNode for AstEmpty {
    implem_iastnode_common!(AstEmpty, "empty_node");
    fn get_identifier(&self) -> &str {
        "empty_node"
    }
}

#[derive(Debug)]
pub struct AstEnumVariant{
    pub raw_pos: usize,
    pub range: Range,
    pub identifier: Token,
    pub value_token: Option<Token>
}
implem_irange!(AstEnumVariant);
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
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
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
    pub variants: Vec<Arc<dyn IAstNode>>,
}
implem_irange!(AstTypeEnum);
impl IAstNode for AstTypeEnum {
    fn get_type(&self) -> &'static str {
        return "Type Enum"
    }

    fn get_raw_pos(&self) -> usize {
        return self.raw_pos;
    }
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(
            self.variants.iter().map(|n| n.as_ast_node()).collect()
        )
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.variants.iter().collect());
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
    fn get_identifier(&self) -> &str {
        "type_enum"
    }
    fn to_string_type(&self) -> String {
        "type_enum".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeReference {
    pub raw_pos: usize,
    pub range: Range,
    pub ref_type: Token,
    pub options: Vec<Token>,
    pub ident_token: Token,
    pub inverse_var_token: Option<Token>, 
}
implem_irange!(AstTypeReference);
impl IAstNode for AstTypeReference {
    implem_iastnode_common!(AstTypeReference,"type_ref");
    fn get_identifier(&self) -> &str {
        return self.ident_token.get_value_as_str();
    }
}

#[derive(Debug)]
pub struct AstTypeSet {
    pub raw_pos: usize,
    pub range: Range,
    pub set_type: Arc<dyn IAstNode>,
}
implem_irange!(AstTypeSet);
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
    fn get_identifier(&self) -> &str {
        self.set_type.get_identifier()
    }
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.set_type.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.set_type);
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
    pub type_node: Arc<dyn IAstNode>
}
implem_irange!(AstTypeDeclaration);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
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
    pub value_token: Token,
    pub is_multi_lang : bool
}
implem_irange!(AstConstantDeclaration);
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
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
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
    pub type_node: Arc<dyn IAstNode>,
    pub modifiers: Option<Arc<AstMemberModifiers>>,
    pub is_memory: bool,
    pub absolute_node : Option<Arc<dyn IAstNode>>
}
implem_irange!(AstGlobalVariableDeclaration);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        self.absolute_node.as_ref().map(|n|{result.push(n.as_ref())});
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        self.absolute_node.as_ref().map(|n|{result.push(n)});
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
    }
    fn to_string_type(&self) -> String {
        "gvar_decl".to_string()
    }
}

#[derive(Debug)]
pub struct AstProcedure {
    pub raw_pos: usize,
    pub range: Range,
    pub identifier: Arc<dyn IAstNode>,
    pub parameter_list: Option<Arc<dyn IAstNode>>,
    pub modifiers: Option<Arc<dyn IAstNode>>,
    pub body: Option<Arc<dyn IAstNode>>,
    pub end_token: Option<Token>,
}
implem_irange!(AstProcedure);
impl IAstNode for AstProcedure {
    implem_iastnode_common!(AstProcedure, "proc_decl");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();

        result.push(self.identifier.as_ast_node());
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        // if self.modifiers.is_some() {result.push(self.modifiers.as_ref().unwrap().as_ast_node());}
        if self.body.is_some() {result.push(self.body.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();

        result.push(&self.identifier);
        result.extend(self.parameter_list.iter());
        // result.extend(self.modifiers.iter());
        result.extend(self.body.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_identifier()
    }
}


#[derive(Debug)]
pub struct AstFunction {
    pub raw_pos: usize,
    pub range: Range,
    pub identifier: Arc<dyn IAstNode>,
    pub parameter_list: Option<Arc<dyn IAstNode>>,
    pub return_type: Arc<dyn IAstNode>,
    pub modifiers: Option<Arc<dyn IAstNode>>,
    pub body: Option<Arc<dyn IAstNode>>,
    pub end_token: Option<Token>,
}
implem_irange!(AstFunction);
impl IAstNode for AstFunction {
    implem_iastnode_common!(AstFunction, "func_decl");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();

        result.push(self.identifier.as_ast_node());
        result.push(self.return_type.as_ref().as_ast_node());
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        // if self.modifiers.is_some() {result.push(self.modifiers.as_ref().unwrap().as_ast_node());}
        if self.body.is_some() {result.push(self.body.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();

        result.push(&self.identifier);
        result.push(&self.return_type);
        result.extend(self.parameter_list.iter());
        // result.extend(self.modifiers.iter());
        result.extend(self.body.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_identifier()
    }
}

#[derive(Debug)]
pub struct AstParameterDeclarationList {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub parameter_list: Vec<Arc<dyn IAstNode>>
}
implem_irange!(AstParameterDeclarationList);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(self.parameter_list.iter().map(|node| {node.as_ref()}).collect());
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.parameter_list.iter().collect());
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "param_decls"
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
    pub type_node: Option<Arc<dyn IAstNode>>,
    pub modifier: Option<Token>
}
implem_irange!(AstParameterDeclaration);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.type_node.is_some() {result.push(self.type_node.as_ref().unwrap().as_ref())}
        return Some(result);
    }
    	
	fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.type_node.iter());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
    }
    fn to_string_type(&self) -> String {
        "param_decl".to_string()
    }
}

#[derive(Debug,Default)]
pub struct MemberModifiers{
    pub is_private: bool,
    pub is_protected: bool,
    pub is_final: bool,
    pub is_override: bool,
}
#[derive(Debug,Default)]
pub struct AstMemberModifiers {
    pub raw_pos: usize,
    pub range: Range,
    pub modifier_tokens: Vec<Token>,
    pub modifiers: MemberModifiers,
}
implem_irange!(AstMemberModifiers);
impl IAstNode for AstMemberModifiers {
    implem_iastnode_common!(AstMemberModifiers, "member_modifiers");
    fn get_identifier(&self) -> &str {
        "member_modifiers"
    }
}
#[derive(Debug,Default)]
pub struct AstMethodModifiers {
    pub raw_pos: usize,
    pub range: Range,
    pub modifier_tokens: Vec<Token>,
    pub member_modifiers: MemberModifiers,
    pub external_dll_name: Option<Arc<str>>,
    pub is_forward: bool
}
implem_irange!(AstMethodModifiers);
impl IAstNode for AstMethodModifiers {
    implem_iastnode_common!(AstMethodModifiers, "method_modifiers");
    fn get_identifier(&self) -> &str {
        "method_modifiers"
    }
}

#[derive(Debug)]
pub struct AstMethodBody {
    pub raw_pos: usize,
    pub range: Range,
    pub statements: Vec<Arc<dyn IAstNode>>,
}
implem_irange!(AstMethodBody);
impl IAstNode for AstMethodBody {
    implem_iastnode_common!(AstMethodBody, "method_body");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(self.statements.iter().map(|node| {node.as_ref()}).collect());
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.statements.iter().collect());
    }
    fn get_identifier(&self) -> &str {
        "method_body"
    }
}

#[derive(Debug)]
pub struct AstComment {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub comment: Arc<str>,
}
implem_irange!(AstComment);
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
    fn get_identifier(&self) -> &str {
        "comment"
    }
    fn to_string_type(&self) -> String {
        "comment".to_string()
    }
}

#[derive(Debug)]
pub struct AstBinaryOp {
    pub raw_pos: usize,
    pub range: Range,
    pub op_token: Token,
    pub left_node: Arc<dyn IAstNode>,
    pub right_node: Arc<dyn IAstNode>
}
implem_irange!(AstBinaryOp);
impl IAstNode for AstBinaryOp {
    implem_iastnode_common!(AstBinaryOp, "bin_op");
    fn get_identifier(&self) -> &str {
        self.op_token.get_value_as_str()
    }
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.left_node.as_ref());
        result.push(self.right_node.as_ref());
        return Some(result);
    }
	fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.left_node);
        result.push(&self.right_node);
        return Some(result);
    }
}

#[derive(Debug)]
pub struct AstCast {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub type_node: Arc<dyn IAstNode>,
    pub expr_node: Arc<dyn IAstNode>
}
implem_irange!(AstCast);
impl IAstNode for AstCast {
    fn get_type(&self) -> &'static str {
        "Cast"
    }

    fn get_identifier(&self) -> &str {
        self.type_node.get_identifier()
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        result.push(self.expr_node.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        result.push(&self.expr_node);
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
    pub expr_node: Arc<dyn IAstNode>
}
implem_irange!(AstUnaryOp);
impl IAstNode for AstUnaryOp {
    fn get_type(&self) -> &'static str {
        "Unary Op"
    }

    fn get_identifier(&self) -> &str {
        self.op_token.get_value_as_str()
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.expr_node.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.expr_node);
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
    pub identifier: Arc<dyn IAstNode>,
    pub parameter_list: Vec<Arc<dyn IAstNode>>
}
implem_irange!(AstMethodCall);
impl IAstNode for AstMethodCall {
    implem_iastnode_common!(AstMethodCall, "method_call");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        // result.push(self.identifier.as_ast_node());
        self.parameter_list.iter().for_each(|node| {result.push(node.as_ast_node())});
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        // result.push(&self.identifier);
        result.extend(self.parameter_list.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_identifier()
    }
}

#[derive(Debug)]
pub struct AstConditionalBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub condition: Option<Arc<dyn IAstNode>>,
    pub statements: Vec<Arc<dyn IAstNode>>,
}
impl AstConditionalBlock{
    pub fn append_node(&mut self, node: Arc<dyn IAstNode>) {
        self.statements.push(node);
    }
}
implem_irange!(AstConditionalBlock);
impl IAstNode for AstConditionalBlock {
    implem_iastnode_common!(AstConditionalBlock, "cond_block");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.condition.is_some() {result.push(self.condition.as_ref().unwrap().as_ast_node());}
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.condition.iter());
        result.extend(self.statements.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "cond_block"
    }
}

#[derive(Debug)]
pub struct AstIfBlock {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub if_block: Arc<dyn IAstNode>,
    // else block also goes here, with conditional node being an empty node
    pub else_if_blocks: Vec<Arc<dyn IAstNode>>,
    pub end_token: Option<Token>
}
impl AstIfBlock {
    pub fn add_else_if_block(&mut self, block: Arc<AstConditionalBlock>){
        self.else_if_blocks.push(block)
    }
}
implem_irange!(AstIfBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.if_block.as_ast_node());

        result.extend(self.else_if_blocks.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.if_block);
        result.extend(self.else_if_blocks.iter());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "if"
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
    pub range_node: Arc<dyn IAstNode>,
    pub step_node: Option<Arc<dyn IAstNode>>,
    pub statements: Option<Vec<Arc<dyn IAstNode>>>,
    pub end_token: Option<Token>
}
impl AstForBlock {
    
}
implem_irange!(AstForBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
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
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.range_node);
        result.extend(self.step_node.iter());
        self.statements.iter().for_each(|s|{s.iter().for_each(|s|{result.push(s)})});
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "for"
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
    pub in_expr_node: Arc<dyn IAstNode>,
    pub using_var: Option<Arc<dyn IAstNode>>,
    pub statements: Option<Vec<Arc<dyn IAstNode>>>,
    pub end_token: Option<Token>,
    pub is_downto: bool
}
impl AstForEachBlock {
    
}
implem_irange!(AstForEachBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
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
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.in_expr_node);
        if let Some(using_var) = self.using_var.as_ref() {
            result.push(using_var);
        }
        if let Some(statements) = self.statements.as_ref() {
            result.extend(statements.iter())
        }
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "foreach"
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
    pub cond_block: Arc<dyn IAstNode>,
    pub end_token: Option<Token>
}
impl AstWhileBlock {
    
}
implem_irange!(AstWhileBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.cond_block.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.cond_block);
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "while"
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
    pub statements: Vec<Arc<dyn IAstNode>>,
    pub end_token: Option<Token>
}
impl AstLoopBlock {
    
}
implem_irange!(AstLoopBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.statements.iter().collect());
    }
	
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "loop"
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
    pub type_node: Arc<dyn IAstNode>,
    pub absolute: Option<Arc<dyn IAstNode>>,
}
implem_irange!(AstLocalVariableDeclaration);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref().as_ast_node());
        if let Some(node) = &self.absolute {
            result.push(node.as_ref().as_ast_node()); 
        }
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        if let Some(node) = &self.absolute {
            result.push(node); 
        }
        return Some(result);
    }
	
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
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
    pub return_expr: Arc<dyn IAstNode>,
}
implem_irange!(AstReturnNode);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.return_expr.as_ref().as_ast_node());
        return Some(result);
    }
    	
	fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.return_expr);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "return"
    }
    fn to_string_type(&self) -> String {
        "return".to_string()
    }
}

#[derive(Debug)]
pub struct AstSetLiteral {
    pub raw_pos: usize,
    pub range: Range,
    pub set_items: Vec<Arc<dyn IAstNode>>,
}
implem_irange!(AstSetLiteral);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        return Some(
            self.set_items.iter().map(|n| n.as_ast_node()).collect()
        );
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        return Some(self.set_items.iter().collect());
    }
	
    fn get_identifier(&self) -> &str {
        "set_lit"
    }
    fn to_string_type(&self) -> String {
        "set_lit".to_string()
    }
}


#[derive(Debug)]
pub struct AstWhenBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub when_expr: Option<Arc<dyn IAstNode>>,
    pub statements: Vec<Arc<dyn IAstNode>>,
}
implem_irange!(AstWhenBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.when_expr.is_some() {result.push(self.when_expr.as_ref().unwrap().as_ast_node());}
        result.extend(self.statements.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.when_expr.iter());
        result.extend(self.statements.iter());
        return Some(result);
    }
	
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "when_block"
    }
    fn to_string_type(&self) -> String {
        "when_block".to_string()
    }
}

#[derive(Debug)]
pub struct AstSwitchBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub switch_expr: Arc<dyn IAstNode>,
    // else block also goes here, with conditional node being an empty node
    pub when_blocks: Vec<Arc<dyn IAstNode>>,
    pub end_token: Option<Token>
}
impl AstSwitchBlock {
    pub fn add_case_block(&mut self, block: Arc<AstWhenBlock>){
        self.when_blocks.push(block);
    }
}
implem_irange!(AstSwitchBlock);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.switch_expr.as_ast_node());
        result.extend(self.when_blocks.iter().map(|n| {
            n.as_ast_node()
        }));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.switch_expr);
        result.extend(self.when_blocks.iter());
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "switch"
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
    pub type_node: Arc<dyn IAstNode>
}
implem_irange!(AstTypeRecordField);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        self.identifier.get_value_as_str()
    }
    fn to_string_type(&self) -> String {
        "type_record_field".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeRecord{
    pub raw_pos: usize,
    pub range: Range,
    pub fields : Vec<Arc<dyn IAstNode>>,
    pub parent : Option<Arc<dyn IAstNode>>
}
implem_irange!(AstTypeRecord);
impl IAstNode for AstTypeRecord {
    implem_iastnode_common!(AstTypeRecord,"type_record");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        match &self.parent {
            Some(p) => result.push(p.as_ast_node()),
            _=> ()
        };
        self.fields.iter().for_each(|field| {
            result.push(field.as_ref());
        });
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        match &self.parent {
            Some(p) => result.push(p),
            _=> ()
        };
        self.fields.iter().for_each(|field| {
            result.push(field);
        });
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "type_record"
    }
}

#[derive(Debug)]
pub struct AstTypePointer{
    pub raw_pos: usize,
    pub range: Range,
    pub type_node : Arc<dyn IAstNode>
}
implem_irange!(AstTypePointer);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.type_node.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.type_node);
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "type_pointer"
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
    pub index_nodes : Vec<Arc<dyn IAstNode>>,
    pub object_type : Arc<dyn IAstNode>
}
implem_irange!(AstTypeArray);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        self.index_nodes.iter().for_each(|node|{
            result.push(node.as_ast_node());
        });
        result.push(self.object_type.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.index_nodes.iter());
        result.push(&self.object_type);
        return Some(result);
    }
    fn as_ast_node(&self) -> &dyn IAstNode{
        self
    }
    fn get_identifier(&self) -> &str {
        "type_array"
    }
    fn to_string_type(&self) -> String {
        "type_array".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeRange{
    pub raw_pos: usize,
    pub range: Range,
    pub from: Arc<dyn IAstNode>,
    pub to: Arc<dyn IAstNode>,
}
implem_irange!(AstTypeRange);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.from.as_ref());
        result.push(self.to.as_ref());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.from);
        result.push(&self.to);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "type_range"
    }
    fn to_string_type(&self) -> String {
        "type_range".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeProcedure {
    pub raw_pos: usize,
    pub range: Range,
    pub parameter_list: Option<Arc<dyn IAstNode>>,
}
implem_irange!(AstTypeProcedure);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.parameter_list.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "type_proc"
    }
    fn to_string_type(&self) -> String {
        "type_proc".to_string()
    }
}


#[derive(Debug)]
pub struct AstTypeFunction {
    pub raw_pos: usize,
    pub range: Range,
    pub parameter_list: Option<Arc<dyn IAstNode>>,
    pub return_type: Arc<dyn IAstNode>
}
implem_irange!(AstTypeFunction);
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        if self.parameter_list.is_some() {result.push(self.parameter_list.as_ref().unwrap().as_ast_node());}
        result.push(self.return_type.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        match self.parameter_list.as_ref(){
            Some(param_list)=> result.push(param_list),
            _=> ()
        }
        result.push(&self.return_type);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "type_func"
    }
    fn to_string_type(&self) -> String {
        "type_func".to_string()
    }
}

#[derive(Debug)]
pub struct AstTypeInstanceOf {
    pub raw_pos: usize,
    pub range: Range,
    pub instance_type: Arc<dyn IAstNode>,
}
implem_irange!(AstTypeInstanceOf);
impl IAstNode for AstTypeInstanceOf {
    fn get_type(&self) -> &'static str {
        return "Type InstanceOf";
    }
    fn get_identifier(&self) -> &str {
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
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.instance_type.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.instance_type);
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
    pub left_node: Arc<dyn IAstNode>,
    pub index_node: Arc<dyn IAstNode>
}
implem_irange!(AstArrayAccess);
impl IAstNode for AstArrayAccess {
    implem_iastnode_common!(AstArrayAccess, "array_access");
    fn get_identifier(&self) -> &str {
        return self.left_node.get_identifier()
    }
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.left_node.as_ast_node());
        result.push(self.index_node.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.left_node);
        result.push(&self.index_node);
        return Some(result);
    }
}

#[derive(Debug)]
pub struct AstRepeatBlock {
    pub raw_pos: usize,
    pub range: Range,
    pub cond_block: Arc<dyn IAstNode>,
    pub end_token: Option<Token>
}
implem_irange!(AstRepeatBlock);
impl IAstNode for AstRepeatBlock {
    implem_iastnode_common!(AstRepeatBlock, "repeat");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.cond_block.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.cond_block);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "repeat"
    }
}

#[derive(Debug)]
pub struct AstMethodNameWithEvent {
    pub raw_pos: usize,
    pub range: Range,
    pub method_name: Arc<dyn IAstNode>,
    pub event: Arc<dyn IAstNode>,
    pub id : Arc<str>
}
implem_irange!(AstMethodNameWithEvent);
impl IAstNode for AstMethodNameWithEvent {
    implem_iastnode_common!(AstMethodNameWithEvent, "method_name_w_event");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.method_name.as_ast_node());
        result.push(self.event.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.method_name);
        result.push(&self.event);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        &self.id
    }
}

#[derive(Debug)]
pub struct AstOQLSelect {
    pub raw_pos: usize,
    pub range: Range,
    pub limit_node: Option<Arc<dyn IAstNode>>,
    pub select_nodes: Vec<Arc<dyn IAstNode>>,
    pub from_nodes: Vec<Arc<dyn IAstNode>>,
    pub where_node: Option<Arc<dyn IAstNode>>,
    pub order_by_nodes: Option<Vec<Arc<dyn IAstNode>>>,
    pub using_node: Option<Arc<dyn IAstNode>>,
    pub is_distinct : bool,
}
implem_irange!(AstOQLSelect);
impl IAstNode for AstOQLSelect {
    implem_iastnode_common!(AstOQLSelect, "oql_select");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        match &self.limit_node{
            Some(n) => {result.push(n.as_ast_node());}
            _=> ()
        }
        result.extend(self.select_nodes.iter().map(|n| {n.as_ast_node()}));
        result.extend(self.from_nodes.iter().map(|n| {n.as_ast_node()}));
        match &self.where_node{
            Some(n) => {result.push(n.as_ast_node());}
            _=> ()
        }
        match &self.order_by_nodes{
            Some(nodes)=>result.extend(nodes.iter().map(|n| {n.as_ast_node()})),
            _=>()
        }
        match &self.using_node{
            Some(n)=>result.push(n.as_ast_node()),
            _=>()
        }
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        match &self.limit_node{
            Some(n) => {result.push(n);}
            _=> ()
        }
        result.extend(self.select_nodes.iter());
        result.extend(self.from_nodes.iter());
        match &self.where_node{
            Some(n) => {result.push(n);}
            _=> ()
        }
        match &self.order_by_nodes{
            Some(nodes)=>result.extend(nodes.iter()),
            _=>()
        }
        match &self.using_node{
            Some(n)=>result.push(n),
            _=>()
        }
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "oql_select"
    }
}

#[derive(Debug)]
pub struct AstOQLFromNode {
    pub raw_pos: usize,
    pub range: Range,
    pub alias_token : Token,
    pub source_node: Arc<dyn IAstNode>,
    pub join_nodes: Vec<Arc<dyn IAstNode>>,
    pub is_conditional : bool,
    pub is_all_versions: bool,
    pub is_phantoms_too:bool,
    pub includes_subclasses: bool
}
implem_irange!(AstOQLFromNode);
impl IAstNode for AstOQLFromNode {
    implem_iastnode_common!(AstOQLFromNode, "oql_from_node");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.source_node.as_ast_node());
        result.extend(self.join_nodes.iter().map(|n| {n.as_ast_node()}));
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.source_node);
        result.extend(self.join_nodes.iter());
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.alias_token.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstOQLJoin {
    pub raw_pos: usize,
    pub range: Range,
    pub join_token : Token,
    pub cond_node: Arc<dyn IAstNode>,
}
implem_irange!(AstOQLJoin);
impl IAstNode for AstOQLJoin {
    implem_iastnode_common!(AstOQLJoin, "oql_join_node");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.cond_node.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.cond_node);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        self.join_token.get_value_as_str()
    }
}

#[derive(Debug)]
pub struct AstOQLOrderBy {
    pub raw_pos: usize,
    pub range: Range,
    pub field_node: Arc<dyn IAstNode>,
    pub is_descending: bool
}
implem_irange!(AstOQLOrderBy);
impl IAstNode for AstOQLOrderBy {
    implem_iastnode_common!(AstOQLOrderBy, "oql_order_by_node");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.push(self.field_node.as_ast_node());
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.push(&self.field_node);
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "oql_order_by_node"
    }
}

#[derive(Debug)]
pub struct AstOQLFetch {
    pub raw_pos: usize,
    pub range: Range,
    pub into_field_nodes: Vec<Arc<dyn IAstNode>>,
    pub using_node: Option<Arc<dyn IAstNode>>,
}
implem_irange!(AstOQLFetch);
impl IAstNode for AstOQLFetch {
    implem_iastnode_common!(AstOQLFetch, "oql_fetch");
    fn get_children_ref(&self) -> Option<Vec<&dyn IAstNode>> {
        let mut result = Vec::new();
        result.extend(self.into_field_nodes.iter().map(|n| {n.as_ast_node()}));
        match &self.using_node{
            Some(n) => {result.push(n.as_ast_node());}
            _=> ()
        }
        return Some(result);
    }
    fn get_children_arc(&self) -> Option<Vec<&Arc<dyn IAstNode>>> {
        let mut result = Vec::new();
        result.extend(self.into_field_nodes.iter());
        match &self.using_node{
            Some(n) => {result.push(n);}
            _=> ()
        }
        return Some(result);
    }
    fn get_identifier(&self) -> &str {
        "oql_fetch"
    }
}