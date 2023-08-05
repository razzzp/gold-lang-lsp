use crate::ast::IAstNode;
use std::fmt::Write;


pub trait IRange {
    fn get_range(&self) -> Range;
    fn set_range(&mut self, new_range: Range){//TODO:implem for all classes
        todo!()
    }
    fn as_range(&self) -> &dyn IRange;
}

#[derive(Debug,Clone,PartialEq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position
}
impl Range{
    pub fn as_lsp_type_range(&self) -> lsp_types::Range{
        lsp_types::Range {start: self.start.as_lsp_type_pos(), end: self.end.as_lsp_type_pos()}
    }
}

#[derive(Debug,Clone,PartialEq, Default)]
pub struct Position {
    pub line: usize,
    pub character: usize
}
impl Position{
    pub fn as_lsp_type_pos(&self) -> lsp_types::Position{
        lsp_types::Position { line: self.line as u32, character: self.character as u32 }
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct DynamicChild<'a, T: ?Sized + 'a>{
    pub data: &'a T,
    pub parent: &'a T,
}
impl<'a,T: ?Sized> DynamicChild<'a, T>{
    pub fn new(data: &'a T, parent: &'a T) -> DynamicChild<'a,T> {
        DynamicChild { data: data, parent: parent}
    }
}

pub fn get_start_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().start.clone();
}

pub fn get_end_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().end.clone();
}

pub fn create_new_range<T: IRange + ?Sized>(first_item: &T, second_item: &T) -> Range{
    return Range{
        start: first_item.get_range().start.clone(),
        end: second_item.get_range().end.clone()
    }
}

pub fn print_ast_brief(ast_node: &dyn IAstNode) -> String{
    return format!("[{}:{}]", ast_node.get_type(), ast_node.get_identifier())
}

pub fn print_ast_brief_recursive(ast_node: &dyn IAstNode) -> String{
    let mut result = String::new();
    _print_ast_brief(&mut result, ast_node, 0);
    return result;
}

fn _print_ast_brief(result: &mut String, ast_node: &dyn IAstNode, indent_level: usize){
    for x in (0..indent_level){
        write!(result, "  ");
    }
    writeln!(result, "[{}:{}]", ast_node.get_type(), ast_node.get_identifier());
    let children = ast_node.get_children();
    match children {
        Some(children) =>{
            for child in children {
                _print_ast_brief(result, child, indent_level+1)
            }
        },
        None => return
    }
}


pub fn inorder(ast_node: &dyn IAstNode) -> Vec<&dyn IAstNode>  {
    let mut result = Vec::new();
    _inorder(ast_node, &mut result);
    return result;
}

fn _inorder<'a>(ast_node: &'a dyn IAstNode, result: &mut Vec<&'a dyn IAstNode>){
    let children = match ast_node.get_children() {
        Some(c) => c,
        None => {result.push(ast_node); return}
    };
    _inorder(children.first().unwrap().as_ast_node(), result);
    result.push(ast_node);
    _inorder(children.last().unwrap().as_ast_node(), result);
}


pub fn dfs(ast_node: &dyn IAstNode) -> Vec<&dyn IAstNode> {
    let mut result = Vec::new();
    _dfs(ast_node, &mut result);
    return result;
}

fn _dfs<'a>(ast_node: &'a dyn IAstNode, result: &mut Vec<&'a dyn IAstNode>){
    let children = match ast_node.get_children() {
        Some(c) => c,
        None => {result.push(ast_node); return}
    };
    children.iter().for_each(|n|{_dfs(n.as_ast_node(), result)});
    result.push(ast_node);
}

#[cfg(test)]
pub mod test_utils{
    use crate::ast::IAstNode;

    pub fn cast_and_unwrap<'a, T: 'static>(node: &'a Box<dyn IAstNode>) -> &'a T{
        return node.as_ref().as_any().downcast_ref::<T>().unwrap();
    }
}