use crate::{parser::ast::IAstNode, lexer::tokens::Token};
use std::{collections::LinkedList, ops::Deref, sync::Arc, fmt::Write};

#[macro_export]
macro_rules! unwrap_or_return {
    ( $e:expr ) => {
        match $e {
            Some(x) => x,
            _=> return,
        }
    }
}

pub trait ILogger :std::fmt::Debug{
    fn log(&mut self, msg: &str);
    fn as_logger_mut(&mut self)->&mut dyn ILogger;
}

pub trait ILoggerV2 : std::fmt::Debug + Send + Sync{
    fn log_error(&self, msg: &str);
    fn log_warning(&self, msg: &str);
    fn log_info(&self, msg: &str);
}

#[derive(Debug)]
pub struct StdErrLogger{
    prefix: String,
    stream: Arc<std::io::Stderr>
}
impl StdErrLogger{
    pub fn new(prefix: &str)-> StdErrLogger{
        return StdErrLogger{
            prefix :prefix.to_string(),
            stream: Arc::new(std::io::stderr())
        }
    }
}
impl ILoggerV2 for StdErrLogger{
    fn log_error(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Error]{}\n",self.prefix,msg));
    }

    fn log_warning(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Warning]{}\n",self.prefix,msg));
    }

    fn log_info(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Info]{}\n",self.prefix,msg));
    }
}

#[derive(Debug)]
pub struct StdOutLogger{
    prefix: String,
    stream: Arc<std::io::Stdout>
}
impl StdOutLogger{
    pub fn new(prefix: &str)-> StdOutLogger{
        return StdOutLogger{
            prefix :prefix.to_string(),
            stream: Arc::new(std::io::stdout())
        }
    }
}
impl ILoggerV2 for StdOutLogger{
    fn log_error(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Error]{}\n",self.prefix,msg));
    }

    fn log_warning(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Warning]{}\n",self.prefix,msg));
    }

    fn log_info(&self, msg: &str) {
        let _ =std::io::Write::write_fmt(&mut self.stream.lock(), format_args!("{}[Info]{}\n",self.prefix,msg));
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleLogger {
    prefix: String
}
impl ConsoleLogger{
    pub fn new(prefix: &str) -> ConsoleLogger{
        ConsoleLogger { prefix: prefix.to_string() }
    }
}
impl ILogger for ConsoleLogger{
    fn log(&mut self, msg: &str) {
        eprintln!("{} {}", self.prefix, msg)
    }

    fn as_logger_mut(&mut self)->&mut dyn ILogger {
        self
    }
}

pub trait IDiagnosticCollector<T : std::fmt::Debug+ Send> : std::fmt::Debug + Send{
    fn add_diagnostic(&mut self, diagnostic: T);
    fn take_diagnostics(self) -> Vec<T>;  
}

#[derive(Debug)]
pub struct GenericDiagnosticCollector<T> {
    diagnostics: Vec<T>
}
impl<T : std::fmt::Debug> GenericDiagnosticCollector<T>{
    pub fn new() -> GenericDiagnosticCollector<T>{
        return GenericDiagnosticCollector { diagnostics:Vec::new() }
    }
}
impl<T : std::fmt::Debug + Send> IDiagnosticCollector<T> for GenericDiagnosticCollector<T>{
    fn add_diagnostic(&mut self, diagnostic: T) {
        self.diagnostics.push(diagnostic)
    }

    fn take_diagnostics(self) -> Vec<T> {
        self.diagnostics
    }
}


pub trait IRange {
    fn get_range(&self) -> Range;
    fn set_range(&mut self, _new_range: Range){//TODO:implem for all classes
        ()
    }
    fn as_range(&self) -> &dyn IRange;
}

pub trait OptionExt {
    type Value;
    fn unwrap_ref(&self) -> &Self::Value;
    fn unwrap_mut(&mut self) -> &mut Self::Value;
}

impl <T> OptionExt for Option<T> {
    type Value = T;
    fn unwrap_ref(&self) -> &T { self.as_ref().unwrap() }
    fn unwrap_mut(&mut self) -> &mut T { self.as_mut().unwrap() }
}
pub trait OptionString {
    fn unwrap_clone_or_empty_string(&self) -> String;
    fn unwrap_clone(&self) -> Option<String>;
}
impl OptionString for Option<String>{
    fn unwrap_clone_or_empty_string(&self) -> String {
        match self.as_ref(){
            Some(s) => s.clone(),
            _=> "".to_string()
        }
    }

    fn unwrap_clone(&self) -> Option<String> {
        match self.as_ref(){
            Some(s) => Some(s.clone()),
            _=> None
        }
    }
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
    pub fn contains_pos(&self, pos: &Position)-> bool{
        if *pos >= self.start && *pos <= self.end {return true} 
        return false
    }
}

#[derive(Debug,Clone,PartialEq,Eq, Default)]
pub struct Position {
    pub line: usize,
    pub character: usize
}
impl Position{
    pub fn new(line : usize, character: usize) -> Position{
        return Position { line, character}
    }
    pub fn as_lsp_type_pos(&self) -> lsp_types::Position{
        lsp_types::Position { line: self.line as u32, character: self.character as u32 }
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
    pub fn to_string_brief(&self) -> String {
        format!("(l:{},c:{})", self.line, self.character)
    }
    pub fn offset_char(&self, offset : usize) -> Position{
        let mut new = self.clone();
        new.character += offset;
        new
    }
}
impl PartialOrd for Position{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.line.partial_cmp(&other.line) {
            Some(core::cmp::Ordering::Equal) => {
                self.character.partial_cmp(&other.character)
            }
            ord => return ord,
        }
    }
}
impl From<lsp_types::Position> for Position{
    fn from(value: lsp_types::Position) -> Self {
        return Position { line: value.line as usize, character: value.character as usize }
    }
}

#[derive(Debug, Clone)]
pub struct DynamicChild<'a, T: ?Sized + 'a>{
    pub data: &'a T,
    pub parent: Option<&'a T>,
}
impl<'a,T: ?Sized> DynamicChild<'a, T>{
    pub fn new(data: &'a T, parent: Option<&'a T>) -> DynamicChild<'a,T> {
        DynamicChild { data: data, parent: parent}
    }
}
impl<'a, T> Deref for DynamicChild<'a, T> {
    type Target = T;
    
    fn deref(& self) -> &Self::Target {
        self.data
    }
}


pub fn get_start_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().start.clone();
}

pub fn get_end_pos(item: &(dyn IRange)) -> Position {
    return item.get_range().end.clone();
}

pub fn create_new_range_from_irange<T: IRange + ?Sized>(first_item: &T, second_item: &T) -> Range{
    return Range{
        start: first_item.get_range().start.clone(),
        end: second_item.get_range().end.clone()
    }
}

pub fn create_new_range(first_item: Range, second_item: Range) -> Range{
    return Range{
        start: first_item.start.clone(),
        end: second_item.end.clone()
    }
}


pub fn create_new_range_from_token_slices(first_slice: &[Token], second_slice: &[Token]) -> Range{
    let first_next = first_slice.iter().next();
    let second_next = second_slice.iter().next();
    if first_next.is_none(){
        return Range::default()
    } else {
        let first_next = first_next.unwrap();
        let second_next = second_next.unwrap_or(first_next);
        return create_new_range(first_next.get_range(), second_next.get_range());
    }
}

// pub fn create_new_range<T: Range>(first_item: &T, second_item: &T) -> Range{
//     return Range{
//         start: first_item.get_range().start.clone(),
//         end: second_item.get_range().end.clone()
//     }
// }

pub fn ast_to_string_brief(ast_node: &dyn IAstNode) -> String{
    return format!("[{}:{}]", ast_node.get_type(), ast_node.get_identifier())
}

pub fn ast_to_string_brief_recursive(ast_node: &dyn IAstNode) -> String{
    let mut result = String::new();
    _write_ast_brief(&mut result, ast_node, 0);
    return result;
}

fn _write_ast_brief(result: &mut String, ast_node: &dyn IAstNode, indent_level: usize){
    for _ in 0..indent_level{
        result.push(' ');
        write!(result, "  ").unwrap();
    }
    result.push_str(format!("[{}:{}]", ast_node.get_type(), ast_node.get_identifier()).as_str());
    let children = ast_node.get_children_ref();
    match children {
        Some(children) =>{
            for child in children {
                _write_ast_brief(result, child, indent_level+1)
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
    let children = match ast_node.get_children_ref() {
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
    let children = match ast_node.get_children_ref() {
        Some(c) => c,
        None => {result.push(ast_node); return}
    };
    children.iter().for_each(|n|{_dfs(n.as_ast_node(), result)});
    result.push(ast_node);
}

pub fn bfs(ast_node: &dyn IAstNode) -> Vec<DynamicChild<dyn IAstNode>> {
    let mut result = Vec::new();
    let mut queue = LinkedList::new();
    queue.push_back(DynamicChild::new(ast_node, None));
    while !queue.is_empty(){
        let cur = queue.pop_front().unwrap();
        match cur.data.get_children_ref_dynamic(){
            Some(children) => {
                queue.extend(children.into_iter());
            },
            None => ()
        }
        result.push(cur);
    }
    return result;
}


#[cfg(test)]
pub mod test_utils{
    use std::sync::Arc;

    use crate::parser::ast::IAstNode;

    pub fn cast_and_unwrap<'a, T: 'static>(node: &'a Arc<dyn IAstNode>) -> &'a T{
        return node.as_ref().as_any().downcast_ref::<T>().unwrap();
    }
}