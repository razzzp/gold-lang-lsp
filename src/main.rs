use std::{io::{Read}, fs::File};

pub mod lexer;
pub mod parser;

fn main() {
    let  mut f = File::open("./test_inputs/aTestClass.god").expect("file not found");
    let mut file_contents = String::new();
    match f.read_to_string(&mut file_contents){
        Ok(_)=>(),
        Err(msg) => panic!("{msg}")
    };
    println!("{file_contents}");
    let tokens = lexer::lex(&file_contents).unwrap();
    println!("{:#?}", tokens);
}

mod test {
    use crate::{lexer, parser::{self, AstNode}};

    #[test]
    fn test_class_with_uses(){
        let input = String::from
        ("
        class aTestClass   (aParentClass)\n\n
        uses aFirstClass, aSecondClass\n
        ");
        let tokens = lexer::lex(&input).unwrap();
        let ast = parser::parse_tokens(&tokens);

        assert_eq!(ast.0.0.len(), 0);
        assert_eq!(ast.1.len(), 0);
        let nodes = ast.0.1;
        //
        let class = match &nodes[0] {
            AstNode::Class(n) => n,
            _ => panic!()
        };
        assert_eq!(class.name.as_str(), "aTestClass");
        assert_eq!(class.parent_class.as_str(), "aParentClass");
        // assert_eq!(class.pos, )

        let uses = match &nodes[1] {
            AstNode::Uses(n) => n,
            _ => panic!()
        };
        let uses_ident = match &uses.list_of_uses[0]{
            AstNode::Terminal(n) => n,
            _ => panic!()
        };
        assert_eq!(uses_ident.token.value.as_ref().unwrap().as_str(), "aFirstClass");
        let uses_ident = match &uses.list_of_uses[1]{
            AstNode::Terminal(n) => n,
            _ => panic!()
        };
        assert_eq!(uses_ident.token.value.as_ref().unwrap().as_str(), "aSecondClass");

        // assert_eq!(class.pos, )
    }
}
