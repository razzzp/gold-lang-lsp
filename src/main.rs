use std::{io::{Read}, fs::File};

use crate::lexer::Lexer;

pub mod lexer;
pub mod parser;
pub mod ast;

fn main() {
    let  mut f = File::open("./test_inputs/aTestClass.god").expect("file not found");
    let mut file_contents = String::new();
    match f.read_to_string(&mut file_contents){
        Ok(_)=>(),
        Err(msg) => panic!("{msg}")
    };
    println!("{file_contents}");
    let lexer = Lexer::new();
    let tokens = lexer.lex(&file_contents).unwrap();
    println!("{:#?}", tokens);
}

#[cfg(test)]
mod test {
    use crate::ast::AstClass;
    use crate::ast::AstNode;
    use crate::ast::AstUses;
    use crate::lexer;
    use crate::lexer::Lexer;
    use crate::parser;

    #[test]
    fn test_class_with_uses(){
        let lexer = Lexer::new();
        let input = String::from
        ("
        class aTestClass   (aParentClass)\n\n
        uses aFirstClass, aSecondClass\n
        ");
        let tokens = lexer.lex(&input).unwrap();
        let ast = parser::parse_gold(&tokens);

        // assert input left is empty
        assert_eq!(ast.0.0.len(), 0);
        // assert no errors
        assert_eq!(ast.1.len(), 0);
        let nodes = ast.0.1;
        // assert first node is class
        let class = &nodes[0].as_any().downcast_ref::<AstClass>().unwrap();
        assert_eq!(class.name.as_str(), "aTestClass");
        assert_eq!(class.parent_class.as_str(), "aParentClass");
        assert_eq!(class.pos, 15);

        // assert second node is uses
        let uses = &nodes[1].as_any().downcast_ref::<AstUses>().unwrap();
        assert_eq!(uses.pos, 53);
        // assert uses list
        let uses_ident = &uses.list_of_uses[0];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aFirstClass");
        let uses_ident = &uses.list_of_uses[1];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aSecondClass");
    }
}
