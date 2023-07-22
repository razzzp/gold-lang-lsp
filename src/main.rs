use std::{io::Read, fs::File};

use crate::{lexer::GoldLexer, parser::parse_gold};

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod utils;

fn main() {
    let  mut f = File::open("./test_inputs/aTestClass.god").expect("file not found");
    let mut file_contents = String::new();
    match f.read_to_string(&mut file_contents){
        Ok(_)=>(),
        Err(msg) => panic!("{msg}")
    };
    println!("{file_contents}");
    let mut lexer = GoldLexer::new();
    let tokens = lexer.lex(&file_contents).0;
    println!("{:#?}", tokens);
    let ast = parse_gold(&tokens);
    println!("{:#?}", ast.0.0);
    println!("{:#?}", ast.0.1);
    println!("{:#?}", ast.1.len());
}

#[cfg(test)]
mod test {
    use crate::ast::AstClass;
    use crate::ast::AstUses;
    use crate::lexer::GoldLexer;
    use crate::parser;

    #[test]
    fn test_class_everything(){
        let mut lexer = GoldLexer::new();
        let input = String::from
        ("class aTestClass   (aParentClass)\r\n\n
uses aFirstClass, aSecondClass\n
const cStringConstant = 'test constant' multiLang\n
const cNumericConstant = 18.5
memory GlobalVariable : refTo [P,A] SomeType
SecondGlobalVariable : listOf [T] AnotherType
IntGlobalVariable : int4
CStringVar : cstring

procedure FirstProcedure(FirstParam: FirstParamType, SecondParam: SecondParamType) private override 
    external 'DLL.Method' forward

proc SecondProc(inout FirstParam)
    ; method body
endproc
        ");
        let tokens = lexer.lex(&input).0;
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
        assert_eq!(class.raw_pos, 6);

        // assert second node is uses
        let uses = &nodes[1].as_any().downcast_ref::<AstUses>().unwrap();
        assert_eq!(uses.raw_pos, 37);
        // assert uses list
        let uses_ident = &uses.list_of_uses[0];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aFirstClass");
        let uses_ident = &uses.list_of_uses[1];
        assert_eq!(uses_ident.value.as_ref().unwrap().as_str(), "aSecondClass");

        println!("{:#?}", nodes);
    }
}
