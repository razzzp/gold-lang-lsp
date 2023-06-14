use std::{io::{Read}, fs::File};

mod lexer;
mod paser;

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
