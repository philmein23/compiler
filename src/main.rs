mod ast;
mod lexer;
mod parser;
mod token;

use std::{env::args, fs};

use lexer::Lexer;
use parser::Parser;

fn main() {
    let mut args = args().skip(1);
    let path = args.next();

    if let Some(p) = path {
        let source = fs::read_to_string(p);

        match source {
            Ok(s) => {
                let mut lexer = Lexer::new(s);
                match lexer.collect_tokens() {
                    Ok(tokens) => {
                        let mut p = Parser { tokens };
                        match p.parse_program() {
                            Ok(stmts) => {
                                for stmt in stmts {
                                    println!("STMT: {}", stmt);
                                }
                            }
                            Err(e) => {
                                println!("Error: {:?}", e);
                            }
                        }
                    }
                    Err(e) => {
                        println!("{}", e);
                    }
                }
            }
            Err(e) => println!("Error: {:?}", e),
        }
    }
}
