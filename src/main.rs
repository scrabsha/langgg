use std::io;

use crate::lexer::Lexer;

mod lexer;

fn read_line() -> String {
    let mut buff = String::with_capacity(50);
    io::stdin().read_line(&mut buff).unwrap();
    dbg!();

    buff
}

fn main() {
    loop {
        let line = read_line();
        let tokens = Lexer::new(line.as_str()).tokens().unwrap();
        println!("{:?}", tokens);
    }
}
