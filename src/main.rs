use crate::parser::Parser;

mod lexer;
mod parser;

fn main() {
    println!(
        "{:#?}",
        Parser::new("success = true")
            .into_iter()
            .collect::<Vec<_>>()
    )
}
