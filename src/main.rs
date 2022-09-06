use crate::parser::Parser;

mod expression;
mod lexer;
mod parser;
mod statement;

fn main() {
    println!(
        "{:#?}",
        Parser::new("main: function integer () = {}")
            .into_iter()
            .flat_map(|v| v)
            .collect::<Vec<_>>()
    )
}
