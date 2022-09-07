use printer::Printer;
use visitor::Visitor;

use crate::parser::Parser;

mod expression;
mod lexer;
mod parser;
mod printer;
mod statement;
mod visitor;

fn main() {
    let statements = Parser::new(
        "main: function integer () = { if (true) {fn();}
for (x:integer=1;x!= 2; x=x+1) {print x;} return 0;
}",
    )
    .into_iter()
    .flat_map(|v| v)
    .collect::<Vec<_>>();

    let mut printer = Printer::new();

    for stmt in &statements {
        printer.visit_statement(stmt).unwrap()
    }
}
