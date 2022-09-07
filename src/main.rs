use crate::parser::Parser;
use printer::Printer;
use visitor::Visitor;

mod expression;
mod lexer;
mod parser;
mod printer;
mod statement;
mod visitor;

fn main() {
    let statements = Parser::new("m[\"hello\"] = 1;")
        .into_iter()
        .flat_map(|v| v)
        .collect::<Vec<_>>();

    let mut printer = Printer::new();

    for stmt in &statements {
        printer.visit_statement(stmt).unwrap()
    }
}
