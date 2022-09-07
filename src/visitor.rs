use crate::{expression::Expression, parser::Param, statement::Statement};

pub trait Visitor<T = ()> {
    fn visit_expression(&mut self, expression: &Expression) -> T;
    fn visit_statement(&mut self, statement: &Statement) -> T;
    fn visit_param(&mut self, param: &Param) -> T;
}
