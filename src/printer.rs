use std::io::Write;

use crate::{expression::Expression, parser::VariableType, statement::Statement, visitor::Visitor};

pub struct Printer {
    depth: usize,
    buffer: Vec<u8>,
}

impl Write for Printer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        unsafe {
            print!("{}", std::str::from_utf8_unchecked(&self.buffer));
        }

        self.buffer.clear();
        Ok(())
    }
}

impl Printer {
    pub fn new() -> Self {
        Self {
            depth: 0,
            buffer: Vec::new(),
        }
    }

    fn fill_start(&mut self) -> std::io::Result<()> {
        if self.depth != 0 {
            write!(self, "{:>fill$}", " ", fill = self.depth * 4)?
        }

        Ok(())
    }
}

impl Visitor<std::io::Result<()>> for Printer {
    fn visit_expression(&mut self, expression: &Expression) -> std::io::Result<()> {
        match expression {
            Expression::Assignment { target, value } => {
                self.visit_expression(target)?;
                write!(self, " = ")?;
                self.visit_expression(&value)
            }
            Expression::Variable { name } => write!(self, "{}", name.lexeme),
            Expression::Literal { value, .. } => {
                write!(self, "{}", value.lexeme)
            }
            Expression::Grouping { expression } => self.visit_expression(expression),
            Expression::Unary { operator, value } => {
                write!(self, "{}", operator.lexeme)?;
                self.visit_expression(value)
            }
            Expression::Exponent { base, power } => {
                self.visit_expression(base)?;
                write!(self, " ^ ")?;
                self.visit_expression(power)
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                self.visit_expression(left)?;
                write!(self, " {} ", operator.lexeme)?;
                self.visit_expression(right)
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                self.visit_expression(left)?;
                write!(self, " {} ", operator.lexeme)?;
                self.visit_expression(right)
            }
            Expression::Call { target, args } => {
                write!(self, "{}(", target.lexeme)?;

                let mut should_write = false;

                for arg in args {
                    if should_write {
                        write!(self, ", ")?;
                    }
                    self.visit_expression(arg)?;
                    should_write = true;
                }

                write!(self, ")")
            }
            Expression::Subscript { target, key } => {
                write!(self, "{}[", target.lexeme)?;
                self.visit_expression(key)?;
                write!(self, "]")
            }
        }
    }

    fn visit_statement(&mut self, statement: &Statement) -> std::io::Result<()> {
        match statement {
            Statement::Block { statements } => {
                write!(self, "{{")?;
                self.depth += 1;
                for stmt in statements {
                    self.visit_statement(stmt)?
                }
                self.depth -= 1;
                self.fill_start()?;
                write!(self, "}}\n")?;
                self.flush()
            }
            Statement::FunctionDeclaration {
                name,
                return_kind,
                params,
                body,
            } => {
                self.fill_start()?;
                write!(self, "{}: {} = (", name.lexeme, return_kind.lexeme)?;

                for param in params {
                    self.visit_param(param)?
                }

                write!(self, ") = ")?;

                self.visit_statement(body)
            }
            Statement::Expression { expression } => {
                self.fill_start()?;
                self.visit_expression(expression)?;
                write!(self, ";\n")?;
                self.flush()
            }
            Statement::If {
                condition,
                then,
                otherwise,
            } => {
                self.fill_start()?;
                write!(self, "if (")?;
                self.visit_expression(condition)?;
                write!(self, ") ")?;
                self.visit_statement(then)?;
                if let Some(stmt) = otherwise {
                    write!(self, " else ")?;
                    self.visit_statement(stmt)?
                }
                Ok(())
            }
            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.fill_start()?;
                write!(self, "for (")?;
                if let Some(statement) = initializer {
                    self.visit_statement(&statement)?;
                } else {
                    write!(self, ";")?;
                }

                if let Some(expression) = condition {
                    write!(self, " ")?;
                    self.visit_expression(&expression)?;
                }

                write!(self, ";")?;

                if let Some(expression) = increment {
                    write!(self, " ")?;
                    self.visit_expression(&expression)?;
                }

                write!(self, ") ")?;

                self.visit_statement(body)
            }
            Statement::Print { expressions } => {
                self.fill_start()?;
                write!(self, "print ")?;

                let mut should_write = false;
                for expr in expressions {
                    if should_write {
                        write!(self, ", ")?;
                    }
                    self.visit_expression(expr)?;
                    should_write = true;
                }

                write!(self, ";\n")?;
                self.flush()
            }
            Statement::PrototypeDeclaration {
                name,
                return_kind,
                params,
            } => {
                self.fill_start()?;
                write!(self, "{}: {} = (", name.lexeme, return_kind.lexeme)?;

                for param in params {
                    self.visit_param(param)?
                }

                write!(self, ");")
            }
            Statement::Return { value } => {
                self.fill_start()?;
                write!(self, "return")?;
                if let Some(expression) = value {
                    write!(self, " ")?;
                    self.visit_expression(expression)?
                }
                write!(self, ";\n")?;
                self.flush()
            }
            Statement::VariableDeclaration {
                name,
                variable_type,
            } => {
                self.fill_start()?;
                write!(self, "{}: ", name.lexeme)?;
                match variable_type {
                    VariableType::Atomic { kind, initializer } => {
                        write!(self, "{}", kind)?;
                        if let Some(expression) = initializer {
                            write!(self, " = ")?;
                            self.visit_expression(expression)?;
                        }
                    }
                    VariableType::Array {
                        kind,
                        size,
                        initializer,
                    } => {
                        write!(self, "array [")?;
                        self.visit_expression(size)?;
                        write!(self, "] {}", kind)?;
                        if let Some(expressions) = initializer {
                            write!(self, " = {{")?;
                            let mut should_write = false;
                            for expr in expressions {
                                if should_write {
                                    write!(self, ", ")?;
                                }
                                self.visit_expression(expr)?;
                                should_write = true;
                            }
                            write!(self, "}}")?;
                        }
                    }
                    VariableType::Map {
                        key_kind,
                        value_kind,
                        initializer,
                    } => {
                        write!(self, "map {} {}", key_kind, value_kind)?;
                        if let Some(expressions) = initializer {
                            write!(self, " = {{")?;
                            let mut should_write = false;
                            for (key, value) in expressions {
                                if should_write {
                                    write!(self, ", ")?;
                                }
                                self.visit_expression(key)?;
                                write!(self, ": ")?;
                                self.visit_expression(value)?;
                                should_write = true;
                            }
                            write!(self, "}}")?;
                        }
                    }
                };
                write!(self, ";\n")?;
                self.flush()
            }
        }
    }

    fn visit_param(&mut self, _param: &crate::parser::Param) -> std::io::Result<()> {
        todo!()
    }
}
