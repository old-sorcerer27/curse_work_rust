#[cfg(test)]
mod tests;

use std::{cell::RefCell, rc::Rc};

use crate::{
    parser::prelude::{Expression, Operator, Primitive, Statement}, 
    environment::prelude::{Environment, Value, ValueType, FALSE, TRUE},
    lexer::prelude::Token
};

pub fn eval(module: crate::parser::prelude::Module, env: Rc<RefCell<Environment>>) {
    for statement in module.program.statements {
        eval_statement(statement, env.clone());
    }
}

fn eval_statement(statement: crate::parser::prelude::Statement, env: Rc<RefCell<Environment>>) {
    match statement {
        Statement::Declaration(declaration) => {
            for identifiers in declaration.identifiers {
                for name in identifiers.names {
                    let mut env = env.borrow_mut();

                    match env.get(&name.value) {
                        None => {
                            env.declare(
                                name.value.clone(), 
                                identifiers.names_type.to_owned().into()
                            );
                        }
                        Some(_) => panic!("multiple declarations")
                    }
                }
            }
        },
        Statement::Operator(operator) => eval_operator(operator, env)
    }
}

fn eval_operator(operator: crate::parser::prelude::Operator, env: Rc<RefCell<Environment>>) {
    match operator {
        Operator::Assignment(assignment) => eval_assignment(assignment, env.clone()),
        Operator::Nested(nested) => {
            nested.operators.into_iter().for_each(|operator|
                eval_operator(operator, env.clone())
            )
        },
        Operator::Conditional(conditional) => {
            let condition = eval_expression(conditional.condition, env.clone());

            match condition {
                TRUE => eval_operator(*conditional.resolution, env.clone()),
                FALSE => match conditional.alternative {
                    Some(alternative) => eval_operator(*alternative, env.clone()),
                    None => {}
                },
                _ => panic!("invalid condition type")
            };
        },
        Operator::ConditionalLoop(loop_) => {
            while let TRUE = eval_expression(loop_.condition.clone(), env.clone()) {
                eval_operator(*loop_.block.clone(), env.clone());
            }
        },
        Operator::FixedLoop(loop_) => {
            eval_assignment(loop_.assignment.clone(), env.clone());

            let ident = loop_.assignment.identifier.value;

            // check if ident._type() is int
            let ident_value = env.borrow().get(&ident)
                .cloned()
                .unwrap();

            let ident_value = match ident_value {
                Value::Integer { value } => value,
                _ => panic!("invalid type")
            };

            // check if to._type() is int
            let to_value = match eval_expression(loop_.to, env.clone()) {
                Value::Integer { value } => value,
                _ => panic!("invalid type"),
            };

            // check if it has step and step._type() is int
            let step = match loop_.step {
                Some(step) => {
                    match eval_expression(step, env.clone()) {
                        Value::Integer { value } => value,
                        _ => panic!("invalid type")
                    }
                },
                None => 1
            };

            // repeat block evaluation and increment ident value by step until value >= to
            for i in (ident_value..=to_value).step_by(step as usize) {
                eval_operator(*loop_.block.clone(), env.clone());

                env.borrow_mut().set(ident.clone(), Value::Integer { value: i + step })
            }
        },
        Operator::Input(input) => {
            let stdin = std::io::stdin();

            input.identifiers.iter().for_each(|ident| {
                let ident_type = env.borrow_mut().get(&ident.value)
                    .expect("undeclared variable")
                    ._type();

                let mut buf = String::from("");

                stdin.read_line(&mut buf).unwrap();

                let escaped = buf.split_whitespace()
                    .nth(0)
                    .unwrap_or_default()
                    .to_string();

                let value = parse_input(escaped, ident_type);

                env.borrow_mut().set(ident.value.clone(), value);
            });
        },
        Operator::Output(output) => {
            let values = output.expressions.into_iter()
                .map(|expression| eval_expression(expression, env.clone()))
                .collect::<Vec<Value>>();

            let string = values.iter()
                .map(|value| format!("{}", value))
                .collect::<Vec<String>>()
                .join("");

            println!("{string}");
        }
    }
}

fn parse_input(input: String, value_type: ValueType) -> Value {
    match value_type {
        ValueType::Integer => Value::Integer { value: input.parse::<i64>().unwrap() },
        ValueType::Float => Value::Float { value: input.parse::<f64>().unwrap() },
        ValueType::String => Value::String { value: input.to_owned() },
        ValueType::Boolean => match input.as_str() {
            "true" => Value::Boolean { value: true },
            "false" => Value::Boolean { value: false },
            _ => panic!("unknown value"),
        }
    }
}

fn eval_assignment(assignment: crate::parser::prelude::Assignment, env: Rc<RefCell<Environment>>) {
    let identifier = assignment.identifier.clone();

    let identifier_type = env.borrow_mut().get(&identifier.value)
        .expect("undeclared variable")
        ._type();

    let value = eval_expression(assignment.value, env.clone());

    if identifier_type != value._type() {
        panic!("type mismatch");
    }
    
    env.borrow_mut().set(identifier.value, value);
}

fn eval_expression(
    expression: crate::parser::prelude::Expression, 
    env: Rc<RefCell<Environment>>
) -> Value {
    match expression {
        Expression::Identifier(ident) => env.borrow()
            .get(&ident.value).cloned().unwrap(),
        Expression::Primitive(primitive) => match primitive {
            Primitive::Int { value, .. } => Value::Integer { value },
            Primitive::Float { value, .. } => Value::Float { value },
            Primitive::String { value, .. } => Value::String { value },
            Primitive::Bool { value, .. } => Value::Boolean { value }
        },
        Expression::Infix(infix) => eval_infix(infix, env.clone()),
        Expression::Prefix(prefix) => eval_prefix(prefix, env.clone()),
        Expression::Nested { expression, .. } => eval_expression(*expression, env.clone())
    }
}

fn eval_prefix(
    prefix: crate::parser::prelude::Prefix, 
    env: Rc<RefCell<Environment>>
) -> Value {
    match prefix.operator {
        Token::Bang => {
            let value = eval_expression(*prefix.expression, env.clone());

            match value {
                FALSE => TRUE,
                TRUE => FALSE,
                _ => panic!("cannot apply unary operator `{}` to `{:?}`", prefix.operator.as_literal(), value._type())
            }
        },
        _ => panic!("unknown unary operator `{}`", prefix.operator.as_literal())
    }
}

fn eval_infix(
    infix: crate::parser::prelude::Infix, 
    env: Rc<RefCell<Environment>>
) -> Value {
    let left = eval_expression(*infix.left, env.clone());
    let right = eval_expression(*infix.right, env.clone());

    match (left, right) {
        (
            Value::Integer { value: left_value }, 
            Value::Integer { value: right_value }
        ) => {
            match infix.operator {
                Token::Plus => Value::Integer { value: left_value + right_value },
                Token::Minus => Value::Integer { value: left_value - right_value },
                Token::Asterisk => Value::Integer { value: left_value * right_value },
                Token::Slash => Value::Float { value: left_value as f64 / right_value as f64 },
                Token::LessThan => Value::Boolean { value: left_value < right_value },
                Token::LessThanOrEqual => Value::Boolean { value: left_value <= right_value },
                Token::GreaterThan => Value::Boolean { value: left_value > right_value },
                Token::GreaterThanOrEqual => Value::Boolean { value: left_value >= right_value },
                Token::Equal => Value::Boolean { value: left_value == right_value },
                Token::NotEqual => Value::Boolean { value: left_value != right_value },
                _ => panic!("invalid operator for int int infix")
            }
        },
        (
            Value::Float { value: left_value }, 
            Value::Float { value: right_value }
        ) => {
            match infix.operator {
                Token::Plus => Value::Float { value: left_value + right_value },
                Token::Minus => Value::Float { value: left_value - right_value },
                Token::Asterisk => Value::Float { value: left_value * right_value },
                Token::Slash => Value::Float { value: left_value / right_value },
                Token::LessThan => Value::Boolean { value: left_value < right_value },
                Token::GreaterThan => Value::Boolean { value: left_value > right_value },
                _ => panic!("invalid operator for float float infix")
            }
        },
        (
            Value::String { value: left },
            Value::String { value: right }
        ) => {
            match infix.operator {
                Token::Plus => Value::String { value: format!("{}{}", left, right)},
                Token::Equal => Value::Boolean { value: left == right },
                Token::NotEqual => Value::Boolean { value: left != right },
                _ => panic!("invalid operator for string string infix")
            }
        }
        (
            Value::Boolean { value: left_value }, 
            Value::Boolean { value: right_value }
        ) => {
            match infix.operator {
                Token::Equal => Value::Boolean { value: left_value == right_value },
                Token::NotEqual => Value::Boolean { value: left_value != right_value },
                Token::And => Value::Boolean { value: left_value && right_value },
                Token::Or => Value::Boolean { value: left_value || right_value },
                _ => panic!("invalid operator for boolean boolean infix")
            }
        }
        (left, right) => {
            match infix.operator {
                Token::Plus => {
                    panic!("cannot add `{:?}` to `{:?}`", left._type(), right._type())
                },
                Token::Minus => {
                    panic!("cannot subtract `{:?}` from `{:?}`", left._type(), right._type())
                },
                Token::Asterisk => {
                    panic!("cannot multiply `{:?}` by `{:?}`", left._type(), right._type())
                },
                Token::Slash => {
                    panic!("cannot divide `{:?}` by `{:?}`", left._type(), right._type())
                },
                Token::LessThan
                | Token::GreaterThan
                | Token::Equal
                | Token::NotEqual
                | Token::LessThanOrEqual
                | Token::GreaterThanOrEqual => {
                    panic!("mismatched types, expected `{:?}`, found `{:?}`", left._type(), right._type())
                },
                _ => panic!("expected operator, got `{}`", infix.operator.as_literal())
            }
        }
    }
}