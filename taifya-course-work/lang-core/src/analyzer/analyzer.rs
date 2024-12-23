
use crate::{
    analyzer::{error::OperatorMismatchSide, prelude::{AnalyzeError, Problems, Warning}}, 
    environment::prelude::{Environment, ValueType}, 
    lexer::prelude::Token, 
    parser::prelude::{Expression, Identifier, Infix, Module, Operator, Prefix, Primitive, Statement}, 
    utils::prelude::{SrcSpan, TypeWarningEmitter}
};

pub enum Outcome<T, E> {
    Ok(T),
    PartialFailure(T, E)
}

#[derive(Debug)]
pub struct ModuleAnalyzer {
    problems: Problems,
}

impl ModuleAnalyzer {
    pub fn analyze(
        module: Module, 
        warnings: &TypeWarningEmitter
    ) -> Outcome<Module, Vec<AnalyzeError>> {
        let mut analyzer = ModuleAnalyzer {
            problems: Default::default()
        };

        let mut env = Environment::new();

        for statement in &module.program.statements {
            analyzer.analyze_statement(statement, &mut env);
        }

        env.convert_unused_to_warnings(&mut analyzer.problems);
        
        analyzer.problems.sort();

        for warning in analyzer.problems.take_warnings() {
            warnings.emit(warning);
        }

        match Vec::try_from(analyzer.problems.take_errors()) {
            Err(_) => Outcome::Ok(module),
            Ok(errors) if errors.len() > 0 => Outcome::PartialFailure(module, errors),
            _ => Outcome::Ok(module)
        }
    }

    fn analyze_statement(&mut self, statement: &Statement, env: &mut Environment) {
        match statement {
            Statement::Declaration(declaration) => {
                if declaration.identifiers.len() == 0 {
                    self.problems.warning(Warning::EmptyDeclaration { location: declaration.location });
                }

                for identifiers in &declaration.identifiers {
                    for name in &identifiers.names {
                        match env.get(&name.value) {
                            None => {
                                env.declare(
                                    name.value.clone(), 
                                    identifiers.names_type.to_owned().into()
                                );
                                env.init_usage(name.value.clone(), name.location, &mut self.problems);
                            }
                            Some(_) => {
                                self.problems.error(AnalyzeError::VariableRedeclaration { 
                                    location_a: env.usages.get(&name.value).unwrap().0, 
                                    location_b: name.location,
                                    variable: name.value.clone()
                                })
                            }
                        }
                    }
                }
            },
            Statement::Operator(operator) => self.analyze_operator(operator, env)
        }
    }

    fn analyze_operator(&mut self, operator: &Operator, env: &mut Environment) {
        match operator {
            Operator::Assignment(assignment) => {
                let identifier = &assignment.identifier;
                let identifier_type = match get_identifier_type(identifier, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };
                env.increment_usage(&assignment.identifier.value);
                env.increment_initialization(&assignment.identifier.value);

                let value = &assignment.value;
                let value_type = match get_expression_type(value, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };
                
                if identifier_type != value_type {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: value.location(),
                        expected: identifier_type,
                        got: value_type
                    })
                }
            },
            Operator::Nested(nested) => {
                nested.operators.iter().for_each(|operator| {
                    self.analyze_operator(&operator, env);
                });
            },
            Operator::Conditional(conditional) => {
                let condition_type = match get_expression_type(&conditional.condition, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if condition_type != ValueType::Boolean {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: conditional.condition.location(),
                        expected: ValueType::Boolean,
                        got: condition_type
                    });
                }

                self.analyze_operator(&conditional.resolution, env);

                if let Some(alternative) = &conditional.alternative {
                    self.analyze_operator(alternative, env);
                }

                match conditional.condition {
                    Expression::Primitive(Primitive::Bool { value, .. }) => match value {
                        true => { 
                            if let Some(alternative) = &conditional.alternative {
                                self.problems.warning(Warning::UnreachableElseClause { location: alternative.location() }); 
                            }
                        },
                        false => {
                            self.problems.warning(Warning::UnreachableIfClause { location: conditional.resolution.location() })
                        }
                    },
                    _ => {}
                }
            },
            Operator::ConditionalLoop(loop_) => {
                let condition_type = match get_expression_type(&loop_.condition, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if condition_type != ValueType::Boolean {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: loop_.condition.location(),
                        expected: ValueType::Boolean,
                        got: condition_type
                    });
                }

                self.analyze_operator(&loop_.block, env);

                match loop_.condition {
                    Expression::Primitive(Primitive::Bool { value, .. }) => match value {
                        true => { 
                            let start = loop_.location.start;
                            let end = loop_.condition.location().end + 1;

                            self.problems.warning(Warning::InfiniteLoop {  location: SrcSpan { start, end } }); 
                        },
                        false => {
                            self.problems.warning(Warning::UnreachableWhileClause { location: loop_.block.location() });
                        }
                    },
                    _ => {}
                }
            },
            Operator::FixedLoop(loop_) => {
                let assignment = &loop_.assignment;

                let identifier = &assignment.identifier;
                let identifier_type = match get_identifier_type(&identifier, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                env.increment_usage(&identifier.value);
                env.increment_initialization(&identifier.value);

                if identifier_type != ValueType::Integer {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: identifier.location,
                        expected: identifier_type,
                        got: ValueType::Integer
                    });
                }

                let value = &assignment.value;
                let value_type = match get_expression_type(&value, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if value_type != ValueType::Integer {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: value.location(),
                        expected: ValueType::Integer,
                        got: value_type 
                    });
                }

                let to_type = match get_expression_type(&loop_.val, env) {
                    Ok(type_) => type_,
                    Err(err) => {
                        self.problems.error(err);
                        return;
                    }
                };

                if to_type != ValueType::Integer {
                    self.problems.error(AnalyzeError::TypeMismatch { 
                        location: loop_.val.location(),
                        expected: ValueType::Integer,
                        got: to_type
                    });
                }

                self.analyze_operator(&loop_.block, env);
            },
            Operator::Input(input) => {
                input.identifiers.iter().for_each(|ident| {
                    match env.get(&ident.value) {
                        Some(_) => {
                            env.increment_usage(&ident.value);
                            env.increment_initialization(&ident.value);
                        },
                        None => self.problems.error(AnalyzeError::VariableNotDeclared { 
                            location: ident.location,
                            variable: ident.value.clone()
                        })
                    }
                });
            },
            Operator::Output(output) => {
                output.expressions.iter().for_each(|expr| {
                    self.analyze_expression(expr, env);
                });
            }
        }
    }

    fn analyze_expression(&mut self, expression: &Expression, env: &mut Environment) {
        if let Err(err) = get_expression_type(expression, env) {
            self.problems.error(err);
        }
    }
}

fn get_expression_type(
    expression: &Expression, 
    env: &mut Environment
) -> Result<ValueType, AnalyzeError> {
    match expression {
        Expression::Identifier(identifier) => {
            if !env.is_initialized(&identifier.value) {
                return Err(AnalyzeError::VariableNotInitialized { 
                    location: identifier.location, 
                    variable: identifier.value.clone()
                })
            }

            env.increment_usage(&identifier.value);

            get_identifier_type(identifier, env)
        },
        Expression::Primitive(primitive) => Ok(get_primitive_type(primitive)),
        Expression::Infix(infix) => get_infix_type(infix, env),
        Expression::Prefix(prefix) => get_prefix_type(prefix, env),
        Expression::Nested { expression, .. } => get_expression_type(&expression, env)
    }
}

fn get_identifier_type(
    identifier: &Identifier, 
    env: &Environment
) -> Result<ValueType, AnalyzeError> {
    match env.get(&identifier.value) {
        Some(value) => Ok(value._type()),
        None => Err(AnalyzeError::VariableNotDeclared {
            location: identifier.location,
            variable: identifier.value.clone()
        })
    }
}

fn get_primitive_type(primitive: &Primitive) -> ValueType {
    match primitive {
        Primitive::Int { .. } => ValueType::Integer,
        Primitive::Float { .. } => ValueType::Float,
        Primitive::Bool { .. } => ValueType::Boolean,
    }
}

fn get_infix_type(
    infix: &Infix, 
    env: &mut Environment
) -> Result<ValueType, AnalyzeError> {
    let left_type = get_expression_type(&infix.left, env)?;
    let right_type = get_expression_type(&infix.right, env)?;

    let allowed_types = get_allowed_types_for(&infix.operator);

    let is_left_allowed = allowed_types.contains(&left_type);
    let is_right_allowed = allowed_types.contains(&right_type);

    let value_type = match (is_left_allowed, is_right_allowed) {
        (true, true) => match (&left_type, &right_type) {
            (ValueType::Integer, ValueType::Integer)
            | (ValueType::Float, ValueType::Float)
            | (ValueType::Boolean, ValueType::Boolean) => left_type, 
            _ => return Err(AnalyzeError::TypeMismatch { 
                location: infix.right.location(), 
                expected: left_type, 
                got: right_type 
            })
        },
        _ => return Err(AnalyzeError::OperatorMismatch { 
            left: OperatorMismatchSide {
                location: infix.left.location(),
                value_type: left_type,
                is_valid: is_left_allowed
            },  
            right: OperatorMismatchSide {
                location: infix.right.location(),
                value_type: right_type,
                is_valid: is_right_allowed
            },
            expected: allowed_types
        })
    };

    let value_type = match value_type {
        ValueType::Integer => match infix.operator {
            Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::Equal
            | Token::NotEqual => ValueType::Boolean,
            Token::Div => ValueType::Float,
            _ => value_type
        },
        ValueType::Float => match infix.operator {
            Token::LessThan
            | Token::GreaterThan => ValueType::Boolean,
            _ => value_type
        },
        ValueType::Boolean => value_type,
        _ => unreachable!("String result should not be possible")
    };

    Ok(value_type)
}

fn get_prefix_type(
    prefix: &Prefix,
    env: &mut Environment
) -> Result<ValueType, AnalyzeError> {
    let expression_type = get_expression_type(&prefix.expression, env)?;

    match (&prefix.operator, &expression_type) {
        (Token::Tilda, ValueType::Boolean) => Ok(expression_type),
        (token, _) => Err(AnalyzeError::InvalidUnaryOperation { location: prefix.location, token: token.clone() })
    }
}

fn get_allowed_types_for(operator: &Token) -> Vec<ValueType> {
    if !operator.is_operator() {
        return vec![];
    }

    match operator {
        Token::Plus => vec![ValueType::Integer, ValueType::Float],
        Token::Minus
        | Token::Mult
        | Token::Div => vec![ValueType::Integer, ValueType::Float],
        Token::And
        | Token::Or => vec![ValueType::Boolean],
        Token::LessThan
        | Token::GreaterThan => vec![ValueType::Integer, ValueType::Float],
        Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => vec![ValueType::Integer],
        Token::Equal
        | Token::NotEqual => vec![ValueType::Integer, ValueType::Boolean],
        _ => unreachable!("This should not match")
    }
}