use std::collections::HashMap;

use inkwell::{builder::Builder, context::Context, module::Module, types::BasicTypeEnum, values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode, PointerValue}, AddressSpace, FloatPredicate, IntPredicate};
use super::variable::Variable;

use crate::{parser::prelude::{Declaration, Expression, IdentifierType, Operator, Primitive, Program, Statement}, lexer::prelude::Token};

fn printf_prototype<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let printf_type = ctx.i32_type().fn_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into()
        ], 
        true
    );

    module.get_function("printf")
        .unwrap_or_else(|| module.add_function("printf", printf_type, None))
}

fn scanf_prototype<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let scanf_type = ctx.i32_type().fn_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into()
        ], 
        true
    );

    module.get_function("scanf")
        .unwrap_or_else(|| module.add_function("scanf", scanf_type, None))
}

fn fflush_prototype<'ctx>(ctx: &'ctx Context, module: &Module<'ctx>) -> FunctionValue<'ctx> {
    let fflush_type = ctx.i32_type().fn_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into()
        ],
        false
    );

    module.get_function("fflush")
        .unwrap_or_else(|| module.add_function("fflush", fflush_type, None))
}

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub program: &'a Program,

    variables: HashMap<String, Variable<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,

    global_strings: HashMap<String, PointerValue<'ctx>>,

    // built-in functions
    printf_fn: FunctionValue<'ctx>,
    scanf_fn: FunctionValue<'ctx>,
    fflush_fn: FunctionValue<'ctx>
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn get_basic_type(&self, var_type: IdentifierType) -> BasicTypeEnum<'ctx> {
        match var_type {
            IdentifierType::Int => self.context.i64_type().into(),
            IdentifierType::Float => self.context.f64_type().into(),
            IdentifierType::Bool => self.context.bool_type().into(),
            // IdentifierType::String => self.context.ptr_type(AddressSpace::default()).into()
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declaration(decl) => self.compile_declaration(decl),
            Statement::Operator(op) => self.compile_operator(op)
        }
    }

    fn compile_declaration(&mut self, declaration: &Declaration) {
        for identifiers in &declaration.identifiers {
            let basic_type = self.get_basic_type(identifiers.names_type);
            
            for name in &identifiers.names {
                let name = &name.value;
                let alloca = self.builder.build_alloca(basic_type, name)
                    .unwrap();

                self.variables.insert(
                    name.to_string(), 
                    Variable::new(
                        identifiers.names_type,
                        basic_type,
                        alloca
                    )
                );
            }
        }
    }

    fn compile_operator(&mut self, operator: &Operator) {
        match operator {
            Operator::Assignment(assign) => {
                let name = &assign.identifier.value;
                let value = self.compile_expression(&assign.value).unwrap();
                let variable = self.variables.get(name).expect("Variable should be defined");

                self.builder.build_store(variable.pointer, value).unwrap();
            },
            Operator::Nested(nested) => {
                if !nested.operators.is_empty() {
                    for operator in &nested.operators {
                        self.compile_operator(operator);
                    }
                }
            },
            Operator::Input(input) => {
                let block = self.context.append_basic_block(
                    self.fn_value(),
                    "input"
                );
                self.builder.build_unconditional_branch(block).unwrap();
                self.builder.position_at_end(block);

                // let mut identifiers = vec![];
                // let mut format_parts = Vec::with_capacity(input.identifiers.len());

                for identifier in &input.identifiers {
                    let variable = self.variables.get(&identifier.value)
                        .expect("Variable should be defined");

                    let formatted_string = match variable.var_type {
                        IdentifierType::Int => "%ld",
                        IdentifierType::Float => "%lf", 
                        IdentifierType::Bool => "%d"
                    };

                    let string_ptr = *self.global_strings.entry(formatted_string.to_string())
                        .or_insert_with(|| unsafe {
                            self.builder.build_global_string(&formatted_string, "").unwrap().as_pointer_value()
                        });

                    let args = [string_ptr.into(), variable.pointer.into()];

                    self.builder.build_direct_call(
                        self.scanf_fn,
                        &args,
                        "scanf"
                    ).unwrap();
                }
            },
            Operator::Output(output) => {
                let mut expressions = vec![];
                let mut format_parts = Vec::with_capacity(output.expressions.len());

                for expression in &output.expressions {
                    let compiled_expr = self.compile_expression(expression)
                        .unwrap();

                    expressions.push(compiled_expr.into());

                    format_parts.push(match compiled_expr {
                        BasicValueEnum::IntValue(_) => "%ld",
                        BasicValueEnum::FloatValue(_) => "%lf",
                        BasicValueEnum::PointerValue(_) | BasicValueEnum::ArrayValue(_) => "%s",
                        _ => unreachable!("Should not be another basic type")
                    });
                }

                let formatted_string = format!("{}\n", format_parts.join(""));

                let string_ptr = *self.global_strings.entry(formatted_string.clone())
                    .or_insert_with(|| unsafe {
                        self.builder.build_global_string(&formatted_string, "").unwrap().as_pointer_value()
                    });

                let mut args = vec![string_ptr.into()];
                args.append(&mut expressions);

                self.builder.build_direct_call(
                    self.printf_fn,
                    &args,
                    "printf"
                ).unwrap();

                let stdout_ptr = self.builder.build_int_to_ptr(
                    self.context.i64_type().const_zero(),
                    self.context.ptr_type(AddressSpace::default()),
                    "stdout"
                ).unwrap();

                self.builder.build_direct_call(
                    self.fflush_fn,
                    &[stdout_ptr.into()],
                    "fflush"
                ).unwrap();
            },
            Operator::Conditional(conditional) => {
                let parent = self.fn_value();
                let one_const = self.context.bool_type().const_int(1, false);

                let condition = self.compile_expression(&conditional.condition).unwrap();
                let condition = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    condition.into_int_value(),
                    one_const,
                    "if_cond"
                ).unwrap();

                let then_block = self.context.append_basic_block(parent, "if_then");
                let else_block = self.context.append_basic_block(parent, "if_else");
                let after_block = self.context.append_basic_block(parent, "if_after");

                self.builder.build_conditional_branch(condition, then_block, else_block).unwrap();

                self.builder.position_at_end(then_block);
                self.compile_operator(&conditional.resolution);
                self.builder.build_unconditional_branch(after_block).unwrap();

                self.builder.position_at_end(else_block);
                if let Some(alt) = &conditional.alternative {
                    self.compile_operator(alt);
                }
                self.builder.build_unconditional_branch(after_block).unwrap();

                self.builder.position_at_end(after_block);
            },
            Operator::ConditionalLoop(loop_) => {
                let parent = self.fn_value();
                let true_const = self.context.bool_type().const_int(1, false);
                
                let before_block = self.context.append_basic_block(parent, "while_before");
                let loop_block = self.context.append_basic_block(parent, "while");
                let after_block = self.context.append_basic_block(parent, "while_after");
                
                self.builder.build_unconditional_branch(before_block).unwrap();
                self.builder.position_at_end(before_block);

                let condition = self.compile_expression(&loop_.condition).unwrap();
                let condition = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    condition.into_int_value(),
                    true_const,
                    "while_cond"
                ).unwrap();

                self.builder.build_conditional_branch(
                    condition, 
                    loop_block, 
                    after_block
                ).unwrap();
                self.builder.position_at_end(loop_block);
                self.compile_operator(&loop_.block);

                if let Some(last_instruction) = loop_block.get_last_instruction() {
                    if last_instruction.get_opcode() != InstructionOpcode::Return {
                        self.builder.build_unconditional_branch(before_block).unwrap();
                    }
                }

                self.builder.position_at_end(after_block);
            },
            Operator::FixedLoop(loop_) => {
                let parent = self.fn_value();

                let name = &loop_.assignment.identifier.value;
                let value = self.compile_expression(&loop_.assignment.value).unwrap();
                let variable = self.variables.get(name).cloned().expect("Variable should be defined");
                let variable_type = self.get_basic_type(variable.var_type);

                self.builder.build_store(variable.pointer, value).unwrap();

                let loop_block = self.context.append_basic_block(parent, "for");
                let after_block = self.context.append_basic_block(parent, "for_after");

                self.builder.build_unconditional_branch(loop_block).unwrap();
                self.builder.position_at_end(loop_block);

                self.compile_operator(&loop_.block);

                let step = loop_.step.as_ref()
                    .map(|step| self.compile_expression(step).unwrap().into_int_value())
                    .unwrap_or_else(|| self.context.i64_type().const_int(1, false));

                let to = self.compile_expression(&loop_.to).unwrap().into_int_value();

                let cur_value = self.builder.build_load(
                    variable_type, 
                    variable.pointer, 
                    &name
                ).unwrap();
                let next_value = self.builder.build_int_nsw_add(
                    cur_value.into_int_value(),
                    step,
                    "nextval"
                ).unwrap();

                self.builder.build_store(variable.pointer, next_value).unwrap();

                let end_condition = self.builder.build_int_compare(
                    IntPredicate::SLT,
                    cur_value.into_int_value(),
                    to,
                    "for_cond"
                ).unwrap();

                self.builder.build_conditional_branch(
                    end_condition, 
                    loop_block, 
                    after_block
                ).unwrap();
                self.builder.position_at_end(after_block);
            }
        }
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        match expression {
            Expression::Identifier(ident) => {
                self.variables.get(&ident.value)
                    .map(|variable| self.builder.build_load(
                        variable.basic_type,
                        variable.pointer,
                        &ident.value
                    ).unwrap())
                    .ok_or("Could not find a matching variable")
            },
            Expression::Infix(infix) => {
                let left = self.compile_expression(&infix.left)?;
                let right = self.compile_expression(&infix.right)?;

                if left.get_type() != right.get_type() {
                    return Err("Value mismatch");
                }

                match left.get_type() {
                    BasicTypeEnum::IntType(_) => Ok(match infix.operator {
                        Token::Plus => self.builder.build_int_add(
                            left.into_int_value(), 
                            right.into_int_value(), 
                            "intadd"
                        ).unwrap().into(),
                        Token::Minus => self.builder.build_int_sub(
                            left.into_int_value(), 
                            right.into_int_value(), 
                            "intsub"
                        ).unwrap().into(),
                        Token::Asterisk => self.builder.build_int_mul(
                            left.into_int_value(),
                            right.into_int_value(),
                            "intmul"
                        ).unwrap().into(),
                        Token::Slash => {
                            let float_type = self.context.f64_type();
                            let left_cast = self.builder.build_unsigned_int_to_float(
                                left.into_int_value(), 
                                float_type, 
                                "intdiv_lcast"
                            ).unwrap();
                            let right_cast = self.builder.build_unsigned_int_to_float(
                                right.into_int_value(),
                                float_type, 
                                "intdiv_rcast"
                            ).unwrap();

                            self.builder.build_float_div(
                                left_cast,
                                right_cast,
                                "intdiv"
                            ).unwrap().into()
                        },
                        Token::LessThan => self.builder.build_int_compare(
                            IntPredicate::SLT, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::LessThanOrEqual => self.builder.build_int_compare(
                            IntPredicate::SLE, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::GreaterThan => self.builder.build_int_compare(
                            IntPredicate::SGT, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::GreaterThanOrEqual => self.builder.build_int_compare(
                            IntPredicate::SGE, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::Equal => self.builder.build_int_compare(
                            IntPredicate::EQ, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::NotEqual => self.builder.build_int_compare(
                            IntPredicate::NE, 
                            left.into_int_value(),
                            right.into_int_value(),
                            "intcmp"
                        ).unwrap().into(),
                        Token::Or => {
                            let lhs = self.builder.build_int_compare(
                                IntPredicate::NE,
                                left.into_int_value(),
                                self.context.bool_type().const_zero(),
                                "lhs_or"
                            ).unwrap();

                            let rhs = self.builder.build_int_compare(
                                IntPredicate::NE,
                                right.into_int_value(),
                                self.context.bool_type().const_zero(),
                                "rhs_or"
                            ).unwrap();

                            self.builder.build_or(lhs, rhs, "intor").unwrap().into()
                        },
                        Token::And => {
                            let lhs = self.builder.build_int_compare(
                                IntPredicate::NE,
                                left.into_int_value(),
                                self.context.bool_type().const_zero(),
                                "lhs_and"
                            ).unwrap();

                            let rhs = self.builder.build_int_compare(
                                IntPredicate::NE,
                                right.into_int_value(),
                                self.context.bool_type().const_zero(),
                                "rhs_and"
                            ).unwrap();

                            self.builder.build_and(lhs, rhs, "intand").unwrap().into()
                        },
                        _ => return Err("Invalid operator")
                    }),
                    BasicTypeEnum::FloatType(_) => Ok(match infix.operator {
                        Token::Plus => self.builder.build_float_add(
                            left.into_float_value(), 
                            right.into_float_value(), 
                            "floatadd"
                        ).unwrap().into(),
                        Token::Minus => self.builder.build_float_sub(
                            left.into_float_value(), 
                            right.into_float_value(), 
                            "floatsub"
                        ).unwrap().into(),
                        Token::Asterisk => self.builder.build_float_mul(
                            left.into_float_value(),
                            right.into_float_value(),
                            "floatmul"
                        ).unwrap().into(),
                        Token::Slash => self.builder.build_float_div(
                            left.into_float_value(),
                            right.into_float_value(),
                            "floatdiv"
                        ).unwrap().into(),
                        Token::LessThan => self.builder.build_float_compare(
                            FloatPredicate::OLT, 
                            left.into_float_value(),
                            right.into_float_value(),
                            "floatcmp"
                        ).unwrap().into(),
                        Token::GreaterThan => self.builder.build_float_compare(
                            FloatPredicate::OGT, 
                            left.into_float_value(),
                            right.into_float_value(),
                            "floatcmp"
                        ).unwrap().into(),
                        _ => return Err("Invalid operator")
                    }),
                    _ => Err("Invalid infix operation")
                }
            },
            Expression::Prefix(prefix) => {
                let expr = self.compile_expression(&prefix.expression)?;
                match prefix.operator {
                    Token::Bang => Ok(expr.into_int_value()
                        .const_neg()
                        .as_basic_value_enum()
                    ),
                    _ => Err("Unexpected prefix operator")
                }
            },
            Expression::Nested { expression, .. } => self.compile_expression(expression),
            Expression::Primitive(primitive) => Ok(match primitive {
                Primitive::Int { value, .. } => self.context.i64_type()
                    .const_int(*value as u64, false)
                    .as_basic_value_enum(),
                Primitive::Float { value, .. } => self.context.f64_type()
                    .const_float(*value)
                    .as_basic_value_enum(),
                Primitive::Bool { value, .. } => self.context.bool_type()
                    .const_int(*value as u64, false)
                    .as_basic_value_enum(),
                Primitive::String { value, .. } => {
                    let string_ptr = self.global_strings
                        .entry(value.to_string())
                        .or_insert_with(|| unsafe {
                            self.builder.build_global_string(value, "").unwrap().as_pointer_value()
                        });
                    (*string_ptr).into()
                }
            })
        }
    }

    fn compile_program(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);

        if self.program.statements.is_empty() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.fn_value_opt = Some(function);

        for stmt in &self.program.statements {
            self.compile_stmt(stmt);
        }

        self.builder.build_return(Some(&self.context.i32_type().const_zero())).unwrap();

        if function.verify(true) {
            Ok(function)
        } else {
            unsafe { function.delete(); }
            Err("Invalid function")
        }
    }

    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        program: &'a Program,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let printf_fn = printf_prototype(context, module);
        let scanf_fn = scanf_prototype(context, module);
        let fflush_fn = fflush_prototype(context, module);

        let mut codegen = Codegen {
            context,
            builder,
            module,
            program,
            variables: HashMap::new(),
            fn_value_opt: None,
            global_strings: HashMap::new(),
            printf_fn,
            scanf_fn,
            fflush_fn
        };

        codegen.compile_program()
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;

    use inkwell::{context::Context, targets::{InitializationConfig, Target, TargetMachine}};

    use crate::parser::prelude::parse_module;

    use super::Codegen;

    #[test]
    fn test_compile() {
        let src = r#"
            begin
                var e: @;;

                e := "Hi";

                writeln "Value: ", e
            end
        "#;
        // let src = r#"
        //     begin
        //         var i: %;;

        //         i := 0;

        //         while (i < 5) i := i + 1;

        //         writeln i
        //     end
        // "#;

        let parsed = parse_module(src).unwrap();

        let context = Context::create();
        let builder = context.create_builder();
        let module = context.create_module("test_compile");

        let _compiled = Codegen::compile(
            &context,
            &builder,
            &module,
            &parsed.module.program
        ).unwrap();

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        // println!("{target_triple:?}");
        let target = Target::from_triple(&target_triple).unwrap();
        // println!("{target:?}");
        let target_machine = target.create_target_machine(
            &target_triple, 
            "generic", 
            "", 
            inkwell::OptimizationLevel::None, 
            inkwell::targets::RelocMode::PIC, 
            inkwell::targets::CodeModel::Default
        ).unwrap();
        // println!("{target_machine:?}");

        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        module.set_triple(&target_triple);
        
        let out_file = "hohiho.o";
        // let buf = target_machine.write_to_memory_buffer(
        //     &module, 
        //     inkwell::targets::FileType::Object
        // ).unwrap();
        target_machine.write_to_file(
            &module, 
            inkwell::targets::FileType::Object, 
            Path::new(&out_file.to_string())
        ).unwrap();

        println!("{}", module.print_to_string().to_string());
    }
}