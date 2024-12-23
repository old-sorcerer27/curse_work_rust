mod cli;
mod rlpl;
mod rppl;
#[cfg(feature = "compiler")]
mod compiler;
#[cfg(feature = "compiler")]
use compiler::{ObjectCompiler, ObjectLinker};

use std::{path::PathBuf, rc::Rc};

use clap::Parser;
use cli::{
    print_analyzed, print_analyzing
};
#[cfg(feature = "compiler")]
use cli::{
    print_compiled, print_compiling
};
#[cfg(feature = "compiler")]
use inkwell::context::Context;
#[allow(unused_imports)]
use lang_core::{
    analyzer::{analyze, analyze_from_stream},
    environment::prelude::Environment,
    utils::prelude::{Warning, WarningEmitterIO}
};
#[cfg(feature = "compiler")]
use lang_core::codegen::prelude::Codegen;

#[derive(Parser)]
enum Command {
    /// Performs lexical, syntactical and semantical analysis
    Analyze {
        /// Path of source file
        path: PathBuf,
        /// Do not print parsed source code
        #[arg(short, long, default_value_t = false)]
        no_output: bool,
        /// Print ast instead of parsed source code
        #[arg(long, default_value_t = false)]
        print_ast: bool,
        /// Show table listing
        #[arg(short, long, default_value_t = false)]
        show_table_listing: bool
    },
    /// Performs lexical, syntactical and semantical analysis
    /// and compiles it in executable file
    #[cfg(feature = "compiler")]
    Compile {
        /// Path of source file
        path: PathBuf,
        /// Path of executable file
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Optimization level to use when compiling
        #[arg(short = 'O', value_name = "OPT_LEVEL")]
        optimization: Option<u8>,
        /// Leave object file after compilation [default: false]
        #[arg(long, default_value_t = false)]
        obj: bool,
        /// Return Intermediate Representation [default: false]
        #[arg(long, default_value_t = false)]
        ir: bool,
        /// Return assembly code [default: false]
        #[arg(short, long, default_value_t = false)]
        asm: bool,
    },
    /// Runs Read Lex Print Loop
    Rlpl,
    /// Runs Read Parse Print Loop
    Rppl
}

fn main() {
    let _ = match Command::parse() {
        Command::Analyze { path, no_output, print_ast, show_table_listing } => {
            let warning_emitter = Rc::new(ConsoleWarningEmitter);

            let buf_writer = crate::cli::stderr_buffer_writer();
            let mut buf = buf_writer.buffer();

            print_analyzing(path.to_str().unwrap());
            let start = std::time::Instant::now();

            match analyze_from_stream(path, warning_emitter.clone(), show_table_listing) {
                Ok(module) => {
                    if !no_output {
                        if print_ast {
                            println!("{:#?}", module.program);
                        } else {
                            println!("{}", module.program);
                        }
                    }
                },
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");
                }
            };

            print_analyzed(std::time::Instant::now() - start);
        },
        #[cfg(feature = "compiler")]
        Command::Compile { 
            path, 
            output,
            optimization,
            obj,
            ir,
            asm
        } => {
            let warning_emitter = Rc::new(ConsoleWarningEmitter);

            let buf_writer = crate::cli::stderr_buffer_writer();
            let mut buf = buf_writer.buffer();

            print_analyzing(path.to_str().unwrap());
            let start = std::time::Instant::now();

            let analyzed = match analyze_from_stream(path.clone(), warning_emitter.clone(), false) {
                Ok(module) => module,
                Err(err) => {
                    err.pretty(&mut buf);
                    buf_writer
                        .print(&buf)
                        .expect("Writing warning to stderr");

                    return;
                }
            };

            print_analyzed(std::time::Instant::now() - start);

            let context = Context::create();
            let builder = context.create_builder();
            let module = context.create_module(path.clone().file_name().unwrap().to_str().unwrap());

            print_compiling(path.to_str().unwrap());
            let start = std::time::Instant::now();

            let _ = Codegen::compile(
                &context,
                &builder,
                &module,
                &analyzed.program
            );

            // println!("{}", module.to_string());

            let output = match output {
                Some(output) => output,
                None => PathBuf::from(path.file_name().unwrap().to_str().unwrap())
            };

            let opt_level = match optimization {
                Some(level) => match level {
                    0 => inkwell::OptimizationLevel::None,
                    1 => inkwell::OptimizationLevel::Less,
                    2 => inkwell::OptimizationLevel::Default,
                    _ => inkwell::OptimizationLevel::Aggressive,
                },
                None => inkwell::OptimizationLevel::None
            };

            if !asm && !ir {
                let mut obj_file = output.clone();
                obj_file.set_extension("o");
    
                ObjectCompiler::compile(
                    opt_level, 
                    inkwell::targets::RelocMode::PIC, 
                    inkwell::targets::CodeModel::Default, 
                    &module, 
                    inkwell::targets::FileType::Object,
                    &obj_file,
                );
    
                let mut exe_file = output.clone();
                exe_file.set_extension("");
    
                ObjectLinker::link(
                    &obj_file,
                    opt_level,
                    &exe_file
                ).unwrap();
    
                if !obj {
                    let _ = std::fs::remove_file(obj_file);
                }
            } else {
                if ir {
                    let mut ir_file = output.clone();
                    ir_file.set_extension("ll");

                    std::fs::write(
                        ir_file, 
                        module.to_string()
                    ).unwrap();
                }

                if asm {
                    let mut asm_file = output.clone();
                    asm_file.set_extension("asm");
    
                    ObjectCompiler::compile(
                        opt_level, 
                        inkwell::targets::RelocMode::PIC, 
                        inkwell::targets::CodeModel::Default, 
                        &module, 
                        inkwell::targets::FileType::Assembly,
                        &asm_file,
                    );
                }
            }

            print_compiled(std::time::Instant::now() - start);
        }
        Command::Rlpl => {
            let _  = rlpl::start();
        },
        Command::Rppl => {
            let _ = rppl::start();
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct ConsoleWarningEmitter;

impl WarningEmitterIO for ConsoleWarningEmitter {
    fn emit_warning(&self, warning: Warning) {
        let buffer_writer = crate::cli::stderr_buffer_writer();
        let mut buffer = buffer_writer.buffer();
        warning.pretty(&mut buffer);
        buffer_writer
            .print(&buffer)
            .expect("Writing warning to stderr");
    }
}