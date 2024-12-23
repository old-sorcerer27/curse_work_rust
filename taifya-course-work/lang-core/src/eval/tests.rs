use std::{cell::RefCell, path::PathBuf, rc::Rc};

use termcolor::BufferWriter;

use crate::{
    analyzer::prelude::{ModuleAnalyzer, Outcome},
    environment::prelude::Environment, 
    parser::prelude::parse_module, 
    utils::prelude::{NullWarningEmitterIO, TypeWarningEmitter, WarningEmitter, Error}
};

use super::eval;

#[test]
fn test_program() {
    let input = r#"
        begin
            var a, b, c: %; d, e, f: !;

            
        end
    "#;

    let parsed = parse_module(input).unwrap();

    let warnings = Rc::new(NullWarningEmitterIO);

    let emitter = &TypeWarningEmitter::new(
        PathBuf::new(), 
        input.to_string(), 
        WarningEmitter::new(warnings.clone())
    );

    let analyzed = ModuleAnalyzer::analyze(parsed.module, &emitter);

    match analyzed {
        Outcome::Ok(module) => {
            let env = Rc::new(RefCell::new(Environment::new()));

            eval(module, env.clone());

            println!("{env:?}")
        },
        Outcome::PartialFailure(_, errors) => {
            let buf_writer = BufferWriter::stderr(termcolor::ColorChoice::Auto);
            let mut buf = buf_writer.buffer();

            let err = Error::Type { 
                path: PathBuf::new(), 
                src: input.to_string(), 
                errors 
            };

            err.pretty(&mut buf);
            buf_writer
                .print(&buf)
                .expect("Writing warning to stderr");
        }
    }
}