use std::{path::PathBuf, rc::Rc};

use crate::{parser::prelude::parse_module, utils::prelude::{NullWarningEmitterIO, TypeWarningEmitter, WarningEmitter}};

use super::ModuleAnalyzer;

#[test]
fn test_module() {
    let input = r#"
        begin
            var a, b, c: %;;

            a := 5;
            b := 10
        end
    "#;

    let parsed = parse_module(input).unwrap();

    let warnings = Rc::new(NullWarningEmitterIO);

    let emitter = &TypeWarningEmitter::new(
        PathBuf::new(), 
        input.to_string(), 
        WarningEmitter::new(warnings.clone())
    );

    let _ = ModuleAnalyzer::analyze(parsed.module, &emitter);
}