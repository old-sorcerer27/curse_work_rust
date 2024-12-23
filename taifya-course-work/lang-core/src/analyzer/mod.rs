pub mod error;
pub mod analyzer;

pub mod prelude {
    pub use super::{
        analyzer::*,
        error::*
    };
}

use std::{path::PathBuf, rc::Rc};

use utf8_chars::BufReadCharsExt;

use crate::{
    analyzer::prelude::{ModuleAnalyzer, Outcome}, lexer::table_element::TableElement, parser::{parser::parse_module_from_stream, prelude::{parse_module, Module}}, utils::prelude::{Error, SrcSpan, TypeWarningEmitter, WarningEmitter, WarningEmitterIO}
};


pub fn analyze(
    path: PathBuf,
    warnings: Rc<dyn WarningEmitterIO>,
) -> Result<Module, Error> {
    let warnings = WarningEmitter::new(warnings);
    let src = match std::fs::read_to_string(path.clone()) {
        Ok(src) => src,
        Err(err) => {
            let error = Error::StdIo { err: err.kind() };
            return Err(error)
        }
    };

    let parsed = match parse_module(&src) {
        Ok(parsed) => parsed,
        Err(err) => {
            // println!("{err:?}");
            let error = Error::Parse { path, src, error: err };
            return Err(error)
        }
    };

    let warnings = TypeWarningEmitter::new(
        path.clone(),
        src.to_string(),
        warnings
    );

    let outcome = ModuleAnalyzer::analyze(parsed.module, &warnings);

    match outcome {
        Outcome::Ok(module) => {
            Ok(module)
        },
        Outcome::PartialFailure(_, errors) => {
            // println!("{errors:?}");
            let error = Error::Type { path, src, errors };
            Err(error)
        }
    }
}

pub fn analyze_from_stream(
    path: PathBuf,
    warnings: Rc<dyn WarningEmitterIO>,
    show_all: bool
) -> Result<Module, Error> {
    let warnings = WarningEmitter::new(warnings);
    let file = match std::fs::File::open(path.clone()) {
        Ok(file) => file,
        Err(err) => {
            let error = Error::StdIo { err: err.kind() };
            return Err(error)
        }
    };

    let file_size = file.metadata()
        .map_err(|err| Error::StdIo { err: err.kind() })?.len() as usize;

    let mut src = String::with_capacity(file_size);
    let mut reader = std::io::BufReader::new(file);
    let stream = reader.chars()
        .map(|c| {
            let c = c.unwrap();
            src.push(c);
            c
        });

    let parsed = match parse_module_from_stream(stream) {
        Ok(parsed) => parsed,
        Err(err) => {
            let error = Error::Parse { path, src, error: err };
            return Err(error)
        }
    };

    let warnings = TypeWarningEmitter::new(
        path.clone(),
        src.to_string(),
        warnings
    );

    if parsed.module.program.location.end != src.len() as u32 {
        warnings.emit(error::Warning::UnreachableCode { 
            location: SrcSpan { 
                start: parsed.module.program.location.end, 
                end: src.len() as u32
            }
        });
    }

    let outcome = ModuleAnalyzer::analyze(parsed.module, &warnings);

    let result = match outcome {
        Outcome::Ok(module) => {
            let mut idents = parsed.table.iter()
                .filter(|el| el.table == 2)
                .collect::<Vec<&TableElement>>();

            idents.sort_by_key(|el| el.idx);
            idents.dedup_by_key(|el| el.idx);
                
            let idents = idents.iter()
                .map(|el| el.to_string_with_token())
                .collect::<Vec<String>>()
                .join("\n");

            let mut numbers = parsed.table.iter()
                .filter(|el| el.table == 3)
                .collect::<Vec<&TableElement>>();

            numbers.sort_by_key(|el| el.idx);
            numbers.dedup_by_key(|el| el.idx);
                
            let numbers = numbers.iter()
                .map(|el| el.to_string_with_token())
                .collect::<Vec<String>>()
                .join("\n");

            println!("Идентификаторы:\n{}\n", idents);
            println!("Числа:\n{}\n", numbers);
            if show_all {
                println!("{}\n", parsed.table.into_iter().map(|el| el.to_string_with_token()).collect::<Vec<String>>().join("\n"))
            } else {
                println!("{}\n", parsed.table.into_iter().map(|el| el.to_string()).collect::<Vec<String>>().join(" "))
            }

            Ok(module)
        },
        Outcome::PartialFailure(_, errors) => {
            // println!("{errors:?}");
            let error = Error::Type { path, src, errors };
            Err(error)
        }
    };

    result
}

#[cfg(test)]
mod tests;