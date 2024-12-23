pub mod error;
pub mod parser;
pub mod ast;

pub mod prelude {
    pub use super::{
        error::*,
        parser::*,
        ast::*
    };
}

#[cfg(test)]
mod tests;