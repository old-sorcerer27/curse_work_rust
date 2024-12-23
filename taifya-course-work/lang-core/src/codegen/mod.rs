pub mod codegen;
pub mod variable;

pub mod prelude {
    pub use super::{
        codegen::*,
        variable::*
    };
}