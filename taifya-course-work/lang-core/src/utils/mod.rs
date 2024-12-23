pub mod diagnostic;
pub mod src_span;
pub mod error;
pub mod warning;

pub mod prelude {
    pub use super::{
        diagnostic::*,
        src_span::*,
        error::*,
        warning::*
    };
}
