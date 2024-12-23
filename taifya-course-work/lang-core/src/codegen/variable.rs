use inkwell::{types::BasicTypeEnum, values::PointerValue};

use crate::parser::prelude::IdentifierType;

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
    pub var_type: IdentifierType,
    pub basic_type: BasicTypeEnum<'ctx>,
    pub pointer: PointerValue<'ctx>
}

impl<'ctx> Variable<'ctx> {
    pub fn new(
        var_type: IdentifierType,
        basic_type: BasicTypeEnum<'ctx>,
        pointer: PointerValue<'ctx>
    ) -> Self {
        Self {
            var_type,
            basic_type,
            pointer
        }
    }
}