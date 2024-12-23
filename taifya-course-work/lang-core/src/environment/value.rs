use std::fmt::Display;

use crate::parser::prelude::IdentifierType;

pub const TRUE: Value = Value::Boolean { value: true };
pub const FALSE: Value = Value::Boolean { value: false };

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer {
        value: i64
    },
    Float {
        value: f64,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool
    },
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer { value } => write!(f, "{value}"),
            Value::Float { value } => write!(f, "{value}"),
            Value::String { value } => write!(f, "{value}"),
            Value::Boolean { value } => write!(f, "{value}")
        }
    }
}

impl Value {
    pub fn _type(&self) -> ValueType {
        match self {
            Self::Integer { .. } => ValueType::Integer,
            Self::Float { .. } => ValueType::Float,
            Self::String { .. } => ValueType::String,
            Self::Boolean { .. } => ValueType::Boolean
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Integer,
    Float,
    String,
    Boolean
}

impl From<IdentifierType> for ValueType {
    fn from(value: IdentifierType) -> Self {
        match value {
            IdentifierType::Int => ValueType::Integer,
            IdentifierType::Float => ValueType::Float,
            IdentifierType::Bool => ValueType::Boolean
        }
    }
}

