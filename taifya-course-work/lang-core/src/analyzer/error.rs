use crate::{environment::prelude::ValueType, lexer::token::Token, utils::prelude::SrcSpan};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Problems {
    errors: Vec<AnalyzeError>,
    warnings: Vec<Warning>,
}

impl Problems {
    pub fn error(&mut self, error: AnalyzeError) {
        self.errors.push(error)
    }

    pub fn take_errors(&mut self) -> Vec<AnalyzeError> {
        std::mem::take(&mut self.errors)
    }

    pub fn warning(&mut self, warning: Warning) {
        self.warnings.push(warning)
    }

    pub fn take_warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }

    pub fn sort(&mut self) {
        self.errors.sort_by_key(|e| e.start_location());
        self.warnings.sort_by_key(|w| w.location().start);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AnalyzeError {
    /// Occurs when the type of the expression on the left side of the assignment operator
    /// does not match the type of the expression on the right side.
    ///
    /// Example:
    /// ```
    /// begin
    ///     var a: %;;
    ///     a := 10.0 <- Type mismatch: expected `Int`, got `Float`
    /// end
    /// ```
    TypeMismatch {
        location: SrcSpan,
        expected: ValueType,
        got: ValueType,
    },
    /// Occurs when a variable is used before it is declared.
    ///
    /// Example:
    /// ```
    /// begin
    ///     a := 10 <- Variable not declared: `a`
    /// end
    /// ```
    VariableNotDeclared {
        location: SrcSpan,
        variable: String,
    },
    /// Occurs when a variable is used before it is initialized.
    /// 
    /// Example:
    /// ```
    /// begin
    ///     var a: %;;
    ///     writeln a <- Variable not initialized: `a`
    /// end
    /// ```
    VariableNotInitialized {
        location: SrcSpan,
        variable: String,
    },
    ///
    /// Occurs when a variable is declared more than once.
    ///
    /// Example:
    /// ```text
    /// begin
    ///     var a: %;;
    ///     var a: %; <- Variable redeclaration: `a`
    /// end
    /// ```
    VariableRedeclaration {
        location_a: SrcSpan,
        location_b: SrcSpan,
        variable: String,
    },
    /// Occurs when an invalid unary operation is performed.
    /// 
    /// Example:
    /// ```
    /// begin
    ///     var a: %;
    ///     -a <- Invalid unary operation: `-`
    /// end
    /// ```
    InvalidUnaryOperation {
        location: SrcSpan,
        token: Token
    },
    /// Occurs when the types of the operands do not match the expected types for the operator.
    ///
    /// Example:
    /// ```
    /// begin
    ///     var a, b: !;
    ///         c: $;;
    ///
    ///     a := 10.0;
    ///     b := 10.0;
    ///
    ///     c := a == b <- Type mismatch. Expected any of `Integer`, `Boolean`, but got `Float` and `Float`
    /// end
    /// ```
    OperatorMismatch {
        left: OperatorMismatchSide,
        right: OperatorMismatchSide,
        expected: Vec<ValueType>,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatorMismatchSide {
    pub location: SrcSpan,
    pub value_type: ValueType,
    pub is_valid: bool
}

impl AnalyzeError {
    pub fn start_location(&self) -> u32 {
        match self {
            AnalyzeError::TypeMismatch { location, .. }
            | AnalyzeError::VariableNotDeclared { location, .. }
            | AnalyzeError::VariableNotInitialized { location, .. }
            | AnalyzeError::VariableRedeclaration { location_b: location, .. }
            | AnalyzeError::InvalidUnaryOperation { location, .. }
            | AnalyzeError::OperatorMismatch { left: OperatorMismatchSide { location, .. }, .. } => location.start
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Warning {
    UnusedVariable {
        location: SrcSpan
    },
    EmptyDeclaration {
        location: SrcSpan
    },
    UnreachableIfClause {
        location: SrcSpan
    },
    UnreachableElseClause {
        location: SrcSpan
    },
    InfiniteLoop {
        location: SrcSpan
    },
    UnreachableWhileClause {
        location: SrcSpan
    },
    UnreachableCode {
        location: SrcSpan
    }
}

impl Warning {
    pub fn location(&self) -> SrcSpan {
        match self {
            Warning::UnusedVariable { location, .. }
            | Warning::EmptyDeclaration { location }
            | Warning::UnreachableIfClause { location }
            | Warning::UnreachableElseClause { location }
            | Warning::InfiniteLoop { location }
            | Warning::UnreachableWhileClause { location }
            | Warning::UnreachableCode { location } => *location
        }
    }
}