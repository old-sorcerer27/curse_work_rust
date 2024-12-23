use crate::{lexer::prelude::{LexicalError, Token}, utils::prelude::SrcSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorType {
    ExpectedIdent,
    ExpectedOperator,
    ExpectedBegin,
    ExpectedVar,
    ExpectedEnd,
    ExpectedColon,
    UnexpectedColonBeforeRSBracket,
    UnexpectedSemicolonBeforeEnd,
    UnexpectedEof,
    UnexpectedToken {
        token: Token,
        expected: Vec<String>,
    },
    ExpectedType,
    MissingColon,
    MissingSemicolon,
    LexError { error: LexicalError },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub error: ParseErrorType,
    pub span: SrcSpan
}

impl ParseError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match &self.error {
            ParseErrorType::ExpectedIdent => ("Expected identifier", vec![]),
            ParseErrorType::ExpectedOperator => ("Expected operator", vec![]),
            ParseErrorType::ExpectedBegin => ("Expected `begin` keyword", vec![]),
            ParseErrorType::ExpectedVar => ("Expected `var` keyword", vec![]),
            ParseErrorType::ExpectedEnd => ("Expected `end` keyword", vec![]),
            ParseErrorType::ExpectedColon => ("Expected `colon`", vec![]),
            ParseErrorType::UnexpectedColonBeforeRSBracket => ("Unexpected colon before RSBracket", vec![]),
            ParseErrorType::UnexpectedSemicolonBeforeEnd => ("Unexpected semicolon before `end`", vec![]),
            ParseErrorType::ExpectedType => ("Expected type", vec![]),
            // ParseErrorType::UnexpectedReservedWord => ("Unexpected reserved word", vec![]),
            ParseErrorType::UnexpectedToken { token, expected } => {
                let found = match token {
                    Token::Int(_) => "an Int".to_string(),
                    Token::Float(_) => "a Float".to_string(),
                    Token::Ident(_) => "an Identifier".to_string(),
                    _ if token.is_reserved_word() => format!("the keyword `{}`", token.as_literal()),
                    _ => format!("`{}`", token.as_literal())
                };

                let expected = expected.iter()
                    .map(|token| token.to_owned())
                    .collect::<Vec<String>>();

                let messages = std::iter::once(format!("Found {found}, expected one of: "))
                    .chain(expected.iter().map(|s| format!("- {s}")))
                    .collect();

                ("Not expected this", messages)
            },
            ParseErrorType::UnexpectedEof => ("Unexpected end of file", vec![]),
            ParseErrorType::MissingColon => ("Missing colon", vec![]),
            ParseErrorType::MissingSemicolon => ("Missing semicolon", vec![]),
            ParseErrorType::LexError { error } => error.details()
        }
    }
}