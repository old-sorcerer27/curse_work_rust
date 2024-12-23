use std::path::PathBuf;

use termcolor::Buffer;
use thiserror::Error;

use crate::{
    analyzer::prelude::AnalyzeError,
    utils::prelude::SrcSpan, 
    parser::prelude::{ParseError, ParseErrorType},
};
use super::diagnostic::{Diagnostic, Label, Level, Location};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum Error {
    #[error("failed to parse source code")]
    Parse {
        path: PathBuf,
        src: String,
        error: ParseError
    },
    #[error("type checking failed")]
    Type {
        path: PathBuf,
        src: String,
        errors: Vec<AnalyzeError>
    },
    #[error("IO operation failed")]
    StdIo {
        err: std::io::ErrorKind
    }
}

impl Error {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }

    pub fn pretty(&self, buf: &mut Buffer) {
        use std::io::Write;

        for diagnostic in self.to_diagnostics() {
            diagnostic.write(buf);
            writeln!(buf).expect("write new line diagnostic");
        }
    }

    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        match self {
            Error::Parse { path, src, error } => {
                let (label, extra) = error.details();
                let text = extra.join("\n");

                let adjusted_location = if matches!(error.error, ParseErrorType::UnexpectedEof | ParseErrorType::ExpectedEnd) {
                    SrcSpan {
                        start: src.len() as u32,
                        end: src.len() as u32,
                    }
                } else {
                    error.span
                };

                vec![Diagnostic {
                    title: "Syntax error".into(),
                    text,
                    level: Level::Error,
                    location: Some(Location {
                        src: &src,
                        path: path.clone(),
                        label: Label {
                            text: Some(label.to_string()),
                            span: adjusted_location,
                        },
                        extra_labels: vec![],
                    }),
                }]
            },
            Error::Type { path, src, errors } => {
                let mut diagnostics = vec![];

                errors
                    .iter()
                    .for_each(|error| {
                        match error {
                            AnalyzeError::InvalidUnaryOperation { location, token } => {
                                let text = format!("Invalid unary operation: `{}`", token.as_literal());

                                diagnostics.push(Diagnostic {
                                    title: "Invalid token".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label: Label {
                                            text: Some("Expected `!`".to_string()),
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzeError::TypeMismatch { 
                                location, 
                                expected, 
                                got 
                            } => {
                                let text = format!("Expected `{expected:?}`, but got `{got:?}`");

                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzeError::VariableNotDeclared { location, variable } => {
                                let text = format!("Variable `{variable}` is not declared.");

                                diagnostics.push(Diagnostic {
                                    title: "Variable not declared".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            },
                            AnalyzeError::VariableNotInitialized { location, variable } => {
                                let text = format!("Variable `{variable}` is not initialized.");

                                diagnostics.push(Diagnostic {
                                    title: "Variable not initialized".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label: Label {
                                            text: None,
                                            span: *location,
                                        },
                                        extra_labels: vec![]
                                    }),
                                })
                            }
                            AnalyzeError::VariableRedeclaration { 
                                location_a, 
                                location_b,
                                variable
                            } => {
                                let text = format!("Variable `{variable}` was declared multiple times.");

                                diagnostics.push(Diagnostic {
                                    title: "Multiple declarations".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label: Label {
                                            text: Some("Another defined here".into()),
                                            span: *location_b,
                                        },
                                        extra_labels: vec![Label {
                                            text: Some("First defined here".into()),
                                            span: *location_a
                                        }]
                                    }),
                                })
                            }
                            AnalyzeError::OperatorMismatch { 
                                left,
                                right,
                                expected,
                            } => {
                                let expected_types = expected.iter()
                                    .map(|type_| format!("{type_:?}"))
                                    .collect::<Vec<String>>()
                                    .join("`, `");

                                let text = format!("Expected any of `{expected_types}`, but got `{:?}` and `{:?}`", left.value_type, right.value_type);

                                let label =match (left.is_valid, right.is_valid) {
                                    (true, false) => Label {
                                            text: Some("Invalid right operand".into()),
                                            span: right.location,
                                    },
                                    (false, true) => Label {
                                            text: Some("Invalid left operand".into()),
                                            span: left.location,
                                    },
                                    (false, false) => {
                                        let location = SrcSpan {
                                            start: left.location.start,
                                            end: right.location.end,
                                        };

                                        Label {
                                            text: Some("Invalid operands".into()),
                                            span: location,
                                        }
                                    },
                                    _ => unreachable!()
                                };

                                diagnostics.push(Diagnostic {
                                    title: "Type mismatch".into(),
                                    text,
                                    level: Level::Error,
                                    location: Some(Location {
                                        src: &src,
                                        path: path.clone(),
                                        label,
                                        extra_labels: vec![]
                                    }),
                                });
                            }
                        };
                    });

                diagnostics
            },
            Error::StdIo { err, } => {
                vec![Diagnostic {
                    title: "Standard IO error".into(),
                    text: format!("{err}"),
                    level: Level::Error,
                    location: None,
                }]
            }
    }}
}