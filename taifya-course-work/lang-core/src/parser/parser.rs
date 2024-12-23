use crate::{lexer::prelude::{LexResult, LexicalError, Lexer, Spanned, Token}, utils::prelude::SrcSpan};
use super::error::{ParseError, ParseErrorType};
use super::ast::{Parsed, Program, Expression, Module, IdentifierType};

pub trait Parse<T: Iterator<Item = LexResult>>
    where Self: Sized,
{
    fn parse(
        parser: &mut Parser<T>, 
        precedence: Option<Precedence>
    ) -> Result<Self, ParseError>;
}

pub trait InfixParse<T: Iterator<Item = LexResult>>
    where Self: Sized,
{
    fn parse(
        parser: &mut Parser<T>, 
        left: Expression, 
        precedence: Option<Precedence>
    ) -> Result<Self, ParseError>;
}

pub struct Parser<T: Iterator<Item = LexResult>> {
    pub current_token: Option<Spanned>,
    pub next_token: Option<Spanned>,
    pub comments: Vec<SrcSpan>,
    pub lex_errors: Vec<LexicalError>,

    tokens: T,
}

impl<T: Iterator<Item = LexResult>> Parser<T> {
    pub fn new(input: T) -> Self {
        let mut parser = Self {
            current_token: None,
            next_token: None,
            comments: vec![],
            lex_errors: vec![],

            tokens: input,
        };

        parser.step();
        parser.step();

        parser
    }

    pub fn step(&mut self) {
        let _ = self.next_token();
    }

    pub fn next_token(&mut self) -> Option<Spanned> {
        let t = self.current_token.take();
        let mut next = None;

        loop {
            match self.tokens.next() {
                Some(Ok((start, Token::Comment, end))) => {
                    self.comments.push(SrcSpan { start, end })
                },
                Some(Err(err)) => {
                    self.lex_errors.push(err);

                    break;
                },
                Some(Ok(tok)) => {
                    next = Some(tok);

                    break;
                },
                None => {
                    break;
                }
            }
        }

        self.current_token = self.next_token.take();
        self.next_token = next.take();

        t
    }

    pub fn skip_newline(&self) {
        while let Some((_, Token::Newline, _)) = self.current_token {
            self.step();
        }
    }

    pub fn current_precedence(&self) -> Precedence {
        match &self.current_token {
            Some((_, token, _)) => Precedence::from(token),
            None => Precedence::Lowest
        }
    }

    pub fn parse(&mut self) -> Result<Parsed, ParseError> {
        let program = Program::parse(self, None);

        if self.lex_errors.len() > 0 {
            let location = self.lex_errors[0].location;

            return parse_error(
                ParseErrorType::LexError { 
                    error: self.lex_errors[0].clone()
                }, 
                SrcSpan { start: location.start, end: location.end }
            );
        }

        let module = Module {
            name: "".into(),
            program: program?
        };

        Ok(Parsed {
            module,
            comments: Default::default()
        })
    }

    pub fn expect_one(&mut self, token: Token) -> Result<(u32, u32), ParseError> {
        match self.current_token.take() {
            Some((start, tok, end)) if tok == token => {
                self.step();
                Ok((start, end))
            },
            Some(t) => {
                let (start, tok, end) = t.clone();
                self.current_token = Some(t);

                parse_error(
                    ParseErrorType::UnexpectedToken {
                        token: tok,
                        expected: vec![token.as_literal()],
                    },
                    SrcSpan { start, end }
                )
            },
            None => {
                self.current_token = None;

                parse_error(
                    ParseErrorType::UnexpectedEof,
                    SrcSpan { start: 0, end: 0 }
                )
            }
        }
    }

    pub fn expect_ident(&mut self) -> Result<(u32, String, u32), ParseError> {
        match self.current_token.take() {
            Some((start, Token::Ident(value), end)) => {
                self.step();
                Ok((start, value, end))
            },
            Some(t) => {
                let (start, _, end) = t.clone();
                self.current_token = Some(t);

                parse_error(
                    ParseErrorType::ExpectedIdent,
                    SrcSpan { start, end }
                )
            },
            None => {
                self.current_token = None;

                parse_error(
                    ParseErrorType::UnexpectedEof,
                    SrcSpan { start: 0, end: 0 }
                )
            }
        }
    }

    pub fn parse_type_annotation(&mut self, start_token: Token) -> Result<(u32, IdentifierType, u32), ParseError> {
        let (start, end) = self.expect_one(start_token)?;

        match self.current_token.take() {
            Some((start, token, end)) if token.is_variable_type() => {
                self.step();
                Ok((start, IdentifierType::from(token), end))
            },
            tok => {
                self.current_token = tok;

                parse_error(
                    ParseErrorType::ExpectedType,
                    SrcSpan { start, end }
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix
}

pub fn parse_module(src: &str) -> Result<Parsed, ParseError> {
    let lexer = Lexer::new(src.char_indices().map(|(i, c)| (i as u32, c)));
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse()?;
    
    Ok(parsed)
}

pub fn parse_module_from_stream(stream: impl Iterator<Item = char>) -> Result<Parsed, ParseError> {
    let lexer = Lexer::new(stream
        .scan(0, |pos, c| { 
            *pos += c.len_utf8() as u32; 
            Some((*pos - c.len_utf8() as u32, c)) 
        })
    );
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse()?;
    
    Ok(parsed)
}



impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LessThan | Token::GreaterThan | 
            Token::LessThanOrEqual | Token::GreaterThanOrEqual => Self::LessGreater,
            Token::Plus | Token::Minus | Token::Or => Self::Sum,
            Token::Div | Token::Mult | Token::And => Self::Product,
            Token::Assign => Self::Assign,
            _ => Self::Lowest,
        }
    }
}

pub fn parse_error<T>(error: ParseErrorType, span: SrcSpan) -> Result<T, ParseError> {
    Err(ParseError { error, span })
}