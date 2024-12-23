use std::fmt::Display;

use crate::{
    lexer::{prelude::{LexResult, Token}, table_element::{DisplayTable, TableElement}}, 
    parser::prelude::{parse_error, InfixParse, Parse, ParseErrorType, Precedence}, 
    utils::prelude::SrcSpan
};

pub trait Postfix {
    fn postfix(&self) -> String;
}

#[derive(Debug)]
pub struct Parsed {
    pub module: Module,
    pub table: Vec<TableElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub program: Program
}

// program -> begin var <statement> {; <statement> } end
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Program {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        parser.skip_newline();
        let (start, mut end) = match parser.expect_one(Token::Begin) {
            Ok((start, _)) => {
                parser.skip_newline();
                match parser.expect_one(Token::Var) {
                    Ok((_, end)) => (start, end),
                    Err(err) => return parse_error(ParseErrorType::ExpectedVar, err.span)
                }
            },
            Err(err) => return parse_error(ParseErrorType::ExpectedBegin, err.span)
        };

        let mut statements = vec![Statement::Declaration(Declaration::parse(parser, None)?)];
        let mut is_ended = false;

        while let Ok(_) = parser.expect_one(Token::Semicolon) {
            println!("prog1 | {:?}, {:?}", parser.current_token, parser.next_token);
            match Statement::parse(parser, None) {
                Ok(stmt) => statements.push(stmt),
                Err(parse_err) => match &parse_err.error {
                    ParseErrorType::UnexpectedToken { 
                        token, 
                        .. 
                    } => match token {
                        Token::End => return parse_error(
                            ParseErrorType::UnexpectedSemicolonBeforeEnd, 
                            parse_err.span
                        ),
                        Token::Eof => return parse_error(
                            ParseErrorType::UnexpectedEof,
                            parse_err.span
                        ),
                        _ => return Err(parse_err)
                    },
                    _ => return Err(parse_err)
                }
            }
            println!("prog2 | {:?}, {:?}", parser.current_token, parser.next_token);
        };

        println!("prog3 | {:?}, {:?}", parser.current_token, parser.next_token);

        if matches!(parser.current_token, Some((_, Token::End, _))) {
            end = parser.current_token.as_ref().unwrap().2;
            is_ended = true;
        }

        if !is_ended {
            if matches!(parser.current_token, Some(_)) {
                let end = statements.last().unwrap().location().end;
                return parse_error(
                    ParseErrorType::MissingSemicolon, 
                    SrcSpan { start: end, end: end + 1 }
                )
            }

            return parse_error(
                ParseErrorType::ExpectedEnd,
                SrcSpan { start: end, end: end + 1 }
            )
        }

        Ok(Self {
            statements,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let statements = self.statements.iter()
            .map(|statement| format!("{}", statement))
            .collect::<Vec<String>>();

        write!(f, "begin {} end", statements.join("; "))
    }
}

// statement -> (<declaration> | <operator>)
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declaration(Declaration),
    Operator(Operator),
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Statement {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        parser.skip_newline();
        let res = match parser.current_token {
            Some((_, Token::Var, _)) => Self::Declaration(Declaration::parse(parser, None)?),
            Some(_) => Self::Operator(Operator::parse(parser, None)?),
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        Ok(res)  
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declaration(declaration) => write!(f, "{declaration}"),
            Self::Operator(operation) => write!(f, "{operation}")
        }
    }
}

impl Statement {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Declaration(decl) => decl.location,
            Self::Operator(op) => op.location()
        }
    }
}

// identifiers -> <identifier> {, <identifier> } : <type>
#[derive(Debug, Clone, PartialEq)]
pub struct Identifiers {
    pub names: Vec<Identifier>,
    pub names_type: IdentifierType
}

impl Display for Identifiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = self.names.iter()
            .map(|name| format!("{name}"))
            .collect::<Vec<String>>();

        write!(f, "{}: {}", names.join(", "), self.names_type)
    }
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IdentifierType {
    Float, 
    Int,     
    Bool 
}

impl Display for IdentifierType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident_type = match self {
            Self::Float => "@",
            Self::Int => "#",
            Self::Bool => "&"
        };

        write!(f, "{ident_type}")
    }
}

impl From<Token> for IdentifierType {
    fn from(value: Token) -> Self {
        match value {
            Token::At => Self::Float,
            Token::Hashtag => Self::Int,
            Token::Ampersand => Self::Bool,
            _ => panic!("Invalid token to identifier type conversion")
        }
    }
}

// declaration -> var { <identifiers> ;}
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub identifiers: Vec<Identifiers>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Declaration {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, mut end) = parser.expect_one(Token::Var)?;

        let mut identifiers_vec = vec![];

        while let Ok(ident) = parser.expect_ident() {
            let mut names = vec![Identifier::from(ident)];

            while let Ok(_) = parser.expect_one(Token::Comma) {
                names.push(parser.expect_ident()?.into());
            }

            let (_, names_type, _end) = parser.parse_type_annotation(Token::Colon)?;

            end = match parser.expect_one(Token::Semicolon) {
                Ok((_, end)) => end,
                Err(_) => return parse_error(
                    ParseErrorType::MissingSemicolon, 
                    SrcSpan { start: _end, end: _end }
                )
            };

            identifiers_vec.push(Identifiers {
                names,
                names_type
            });
        }

        Ok(Declaration {
            identifiers: identifiers_vec,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identifiers = self.identifiers.iter()
            .map(|idents| idents.to_string())
            .collect::<Vec<String>>();

        if identifiers.len() > 0 {
            write!(f, "var {};", identifiers.join("; "))
        } else {
            write!(f, "var")
        }
    }
}

// operator -> <nested> | <assignment> | <conditional> | <fixed_loop> | <conditional_loop> | <input> | <output>
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Nested(Nested),
    Assignment(Assignment),
    Conditional(Conditional),
    FixedLoop(FixedLoop),
    ConditionalLoop(ConditionalLoop),
    Input(Input),
    Output(Output),
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Operator {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        parser.skip_newline();
        let res = match &parser.current_token {
            Some((start, token, end)) => match token {
                Token::LSBracket => Self::Nested(Nested::parse(parser, None)?),
                Token::Ident(_) => Self::Assignment(Assignment::parse(parser, None)?),
                Token::If => Self::Conditional(Conditional::parse(parser, None)?),
                Token::For => Self::FixedLoop(FixedLoop::parse(parser, None)?), 
                Token::While => Self::ConditionalLoop(ConditionalLoop::parse(parser, None)?),
                Token::Enter => Self::Input(Input::parse(parser, None)?),
                Token::Displ => Self::Output(Output::parse(parser, None)?),
                _ => return parse_error(
                    ParseErrorType::UnexpectedToken { 
                        token: token.clone(), 
                        expected: vec!["Any operator".to_string()] 
                    },
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        Ok(res)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nested(nested) => write!(f, "{nested}"),
            Self::Assignment(assignment) => write!(f, "{assignment}"),
            Self::Conditional(conditional) => write!(f, "{conditional}"),
            Self::FixedLoop(loop_) => write!(f, "{loop_}"),
            Self::ConditionalLoop(loop_) => write!(f, "{loop_}"),
            Self::Input(input) => write!(f, "{input}"),
            Self::Output(output) => write!(f, "{output}")
        }
    }
}

impl Operator {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Nested(nested) => nested.location,
            Self::Assignment(assignment) => assignment.location,
            Self::Conditional(conditional) => conditional.location,
            Self::FixedLoop(loop_) => loop_.location,
            Self::ConditionalLoop(loop_) => loop_.location,
            Self::Input(input) => input.location,
            Self::Output(output) => output.location
        }
    }
}

// nested -> [ <operator> {(: | \n ) <operator> } ]
#[derive(Debug, Clone, PartialEq)]
pub struct Nested {
    pub operators: Vec<Operator>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Nested {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, mut end) = parser.expect_one(Token::LSBracket)?;

        let mut operators = vec![Operator::parse(parser, None)?];
        let mut is_ended = false;

        while let Some((start, token, _end)) = parser.current_token.take() {
            if matches!(token, Token::Colon | Token::Newline) {
                parser.step();
                continue;
            }

            if matches!(token, Token::RSBracket) {
                is_ended = true;
                end = _end;
                parser.step();
                break;
            }

            parser.current_token = Some((start, token, _end));

            operators.push(Operator::parse(parser, None)?);


            match (&parser.current_token, &parser.next_token) {
                (
                    Some((start, Token::Colon, end)), 
                    Some((_, Token::RSBracket, _))
                ) => return parse_error(
                    ParseErrorType::UnexpectedColonBeforeRSBracket, 
                    SrcSpan { start: *start, end: *end }
                ),
                (Some((_, Token::Colon, _)), _)  => parser.step(),
                (Some((_, Token::Newline, _)), _)  => parser.step(),
                (Some((_, Token::RSBracket, _)), _) => {
                    is_ended = true;
                    end = parser.next_token().unwrap().2;
                    break 
                },
                (None, _) => return parse_error(
                    ParseErrorType::ExpectedColon, 
                    SrcSpan { start: 0, end: 0 }
                ),
                (Some((start, _, _)), _) => return parse_error(
                    ParseErrorType::MissingColon,
                    SrcSpan { start: *start - 1, end: *start }
                ),
            }
        };

        if !is_ended {
            return parse_error(
                ParseErrorType::ExpectedColon,
                SrcSpan { start: end, end: end + 1 }
            )
        }

        Ok(Self {
            operators,
            location: SrcSpan { start, end }
        })
    }
}



impl Display for Nested {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operators = self.operators.iter()
            .map(|operator| format!("{}", operator))
            .collect::<Vec<String>>();

        write!(f, "begin {} end", operators.join("; "))
    }
}

impl Postfix for Nested {
    fn postfix(&self) -> String {
        let operators = self.operators.iter()
            .map(|operator| format!("{}", operator))
            .collect::<Vec<String>>();
        
        format!("{}", operators.join(" "))
    }
}

// assignment -> <identifier> := <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub identifier: Identifier,
    pub value: Expression,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Assignment {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>,
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        // println!("assign");
        let ident = parser.expect_ident()?;
        let start = ident.0;

        parser.expect_one(Token::Assign)?;

        let value = Expression::parse(parser, None)?;
        let end = value.location().end;

        Ok(Self {
            identifier: ident.into(),
            value,
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.identifier, self.value)
    }
}

impl Postfix for Assignment {
    fn postfix(&self) -> String {
        format!("{} {} :=", self.identifier, self.value)
    }
}

// conditional -> if <expression> then <operator> [else <operator>]
#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Expression,
    pub resolution: Box<Operator>,
    pub alternative: Option<Box<Operator>>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Conditional {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, _) = parser.expect_one(Token::If)?;

        let condition = Expression::parse(parser, None)?;
        
        let _ = parser.expect_one(Token::Then)?;

        let resolution = Box::new(Operator::parse(parser, None)?);

        let mut end = resolution.location().end;

        let alternative = match parser.expect_one(Token::Else) {
            Ok((_, _)) => {
                let alternative = Operator::parse(parser, None)?;

                end = alternative.location().end;

                Some(Box::new(alternative))
            },
            Err(_) => None
        };

        let location = SrcSpan { start, end };

        Ok(Self {
            condition,
            resolution,
            alternative,
            location
        })
    }
}


impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}{}", 
            self.condition, 
            self.resolution, 
            if self.alternative.is_some() {
                format!(" else {}", self.alternative.as_ref().unwrap())
            } else {
                "".to_string()
            }
        )
    }
}

impl Postfix for Conditional {
    fn postfix(&self) -> String {
        format!("{} {} if {} {}", 
            self.condition, 
            self.resolution.location().start,
            self.resolution,
            if self.alternative.is_some() {
                let alt = self.alternative.as_ref().unwrap();
                format!("{} else {}", 
                    alt.location().end,
                    alt
                )
            } else {
                "".to_string()
            }
        )
    }
}

// fixed_loop -> for <assignment> val <expression> do <operator> next
#[derive(Debug, Clone, PartialEq)]
pub struct FixedLoop {
    pub assignment: Assignment,
    pub val: Expression,
    // pub step: Option<Expression>,
    pub block: Box<Operator>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for FixedLoop {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, _) = parser.expect_one(Token::For)?;

        let assignment = Assignment::parse(parser, None)?;
        let _ = parser.expect_one(Token::Val);

        let val = Expression::parse(parser, None)?;
        let _ = parser.expect_one(Token::Do);

        let block = Box::new(Operator::parse(parser, None)?);

        let location = SrcSpan { start, end: block.location().end };

        Ok(Self {
            assignment,
            val,
            block,
            location
        })
    }
}

impl Display for FixedLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "for {} val {} {} next",
            self.assignment,
            self.val,
            self.block
        )
    }
}

// conditional_loop -> while  <expression> do <operator> next
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalLoop {
    pub condition: Expression,
    pub block: Box<Operator>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for ConditionalLoop {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, _) = parser.expect_one(Token::While)?;


        let condition = Expression::parse(parser, None)?;
        let _ = parser.expect_one(Token::Do)?;

        let block = Box::new(Operator::parse(parser, None)?);
        let (_, end) = parser.expect_one(Token::Next)?;

        let location = SrcSpan { start, end };

        Ok(Self {
            condition,
            block,
            location
        })
    }
}

impl Display for ConditionalLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {} {}", self.condition, self.block)
    }
}

impl Postfix for ConditionalLoop {
    fn postfix(&self) -> String {
        todo!()
    }
}

// input -> enter <identifier> {space <identifier> }
#[derive(Debug, Clone, PartialEq)]
pub struct Input {
    pub identifiers: Vec<Identifier>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Input {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, _) = parser.expect_one(Token::Enter)?;

        let mut identifiers = vec![Identifier::from(parser.expect_ident()?)];

        while let Ok(tok) = parser.expect_ident() {
            identifiers.push(tok.into());
        }

        let end = identifiers.iter().last().unwrap().location.end;

        let location = SrcSpan { start, end };

        Ok(Self {
            identifiers,
            location
        })
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let identifiers = self.identifiers.iter()
            .map(|ident| ident.value.clone())
            .collect::<Vec<String>>();

        write!(f, "readln {}", identifiers.join(", "))
    }
}

impl Postfix for Input {
    fn postfix(&self) -> String {
        let identifiers = self.identifiers.iter()
            .map(|ident| ident.value.clone())
            .collect::<Vec<String>>();

        format!("{} readln", identifiers.join(" readln "))
    }
}

// output -> displ <expression> {, <expression> }
#[derive(Debug, Clone, PartialEq)]
pub struct Output {
    pub expressions: Vec<Expression>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Output {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, _) = parser.expect_one(Token::Displ)?;

        let mut expressions = vec![Expression::parse(parser, None)?];

        while let Ok(_) = parser.expect_one(Token::Comma) {
            expressions.push(Expression::parse(parser, None)?);
        }

        let end = expressions.iter().last().unwrap().location().end;

        let location = SrcSpan { start, end };

        Ok(Self {
            expressions,
            location
        })    
    }
}

impl Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expressions = self.expressions.iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<String>>();

        write!(f, "writeln {}", expressions.join(", "))
    }
}

impl Postfix for Output {
    fn postfix(&self) -> String {
        let expressions = self.expressions.iter()
            .map(|expr| expr.postfix())
            .collect::<Vec<String>>();

        format!("{} writeln", expressions.join(" writeln "))
    }
}

// expression -> <identifier> | <infix> | <prefix> | <primitive> | "(" <expression> ")" 
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Infix(Infix),
    Prefix(Prefix),
    Primitive(Primitive),
    Nested {
        expression: Box<Expression>,
        location: SrcSpan
    }
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Expression {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        println!("{:?}, {:?}", parser.current_token, parser.next_token);
        let mut expr = match &parser.current_token {
            Some((start, token, end)) => match token {
                Token::Ident(_) => {
                    let (start, ident, end) = parser.expect_ident()?;

                    Self::Identifier(Identifier::from((start, ident, end)))
                },
                Token::Tilda => Self::Prefix(Prefix::parse(parser, None)?),
                Token::Int(_)
                | Token::Float(_) 
                | Token::True 
                | Token::False => Self::Primitive(Primitive::parse(parser, None)?),
                Token::LParen => {
                    let (start, _) = parser.expect_one(Token::LParen)?;

                    let expression = Box::new(Expression::parse(parser, None)?);

                    let (_, end) = parser.expect_one(Token::RParen)?;

                    Self::Nested {
                        expression,
                        location: SrcSpan { start, end }
                    }
                }
                _ => return parse_error(
                    ParseErrorType::UnexpectedToken {
                        token: token.clone(),
                        expected: vec!["an Identifier, `!`, Number or `(`".to_string()]
                    },
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        while parser.current_token.as_ref()
            .is_some_and(|token| token.1 != Token::Semicolon) && 
            precedence.unwrap_or(Precedence::Lowest) < parser.current_precedence() 
        {
            println!("hey");
            expr = match &parser.current_token {
                Some((_, next_token, _)) => match next_token {
                    Token::Plus | Token::Minus | Token::Div | 
                    Token::Mult | Token::Equal | Token::NotEqual | 
                    Token::LessThan | Token::GreaterThan | 
                    Token::LessThanOrEqual | Token::And | Token::Or |
                    Token::GreaterThanOrEqual => {
                        Self::Infix(Infix::parse(parser, expr, precedence)?)
                    },
                    _ => break
                },
                None => break
            }
        }

        Ok(expr) 
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
            Self::Primitive(primitive) => write!(f, "{primitive}"),
            Self::Nested { expression, .. } => write!(f, "({expression})")
        }
    }
}

impl Postfix for Expression {
    fn postfix(&self) -> String {
        match self {
            Self::Identifier(ident) => format!("{ident}"),
            Self::Infix(infix) => format!("{}", infix.postfix()),
            Self::Prefix(prefix) => format!("{}", prefix.postfix()),
            Self::Primitive(primitive) => format!("{primitive}"),
            Self::Nested { expression, .. } => format!("{}", expression.postfix())
        }
    }
}

impl Expression {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Identifier(ident) => ident.location,
            Self::Infix(infix) => infix.location,
            Self::Prefix(prefix) => prefix.location,
            Self::Primitive(primitive) => primitive.location(),
            Self::Nested { expression, .. } => expression.location()
        }
    }
}

// identifier -> <letter> | { (<letter> | <number>) }
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub value: String,
    pub location: SrcSpan
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<(u32, String, u32)> for Identifier {
    fn from(value: (u32, String, u32)) -> Self {
        Identifier {
            value: value.1,
            location: SrcSpan { start: value.0, end: value.2 }
        }
    }
}

// infix -> <expression> <operator> <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> InfixParse<T> for Infix {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        left: Expression, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let precedence = parser.current_precedence();

        let SrcSpan { start, .. } = left.location();

        let operator = match &parser.current_token {
            Some((start, token, end)) => match token {
                token if token.is_operator() => {
                    parser.next_token().unwrap().1
                },
                _ => return parse_error(
                    ParseErrorType::ExpectedOperator,
                    SrcSpan { start: *start, end: *end }
                )
            },
            None => return parse_error(
                ParseErrorType::UnexpectedEof, 
                SrcSpan { start: 0, end: 0 }
            )
        };

        let right = Expression::parse(parser, Some(precedence))?;

        let SrcSpan { end, .. } = right.location();

        Ok(Self {
            left: Box::new(left), 
            operator,
            right: Box::new(right), 
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator.as_literal(), self.right)
    }
}

impl Postfix for Infix {
    fn postfix(&self) -> String {
        format!("{} {} {}", self.left.postfix(), self.right.postfix(), self.operator.as_literal())
    }
}

// prefix -> <unary_operation> <expression>
#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub expression: Box<Expression>,
    pub location: SrcSpan
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Prefix {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let (start, token, _) = parser.next_token().unwrap();

        let expression = Expression::parse(parser, Some(Precedence::Prefix))?;
        let end = expression.location().end;

        Ok(Self {
            operator: token, 
            expression: Box::new(expression),
            location: SrcSpan { start, end }
        })
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator.as_literal(), self.expression)
    }
}

impl Postfix for Prefix {
    fn postfix(&self) -> String {
        format!("{} {}", self.expression.postfix(), self.operator.as_literal())
    }
}

// primitive -> <string> | <float> | <int> | <bool>
#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int {
        value: i64,
        location: SrcSpan
    },
    Float {
        value: f64,
        location: SrcSpan
    },
    Bool {
        value: bool,
        location: SrcSpan
    }
}

impl<T: Iterator<Item = LexResult> + DisplayTable> Parse<T> for Primitive {
    fn parse(
        parser: &mut crate::parser::prelude::Parser<T>, 
        _precedence: Option<Precedence>
    ) -> Result<Self, crate::parser::prelude::ParseError> {
        let span = parser.next_token();

        match span {
            Some((start, token, end)) => match token {
                Token::Int(value) => {
                    Ok(Self::Int {
                        value,
                        location: SrcSpan { start, end }
                    })
                }
                Token::Float(value) => {
                    Ok(Self::Float {
                        value,
                        location: SrcSpan { start, end }
                    })
                },
                Token::True => {
                    Ok(Self::Bool {
                        value: true,
                        location: SrcSpan { start, end }
                    })
                },
                Token::False => {
                    Ok(Self::Bool {
                        value: false,
                        location: SrcSpan { start, end }
                    })
                },
                _ => todo!("parse err Unexpected token, but should be unreachable"),
            },
            None => todo!("parse err Unexpected EOF, but should be unreachable"),
        }    
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{value}"),
            Self::Float { value, .. } => write!(f, "{value}"),
            Self::Bool { value, .. } => write!(f, "{value}")
        }
    }
}

impl Primitive {
    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Int { location, .. } |
            Self::Float { location, .. } |
            Self::Bool { location, .. } => *location
        }
    }
}