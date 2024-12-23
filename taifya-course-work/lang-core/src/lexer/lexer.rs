use super::error::{LexicalError, LexicalErrorType};
use super::table_element::{DisplayTable, TableElement};
use super::token::Token;
use std::collections::HashMap;
use std::fmt::Display;
use crate::utils::prelude::SrcSpan;

pub type Spanned = (u32, Token, u32);
pub type LexResult = std::result::Result<Spanned, LexicalError>;

pub fn str_to_keyword(word: &str) -> Option<Token> {
	Some(match word {
		"var" => Token::Var,
		"begin" => Token::Begin,
		"end" => Token::End,
		"if" => Token::If,
		"then" => Token::Then,
		"else" => Token::Else,
		"for" => Token::For,
		"do" => Token::Do,
		"while" => Token::While,
		"next" =>  Token::Next,
		"val" => Token::Val,

		"NE" => Token::NotEqual,
		"EQ" => Token::Equal,
		"LT" => Token::LessThan,
		"LE" => Token::LessThanOrEqual,
		"GT" => Token::GreaterThan,
		"GE" => Token::GreaterThanOrEqual,

		"plus" => Token::Plus,
		"min" => Token::Minus,
		"or" => Token::Or,

		"mult" => Token::Mult,
		"div" => Token::Div,
		"and" => Token::And,

		"assign" => Token::Assign,

		"true" => Token::True,
		"false" => Token::False,

		"enter" => Token::Enter,
		"displ" => Token::Displ,

	
		_ => return None
	})
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub enum NumberType {
	Binary,
	Octal,
	Int,
	Hex,
	Float,
}

#[derive(Debug)]
pub struct Lexer<T: Iterator<Item = (u32, char)>> {
	position: u32,
	next_position: u32,
	ch: Option<char>,
	next_ch: Option<char>,
	input: T,

	ident_map: HashMap<String, u32>,
	number_map: HashMap<String, u32>,
	table: Vec<TableElement>,
}

impl<T: Iterator<Item = (u32, char)>> Display for Lexer<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, 
			"Lexer {{\n\tposition: {},\n\tnext_position: {},\n\tch: {:?}, next_ch: {:?}\n}}", 
			self.position, self.next_position, self.ch, self.next_ch
		)
	}
}

impl<T: Iterator<Item = (u32, char)>> Lexer<T> {
	pub fn new(input: T) -> Self {
        let mut lexer = Self {
            position: 0,
            next_position: 0,
            ch: None,
			next_ch: None,
            input,

			ident_map: HashMap::new(),
			number_map: HashMap::new(),
			table: vec![]
        };

        lexer.next_char();
        lexer.next_char();

        return lexer;
    }

    pub fn next_token(&mut self) -> LexResult {
		let span  = match self.ch {
			Some(ch) => match ch {
				'%'  => return self.lex_comment(),
				'[' => self.eat_one_char(Token::LSBracket),
				']' => self.eat_one_char(Token::RSBracket),
				'(' => self.eat_one_char(Token::LParen),
				')' => self.eat_one_char(Token::RParen),
				':' => self.eat_one_char(Token::Colon),
				';' => self.eat_one_char(Token::Semicolon),
				'~' => self.eat_one_char(Token::Tilda),
				',' => self.eat_one_char(Token::Comma),
				'#' => self.eat_one_char(Token::Hashtag),
				'@' => self.eat_one_char(Token::At),
				'&' => self.eat_one_char(Token::Ampersand),
				'=' => self.eat_one_char(Token::Assign),
				'a'..='z' | 'A'..='Z' => {
					return Ok(self.lex_ident());
				},
				'0'..='9' | '.' => {
					return self.lex_number();
				},
				'\n' | ' ' | '\t' | '\x0C' | '\r' => {
					let tok_start = self.position;
					let _ = self.next_char();
					let tok_end = self.position;

					return if ch == '\n' {
						Ok((tok_start, Token::Newline, tok_end))
					} else {
						self.next_token()
					}
				}
				c => {
					let location = self.position as u32;
					return Err(LexicalError {
						error: LexicalErrorType::UnrecognizedToken { tok: c },
						location: SrcSpan {
							start: location,
							end: location,
						},
					});
				}
			},
			None => {
				self.eat_one_char(Token::Eof)
			}
		};

		Ok(span)
    }

	fn next_char(&mut self) -> Option<char> {
		let ch = self.ch;
		
		let next = match self.input.next() {
			Some((pos, ch)) => {
				self.position = self.next_position;
				self.next_position = pos;

				Some(ch)
			},
			None => {
				self.position = self.next_position;
				self.next_position += 1;

				None
			}
		};

		self.ch = self.next_ch;
		self.next_ch = next;

		ch
	}

	fn eat_one_char(&mut self, token: Token) -> Spanned {
		let start_pos = self.position;
		self.next_char();
		let end_pos = self.position;

		let table_el = token.as_table_literal(&mut self.ident_map, &mut self.number_map);

		if table_el.is_some() {
			self.table.push(table_el.unwrap());
		}

		(start_pos as u32, token, end_pos as u32)
	}

	fn lex_ident(&mut self) -> Spanned {
        let start_pos = self.position;
		let mut ident = String::new();

		loop {
			match self.ch {
				Some(ch) if ch.is_ascii_alphanumeric() => ident.push(self.next_char().unwrap()),
				_ => break
			}
		}

        let end_pos = self.position;

        if let Some(tok) = str_to_keyword(&ident) {
			let table_el = tok.as_table_literal(&mut self.ident_map, &mut self.number_map);

			if table_el.is_some() {
				self.table.push(table_el.unwrap());
			}

           	(start_pos as u32, tok, end_pos as u32)
        } else {
			let tok = Token::Ident(ident);

			let table_el = tok.as_table_literal(&mut self.ident_map, &mut self.number_map);

			if table_el.is_some() {
				self.table.push(table_el.unwrap());
			}

            (start_pos as u32, tok, end_pos as u32)
        }
	}

	fn lex_number(&mut self) -> LexResult {
		let start_pos = self.position;
		
		let mut value = String::from("");

		let mut has_period = false;
		let mut has_hex = false;
		let mut has_exponent = false;
		let mut has_number_before_exponent = false;
		let mut has_number_after_exponent = false;

		fn is_radix(ch: char) -> bool {
			match ch {
				'B' | 'b' => true,
				'O' | 'o' => true,
				'D' | 'd' => true,
				'H' | 'h' => true,
				_ => false
			}
		}

		loop {
			// println!("{:?}", self.ch);
			match self.ch {
				Some(ch) if ch.is_ascii_digit() => {
					value.push(self.next_char().unwrap());
				},
				Some(ch) if matches!(ch, 'a'..='f' | 'A'..='F') => {
					if !has_exponent && matches!(ch, 'e' | 'E') {
						has_number_before_exponent = value.chars()
							.nth(value.len() - 1)
							.unwrap()
							.is_ascii_digit();

						has_exponent = true;

						value.push(self.next_char().unwrap());

						if matches!(self.ch, Some('+') | Some('-')) {
							value.push(self.next_char().unwrap());
						}

						has_number_after_exponent = match self.ch {
							Some(ch) if ch.is_ascii_digit() => true,
							_ => false
						};
					} else {
						if !has_hex {
							has_hex = true;
						}
	
						value.push(self.next_char().unwrap());
					}
				},
				Some(ch) if is_radix(ch) => {
					value.push(self.next_char().unwrap());
				},
				Some(ch) if ch == '.' => {
					if has_period {
						let _ = self.next_char().unwrap();

						let end_pos = self.position;

						return Err(LexicalError {
							error: LexicalErrorType::MultipleFloatingPoints,
							location: SrcSpan::from(start_pos, end_pos)
						});
					} else {
						has_period = true;
						value.push(self.next_char().unwrap());
					}
				},
				Some(_)
				| None => break
			}
		}

		let end_pos = self.position;

		let last_char = value.chars().nth(value.len() - 1).unwrap();

		let expected_type = match last_char {
			'B' | 'b' => NumberType::Binary,
			'O' | 'o' => NumberType::Octal,
			'H' | 'h' => NumberType::Hex,
			_ if has_period || has_exponent => {
				if has_exponent {
					if !has_number_before_exponent {
						return Err(LexicalError {
							error: LexicalErrorType::MissingDigitBeforeExponent,
							location: SrcSpan::from(start_pos, end_pos)
						});
					}
					if !has_number_after_exponent {
						return Err(LexicalError {
							error: LexicalErrorType::MissingDigitsAfterExponent,
							location: SrcSpan::from(start_pos, end_pos)
						});
					}
				}

				NumberType::Float
			},
			'D' | 'd' | _ => NumberType::Int
		};

		if is_radix(last_char) {
			let _  = value.pop();
		}

		let token = match expected_type {
			NumberType::Binary => match i64::from_str_radix(&value, 2) {
				Ok(value) => Token::Int(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Octal => match i64::from_str_radix(&value, 8) {
				Ok(value) => Token::Int(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Hex => match i64::from_str_radix(&value, 16) {
				Ok(value) => Token::Int(value),
				Err(_) => return Err(LexicalError {
					error: if !has_period { 
						LexicalErrorType::DigitOutOfRadix 
					} else { 
						LexicalErrorType::UnsupportedFloatingPoint 
					},
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Int => match i64::from_str_radix(&value, 10) {
				Ok(value) => Token::Int(value),
				Err(_) => return Err(LexicalError {
					error: LexicalErrorType::DigitOutOfRadix,
					location: SrcSpan::from(start_pos, end_pos)
				})
			},
			NumberType::Float => match value.parse::<f64>() {
				Ok(value) => Token::Float(value),
				Err(_) => return Err(LexicalError {
					error: LexicalErrorType::DigitOutOfRadix,
					location: SrcSpan::from(start_pos, end_pos)
				})
			}
		};

		let table_el = token.as_table_literal(&mut self.ident_map, &mut self.number_map);

		if table_el.is_some() {
			self.table.push(table_el.unwrap());
		}

		Ok((start_pos as u32, token, end_pos as u32))
	}

	fn lex_comment(&mut self) -> LexResult {
		let start_pos = self.position;
		
		self.next_char(); // skip procent

		while Some('%') != self.ch {
			if self.next_char().is_none() {
				return Err(LexicalError {
					error: LexicalErrorType::MissingCommentEnd,
					location: SrcSpan::from(start_pos, self.position)
				})
			};
		}

		self.next_char(); // skip procent
		
		let end_pos = self.position;

		Ok((start_pos as u32, Token::Comment, end_pos as u32))
	}
}

impl<T: Iterator<Item = (u32, char)>> DisplayTable for Lexer<T> {
	fn get_table_listing(&self) -> &Vec<TableElement> {
		&self.table
	}
}

impl<T: Iterator<Item = (u32, char)>> Iterator for Lexer<T> {
	type Item = LexResult;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next_token();

		Some(token)
	}
}