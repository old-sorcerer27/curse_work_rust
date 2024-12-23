use std::io::Write;

use lang_core::lexer::prelude::{Lexer, Token};

const PROMPT: &str = ">> ";

pub fn start() -> std::io::Result<()> {
	let stdin = std::io::stdin();
	
	loop {
		let mut input = String::from("");

		print!("{}", PROMPT);
		std::io::stdout().flush()?;
		stdin.read_line(&mut input)?;

		if let Some('\n') = input.chars().next_back() {
			input.pop();
		}
		if let Some('\r') = input.chars().next_back() {
			input.pop();
		}

		match input.as_str() {
			"" => {},
			".exit" => return Ok(()),
			_ => {
				let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

				while let Some(res) = lexer.next() {
                    match res {
                        Ok((_, token, _)) => {
                            println!("{:?}", token);
        
                            if token == Token::Eof {
                                break;
                            }
                        },
                        Err(err) => {
                            let details = err.details();
                            let location = err.location;
                            println!("[at {}] Lexical Error: {}", location.start, details.0);
                            if details.1.len() > 0 {
                                println!("{}", details.1.join("\n"));
                            }
							break;
                        }
                    }
				}
			}
		}
	}
}