use super::prelude::{Lexer, LexicalError, LexicalErrorType, Token};

#[test]
fn test_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        10
        125d
        196D
        01010b
        1001B
        5424o
        12367O
        1A3h
        1E5H
        10e5
        10.4E5
        .0e-5
        1.5
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let tokens = vec![
        Token::Int(10),
        Token::Int(125),
        Token::Int(196),
        Token::Int(10),
        Token::Int(9),
        Token::Int(2836),
        Token::Int(5367),
        Token::Int(419),
        Token::Int(485),
        Token::Float(1000000.0),
        Token::Float(1040000.0),
        Token::Float(0.0),
        Token::Float(1.5),
    ];

    for (idx, token) in tokens.iter().enumerate() {
        let (_, next_token, _) = match lexer.next_token() {
            Ok(next_token) => next_token,
            Err(err) => {
                println!("stopped at {token:?} ({idx})");
                panic!("{err:?}")
            }
        };

        assert_eq!(
            *token, next_token, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            next_token, token, idx
        );
    }

    Ok(())
}

#[test]
fn test_invalid_numbers() -> std::result::Result<(), LexicalError> {
    let input = r#"
        1.eh
        1e+5e
        123b
        789o
        1A3
        1B6.F4h
        435.54o
        0101000.101b
        1.2.
        10.e5
        .0e
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let fails = vec![
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::DigitOutOfRadix,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::UnsupportedFloatingPoint,
        LexicalErrorType::MultipleFloatingPoints,
        LexicalErrorType::MissingDigitBeforeExponent,
        LexicalErrorType::MissingDigitsAfterExponent
    ];

    for (idx, fail) in fails.iter().enumerate() {
        let err = match lexer.next_token() {
            Err(err) => err,
            Ok(value) => {
                panic!("Stopped at {fail:?} ({idx}). Expected Err but got Ok({value:?})");
            }
        };

        assert_eq!(
            *fail, err.error, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            fail, err.error, idx
        );
    }

    Ok(())
}

#[test]
fn test_input() -> std::result::Result<(), LexicalError> {
    let input = r#"
    begin
        var
            var
            a : #;
            b , c : @;
            d: &;;

        a assign 10;
        b assign 13e+4;
        c assign 2.6;
        d assign true;
        % комментарий %

        a plus c;
        10 min 5;
        .5 div 5.0;
        2.104e5 mult 2;
        c NE true;
        c EQ false;

        for e assign 5 val e LT 10 do [
            e assign e plus 1
            displ(4 plus 13,6 div 3) : enter(e a)
        ];

        if ~d then displ(true) else displ(false) end;

        while a GT 5 do  a assign a min 1 next

    end
    "#;

    let mut lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));

    let tokens = vec![
        Token::Begin,
        Token::Newline,
        Token::Var,
        Token::Newline,
        Token::Var,
        Token::Newline,

        Token::Ident(String::from("a")),
        Token::Colon,
        Token::Hashtag,
        Token::Semicolon,
        Token::Newline,

        Token::Ident(String::from("b")),
        Token::Comma,
        Token::Ident(String::from("c")),
        Token::Colon,
        Token::At,
        Token::Semicolon,
        Token::Newline,

        Token::Ident(String::from("a")),
        Token::Colon,
        Token::Ampersand,
        Token::Semicolon,
        Token::Semicolon,
        Token::Newline,
        Token::Newline,

        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        Token::Newline,

        Token::Ident(String::from("b")),
        Token::Assign,
        Token::Int(13e+4),
        Token::Semicolon,
        Token::Newline,
        
        Token::Ident(String::from("c")),
        Token::Assign,
        Token::Float(2.4),
        Token::Semicolon,
        Token::Newline,

        Token::Ident(String::from("d")),
        Token::Assign,
        Token::True,
        Token::Semicolon,
        Token::Newline,
        Token::Comment,
        Token::Newline,
        //////////
        Token::Newline,
        Token::Ident(String::from("a")),
        Token::Plus,
        Token::Ident(String::from("c")),
        Token::Semicolon,
        Token::Newline,

        Token::Int(10),
        Token::Minus,
        Token::Int(5),
        Token::Semicolon,
        Token::Newline,

        Token::Float(0.5),
        Token::Div,
        Token::Float(5.0),
        Token::Semicolon,
        Token::Newline,

        Token::Float(210400.0),
        Token::Mult,
        Token::Int(2),
        Token::Semicolon,
        Token::Newline,
        Token::Newline,

        Token::Ident(String::from("c")),
        Token::NotEqual,
        Token::True,
        Token::Semicolon,
        Token::Newline,
        

        Token::Ident(String::from("c")),
        Token::Equal,
        Token::False,
        Token::Semicolon,
        Token::Newline,

        Token::For,
        Token::Ident(String::from("e")),
        Token::Assign,
        Token::Int(5),
        Token::Var,
        Token::Ident(String::from("e")),
        Token::LessThan,
        Token::Int(10),
        Token::Do,
        Token::LSBracket,
        Token::Newline,
        Token::Ident(String::from("e")),
        Token::Assign,
        Token::Ident(String::from("e")),
        Token::Plus,
        Token::Int(1),
        Token::Newline,
        Token::Displ,
        Token::LParen,
        Token::Int(4),
        Token::Plus,
        Token::Int(13.6),
        Token::Div,
        Token::Int(3),
        Token::RParen,
        Token::Colon,
        Token::Enter,
        Token::LParen,
        Token::Ident(String::from("e")),
        Token::Ident(String::from("a")),
        Token::RParen,
        Token::Newline,
        Token::RSBracket,
        Token::Semicolon,
        Token::Newline,
        Token::Newline,

        Token::If,
        Token::Tilda,
        Token::Ident(String::from("c")),
        Token::Then,
        Token::Displ,
        Token::LParen,
        Token::True,
        Token::RParen,
        Token::Else,
        Token::Displ,
        Token::LParen,
        Token::False,
        Token::RParen,
        Token::End,
        Token::Semicolon,
        Token::Newline,

        
        Token::While,
        Token::LParen,
        Token::Ident(String::from("a")),
        Token::GreaterThan,
        Token::Int(5),
        Token::RParen,
        Token::Do,
        Token::Ident(String::from("a")),
        Token::Assign,
        Token::Ident(String::from("a")),
        Token::Minus,
        Token::Int(1),
        Token::Next,
        Token::Semicolon,
        Token::Newline,
        Token::Newline,

        Token::End,
        Token::Eof,
    ];

    for (idx, token) in tokens.iter().enumerate() {
        let (_, next_token, _) = match lexer.next_token() {
            Ok(next_token) => next_token,
            Err(err) => {
                println!("stopped at {token:?} ({idx})");
                panic!("{err:?}")
            }
        };

        println!("{token:?}, {next_token:?}");

        assert_eq!(
            *token, next_token, 
            "Next token does not match expected token ({:?}, {:?}) at {}", 
            next_token, token, idx
        );
    }

    Ok(())
}