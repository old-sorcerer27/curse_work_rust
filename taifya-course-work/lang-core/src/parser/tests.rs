use crate::{
    lexer::prelude::Lexer, 
    parser::prelude::{parse_module, ParseError, Parser}
};

#[test]
fn test_declarations() -> Result<(), ParseError> {
    let input =  r#"
        begin
            var a, e: #;
                c, d: @;
                d: &;;
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_infixes() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a, b: #;;

            if (false && true) writeln true;
            if (true || false) writeln 1;

           
                a assign 5 plus 5;
                b assign 10 minus 10;
                a assign 6 mult 10;
                b assign 10 div 6
            
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_prefix() -> Result<(), ParseError> {
    let input = r#"
        begin
            if !!!false then writeln true;
            if !true == !!false then writeln false
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_blocks() -> Result<(), ParseError> {
    let input =  r#"
        begin 
            var
                var a: $;;

            a assign true;

            [
                if a == true then writeln a: a assign false
            ];

            [
                if a NE true then writeln a
                a assign true
            ]
            
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_conditionals() -> Result<(), ParseError> {
    let input = r#"
        begin
            var
                if a EQ b then writeln b else writeln a;
                if a EQ b then begin writeln a else writeln b end;
                if a EQ b then writeln a else writeln b end;
                if a EQ b then begin writeln a else begin writeln b end;

            while a > 5 do a assign a + 1 next;
            while a > 10 a assign a + 1 next;

            for i assign 0 to 10 writeln i next;
            for i assign 0 to 10 begin writeln i end next;

            for i assign 0 to 10 step 2 writeln i next;
            for i assign 0 to 10 step 2 begin writeln i end next
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_values() -> Result<(), ParseError> {
    let input = r#"
        begin
            var 
                var a, b, c, d: %;
                    e, f, g: !;
                    h, j: @;
                    k, l: $;;

            a assign 23;
            b assign 10101b;
            c assign 2345o;
            d assign 1A5D9h;
            e assign 10.15;
            f assign 1e-5;
            g assign 1.0e10;
            h assign "hello";
            j assign "world";
            k assign true;
            l assign false
        end
    "#;

    let parsed = parse_module(input)?;

    println!("{}", parsed.module.program);

    Ok(())
}

#[test]
fn test_program() -> Result<(), ParseError> {
    let input = r#"
        begin
            var a: $; b: !;;
            var c: !;;
            var
        end
    "#;

    let lexer = Lexer::new(input.char_indices().map(|(i, c)| (i as u32, c)));
    let mut parser = Parser::new(lexer);

    let parsed = parser.parse()?;

    println!("{}", parsed.module.program);

    Ok(())
}