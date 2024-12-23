use std::collections::HashMap;

use super::table_element::TableElement;

#[derive(Debug, Clone, PartialEq)]


pub enum Token {
    // <буква>{<буква>|<цифра>}
    Ident(String), 
    // Система Счисления 10: {/ <цифра> /}[D|d]
    // СС 2: {/ 0|1 /}(B|b)
    // СС 8: {/ 0|1|2|3|4|5|6|7 /}(O|o)
    // СС 16: {/ <цифра>|A|B|C|D|E|F|a|b|c|d|e|f /}(H|h)
    Int(i64),
    // <числовая_строка><порядок>|[<числовая_строка>].<числовая_строка>[<порядок>],
    // где <порядок>::= (E|e)[+|-]<числовая_строка>
    Float(f64),
    // % многострочный комментарий %
    Comment, 

       // Операции группы отношения
    Equal, // EQ
    NotEqual, // NE
    LessThan, // LT
    LessThanOrEqual, // LE
    GreaterThan, // RT
    GreaterThanOrEqual, // RE

    // Операции группы сложения
    Plus, // plus
    Minus, // min
    Or, // or

    // Операции группы умножения
    Mult, // mult
    Div, // div
    And, // and

    // Унарная операция 
    Tilda, // ~

    // Операция присваивания
    Assign, // assign

    // Типы данных
    Hashtag, // #
    At, // @
    Ampersand, // &

    // Ключевые слова
    Val, // Val
    Begin, // begin
    End, // end
    If, // if
    Then, // then
    Else, // else
    For, // for
    Do, // do
    While, // while
    Next, // next

    // Ввод и вывод
    Enter, // enter
    Displ, // displ

    // Прочие разделители
    Comma, // ,
    Colon, // :
    Semicolon, // ;
    LSBracket, // [
    RSBracket, // ]
    LParen, // (
    RParen, // )

    Newline, // перевод строки

    // Логические константы
    True,
    False,

    Eof,
}

impl Token {
    pub fn is_reserved_word(&self) -> bool {
        match self {
            Token::Val
            | Token::Begin
            | Token::End
            | Token::If
            | Token::Then
            | Token::Else
            | Token::For
            | Token::Do
            | Token::While
            | Token::Next 
            | Token::Val
            | Token::Enter 
            | Token::Displ 
            | Token::Enter => true,
            _ => false
        }
    }

    pub fn is_Valiable_type(&self) -> bool {
        match self {
            Token::Hashtag //13
            | Token::At //14
            | Token::Ampersand => true, //15
            _ => false,
        }
    }

    pub fn is_operator(&self) -> bool {
        match self {
            Token::Plus //16
            | Token::Minus //17
            | Token::Or //18
            | Token::Mult
            | Token::Div //19
            | Token::And //20
            | Token::Assign //21
            | Token::Tilda //22
            | Token::Equal //23
            | Token::NotEqual //24
            | Token::LessThan //25
            | Token::LessThanOrEqual //26
            | Token::GreaterThan //27
            | Token::GreaterThanOrEqual => true, //28
            _ => false,
        }
    }

    pub fn as_literal(&self) -> String {
        match self {
            Token::Ident(value) => format!("{}", value),
            Token::Int(value) => format!("{}", value),
            Token::Float(value) => format!("{}", value),
            Token::Comment => "Comment".to_string(),

            Token::Equal => "EQ".to_string(),
            Token::NotEqual => "NE".to_string(),
            Token::LessThan => "LT".to_string(),
            Token::LessThanOrEqual => "LE".to_string(),
            Token::GreaterThan => "GT".to_string(),
            Token::GreaterThanOrEqual => "GE".to_string(),
            Token::Plus => "plus".to_string(),
            Token::Minus => "min".to_string(),
            Token::Or => "or".to_string(),
            Token::Div => "div".to_string(),
            Token::Mult => "mult".to_string(),
            Token::And => "and".to_string(),
            Token::Tilda => "~".to_string(),
            Token::Assign => "assign".to_string(),
            Token::Comma => ",".to_string(),
            Token::Colon => ":".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LSBracket => "[".to_string(),
            Token::RSBracket => "]".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),

            Token::Hashtag => "#".to_string(),
            Token::At => "@".to_string(),
            Token::Ampersand => "&".to_string(),
            Token::Val => "Val".to_string(),
            Token::Begin => "begin".to_string(),
            Token::End  => "end".to_string(),
            Token::If => "if".to_string(),
            Token::Then => "then".to_string(),
            Token::Else => "else".to_string(),
            Token::For => "for".to_string(),
            Token::Do => "do".to_string(),
            Token::While => "while".to_string(),
            Token::Next => "next".to_string(),
            Token::Val  => "val".to_string(),
            Token::Enter => "enter".to_string(),
            Token::Displ => "displ".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),

            Token::Newline => "\n".to_string(),
            Token::Eof => "\0".to_string(),
        }.to_string()
    }

    pub fn as_table_literal(
        &self, 
        ident_map: &mut HashMap<String, u32>, 
        number_map: &mut HashMap<String, u32>
    ) -> Option<TableElement> {
        Some(match self {
            Token::Ident(ident) => {
                let ident_idx = match ident_map.get(ident) {
                    Some(idx) => *idx,
                    None => {
                        let new_idx = ident_map.len() as u32;
                        ident_map.insert(ident.clone(), new_idx);

                        new_idx
                    }
                };

                TableElement::from(2, ident_idx, self.clone())
            },
            Token::Int(val) => {
                let val_str = val.to_string();
                let number_idx = match number_map.get(&val_str) {
                    Some(idx) => *idx,
                    None => {
                        let new_idx = number_map.len() as u32;
                        number_map.insert(val_str, new_idx);

                        new_idx
                    }
                };

                TableElement::from(3, number_idx, self.clone())
            },
            Token::Float(val) => {
                let val_str = val.to_string();
                let number_idx = match number_map.get(&val_str) {
                    Some(idx) => *idx,
                    None => {
                        let new_idx = number_map.len() as u32;
                        number_map.insert(val_str, new_idx);

                        new_idx
                    }
                };

                TableElement::from(3, number_idx, self.clone())
            },
            Token::Comma => TableElement::from(1, 0, self.clone()),
            Token::Colon => TableElement::from(1, 1, self.clone()),
            Token::Semicolon => TableElement::from(1, 2, self.clone()),
            Token::LSBracket => TableElement::from(1, 3, self.clone()),
            Token::RSBracket => TableElement::from(1, 4, self.clone()),
            Token::LParen => TableElement::from(1, 5, self.clone()),
            Token::RParen => TableElement::from(1, 6, self.clone()),
            Token::Equal => TableElement::from(1, 7, self.clone()),
            Token::NotEqual => TableElement::from(1, 8, self.clone()),
            Token::LessThan => TableElement::from(1, 9, self.clone()),
            Token::LessThanOrEqual => TableElement::from(1, 10, self.clone()),
            Token::GreaterThan => TableElement::from(1, 11, self.clone()),
            Token::GreaterThanOrEqual => TableElement::from(1, 12, self.clone()),
            Token::Plus => TableElement::from(1, 13, self.clone()),
            Token::Minus => TableElement::from(1, 14, self.clone()),
            Token::Or => TableElement::from(1, 15, self.clone()),
            Token::Mult => TableElement::from(1, 16, self.clone()),
            Token::Div => TableElement::from(1, 17, self.clone()),
            Token::And => TableElement::from(1, 18, self.clone()),
            Token::Tilda => TableElement::from(1, 19, self.clone()),
            Token::Assign => TableElement::from(1, 20, self.clone()),

            Token::Hashtag => TableElement::from(0, 0, self.clone()),
            Token::At => TableElement::from(0, 1, self.clone()),
            Token::Ampersand => TableElement::from(0, 2, self.clone()),
            Token::Val => TableElement::from(0, 3, self.clone()),
            Token::Begin => TableElement::from(0, 4, self.clone()),
            Token::End => TableElement::from(0, 5, self.clone()),
            Token::If => TableElement::from(0, 6, self.clone()),
            Token::Then => TableElement::from(0, 7, self.clone()),
            Token::Else => TableElement::from(0, 8, self.clone()),
            Token::For => TableElement::from(0, 9, self.clone()),
            Token::Do => TableElement::from(0, 10, self.clone()),
            Token::While => TableElement::from(0, 11, self.clone()),
            Token::Next => TableElement::from(0, 12, self.clone()),
            Token::Val  => TableElement::from(0, 13, self.clone()),
            Token::Enter => TableElement::from(0, 14, self.clone()),
            Token::Displ => TableElement::from(0, 15, self.clone()),
            Token::True => TableElement::from(0, 16, self.clone()),
            Token::False => TableElement::from(0, 17, self.clone()),
            _ => return None
        })
    }
}



