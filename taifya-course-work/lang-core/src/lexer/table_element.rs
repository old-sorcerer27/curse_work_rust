use std::fmt::Display;

use super::token::Token;

pub trait DisplayTable {
    fn get_table_listing(&self) -> &Vec<TableElement>;
}

#[derive(Debug, Clone)]
pub struct TableElement {
    pub table: u32,
    pub idx: u32,
    pub token: Token
}

impl TableElement {
    pub fn from(table: u32, idx: u32, token: Token) -> Self {
        Self {
            table,
            idx,
            token
        }
    }

    pub fn to_string_with_token(&self) -> String {
        format!("({}, {}): {}", self.table, self.idx, self.token.as_literal())
    }
}

impl Display for TableElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.table, self.idx)
    }
}