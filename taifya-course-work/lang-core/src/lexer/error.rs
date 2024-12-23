use crate::utils::prelude::SrcSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexicalErrorType {
    UnrecognizedToken { tok: char },
    MissingDigitAfterPeriod,
    MissingDigitBeforeExponent,
    MissingDigitsAfterExponent,
    MultipleFloatingPoints,
    DigitOutOfRadix,
    UnsupportedFloatingPoint,
    MissingCommentEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: SrcSpan
}

impl LexicalError {
    pub fn details(&self) -> (&'static str, Vec<String>) {
        match self.error {
            LexicalErrorType::DigitOutOfRadix => {
                ("Число слишком велико для данной системы счисления", vec![])
            },
            LexicalErrorType::MissingDigitAfterPeriod => {
                ("Отсутствует число после точки", vec![])
            },
            LexicalErrorType::MissingDigitBeforeExponent => {
                ("Отсутствует число перед экспонентой", vec![])
            },
            LexicalErrorType::MissingDigitsAfterExponent => {
                ("Отсутствует число после экспоненты", vec![])
            },
            LexicalErrorType::MultipleFloatingPoints => {
                ("Найдено несколько точек в числе с плавающей точкой", vec![])
            },
            LexicalErrorType::UnrecognizedToken { .. } => {
                ("Не знаю, что делать с этим символом", vec![])
            },
            LexicalErrorType::UnsupportedFloatingPoint => {
                ("Эта система счисления не поддерживает числа с плавающей точкой", vec![])
            },
            LexicalErrorType::MissingCommentEnd => {
                ("Отсутствует заканчивающий комментарий символ `#`", vec![])
            }
        }
    }
}