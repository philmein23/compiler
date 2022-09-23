use crate::token::Token;
use core::fmt;
use std::{iter::Peekable, num};

#[derive(Debug)]
pub enum LexerError {
    InvalidToken(char),
    EndOfLine,
    Parse(num::ParseIntError),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerError::Parse(err) => err.fmt(f),
            LexerError::EndOfLine => write!(f, "Source code has been completely scanned"),
            LexerError::InvalidToken(ch) => write!(f, "Token {} is not recognized", ch),
        }
    }
}

impl From<num::ParseIntError> for LexerError {
    fn from(err: num::ParseIntError) -> Self {
        LexerError::Parse(err)
    }
}

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        let tokens = vec![];
        Lexer { source, tokens }
    }

    pub fn collect_tokens(&mut self) -> Result<Peekable<std::slice::Iter<'_, Token>>, LexerError> {
        for line in self.source.lines() {
            let mut char_iter = line.trim().char_indices().peekable();
            while let Some((_pos, ch)) = char_iter.next() {
                let token = match ch {
                    ' ' => continue,
                    '>' => match char_iter.next_if_eq(&(_pos + 1, '=')) {
                        Some(_equals) => (Token::GreaterEqual),
                        None => (Token::Greater),
                    },
                    '<' => match char_iter.next_if_eq(&(_pos + 1, '=')) {
                        Some(_equals) => (Token::LessEqual),
                        None => (Token::Less),
                    },
                    '=' => match char_iter.next_if_eq(&(_pos + 1, '=')) {
                        Some(_equals) => (Token::Equal),
                        None => (Token::Assign),
                    },
                    '!' => match char_iter.next_if_eq(&(_pos + 1, '=')) {
                        Some(_equals) => (Token::NotEqual),
                        None => (Token::Bang),
                    },
                    '.' => (Token::Dot),
                    ',' => (Token::Comma),
                    '{' => (Token::LeftBrace),
                    '}' => (Token::RightBrace),
                    '(' => (Token::LeftParen),
                    ')' => (Token::RightParen),
                    '+' => (Token::Plus),
                    '-' => (Token::Minus),
                    '*' => (Token::Star),
                    '/' => (Token::Slash),
                    ';' => (Token::Semicolon),
                    ':' => (Token::Colon),
                    ch if ch.is_alphabetic() || ch == '_' => {
                        let mut iden = ch.to_string();
                        while let Some((_, ch)) = char_iter.next_if(|(_, ch)| ch.is_alphabetic()) {
                            iden.push(ch);
                        }

                        let token = match iden.as_str() {
                            "let" => Token::Let,
                            "fn" => Token::Fn,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "return" => Token::Return,
                            "for" => Token::For,
                            "while" => Token::While,
                            "true" => Token::True,
                            "false" => Token::False,
                            _ => Token::Identifier(iden),
                        };
                        token
                    }

                    ch if ch.is_digit(10) => {
                        let mut num = ch.to_string();
                        while let Some((_, ch)) = char_iter.next_if(|(_, ch)| ch.is_digit(10)) {
                            num.push(ch);
                        }

                        let parsed_num = num.parse::<isize>()?;
                        Token::Number(parsed_num)
                    }
                    '"' => {
                        let mut str_val = "".to_string();
                        while let Some((_, ch)) = char_iter.next_if(|(_, ch)| *ch != '"') {
                            str_val.push(ch);
                        }

                        match char_iter.next() {
                            Some((_, ch)) if ch == '"' => (Token::String(str_val)),
                            Some((_, ch)) => return Err(LexerError::InvalidToken(ch)),
                            None => {
                                return Err(LexerError::EndOfLine);
                            }
                        }
                    }
                    _ => return Err(LexerError::InvalidToken(ch)),
                };
                self.tokens.push(token);
            }
            self.tokens.push(Token::EOF);
        }
        Ok(self.tokens.iter().peekable())
    }
}
