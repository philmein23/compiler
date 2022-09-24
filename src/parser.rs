use crate::ast::Expression;
use crate::ast::Statement;
use crate::token::Token;
use core::fmt;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec;

#[derive(Debug)]
pub enum ParserError {
    InvalidToken { expected: Token, found: Token },
    Unexpected(Token),
    UnexpectedToken,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::InvalidToken { expected, found } => {
                write!(f, "Expected {}, but found {} instead", expected, found)
            }
            ParserError::Unexpected(t) => write!(f, "Unexpected token {}", t),
            ParserError::UnexpectedToken => write!(f, "Unexpected token"),
        }
    }
}

pub struct Parser<'a> {
    pub tokens: Peekable<std::slice::Iter<'a, Token>>,
}

impl Parser<'_> {
    // pub fn new() -> Self {
    //     Parser {
    //         tokens: default()
    //     }
    // }
    pub fn parse_program(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];

        loop {
            if let Some(Token::EOF) = self.tokens.peek() {
                self.tokens.next();
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => return Err(ParserError::UnexpectedToken),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.next(); // consume the Let token
        let iden = if let Some(Token::Identifier(iden)) = self.tokens.peek() {
            self.tokens.peek();
            iden
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        if let Some(Token::Assign) = self.tokens.peek() {
            self.tokens.next(); // consume the equal token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        // temp solution until expr parsing logic is completed
        let expr = if let Some(Token::Number(num)) = self.tokens.peek() {
            self.tokens.next();
            Expression::Number(*num)
        } else {
            return Err(ParserError::UnexpectedToken);
        };
        self.tokens.next(); //consume the semicolon token

        Ok(Statement::Let(iden.into(), expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.next(); // consume the return token
        
        // temp: remove loop when parsing expression is complete
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::Semicolon => {
                    self.tokens.next();
                    break;
                }
                _ => self.tokens.next()
            };
        };

        Ok(Statement::Return(()))
    }
}
