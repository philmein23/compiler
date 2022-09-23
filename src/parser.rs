use crate::ast::Expression;
use crate::ast::Statement;
use crate::token::Token;
use std::iter::Peekable;
use std::vec;

#[derive(Debug)]
pub enum ParserError {
    WrongToken,
    InvalidToken,
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
            _ => return Err(ParserError::InvalidToken),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.next(); // consume the Let token
        let iden = if let Some(Token::Identifier(iden)) = self.tokens.peek() {
            self.tokens.next(); // consume the identifier
            iden
        } else {
            return Err(ParserError::WrongToken);
        };

        if let Some(Token::Assign) = self.tokens.peek() {
            self.tokens.next(); // consume the equal token
        } else {
            return Err(ParserError::WrongToken);
        }

        // temp solution until expr parsing logic is completed
        let expr = if let Some(Token::Number(num)) = self.tokens.peek() {
            self.tokens.next();
            Expression::Number(*num)
        } else {
            return Err(ParserError::WrongToken);
        };
        self.tokens.next(); //consume the semicolon token
        
        Ok(Statement::Let(iden.into(), expr))
    }
}
