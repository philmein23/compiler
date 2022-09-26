use crate::ast::{Expression, Infix, Prefix, Statement};
use crate::token::Token;
use core::fmt;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec;
use tracing::{event, info, debug, instrument, Level};

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

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: Peekable<std::slice::Iter<'a, Token>>,
}

impl Parser<'_> {
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

    #[instrument(skip(self))]
    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.tokens.peek() {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_statement_expression(),
        }
    }

    #[instrument(skip(self))]
    fn parse_statement_expression(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        self.tokens.next(); // consume the semi-colon token

        Ok(Statement::Expression(expr))
    }

    #[instrument(skip(self))]
    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, ParserError> {
        let mut left_expr = self.prefix_parse_methods()?;
        debug!("Prefix expr: {:?}", left_expr);

        loop {
            let some_token = self.tokens.peek().map(|token| *token);

            debug!(
                "Right-binding precedence: {:?}, Left-binding precedence: {:?}",
                prec,
                self.lookup_precedence(some_token).0
            );

            if let Some(Token::Semicolon) = self.tokens.peek() {
                break;
            }
            // ex. 5 + (10 * 20); * has a higher left-binding power than +'s right-binding power so
            // * "sucks in" 10 and becomes the right arm of the AST.
            // ex. (10 * 5) - 5;
            // ex. ((10 * 2)/ 2) + 5);
            // Don't like what I did here- need to refactor this
            if prec >= self.lookup_precedence(some_token).0 {
                break;
            }

            left_expr = self.infix_parse_methods(left_expr)?;
        }

        Ok(left_expr)
    }

    fn lookup_precedence(&self, token: Option<&Token>) -> (Precedence, Option<Infix>) {
        match token {
            Some(Token::Plus) => (Precedence::Sum, Some(Infix::Plus)),
            Some(Token::Minus) => (Precedence::Sum, Some(Infix::Minus)),
            Some(Token::Slash) => (Precedence::Product, Some(Infix::Slash)),
            Some(Token::Star) => (Precedence::Product, Some(Infix::Star)),
            Some(Token::Greater) => (Precedence::LessGreater, Some(Infix::GreaterThan)),
            Some(Token::GreaterEqual) => (Precedence::LessGreater, Some(Infix::GreaterEqual)),
            Some(Token::Less) => (Precedence::LessGreater, Some(Infix::LessThan)),
            Some(Token::LessEqual) => (Precedence::LessGreater, Some(Infix::LessEqual)),
            Some(Token::Equal) => (Precedence::Equals, Some(Infix::Equal)),
            Some(Token::NotEqual) => (Precedence::Equals, Some(Infix::NotEqual)),
            _ => (Precedence::Lowest, None),
        }
    }

    #[instrument(skip(self))]
    fn prefix_parse_methods(&mut self) -> Result<Expression, ParserError> {
        match self.tokens.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier(),
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::Bang) | Some(Token::Minus) => self.parse_prefix_expression(),
            Some(Token::True) | Some(Token::False) => self.parse_boolean(),
            Some(Token::LeftParen) => self.parse_grouped_expression(),
            _ => return Err(ParserError::UnexpectedToken),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.tokens.next(); // consume the left paren token

        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Some(Token::RightParen) = self.tokens.peek() {
            self.tokens.next(); // consume the right paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        Ok(expr)
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        let bool_expr = if let Some(Token::True) = self.tokens.peek() {
            Expression::Boolean(true)
        } else {
            Expression::Boolean(false)
        };

        self.tokens.next(); //consume the boolean token
        Ok(bool_expr)
    }


    #[instrument(skip(self))]
    fn infix_parse_methods(&mut self, expr: Expression) -> Result<Expression, ParserError> {
        match self.tokens.peek() {
            Some(Token::Plus)
            | Some(Token::Minus)
            | Some(Token::Star)
            | Some(Token::Slash)
            | Some(Token::Greater)
            | Some(Token::GreaterEqual)
            | Some(Token::Less)
            | Some(Token::LessEqual)
            | Some(Token::Equal)
            | Some(Token::NotEqual) => self.parse_infix_expression(expr),
            _ => return Err(ParserError::UnexpectedToken),
        }
    }

    #[instrument(skip(self))]
    fn parse_infix_expression(&mut self, left_expr: Expression) -> Result<Expression, ParserError> {
        let token = self.tokens.next(); //consume infix token
        let (prec, maybe_infix) = self.lookup_precedence(token);
        let infix = if let Some(i) = maybe_infix {
            i
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        let right_expr = self.parse_expression(prec)?;

        Ok(Expression::Binary(
            Box::new(left_expr),
            infix,
            Box::new(right_expr),
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let prefix = if let Some(Token::Bang) = self.tokens.peek() {
            Prefix::Bang
        } else {
            Prefix::Minus
        };

        self.tokens.next(); //consume prefix token

        let expr = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Unary(prefix, Box::new(expr)))
    }

    fn parse_number(&mut self) -> Result<Expression, ParserError> {
        let number = if let Some(Token::Number(n)) = self.tokens.peek() {
            self.tokens.next(); // consume the number token
            n
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        Ok(Expression::Number(*number))
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        let iden = if let Some(Token::Identifier(iden)) = self.tokens.peek() {
            self.tokens.next(); // consume the identifier token
            iden
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        Ok(Expression::Variable(iden.into()))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.next(); // consume the Let token
        let iden = if let Some(Token::Identifier(iden)) = self.tokens.peek() {
            self.tokens.next(); // consume identifier token
            iden
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        if let Some(Token::Assign) = self.tokens.peek() {
            self.tokens.next(); // consume the equal token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        let expr = self.parse_expression(Precedence::Lowest)?;
        self.tokens.next(); //consume the semicolon token

        Ok(Statement::Let(iden.into(), expr))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.tokens.next(); // consume the return token

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.tokens.next(); // consume the semicolon token

        Ok(Statement::Return(expr))
    }
}
