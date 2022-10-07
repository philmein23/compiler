use crate::ast::{BlockStatement, Expression, Infix, Prefix, Statement};
use crate::token::Token;
use core::fmt;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec;
use tracing::{debug, event, info, instrument, Level};

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

        if let Some(Token::Semicolon) = self.tokens.peek() {
            self.tokens.next(); // consume the semi-colon token
        }

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
                debug!("Semicolon is found - break out of loop");
                break;
            }
            // ex. 5 + (10 * 20); * has a higher left-binding power than +'s right-binding power so
            // * "sucks in" 10 and becomes the right arm of the AST.
            // ex. (10 * 5) - 5;
            // ex. ((10 * 2)/ 2) + 5);
            // Don't like what I did here- need to refactor this
            if prec >= self.lookup_precedence(some_token).0 {
                debug!("lbp is > than rbp- break out of loop");
                break;
            }

            left_expr = self.infix_parse_methods(left_expr)?;
        }

        println!("NEXT TOKEN {:?}", self.tokens.peek());
        debug!("Return left_expr {:?}", left_expr);
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
            Some(Token::LeftParen) => (Precedence::Call, Some(Infix::LeftParen)),
            _ => (Precedence::Lowest, None),
        }
    }

    #[instrument(skip(self))]
    fn prefix_parse_methods(&mut self) -> Result<Expression, ParserError> {
        match self.tokens.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier(),
            Some(Token::String(_)) => self.parse_string_literal(),
            Some(Token::Number(_)) => self.parse_number(),
            Some(Token::Bang) | Some(Token::Minus) => self.parse_prefix_expression(),
            Some(Token::True) | Some(Token::False) => self.parse_boolean(),
            Some(Token::LeftParen) => self.parse_grouped_expression(),
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Fn) => self.parse_function_expression(),
            Some(Token::LeftBrace) => self.parse_hash(),
            _ => return Err(ParserError::UnexpectedToken),
        }
    }

    fn parse_hash(&mut self) -> Result<Expression, ParserError> {
        self.tokens.next(); // consume the left brace

        let mut pairs = vec![];
        loop {
            if let Some(Token::RightBrace) = self.tokens.peek() {
                break;
            }
            let key = self.parse_expression(Precedence::Lowest)?;

            if let Some(Token::Colon) = self.tokens.peek() {
                self.tokens.next(); // consume the colon token
            } else {
                return Err(ParserError::UnexpectedToken);
            }

            let value = self.parse_expression(Precedence::Lowest)?;
            
            pairs.push((key, value));

            match self.tokens.peek() {
                Some(Token::Comma) => {
                    self.tokens.next(); // consume the comma token
                }
                Some(Token::RightBrace) => {
                    break;
                }
                _ => {
                    return Err(ParserError::UnexpectedToken);
                }
            }

        }

        self.tokens.next(); // consume the right brace

        Ok(Expression::Hash(pairs))
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
            | Some(Token::Assign)
            | Some(Token::NotEqual) => self.parse_infix_expression(expr),
            Some(Token::LeftParen) => self.parse_call_expression(expr),
            _ => return Err(ParserError::UnexpectedToken),
        }
    }

    fn parse_call_expression(&mut self, func: Expression) -> Result<Expression, ParserError> {
        self.tokens.next(); // consume the left paren token
        let args = self.parse_call_arguments()?;

        if let Some(Token::RightParen) = self.tokens.peek() {
            self.tokens.next(); // consume the right paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        Ok(Expression::Call(Box::new(func), args))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Box<Expression>>, ParserError> {
        let mut args = vec![];

        let arg = self.parse_expression(Precedence::Lowest)?;
        args.push(Box::new(arg));

        while let Some(Token::Comma) = self.tokens.peek() {
            self.tokens.next(); // consume the comma token

            let arg = self.parse_expression(Precedence::Lowest)?;
            args.push(Box::new(arg));
        }

        Ok(args)
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

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.tokens.next(); // consume the if token
        if let Some(Token::LeftParen) = self.tokens.peek() {
            self.tokens.next(); // consume the left paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        let cond = self.parse_expression(Precedence::Lowest)?;

        if let Some(Token::RightParen) = self.tokens.peek() {
            self.tokens.next(); // consume the right paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        }

        let block_stmt = self.parse_block_statement()?;

        let maybe_else = if let Some(Token::Else) = self.tokens.peek() {
            self.tokens.next(); // consume the else token

            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If(Box::new(cond), block_stmt, maybe_else))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParserError> {
        self.tokens.next(); //consume the function token
        let iden = if let Some(Token::Identifier(iden)) = self.tokens.peek() {
            self.tokens.next(); // consume the identifier token
            iden
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        let params = self.parse_function_params()?;
        let stmt = self.parse_block_statement()?;

        Ok(Expression::Function(iden.into(), params, stmt))
    }

    fn parse_function_params(&mut self) -> Result<Vec<String>, ParserError> {
        let mut params = vec![];

        if let Some(Token::LeftParen) = self.tokens.peek() {
            self.tokens.next(); // consume the left paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        match self.tokens.peek() {
            Some(Token::RightParen) => {
                self.tokens.next(); // consume the right paren token
            }
            Some(Token::Identifier(s)) => {
                self.tokens.next(); // consume iden token
                params.push(s.into());
            }
            _ => return Err(ParserError::UnexpectedToken),
        }

        while let Some(Token::Comma) = self.tokens.peek() {
            self.tokens.next(); // consume the comma token
            if let Some(Token::Identifier(s)) = self.tokens.peek() {
                self.tokens.next(); // consume the iden token
                params.push(s.into());
            } else {
                return Err(ParserError::UnexpectedToken);
            };
        }

        if let Some(Token::RightParen) = self.tokens.peek() {
            self.tokens.next(); // consume the left paren token
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        Ok(params)
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        if let Some(Token::LeftBrace) = self.tokens.peek() {
            self.tokens.next(); // consume the left brace
        } else {
            return Err(ParserError::UnexpectedToken);
        }
        let mut else_stmts = vec![];

        loop {
            match self.tokens.peek() {
                Some(Token::EOF) | Some(Token::RightBrace) => break,
                _ => {
                    else_stmts.push(self.parse_statement()?);
                }
            }
        }

        self.tokens.next(); // consume the right brace token

        Ok(BlockStatement {
            statements: else_stmts,
        })
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

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        let s = if let Some(Token::String(s)) = self.tokens.peek() {
            self.tokens.next(); // consume the string token
            s
        } else {
            return Err(ParserError::UnexpectedToken);
        };

        Ok(Expression::StringLiteral(s.into()))
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
