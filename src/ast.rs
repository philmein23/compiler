#[derive(Debug)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Number(isize),
    StringLiteral(String),
    Binary(Box<Expression>, Infix, Box<Expression>),
    Unary(Prefix, Box<Expression>),
    Variable(String),
}

#[derive(Debug)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Debug)]
pub enum Infix {
    Plus,
    Minus,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Slash,
    Star,
}
