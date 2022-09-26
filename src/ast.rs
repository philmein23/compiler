use std::fmt::Display;

#[derive(Debug)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(iden, expr) => write!(f, "let {} = {};", iden, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Number(isize),
    StringLiteral(String),
    Binary(Box<Expression>, Infix, Box<Expression>),
    Unary(Prefix, Box<Expression>),
    Variable(String),
    Boolean(bool),
}


impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{}", n),
            Expression::StringLiteral(s) => write!(f, "{}", s),
            Expression::Binary(lhs, infix, rhs) => write!(f, "({} {} {})", lhs, infix, rhs),
            Expression::Unary(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expression::Variable(iden) => write!(f, "{}", iden),
            Expression::Boolean(bool) => write!(f, "{}", bool),
        }
    }
}

#[derive(Debug)]
pub enum Prefix {
    Minus,
    Bang,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prefix::Minus => write!(f, "-"),
            Prefix::Bang => write!(f, "!"),
        }
    }
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

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::LessThan => write!(f, "<"),
            Infix::LessEqual => write!(f, "<="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::GreaterEqual => write!(f, ">="),
            Infix::Slash => write!(f, "/"),
            Infix::Star => write!(f, "*"),
        }
    }
}
