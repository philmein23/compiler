use std::fmt::Display;

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for stmt in self.statements.iter() {
            write!(f, "{}\n", stmt)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

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
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Function(String, Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Box<Expression>>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Expression::Number(n) => write!(f, "{}", n),
            Expression::StringLiteral(s) => write!(f, "\"{}\"", s),
            Expression::Binary(lhs, infix, rhs) => write!(f, "({} {} {})", lhs, infix, rhs),
            Expression::Unary(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expression::Variable(iden) => write!(f, "{}", iden),
            Expression::Boolean(bool) => write!(f, "{}", bool),
            Expression::If(cond, consequence, alternative) => {
                match alternative {
                    None => write!(f, "if ({}) {}", cond, consequence)?,
                    Some(alt) => write!(f, "if ({}) {} else {}", cond, consequence, alt)?,
                }
                Ok(())
            }
            Expression::Function(name, params, stmts) => {
                let params = params.join(",");
                write!(f, "fn {}({}) {}", name, params, stmts)
            }
            Expression::Call(iden, args) => {
                let comma_joined_args = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", iden, comma_joined_args)?;

                Ok(())
            }
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
    LeftParen,
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
            Infix::LeftParen => write!(f, "("),
        }
    }
}
