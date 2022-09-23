#[derive(Debug)]
pub enum Statement {
    Let(String, Expression),
}

#[derive(Debug)]
pub enum Expression {
    Number(isize),
    StringLiteral(String),
}
