//! Concrete Syntax Tree (CST) for the LambdaR language.

use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    /// let ID = Expr in Expr
    Let(ID, Box<Expr>, Box<Expr>),
    /// fun ID with ID* = Expr in Expr
    Fun(ID, Vec<ID>, Box<Expr>, Box<Expr>),
    /// fun rec ID with ID* = Expr in Expr
    RecFun(ID, Vec<ID>, Box<Expr>, Box<Expr>),
    /// If Expr then Expr else Expr
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Match Expr with | Nil -> Expr | ID :: ID -> Expr
    Match(Box<Expr>, Box<Expr>, ID, ID, Box<Expr>),
    /// INT_LITERAL
    Integer(i32),
    /// ID
    Variable(String),
    /// Expr Op Expr
    Op(Box<Expr>, BinOp, Box<Expr>),
    /// Expr :: Expr
    Cons(Box<Expr>, Box<Expr>),
    /// f(Vec<Expr>)
    App(ID, Vec<Expr>),
    Nil,
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Arith(ArithOp),
    Pred(Pred),
}

#[derive(Debug, Clone)]
pub enum ArithOp {
    Mul,
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum Pred {
    Eq,
    Gt,
    Lt,
    Le,
    Ge
}

pub type ID = String;

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Arith(op) => write!(f, "{}", op),
            BinOp::Pred(op) => write!(f, "{}", op),
        }
    }
}

impl fmt::Display for ArithOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithOp::Mul => write!(f, "*"),
            ArithOp::Add => write!(f, "+"),
            ArithOp::Sub => write!(f, "-"),
        }
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pred::Eq => write!(f, "="),
            Pred::Gt => write!(f, ">"),
            Pred::Lt => write!(f, "<"),
            Pred::Le => write!(f, "<="),
            Pred::Ge => write!(f, ">="),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Let(id, expr, body) => write!(f, "let {} = {} in {}", id, expr, body),
            Expr::Fun(id, args, expr, body) => {
                write!(
                    f,
                    "fun {} with {} = {} in {}",
                    id,
                    args.join(", "),
                    expr,
                    body
                )
            }
            Expr::RecFun(id, args, expr, body) => {
                write!(
                    f,
                    "fun rec {} with {} = {} in {}",
                    id,
                    args.join(", "),
                    expr,
                    body
                )
            }
            Expr::If(cond, then, else_) => write!(f, "if {} then {} else {}", cond, then, else_),
            Expr::Match(expr, nil, id1, id2, cons) => {
                write!(
                    f,
                    "match {} with | Nil -> {} | {} :: {} -> {}",
                    expr, nil, id1, id2, cons
                )
            }
            Expr::Integer(i) => write!(f, "{}", i),
            Expr::Variable(id) => write!(f, "{}", id),
            Expr::Op(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Expr::Cons(left, right) => write!(f, "{} :: {}", left, right),
            Expr::App(id, args) => write!(
                f,
                "{}({})",
                id,
                args.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expr::Nil => write!(f, "Nil"),
            Expr::Bool(b) => write!(f, "{b}"),
        }
    }
}
