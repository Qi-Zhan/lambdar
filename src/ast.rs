//! Abstract Syntax Tree for the LambdaR language.
//!
//! Mainly derived from the Concrete Syntax Tree (CST) with sytax desugaring.

use std::{collections::HashSet, fmt};

pub use crate::cst::{ArithOp, BinOp, Pred, ID};
use crate::{cst, error::Error};

#[derive(Debug, Clone)]
pub enum Expr {
    /// integer
    Integer(i32),
    /// variable
    Variable(ID),
    /// empty list
    Nil,
    /// bool
    Bool(bool),
    /// binary op
    Op(Box<Expr>, BinOp, Box<Expr>),
    /// if-then-else
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// const cell
    Cons(Box<Expr>, Box<Expr>),
    /// list pattern matching
    Match(Box<Expr>, Box<Expr>, ID, ID, Box<Expr>),
    /// lambda abstraction
    Lambda(ID, Box<Expr>),
    /// function application
    App(Box<Expr>, Box<Expr>),
}

use std::sync::atomic::{AtomicUsize, Ordering};
static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn fresh_var() -> String {
    format!("x{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

impl Expr {
    pub fn eval(&self) -> Result<Expr, Error> {
        let value = match self {
            Expr::Integer(_) | Expr::Nil | Expr::Bool(_) => Ok(self.clone()),
            Expr::Variable(x) => Err(Error::Runtime(format!("unbound variable {x}"))),
            Expr::Op(left, op, right) => {
                let left = left.eval()?;
                let right = right.eval()?;
                match op {
                    BinOp::Arith(arith) => match (left, right) {
                        (Expr::Integer(left), Expr::Integer(right)) => match arith {
                            ArithOp::Mul => Ok(Expr::Integer(left * right)),
                            ArithOp::Add => Ok(Expr::Integer(left + right)),
                            ArithOp::Sub => Ok(Expr::Integer(left - right)),
                        },
                        _ => Err(Error::Runtime("can only add two int".to_string())),
                    },
                    BinOp::Pred(p) => match (left, right) {
                        (Expr::Integer(left), Expr::Integer(right)) => match p {
                            Pred::Eq => Ok(Expr::Bool(left == right)),
                            Pred::Gt => Ok(Expr::Bool(left > right)),
                            Pred::Lt => Ok(Expr::Bool(left < right)),
                            Pred::Le => Ok(Expr::Bool(left <= right)),
                            Pred::Ge => Ok(Expr::Bool(left >= right)),
                        },
                        _ => Err(Error::Runtime("can only compare two int".to_string())),
                    },
                }
            }
            Expr::If(cond, then, else_) => {
                let cond = cond.eval()?;
                if let Expr::Bool(b) = cond {
                    if b {
                        then.eval()
                    } else {
                        else_.eval()
                    }
                } else {
                    Err(Error::Runtime("can only cond bool".to_string()))
                }
            }
            Expr::Lambda(x, e) => Ok(Expr::Lambda(x.to_string(), e.clone())),
            Expr::Cons(head, tail) => {
                let head = head.as_ref().eval()?;
                let tail = tail.as_ref().eval()?;
                Ok(Expr::Cons(Box::new(head), Box::new(tail)))
            }
            Expr::Match(e, e2, x, y, e3) => match e.eval()? {
                Expr::Nil => e2.eval(),
                Expr::Cons(head, tail) => {
                    let e3 = e3.substitution(x, &head);
                    let e3 = e3.substitution(y, &tail);
                    e3.eval()
                }
                _ => Err(Error::Runtime("can only match list".to_string())),
            },
            Expr::App(e1, e2) => {
                if let Expr::Lambda(x, y) = e1.eval()? {
                    let value = e2.eval()?;
                    let y = y.substitution(&x, &value);
                    y.eval()
                } else {
                    println!("e1 {} e2 {}", e1, e2);
                    panic!("app e1 not lambda")
                }
            }
        };
        println!("eval {} to {}", self, value.clone().unwrap());
        value
    }

    fn rename(&self, x: &str, fresh: &str) -> Self {
        match self {
            Expr::Integer(_) | Expr::Nil | Expr::Bool(_) => self.clone(),
            Expr::Variable(s) => {
                if x == s {
                    Expr::Variable(fresh.to_string())
                } else {
                    self.clone()
                }
            }
            Expr::Op(left, op, right) => {
                let left = Box::new(left.rename(x, fresh));
                let right = Box::new(right.rename(x, fresh));
                Expr::Op(left, op.clone(), right)
            }
            Expr::If(cond, then, else_) => {
                let cond = Box::new(cond.rename(x, fresh));
                let then = Box::new(then.rename(x, fresh));
                let else_ = Box::new(else_.rename(x, fresh));
                Expr::If(cond, then, else_)
            }
            Expr::Cons(head, tail) => {
                let head = Box::new(head.rename(x, fresh));
                let tail = Box::new(tail.rename(x, fresh));
                Expr::Cons(head, tail)
            }
            Expr::Match(e, e2, head, tail, e3) => {
                let e = Box::new(e.rename(x, fresh));
                let e2 = Box::new(e2.rename(x, fresh));
                let e3 = Box::new(e3.rename(x, fresh));
                let head = if head == x {
                    fresh.to_string()
                } else {
                    head.clone()
                };
                let tail = if tail == x {
                    fresh.to_string()
                } else {
                    tail.clone()
                };
                Expr::Match(e, e2, head, tail, e3)
            }
            Expr::Lambda(y, t) => {
                let y = if y == x { fresh } else { y };
                Expr::Lambda(y.to_string(), t.rename(x, fresh).into())
            }
            Expr::App(e1, e2) => Expr::App(e1.rename(x, fresh).into(), e2.rename(x, fresh).into()),
        }
    }

    fn substitution(&self, x: &str, term: &Expr) -> Self {
        match self {
            Expr::Integer(_) | Expr::Nil | Expr::Bool(_) => self.clone(),
            Expr::Variable(s) => {
                if x == s {
                    term.clone()
                } else {
                    self.clone()
                }
            }
            Expr::Op(left, op, right) => {
                let left = Box::new(left.substitution(x, term));
                let right = Box::new(right.substitution(x, term));
                Expr::Op(left, op.clone(), right)
            }
            Expr::If(cond, then, else_) => {
                let cond = Box::new(cond.substitution(x, term));
                let then = Box::new(then.substitution(x, term));
                let else_ = Box::new(else_.substitution(x, term));
                Expr::If(cond, then, else_)
            }
            Expr::Cons(head, tail) => {
                let head = Box::new(head.substitution(x, term));
                let tail = Box::new(tail.substitution(x, term));
                Expr::Cons(head, tail)
            }
            Expr::Match(e, e2, head, tail, e3) => {
                let e = Box::new(e.substitution(x, term));
                let e2 = Box::new(e2.substitution(x, term));
                let e3 = Box::new(e3.substitution(x, term));
                Expr::Match(e, e2, head.clone(), tail.clone(), e3)
            }
            Expr::Lambda(y, t) => {
                if y == x {
                    self.clone()
                } else {
                    let fv = term.fv();
                    if fv.contains(y) {
                        let fresh = fresh_var();
                        let t = t.rename(y, &fresh);
                        Expr::Lambda(fresh, t.substitution(x, term).into())
                    } else {
                        Expr::Lambda(y.clone(), t.substitution(x, term).into())
                    }
                }
            }
            Expr::App(e1, e2) => Expr::App(
                e1.substitution(x, term).into(),
                e2.substitution(x, term).into(),
            ),
        }
    }

    fn fv(&self) -> FV {
        match self {
            Expr::Integer(_) | Expr::Nil | Expr::Bool(_) => FV::new(),
            Expr::Variable(s) => FV::from([s.clone()]),
            Expr::Op(left, _, right) => {
                let mut fv = left.fv();
                fv.extend(right.fv());
                fv
            }
            Expr::If(cond, then, else_) => {
                let mut fv = cond.fv();
                fv.extend(then.fv());
                fv.extend(else_.fv());
                fv
            }
            Expr::Cons(head, tail) => {
                let mut fv = head.fv();
                fv.extend(tail.fv());
                fv
            }
            Expr::Match(e, e2, x, y, e3) => {
                let mut fv = e.fv();
                fv.extend(e2.fv());
                fv.extend(e3.fv());
                fv.remove(x);
                fv.remove(y);
                fv
            }
            Expr::Lambda(x, e) => {
                let mut fv = e.fv();
                fv.remove(x);
                fv
            }
            Expr::App(e1, e2) => {
                let mut fv = e1.fv();
                fv.extend(e2.fv());
                fv
            }
        }
    }
}

type FV = HashSet<String>;

impl From<&Box<cst::Expr>> for Box<Expr> {
    fn from(value: &Box<cst::Expr>) -> Self {
        let value = Expr::from(value.as_ref());
        Box::new(value)
    }
}

impl From<&cst::Expr> for Expr {
    fn from(value: &cst::Expr) -> Self {
        match value {
            cst::Expr::Let(f, n, m) => {
                let lambda = Box::new(Expr::Lambda(f.clone(), m.into()));
                Expr::App(lambda, n.into())
            }
            cst::Expr::Fun(f, params, body, m) => {
                let lambda = Box::new(Expr::Lambda(f.clone(), m.into()));
                let mut body: Box<Expr> = body.into();
                for para in params.iter().rev() {
                    body = Box::new(Expr::Lambda(para.clone(), body));
                }
                Expr::App(lambda, body)
            }
            cst::Expr::RecFun(f, params, body, m) => {
                // Y ≡ (λy.(λx.y(xx))(λx.y(xx)))
                let y_combinator = {
                    let x = fresh_var();
                    let y = fresh_var();
                    Expr::Lambda(
                        y.clone(),
                        Box::new(Expr::App(
                            Box::new(Expr::Lambda(
                                x.clone(),
                                Box::new(Expr::App(
                                    Box::new(Expr::Variable(y.clone())),
                                    Box::new(Expr::App(
                                        Box::new(Expr::Variable(x.clone())),
                                        Box::new(Expr::Variable(x.clone())),
                                    )),
                                )),
                            )),
                            Box::new(Expr::Lambda(
                                x.clone(),
                                Box::new(Expr::App(
                                    Box::new(Expr::Variable(y.clone())),
                                    Box::new(Expr::App(
                                        Box::new(Expr::Variable(x.clone())),
                                        Box::new(Expr::Variable(x.clone())),
                                    )),
                                )),
                            )),
                        )),
                    )
                };
                let lambda = Box::new(Expr::Lambda(f.clone(), m.into()));
                let mut body: Box<Expr> = body.into();
                for para in params.iter().rev() {
                    body = Box::new(Expr::Lambda(para.clone(), body));
                }
                Expr::App(lambda, Box::new(Expr::App(Box::new(y_combinator), body)))
            }
            cst::Expr::If(cond, then, else_) => Expr::If(cond.into(), then.into(), else_.into()),
            cst::Expr::Match(a, b, c, d, e) => {
                Expr::Match(a.into(), b.into(), c.clone(), d.clone(), e.into())
            }
            cst::Expr::Integer(i) => Expr::Integer(*i),
            cst::Expr::Variable(x) => Expr::Variable(x.clone()),
            cst::Expr::Op(left, op, right) => Expr::Op(left.into(), op.clone(), right.into()),
            cst::Expr::Cons(first, rest) => Expr::Cons(first.into(), rest.into()),
            cst::Expr::Nil => Expr::Nil,
            cst::Expr::App(f, e2) => {
                let mut expr = Box::new(Expr::Variable(f.clone()));
                for arg in e2 {
                    expr = Box::new(Expr::App(expr, Box::new(arg.into())));
                }
                *expr
            }
            cst::Expr::Bool(b) => Expr::Bool(*b),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(i) => write!(f, "{}", i),
            Expr::Variable(x) => write!(f, "{}", x),
            Expr::Nil => write!(f, "[]"),
            Expr::Op(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expr::If(cond, then, else_) => write!(f, "(if {} then {} else {})", cond, then, else_),
            Expr::Cons(head, tail) => write!(f, "({} :: {})", head, tail),
            Expr::Match(e, e2, x, y, e3) => {
                write!(
                    f,
                    "match {} with | [] -> {} | {} :: {} -> {}",
                    e, e2, x, y, e3
                )
            }
            Expr::Lambda(x, e) => write!(f, "λ {} . {}", x, e),
            Expr::App(e1, e2) => write!(f, "({}) ({})", e1, e2),
            Expr::Bool(b) => write!(f, "{b}"),
        }
    }
}
