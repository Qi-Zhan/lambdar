//! Simplified Syntax Tree
//!
//! basiclly lambda calculus

use std::{collections::HashSet, fmt};

use crate::ast::{self, ID};
use crate::error::Error;

#[derive(Debug, Clone, Eq)]
pub enum Term {
    Variable(ID),
    Lambda(ID, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DeBruijn {
    Variable(usize),
    Lambda(Box<DeBruijn>),
    App(Box<DeBruijn>, Box<DeBruijn>),
}

use std::sync::atomic::{AtomicUsize, Ordering};
static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn fresh_var() -> String {
    format!("x{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

impl Term {
    fn beta_reduce(&self) -> Term {
        match self {
            Term::Variable(_) => self.clone(),
            Term::Lambda(x, e) => Term::Lambda(x.clone(), e.beta_reduce().into()),
            Term::App(e1, e2) => {
                if let Term::Lambda(x, e) = e1.as_ref() {
                    e.substitution(x, e2)
                } else {
                    Term::App(e1.beta_reduce().into(), e2.beta_reduce().into())
                }
            }
        }
    }

    pub fn eval(&self) -> Result<Term, Error> {
        let mut results = self.clone();
        let mut i = 0;
        while i < 150 {
            let next = results.beta_reduce();
            if next == results {
                break;
            }
            results = next;
            i += 1;
        }
        Ok(results)
    }

    fn debruijn(&self, env: &mut Vec<ID>) -> DeBruijn {
        match self {
            Term::Variable(x) => {
                let i = env.iter().rev().position(|y| y == x).unwrap();
                DeBruijn::Variable(i)
            }
            Term::Lambda(x, e) => {
                env.push(x.clone());
                let e = e.debruijn(env);
                env.pop();
                DeBruijn::Lambda(Box::new(e))
            }
            Term::App(e1, e2) => {
                let e1 = e1.debruijn(env);
                let e2 = e2.debruijn(env);
                DeBruijn::App(Box::new(e1), Box::new(e2))
            }
        }
    }

    fn rename(&self, x: &str, fresh: &str) -> Self {
        match self {
            Term::Variable(s) => {
                if x == s {
                    Term::Variable(fresh.to_string())
                } else {
                    self.clone()
                }
            }
            Term::Lambda(y, t) => {
                let y = if y == x { fresh } else { y };
                Term::Lambda(y.to_string(), t.rename(x, fresh).into())
            }
            Term::App(e1, e2) => Term::App(e1.rename(x, fresh).into(), e2.rename(x, fresh).into()),
        }
    }

    fn substitution(&self, x: &str, term: &Term) -> Self {
        match self {
            Term::Variable(s) => {
                if x == s {
                    term.clone()
                } else {
                    self.clone()
                }
            }
            Term::Lambda(y, t) => {
                if y == x {
                    self.clone()
                } else {
                    let fv = term.fv();
                    if fv.contains(y) {
                        let fresh = fresh_var();
                        let t = t.rename(y, &fresh);
                        Term::Lambda(fresh, t.substitution(x, term).into())
                    } else {
                        Term::Lambda(y.clone(), t.substitution(x, term).into())
                    }
                }
            }
            Term::App(e1, e2) => Term::App(
                e1.substitution(x, term).into(),
                e2.substitution(x, term).into(),
            ),
        }
    }

    fn fv(&self) -> FV {
        match self {
            Term::Variable(s) => FV::from([s.clone()]),
            Term::Lambda(x, e) => {
                let mut fv = e.fv();
                fv.remove(x);
                fv
            }
            Term::App(e1, e2) => {
                let mut fv = e1.fv();
                fv.extend(e2.fv());
                fv
            }
        }
    }
}

type FV = HashSet<String>;

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.debruijn(&mut vec![]) == other.debruijn(&mut vec![])
    }
}

impl From<&i32> for Term {
    fn from(value: &i32) -> Self {
        if *value >= 0 {
            let mut t = Term::Variable("x".to_string());
            for _ in 0..*value {
                t = Term::App(Box::new(Term::Variable("f".to_string())), Box::new(t));
            }
            Term::Lambda(
                "f".to_string(),
                Box::new(Term::Lambda("x".to_string(), Box::new(t))),
            )
        } else {
            todo!()
        }
    }
}

/// TRUE := λx.λy.x
/// FALSE := λx.λy.y
impl From<&bool> for Term {
    fn from(value: &bool) -> Self {
        match value {
            true => Term::Lambda(
                "x".to_string(),
                Box::new(Term::Lambda(
                    "y".to_string(),
                    Box::new(Term::Variable("x".to_string())),
                )),
            ),
            false => Term::Lambda(
                "x".to_string(),
                Box::new(Term::Lambda(
                    "y".to_string(),
                    Box::new(Term::Variable("y".to_string())),
                )),
            ),
        }
    }
}

// nil := λx. TRUE
impl From<()> for Term {
    fn from(_: ()) -> Self {
        let t = Term::from(&true);
        Term::Lambda("x".to_string(), Box::new(t))
    }
}

impl From<&ast::BinOp> for Term {
    fn from(value: &ast::BinOp) -> Self {
        match value {
            ast::BinOp::Arith(ari) => ari.into(),
            ast::BinOp::Pred(p) => p.into(),
        }
    }
}

impl From<&ast::ArithOp> for Term {
    fn from(value: &ast::ArithOp) -> Self {
        // SUCC := λn.λf.λx.f (n f x)
        let _succ = Term::Lambda(
            "n".to_string(),
            Box::new(Term::Lambda(
                "f".to_string(),
                Box::new(Term::Lambda(
                    "x".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Variable("f".to_string())),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::Variable("n".to_string())),
                                Box::new(Term::Variable("f".to_string())),
                            )),
                            Box::new(Term::Variable("x".to_string())),
                        )),
                    )),
                )),
            )),
        );
        match value {
            // MULT := λm.λn.λf.m (n f)
            ast::ArithOp::Mul => Term::Lambda(
                "m".to_string(),
                Box::new(Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::Lambda(
                        "f".to_string(),
                        Box::new(Term::App(
                            Box::new(Term::Variable("m".to_string())),
                            Box::new(Term::App(
                                Box::new(Term::Variable("n".to_string())),
                                Box::new(Term::Variable("f".to_string())),
                            )),
                        )),
                    )),
                )),
            ),
            // PLUS := λm.λn.λf.λx.m f (n f x)
            ast::ArithOp::Add => Term::Lambda(
                "m".to_string(),
                Box::new(Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::Lambda(
                        "f".to_string(),
                        Box::new(Term::Lambda(
                            "x".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Variable("m".to_string())),
                                    Box::new(Term::Variable("f".to_string())),
                                )),
                                Box::new(Term::App(
                                    Box::new(Term::App(
                                        Box::new(Term::Variable("n".to_string())),
                                        Box::new(Term::Variable("f".to_string())),
                                    )),
                                    Box::new(Term::Variable("x".to_string())),
                                )),
                            )),
                        )),
                    )),
                )),
            ),
            ast::ArithOp::Sub => {
                // PRED := λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
                let pred = Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::Lambda(
                        "f".to_string(),
                        Box::new(Term::Lambda(
                            "x".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::App(
                                        Box::new(Term::Variable("n".to_string())),
                                        Box::new(Term::Lambda(
                                            "g".to_string(),
                                            Box::new(Term::Lambda(
                                                "h".to_string(),
                                                Box::new(Term::App(
                                                    Box::new(Term::Variable("h".to_string())),
                                                    Box::new(Term::App(
                                                        Box::new(Term::Variable("g".to_string())),
                                                        Box::new(Term::Variable("f".to_string())),
                                                    )),
                                                )),
                                            )),
                                        )),
                                    )),
                                    Box::new(Term::Lambda(
                                        "u".to_string(),
                                        Box::new(Term::Variable("x".to_string())),
                                    )),
                                )),
                                Box::new(Term::Lambda(
                                    "u".to_string(),
                                    Box::new(Term::Variable("u".to_string())),
                                )),
                            )),
                        )),
                    )),
                );
                // SUB := λm.λn.n PRED m
                Term::Lambda(
                    "m".to_string(),
                    Box::new(Term::Lambda(
                        "n".to_string(),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::Variable("n".to_string())),
                                Box::new(pred),
                            )),
                            Box::new(Term::Variable("m".to_string())),
                        )),
                    )),
                )
            }
        }
    }
}

impl From<&ast::Pred> for Term {
    fn from(value: &ast::Pred) -> Self {
        let f = Term::from(&false);
        let t = Term::from(&true);
        // ISZERO := λn.n (λx.FALSE) TRUE
        let is_zero = Term::Lambda(
            "n".to_string(),
            Box::new(Term::App(
                Box::new(Term::App(
                    Box::new(Term::Variable("n".to_string())),
                    Box::new(Term::Lambda("x".to_string(), Box::new(f.clone()))),
                )),
                Box::new(t.clone()),
            )),
        );
        // AND := λp.λq.p q p
        let and = Term::Lambda(
            "p".to_string(),
            Box::new(Term::Lambda(
                "q".to_string(),
                Box::new(Term::App(
                    Box::new(Term::App(
                        Box::new(Term::Variable("p".to_string())),
                        Box::new(Term::Variable("q".to_string())),
                    )),
                    Box::new(Term::Variable("p".to_string())),
                )),
            )),
        );
        // OR := λp.λq.p p q
        let _or = Term::Lambda(
            "p".to_string(),
            Box::new(Term::Lambda(
                "q".to_string(),
                Box::new(Term::App(
                    Box::new(Term::App(
                        Box::new(Term::Variable("p".to_string())),
                        Box::new(Term::Variable("p".to_string())),
                    )),
                    Box::new(Term::Variable("q".to_string())),
                )),
            )),
        );
        // NOT := λp.p FALSE TRUE
        let not = Term::Lambda(
            "p".to_string(),
            Box::new(Term::App(
                Box::new(Term::App(
                    Box::new(Term::Variable("p".to_string())),
                    Box::new(f.clone()),
                )),
                Box::new(t.clone()),
            )),
        );
        let first_minus_second = Term::App(
            Box::new(Term::App(
                Box::new(Term::from(&ast::ArithOp::Sub)),
                Box::new(Term::Variable("m".to_string())),
            )),
            Box::new(Term::Variable("n".to_string())),
        );
        let second_minus_first = Term::App(
            Box::new(Term::App(
                Box::new(Term::from(&ast::ArithOp::Sub)),
                Box::new(Term::Variable("n".to_string())),
            )),
            Box::new(Term::Variable("m".to_string())),
        );
        match value {
            // EQ := λm.λn.AND (ISZERO (SUB m n)) (ISZERO (SUB n m))
            ast::Pred::Eq => Term::Lambda(
                "m".to_string(),
                Box::new(Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(and),
                            Box::new(Term::App(
                                Box::new(is_zero.clone()),
                                Box::new(first_minus_second),
                            )),
                        )),
                        Box::new(Term::App(
                            Box::new(is_zero.clone()),
                            Box::new(second_minus_first),
                        )),
                    )),
                )),
            ),
            // ge := λm.λn.ISZERO (SUB n m)
            ast::Pred::Ge => Term::Lambda(
                "m".to_string(),
                Box::new(Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::App(
                        Box::new(is_zero.clone()),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::from(&ast::ArithOp::Sub)),
                                Box::new(Term::Variable("n".to_string())),
                            )),
                            Box::new(Term::Variable("m".to_string())),
                        )),
                    )),
                )),
            ),
            // LT := λm.λn.AND (ISZERO (SUB m n)) (NOT (ISZERO (SUB n m)))
            ast::Pred::Lt => {
                let not_is_zero = Term::App(
                    Box::new(not),
                    Box::new(Term::App(
                        Box::new(is_zero.clone()),
                        Box::new(second_minus_first.clone()),
                    )),
                );
                Term::Lambda(
                    "m".to_string(),
                    Box::new(Term::Lambda(
                        "n".to_string(),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(and),
                                Box::new(Term::App(
                                    Box::new(is_zero.clone()),
                                    Box::new(first_minus_second.clone()),
                                )),
                            )),
                            Box::new(not_is_zero),
                        )),
                    )),
                )
            }
            // LEQ := λm.λn.ISZERO (SUB m n)
            ast::Pred::Le => Term::Lambda(
                "m".to_string(),
                Box::new(Term::Lambda(
                    "n".to_string(),
                    Box::new(Term::App(
                        Box::new(is_zero.clone()),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(Term::from(&ast::ArithOp::Sub)),
                                Box::new(Term::Variable("m".to_string())),
                            )),
                            Box::new(Term::Variable("n".to_string())),
                        )),
                    )),
                )),
            ),
            // Gt := λm.λn.AND (ISZERO (SUB n m)) (NOT (ISZERO (SUB m n)))
            ast::Pred::Gt => {
                let not_is_zero = Term::App(
                    Box::new(not),
                    Box::new(Term::App(
                        Box::new(is_zero.clone()),
                        Box::new(first_minus_second.clone()),
                    )),
                );
                Term::Lambda(
                    "m".to_string(),
                    Box::new(Term::Lambda(
                        "n".to_string(),
                        Box::new(Term::App(
                            Box::new(Term::App(
                                Box::new(and),
                                Box::new(Term::App(
                                    Box::new(is_zero.clone()),
                                    Box::new(second_minus_first.clone()),
                                )),
                            )),
                            Box::new(not_is_zero),
                        )),
                    )),
                )
            }
        }
    }
}

impl From<&Box<ast::Expr>> for Box<Term> {
    fn from(value: &Box<ast::Expr>) -> Self {
        let value = Term::from(value.as_ref());
        Box::new(value)
    }
}

impl From<&ast::Expr> for Term {
    fn from(value: &ast::Expr) -> Self {
        match value {
            ast::Expr::Integer(i) => i.into(),
            ast::Expr::Variable(x) => Term::Variable(x.clone()),
            ast::Expr::Nil => ().into(),
            ast::Expr::Bool(b) => b.into(),
            ast::Expr::Op(left, op, right) => {
                let op: Term = op.into();
                Term::App(Box::new(Term::App(Box::new(op), left.into())), right.into())
            }
            // IF := λc.λt.λe.c t e
            ast::Expr::If(cond, then, els) => {
                let if_ = Term::Lambda(
                    "c".to_string(),
                    Box::new(Term::Lambda(
                        "t".to_string(),
                        Box::new(Term::Lambda(
                            "e".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Variable("c".to_string())),
                                    Box::new(Term::Variable("t".to_string())),
                                )),
                                Box::new(Term::Variable("e".to_string())),
                            )),
                        )),
                    )),
                );
                Term::App(
                    Box::new(Term::App(
                        Box::new(Term::App(Box::new(if_), cond.into())),
                        then.into(),
                    )),
                    els.into(),
                )
            }
            // Cons: λx.λy.λz.z x y
            ast::Expr::Cons(x, y) => {
                let pair = Term::Lambda(
                    "x".to_string(),
                    Box::new(Term::Lambda(
                        "y".to_string(),
                        Box::new(Term::Lambda(
                            "z".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Variable("z".to_string())),
                                    Box::new(Term::Variable("x".to_string())),
                                )),
                                Box::new(Term::Variable("y".to_string())),
                            )),
                        )),
                    )),
                );
                Term::App(Box::new(Term::App(Box::new(pair), x.into())), y.into())
            }
            // match e nil x y cons => if ISNULL e then nil else (λx.λy. cons) FST e SND e
            ast::Expr::Match(e, nil, x, y, cons) => {
                let f: Term = Term::from(&false);
                let t: Term = Term::from(&true);
                // FIRST := λp.p TRUE
                let first = Term::Lambda(
                    "p".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Variable("p".to_string())),
                        t.into(),
                    )),
                );
                // SECOND := λp.p FALSE
                let second = Term::Lambda(
                    "p".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Variable("p".to_string())),
                        f.clone().into(),
                    )),
                );
                // ISNULL := λp.p(λx.λy. FALSE)
                let is_nil = Term::Lambda(
                    "p".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Variable("p".to_string())),
                        Box::new(Term::Lambda(
                            "x".to_string(),
                            Box::new(Term::Lambda("y".to_string(), Box::new(f))),
                        )),
                    )),
                );
                let if_ = Term::Lambda(
                    "c".to_string(),
                    Box::new(Term::Lambda(
                        "t".to_string(),
                        Box::new(Term::Lambda(
                            "e".to_string(),
                            Box::new(Term::App(
                                Box::new(Term::App(
                                    Box::new(Term::Variable("c".to_string())),
                                    Box::new(Term::Variable("t".to_string())),
                                )),
                                Box::new(Term::Variable("e".to_string())),
                            )),
                        )),
                    )),
                );
                let cond = Box::new(Term::App(Box::new(is_nil), e.into()));
                let then = nil.into();
                let els = Box::new(Term::App(
                    Box::new(Term::App(
                        Box::new(Term::Lambda(
                            x.to_string(),
                            Box::new(Term::Lambda(y.to_string(), cons.into())),
                        )),
                        Box::new(Term::App(Box::new(first), e.into())),
                    )),
                    Box::new(Term::App(Box::new(second), e.into())),
                ));
                Term::App(
                    Box::new(Term::App(Box::new(Term::App(Box::new(if_), cond)), then)),
                    els,
                )
            }
            ast::Expr::Lambda(x, y) => Term::Lambda(x.clone(), y.into()),
            ast::Expr::App(x, y) => Term::App(x.into(), y.into()),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(x) => write!(f, "{}", x),
            Term::Lambda(x, e) => write!(f, "λ{}.{}", x, e),
            Term::App(e1, e2) => write!(f, "({} {})", e1, e2),
        }
    }
}

impl fmt::Display for DeBruijn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeBruijn::Variable(i) => write!(f, "{}", i),
            DeBruijn::Lambda(e) => write!(f, "λ{}", e),
            DeBruijn::App(e1, e2) => write!(f, "{}{}", e1, e2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debrui() {
        // λx. λy. x
        let k = Term::Lambda(
            "x".to_string(),
            Box::new(Term::Lambda(
                "y".to_string(),
                Box::new(Term::Variable("x".to_string())),
            )),
        );
        let mut env = vec![];
        let k = k.debruijn(&mut env);
        assert_eq!(k.to_string(), "λλ1");
        // λx. λy. λz. x z (y z)
        let s = Term::Lambda(
            "x".to_string(),
            Box::new(Term::Lambda(
                "y".to_string(),
                Box::new(Term::Lambda(
                    "z".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::App(
                            Box::new(Term::Variable("x".to_string())),
                            Box::new(Term::Variable("z".to_string())),
                        )),
                        Box::new(Term::App(
                            Box::new(Term::Variable("y".to_string())),
                            Box::new(Term::Variable("z".to_string())),
                        )),
                    )),
                )),
            )),
        );
        let mut env = vec![];
        let s = s.debruijn(&mut env);
        assert_eq!(s.to_string(), "λλλ2010");
    }
}
