use std::io;

use lambdar::error::Error;
use lambdar::{ast, cst, lambda};

fn run(content: &str) -> Result<(), Error> {
    let expr: Box<cst::Expr> = lambdar::grammar::ExprParser::new()
        .parse(content)
        .map_err(|x| Error::ParseError(x.to_string()))?;
    println!("\nConcrete Syntax Tree\n{}", expr);

    let expr: ast::Expr = expr.as_ref().into();
    println!("\nAbstract Synatx Tree\n{}\n", expr);
    // let v = expr.eval()?;
    // println!("Eval to {v}");

    let expr: lambda::Term = (&expr).into();
    println!("\nLambda Calculus\n{}\n", expr);
    let v = expr.eval()?;
    println!("Eval to {v}");
    Ok(())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        interact();
    } else {
        let path = &args[1];
        let content = std::fs::read_to_string(path).unwrap();
        if let Err(e) = run(&content) {
            println!("{e}");
        }
    }
}

fn interact() {
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        if let Err(e) = run(&input) {
            println!("{e}");
        }
    }
}
