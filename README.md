# LambdaR Programming Language

LambdaR(lambda, really), a simple programming language to show the power of lambda calculus.

## Example

The language itself look like a very simple Rust/Ocaml with dynamic type.
The **interesting** part of the project is that we donot need to write an interpreter for the concrete grammar.
Instead, in this project, we try to tranform the modern language to pure lambda calculus.

```rust
let a = if 2 == 2 {
    1
} else {
    3
}; 

let b = if 3 >= 11 {
    2
} else {
    4
};

a + b
```

### Stage1 CST

```ocaml
let a = if 2 = 2 then 1 else 3 in let b = if 3 >= 11 then 2 else 4 in a + b
```

### Stage2 AST

Lambda Calculus with basic structure.

```ocaml
(λ a . (λ b . (a + b)) ((if (3 >= 11) then 2 else 4))) ((if (2 = 2) then 1 else 3))
```

### Stage3 Pure Lambda Calculus

The most interesting part is all the modern syntax (match, if, let, fn...) can be desuager the a standard lambda calculus model:

```text
(λa.(λb.((λm.λn.λf.λx.((m f) ((n f) x)) a) b) (((λc.λt.λe.((c t) e) ((λm.λn.(λn.((n λx.λx.λy.y) λx.λy.x) ((λm.λn.((n λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)) m) n) m)) λf.λx.(f (f (f x)))) λf.λx.(f (f (f (f (f (f (f (f (f (f (f x))))))))))))) λf.λx.(f (f x))) λf.λx.(f (f (f (f x)))))) (((λc.λt.λe.((c t) e) ((λm.λn.((λp.λq.((p q) p) (λn.((n λx.λx.λy.y) λx.λy.x) ((λm.λn.((n λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)) m) m) n))) (λn.((n λx.λx.λy.y) λx.λy.x) ((λm.λn.((n λn.λf.λx.(((n λg.λh.(h (g f))) λu.x) λu.u)) m) n) m))) λf.λx.(f (f x))) λf.λx.(f (f x)))) λf.λx.(f x)) λf.λx.(f (f (f x)))))
```

which evaluate to `λf.λx.(f (f (f (f (f x)))))`, i.e. 5.

## Reference

[wiki](https://en.wikipedia.org/wiki/Lambda_calculus)
