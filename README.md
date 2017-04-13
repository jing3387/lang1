# Schminke
Schminke, affectionately pronounced "schmin-key" though actually pronounced
["schmin-kuh"](https://translate.google.com/#de/en/Schminke), is a language with a delightful
[s-expression](https://en.wikipedia.org/wiki/S-expression) syntax, delicious local [type
inference](https://en.wikipedia.org/wiki/Type_inference), and defiant [low-level
operations](https://en.wikipedia.org/wiki/Low-level_programming_language). Basically a thin wrapper
around [LLVM IR](http://llvm.org/docs/LangRef.html), Schminke aims to make interfacing with C
libraries fun and easy; perhaps it'll come about that C will have an enjoyable time interfacing
with Schminke.

## Core constructs
* `..., -1, 0, 1, ...`: 64-bit integer literals
* `a, b, ...`: symbols, primarily used as variables; must start with a letter or
  Unicode symbol, can contain digits thereafter
* `(define <symbol> (<symbol>*) <expr>*)`: define a function; requires a corresponding declaration;
  definitions can refer to later definitions allowing for mutual recursion
* `(declare <symbol> <type> <type>*)`: declare the type of a function; required for all definitions
  as they serve as a means of documentation.
* `(let ((<symbol> <expr>)*) <expr>*)`: introduce local variables; variables are bound to
  expressions sequentially meaning that later bindings can refer to earlier bindings but not vice
  versa.
* `(if <expr> <expr> <expr>)`: the basic conditional
* `(<symbol> <expr>*)`: call a function with zero or more arguments

## Examples

### The classic
```scheme
(declare fac i64 (i64))
(define fac (n)
  (if (eq n 0)
      1
      (mul n (fac (sub n 1)))))

(fac 5)
```
