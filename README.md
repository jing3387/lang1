# Schminke
Schminke, affectionately pronounced "schmin-key" though actually pronounced
["schmin-kuh"](https://translate.google.com/#de/en/Schminke), is a language
with an [s-expression](https://en.wikipedia.org/wiki/S-expression) syntax,
local [type inference](https://en.wikipedia.org/wiki/Type_inference), and
[low-level
operations](https://en.wikipedia.org/wiki/Low-level_programming_language).
Basically a thin wrapper around [LLVM IR](http://llvm.org/docs/LangRef.html),
Schminke aims to make interfacing with C libraries fun and easy; perhaps it'll
come about that C will have an enjoyable time interfacing with Schminke.

## Core constructs
* `..., -1, 0, 1, ...`: 64-bit integer literals
* `a, b, ...`: symbols, primarily used as variables; must start with a letter
  or Unicode symbol, can contain digits thereafter
* `(define <symbol> (<symbol>*) <expr>*)`: define a function; requires a
  corresponding declaration; definitions can refer to later definitions
  allowing for mutual recursion
* `(declare <symbol> <symbol>* <type> <type>*)`: declare the type of a
  function; required for all definitions as they serve as a means of
  documentation.
* `(let ((<symbol> <expr>)*) <expr>*)`: introduce local variables; variables
  are bound to expressions sequentially meaning that later bindings can refer
  to earlier bindings but not vice versa.
* `(if <expr> <expr> <expr>)`: the basic conditional
* `(<symbol> <expr>*)`: call a function with zero or more arguments

## Examples

### The classic
```scheme
(declare fac () i64 (i64))
(define fac (n)
  (if (eq n 0)
      1
      (mul n (fac (sub n 1)))))

(fac 5)
```

## Dependencies
* [stack](http://haskellstack.org/)
* [llvm](http://llvm.org/)
* [bats](https://github.com/sstephenson/bats)

## Testing
To test the assembler run:
```bash
$ stack test
```

To test the resulting executable run:
```bash
$ stack install
$ bats bats/test.bats
```
It is required that stack's installation location, defaulting to
`~/.local/bin`, is on the path before running the BATS test suite.
