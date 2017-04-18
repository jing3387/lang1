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
  allowing for mutual recursion.
* `(declare <symbol> <symbol>* <type> <type>*)`: declare the type of a
  function; required for all definitions as they serve as a means of
  documentation.
* `(let ((<symbol> <expr>)*) <expr>*)`: introduce local variables; variables
  are bound to expressions sequentially meaning that later bindings can refer
  to earlier bindings but not vice versa.
* `(if <expr> <expr> <expr>)`: the basic conditional.
* `(<symbol> <expr>*)`: call a function with zero or more arguments.

## Examples
Examples can be found in `examples/`.

## Dependencies
* [Stack](http://haskellstack.org/)
* [LLVM 4.0.0](http://llvm.org/)

### Testing
* [BATS](https://github.com/sstephenson/bats)

### Nix
Nix is disabled by default to support nixless users. If you have Nix installed
simply pass `--nix` to Stack commands to resolve dependencies. I personally
have `stack` aliased to `stack --nix`.

Note: because you basically need LLVM for processing Schminke's output and BATS
for testing the executable you'll need to install them in your environment:

```
$ nix-env -i llvm_4 bats
```

## Testing
To test the assembler run:
```
$ stack test
```

To test the resulting executable run:
```
$ stack install
$ bats test.bats
```
It is required that stack's installation location, defaulting to
`~/.local/bin`, is on the path before running the BATS test suite.

## Usage
Schminke generates LLVM IR to `stdout` which can then be manipulated in
arbitrary ways by the [LLVM tools](http://llvm.org/docs/CommandGuide/). An
example would be piping the output of Schminke to the LLVM interpreter:

```bash
$ stack install
$ sch examples/fac.sch | lli-4.0
$ echo $?
120
```
