# Schminke
Schminke, affectionately pronounced "schmin-key" though actually pronounced
"schmin-keh", is a statically-typed, functional language with
an [s-expression](https://en.wikipedia.org/wiki/S-expression) syntax and
local [type inference](https://en.wikipedia.org/wiki/Type_inference).

## Core constructs
* `..., -1, 0, 1, ...`: integer literals
* `a, b, ...`: symbols, primarily used as variables; must start with a letter or
  Unicode symbol, can contain digits thereafter
* `(lambda (<symbol>*) <expr>)`: create an anonymous function introducing zero
  or more parameters that can be used in the body
* `(define <symbol> <expr>)`: create a global definition `x`i usually used to
  name a `lambda`; allows for recursion; requires a corresponding declaration;
  definitions can refer to earlier definitions allowing for mutual recursion
* `(declare <symbol> : <type>)`: declare the type of a global definition;
  required for all global definitions
* `(let ((<symbol> <expr>)*) <expr>)`: create local definitions; definitions are
  bound sequentially (that is, you can't refer to a later definition, if mutual
  recursion is required create global definitions)
* `(if <expr> <expr> <expr>)`: the basic conditional; take the first branch if
  `t`, otherwise the second branch if `nil` (note: `t` is the super-type of all
  objects; `nil` is the unit type, the sub-type of all objects except `void`;
  `void` represents a computation that doesn't return which is different from a
  computation that returns no value, i.e. `nil`)
* `(<symbol> <expr>*)`: application is the process of calling a function with
  zero or more arguments, returns a value or `nil`

## Examples

### The classic
```scheme
(declare fac : Int -> Int)
(define fac
  (lambda (n)
    (if (eq n 0)
        1
        (mul n (fac (sub n 1))))))

(fac 5)
```
