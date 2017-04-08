# Schminke
A statically typed language with an
[s-expression](https://en.wikipedia.org/wiki/S-expression) syntax and local
[type inference](https://en.wikipedia.org/wiki/Type_inference).

## TODO
* Introduce mu (μ) for recursive definitions. A mu expression is defined as
  having the identifiers of all the (mutually) recursive functions used in the
  body.
* Require top-level type annotations. This allows for polymorphic recursion
  which in turn allows for
  the
  [Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding).
  Top-level type annotations also serve as a form of documentation, very useful
  for any programmer reading the source.
* How can we relate mu expressions to continuations?
  See [λμ calculus](https://en.wikipedia.org/wiki/Lambda-mu_calculus):
  > Semantically these operators correspond to continuations, ...
* Transformation to continuation-passing style (CPS).
  See:
  [Compiling with Continuations](https://books.google.com.au/books?id=zbDBQgAACAAJ&dq=isbn:0521416957&hl=en&sa=X&ved=0ahUKEwiD3fX6-5LTAhWMTbwKHWj1CSQQ6AEIGzAA)
* Write a parser and pretty printer for the Core language
