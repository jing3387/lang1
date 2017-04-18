# TODO
* Parametric polymorphism; through raw substitution for now.
* Varargs to support `printf` and ilk.
* Support all the [LLVM built-ins](http://llvm.org/docs/LangRef.html); which
    begs the questions, should `cbr` and `phi` be in the language even though
    we have `if`? Might as well give the user full control.
* Additional primitive and composite types: floats, chars, strings, arrays,
    structs, unions, enums (symbols).
* Proper error reporting (hard with constraint generation being separate from
    unification).

## Future extensions
* RAII.
* Algebraic data types and pattern matching.

## Unplanned features
* Garbage collection; which eliminates convenient use of a lot of higher-level
    programming constructs, but our aim is low anyways.
