# TODO
* Varargs to support `printf` and ilk.
* Support all the [LLVM built-ins](http://llvm.org/docs/LangRef.html); which
    begs the questions, should `cbr` and `phi` be in the language even though
    we have `if`? Might as well give the user full control.
* Additional primitive and composite types: floats, chars, strings, arrays,
    structs, enums (symbols), lists.
* Proper error reporting; hard with type inference.
* Macros.

## Possible extensions
* Downwards funargs.
* Unions.
* RAII.
* Algebraic data types and pattern matching; but tagged unions would make
    interoperability with C harder.
* Parametric polymorphism; reserving lowercase characters for this extension.
* Type classes; mesh well with parametric polymorphism.

## Unplanned features
* Garbage collection; which eliminates convenient use of a lot of higher-level
    programming constructs, but our aim is low anyways.
* Upwards funargs; closures mess with C interoperability.
