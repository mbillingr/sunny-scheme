# sunny-scheme
Scheme, Rust, and more...

# Crates

## `scm-libs`
Scheme libraries.
Here, the R7RS standard libraries are defined.

A custom file type `.slx` (Scheme library exports) is used to declare names
exported by native libraries. For example `native/file.slx`:

```scheme
(declare-library-exports (native file)
  (export
    file-exists?
    open-input-file
    open-output-file))
```

## `sunny-core`
The core runtime of Sunny Scheme.

## `sunny-libs-native`
Scheme library functions implemented natively in Rust.
These form the building blocks for R7RS compliant standard libraries.

## `sunny-parse`
Parsing of S-expressions. Currently, this simply uses the parser from the [`lexpr` crate](https://crates.io/crates/lexpr) to get the project kick-started.

## `sunny-rustify`
This crate is the meat of the project.
It contains a program written in Scheme that compiles Scheme source code to Rust.
At the moment it does not completely support Scheme although it is complete enough to compile and run itself.

### Features
- [X] Top-level programs
- [X] R7RS-style libraries
- Primitive syntactic forms
  - [x] variable references
  - [x] literal expressions, including `(quote <datum>)`
  - [x] procedure calls
  - [x] procedures `(lambda <formals> <body>)`
  - [x] conditional `(if <test> <consequent> <alternate>?)`
  - [x] assignment `(set! <variable> <expression>)`
  - [ ] inclusion `(include <file1> <file2> ...)`
- Derived syntactic forms
  - [ ] Conditionals
    - [x] `(cond <clause> <clause> ...)`
    - [ ] `(case <key> <clause> ...)`
    - [x] `(and <test> ...)`
    - [ ] `(or <test> ...)`
    - [ ] `(when <test> <expression> ...)`
    - [ ] `(unless <test> <expression> ...)`
    - [ ] `(cond-expand <clause> <clause> ...)`
  - [ ] Binding constructs
    - [x] `let`
    - [x] `let*`
    - [x] `letrec`
    - [ ] `letrec*`
    - [ ] `let-values`
    - [ ] `let*-values`
  - [ ] Sequencing
    - [x] `begin`
  - [ ] Iteration
    - [ ] `do`
    - [ ] named `let`
  - [ ] Delayed evaluation
  - [ ] Dynamic bindings
  - [ ] Exception handling
  - [ ] Quasiquotation
  - [ ] Case-lambda
- [ ] Macros
- R7RS standard libraries
  - [ ] `(scheme base)`
  - [ ] `(scheme read)`
  - [ ] `(scheme write)`
  - [ ] more ...

## `sunny-macro`
Scheme implementation in a Rust macro.

Example usage:

```rust
#[test]
#[cfg(feature = "scm_copy")]
fn fibonacci() {
    scm![(define (fib n)
             (if (less n 2)
                 1
                 (add (fib (sub n 1))
                      (fib (sub n 2)))))];
    assert_eq!(scm![(fib 5)], Scm::Int(8));
}
```

This is mostly a proof-of-concept; I'm actually amazed that a nearly complete subset
of Scheme can be implemented in Rust macros at all.

The limitations (listed below) curb the practical usefulness of this macro.
And why would anybody want to write inline Scheme in Rust anyway?

### Limitations
- Only the most basic forms implemented.
- Most of the standard library is missing.
- No (guaranteed) tail call optimization.
- Rust macros can only work with valid Rust token streams. Thus, it's not possible to
write, for example `'symbol` because that constitutes an invalid character literal. The
long form `(quote symbol)` can be used instead.
- Symbols must be valid Rust identifiers (no names like `do-this`, `*`, `number?`, `set-car!`).
- If `sunny-core` is not compiled with the `"leaking"` feature it is not possible to
move variables into more than one closure (e.g. `(lambda (x) (lambda () x) (lambda () x))`).
This also causes problems with recursion because the value holding the function is moved
into its own closure. However, with the `"leaking"` feature enabled the `Scm` data type is `Copy` and can be freely moved.
- Rust macros are limited to pattern matching so there is no fancy code analysis to
determine if variables are mutable/immutable. All variables are assumed to be mutable.
- There is a lot of redundant dereferencing and cloning. Maybe Rust/LLVM can optimize some
of that away, though.
- No Scheme macros because too much is too much :)
