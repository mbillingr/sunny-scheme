# sunny-scheme
Sunny Scheme shines a lispy sun on friendly crabs

# Crates

## `sunny-core` 
The core runtime of Sunny Scheme.

## `sunny-macro`
Scheme implemented as a rust macro.

Example:

```TODO``` 

### Limitations
- Not yet fully implemented.
- Rust macros can only work with valid Rust token streams. Thus, it's not possible to 
write, for example `'symbol` because that constitutes an invalid character literal. The 
long form `(quote symbol)` can be used instead.
- If `sunny-core` is not compiled with the `"leaking"` feature it is not possible to
move variables into more than one closure (e.g. `(lambda (x) (lambda () x) (lambda () x))`).
With the `"leaking"` feature enabled the `Scm` data type is `Copy` and can be freely moved.
- Rust macros are limited to pattern matching so there is no fancy code analysis to 
determine if variables are mutable/immutable. All variables are assumed to be mutable.
- There is a lot of redundant dereferencing and cloning. Maybe Rust/LLVM can optimize some
of that away, though.
- No Scheme macros because too much is too much :)