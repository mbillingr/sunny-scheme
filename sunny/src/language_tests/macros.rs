use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn can_define_simple_macros() {
    assert_that!(
        "(begin
           (define-syntax delay
             (simple-macro (expr) (lambda () expr)))
           (delay 1))",
        EvaluatesTo::a_procedure()
    );
    assert_that!(
        "(begin
           (define-syntax delay
             (simple-macro (expr) (lambda () expr)))
           ((delay 1)))",
        EvaluatesTo::the_integer(1)
    );
}

#[test]
fn macros_resolve_symbols_in_their_definition_environment() {
    assert_that!(
        "(begin
           (define-syntax delay
             (simple-macro (expr) (lambda () expr)))
           (define lambda 0)
           (delay 1))",
        EvaluatesTo::a_procedure()
    );
}

#[test]
fn syntax_used_as_value_is_an_error() {
    assert_that!(
        vec!["(define-syntax zero (simple-macro () 0))", "zero"],
        EvaluatesTo::an_error()
    );
}

#[test]
fn support_syntax_rules_transformers() {
    assert_that!(
        vec![
            "(define-syntax kwote
              (syntax-rules ()
                ((kwote x) (quote x))))",
            "(kwote 0)"
        ],
        EvaluatesTo::the_integer(0)
    );
}

#[test]
fn syntax_rules_hygiene_template_uses_bindings_from_definition_env() {
    assert_that!(
        vec![
            "(define x 42)",
            "(define-syntax foo
              (syntax-rules ()
                ((foo) x)))",
            "(let ((x 123)) (foo))"
        ],
        EvaluatesTo::the_integer(42)
    );
}
