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

#[test]
fn syntax_rules_hygiene_template_uses_bound_bindings_from_usage_env() {
    assert_that!(
        vec![
            "(define x 42)",
            "(define-syntax foo
              (syntax-rules ()
                ((foo x) x)))",
            "(let ((x 123)) (foo x))"
        ],
        EvaluatesTo::the_integer(123)
    );
}

#[test]
fn syntax_rules_expand_recursively() {
    assert_that!(
        vec![
            "(define-syntax foo
              (syntax-rules ()
                ((foo) '())
                ((foo x1 x2 ...) (cons x1 (foo x2 ...))) ))",
            "(foo 1 2 3)"
        ],
        EvaluatesTo::the_list(vec![1, 2, 3])
    );
}

#[test]
fn syntax_rules_respect_literals_list() {
    assert_that!(
        vec![
            "(define-syntax foo
              (syntax-rules (a b)
                ((foo a) 1)
                ((foo b) 2) ))",
            "(foo b)"
        ],
        EvaluatesTo::the_integer(2)
    );
}

#[test]
fn syntax_rules_multiple_repetitions() {
    assert_that!(
        vec![
            "(define (list . x) x)",
            "(define-syntax foo
              (syntax-rules ()
                ((_ (a b ...) z ...)
                 (list a b ... z ...))))",
            "(foo (1 2) 3 4)"
        ],
        EvaluatesTo::the_list(vec![1, 2, 3, 4])
    );
}

#[test]
fn syntax_rules_use_local_as_call_argument_in_expansion() {
    assert_that!(
        vec![
            "(define-syntax foo
              (syntax-rules ()
                ((foo x) x)))",
            "(define (use-foo y) (foo (- y)))",
            "(use-foo 42)"
        ],
        EvaluatesTo::the_integer(-42)
    );
}

#[test]
fn syntax_rules_recursion_with_multiple_repetitions() {
    assert_that!(
        vec![
            "(define-syntax last
              (syntax-rules ()
                ((last (x))
                 x)
                ((last (x) rest ...)
                 (last rest ...))
              ))",
            "(last (1) (2))"
        ],
        EvaluatesTo::the_integer(2)
    );
}
