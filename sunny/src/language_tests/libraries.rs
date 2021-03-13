use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn library_definition_evaluates_to_void() {
    assert_that!(SIMPLE_LIBRARY_DEFINITION, EvaluatesTo::void());
}

#[test]
fn can_import_after_library_was_defined() {
    assert_that!(
        vec![SIMPLE_LIBRARY_DEFINITION, "(import (foo bar))"],
        EvaluatesTo::void()
    );
}

#[test]
fn reexports_target_same_variable() {
    assert_that!(
        vec![
            // todo: use renaming import or export when they are implemented
            "(define-library (foo) (export x) (begin (define x 1)))",
            "(define-library (bar) (export x) (import (foo)))",
            "(import (bar))",
            "(set! x 42)",
            "(import (foo))",
            "x",
        ],
        EvaluatesTo::the_integer(42)
    );
}

#[test]
fn can_import_macros_from_library() {
    assert_that!(
        vec![
            SIMPLE_LIBRARY_DEFINITION,
            "(import (foo bar))",
            "(get-private)"
        ],
        EvaluatesTo::the_integer(123)
    );
}

const SIMPLE_LIBRARY_DEFINITION: &str = "
(define-library (foo bar)
    (export baz get-private)
    (begin
        (define baz 42)
        (define private 123)
        (define-syntax get-private (simple-macro () private))
    )
)";
