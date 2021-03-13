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

const SIMPLE_LIBRARY_DEFINITION: &str = "
(define-library (foo bar)
    (import (scheme base))
    (export baz)
    (begin
        (define baz 42)
    )
)";
