use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn library_definition_evaluates_to_void() {
    assert_that!(SIMPLE_LIBRARY_DEFINITION, EvaluatesTo::void());
}

const SIMPLE_LIBRARY_DEFINITION: &str = "
(define-library (foo bar)
    (import (scheme base))
    (export baz)
    (begin
        (set! baz 0)
    )
)";
