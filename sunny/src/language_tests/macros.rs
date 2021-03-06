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
