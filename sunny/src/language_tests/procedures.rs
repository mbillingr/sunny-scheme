use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn lambda_defines_procedures() {
    assert_that!("(lambda () 0)", EvaluatesTo::a_procedure());
}

#[test]
fn functions_can_take_fixed_arguments() {
    assert_that!("(lambda (x y z) y)", EvaluatesTo::a_procedure());
}

#[test]
fn functions_can_take_fixed_and_variable_arguments() {
    assert_that!("(lambda (x y . z) z)", EvaluatesTo::a_procedure());
}

#[test]
fn passing_too_few_arguments_is_an_error() {
    assert_that!("((lambda (x) x))", EvaluatesTo::an_error());
    assert_that!("((lambda (x . y) x))", EvaluatesTo::an_error());
}

#[test]
fn passing_too_many_arguments_is_an_error() {
    assert_that!("((lambda (x) x) 1 2)", EvaluatesTo::an_error());
}

#[test]
fn passing_the_right_number_of_arguments_works() {
    assert_that!("((lambda (x) x) 3)", EvaluatesTo::the_integer(3));
}

#[test]
fn vararg_arguments_are_put_into_list() {
    assert_that!(
        "((lambda (x . y) y) 1 2 3)",
        EvaluatesTo::the_list(vec![2, 3])
    );
}

#[test]
fn fibonacci() {
    assert_that!(
        vec![
            "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))",
            "(fib 5)"
        ],
        EvaluatesTo::the_integer(8)
    );
}
