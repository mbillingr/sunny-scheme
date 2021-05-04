use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn boolean_predicate_is_true_only_for_bool_types() {
    assert_that!("(boolean? #t)", EvaluatesTo::the_boolean(true));
    assert_that!("(boolean? #f)", EvaluatesTo::the_boolean(true));
    assert_that!("(boolean? '())", EvaluatesTo::the_boolean(false));
    assert_that!("(boolean? 0)", EvaluatesTo::the_boolean(false));
}
