use super::EvaluatesTo;
use hamcrest2::assert_that;
use hamcrest2::prelude::*;

#[test]
fn using_extended_identifier_characters_is_not_an_error() {
    // R7RS section 2.1
    assert_that!(
        vec![
            "(define ! 0)",
            "(define $ 0)",
            "(define % 0)",
            "(define & 0)",
            "(define * 0)",
            "(define + 0)",
            "(define - 0)",
            "(define .. 0)", // a single . has special meaning
            "(define / 0)",
            "(define : 0)",
            "(define < 0)",
            "(define = 0)",
            "(define > 0)",
            "(define ? 0)",
            "(define @ 0)",
            "(define ^ 0)",
            "(define _ 0)",
            "(define ~ 0)",
        ],
        EvaluatesTo::void()
    );
}

#[test]
fn pipes_escape_verbatim_identifiers() {
    // R7RS section 2.1
    assert_that!(
        vec![
            "(define || 0)",
            "(define |.| 0)",
            "(define |Hello World| 0)",
        ],
        EvaluatesTo::void()
    );
}

#[test]
fn line_comments_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 ; 2 3", EvaluatesTo::the_integer(1));
}

#[test]
fn commented_data_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 #;2 3", EvaluatesTo::the_integer(3));
    assert_that!("1 #; 2 3", EvaluatesTo::the_integer(3));
}

#[test]
fn block_comments_are_ignored() {
    // R7RS section 2.2
    assert_that!("1 #| 2 |# 3", EvaluatesTo::the_integer(3));
}

#[test]
fn block_comments_must_be_properly_nested() {
    // R7RS section 2.2
    assert_that!("1 #| 2 #| 3 |# 4", EvaluatesTo::an_error());
    assert_that!("1 #| 2 |# 3 |# 4", EvaluatesTo::an_error());
}
