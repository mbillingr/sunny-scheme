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
