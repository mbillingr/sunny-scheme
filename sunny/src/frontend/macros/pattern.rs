#[cfg(test)]
mod tests {
    use super::*;
    use sexpr_generics::prelude::*;
    use sexpr_generics::sexpr;

    #[test]
    fn match_any() {
        let pattern = sexpr![()];
    }
}
