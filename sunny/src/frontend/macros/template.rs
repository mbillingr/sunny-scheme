use crate::frontend::macros::bindings::MatchBindings;
use sexpr_generics::prelude::*;
use sexpr_generics::with_sexpr_matcher;
use sunny_scm::Scm;

#[derive(Debug)]
pub struct Transcriber {
    ellipsis: Scm,
}

impl Default for Transcriber {
    fn default() -> Self {
        Self::new(Scm::symbol("..."))
    }
}

impl Transcriber {
    pub fn new(ellipsis: Scm) -> Self {
        Transcriber { ellipsis }
    }

    pub fn transcribe(&self, template: &Scm, bindings: &MatchBindings) -> Option<Scm> {
        with_sexpr_matcher! {
            match template, {
                (t {self.ellipsis} . tail) => {
                    let mut sub_transcriptions = vec![];
                    for sub_binding in bindings.unwrap_ellipsis()? {
                        let trans = self.transcribe(t, &sub_binding)?;
                        sub_transcriptions.push(trans);
                    }
                    Some(sub_transcriptions.into_iter().rfold(tail.clone(), swap_args(Scm::cons)))
                }
                (head . tail) => {
                    Some(Scm::cons(self.transcribe(head, bindings)?,
                                   self.transcribe(tail, bindings)?))
                }
                {_: Symbol} => { Some(bindings.lookup(template).unwrap_or(template).clone()) }
                _ => { Some(template.clone()) }
            }
        }
    }
}

fn swap_args<S, T, U>(func: impl Fn(S, T) -> U) -> impl Fn(T, S) -> U {
    move |t, s| func(s, t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use sexpr_generics::sexpr;

    #[test]
    fn transcribe_simple_template() {
        let template = sexpr![42];
        let result = Transcriber::default().transcribe(&template, &MatchBindings::empty());
        assert_eq!(result, Some(sexpr![42]));
    }

    #[test]
    fn transcribe_list_template() {
        let template = sexpr![(1 2 3)];
        let result = Transcriber::default().transcribe(&template, &MatchBindings::empty());
        assert_eq!(result, Some(sexpr![(1 2 3)]));
    }

    #[test]
    fn transcribe_repetition() {
        let template = sexpr![(x ...)];
        let result = Transcriber::default().transcribe(
            &template,
            &MatchBindings::repeated(sexpr![x], vec![1, 2, 3]),
        );
        assert_eq!(result, Some(sexpr![(1 2 3)]));
    }

    #[test]
    fn transcribe_unbound_symbol() {
        let template = sexpr![x];
        let result = Transcriber::default().transcribe(&template, &MatchBindings::empty());
        assert_eq!(result, Some(sexpr![x]));
    }
}
