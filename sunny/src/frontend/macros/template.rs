use crate::frontend::macros::pattern::MatchResult;
use sexpr_generics::prelude::*;
use sexpr_generics::with_sexpr_matcher;
use sunny_scm::Scm;

pub struct Transcriber {
    ellipsis: Scm,
}

impl Transcriber {
    pub fn new() -> Self {
        Transcriber {
            ellipsis: Scm::symbol("..."),
        }
    }

    pub fn transcribe(&self, template: &Scm, bindings: &MatchResult) -> Option<Scm> {
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
                {_: Symbol} => { bindings.lookup(template).cloned() }
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
        let result = Transcriber::new().transcribe(&template, &MatchResult::empty());
        assert_eq!(result, Some(sexpr![42]));
    }

    #[test]
    fn transcribe_list_template() {
        let template = sexpr![(1 2 3)];
        let result = Transcriber::new().transcribe(&template, &MatchResult::empty());
        assert_eq!(result, Some(sexpr![(1 2 3)]));
    }

    #[test]
    fn transcribe_repetition() {
        let template = sexpr![(x ...)];
        let result = Transcriber::new()
            .transcribe(&template, &MatchResult::repeated(sexpr![x], vec![1, 2, 3]));
        assert_eq!(result, Some(sexpr![(1 2 3)]));
    }
}
