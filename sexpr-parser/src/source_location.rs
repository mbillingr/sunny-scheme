use crate::str_utils::{find_end_of_line, find_start_of_line, line_number};
use crate::{CxR, Sexpr};
use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum SourceKind {
    None,
    String(Arc<String>),
    Filename(Arc<PathBuf>),
}

impl Default for SourceKind {
    fn default() -> Self {
        SourceKind::None
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceLocation<T> {
    inner_value: T,
    span: Range<usize>,
    source: SourceKind,
}

impl<T> SourceLocation<T> {
    pub fn new(value: T) -> Self {
        SourceLocation {
            inner_value: value,
            span: Range::default(),
            source: SourceKind::None,
        }
    }

    pub fn in_string(self, s: impl ToString) -> Self {
        SourceLocation {
            source: SourceKind::String(Arc::new(s.to_string())),
            ..self
        }
    }

    pub fn in_file(self, path: impl Into<PathBuf>) -> Self {
        SourceLocation {
            source: SourceKind::Filename(Arc::new(path.into())),
            ..self
        }
    }

    pub fn with_span(self, span: Range<usize>) -> Self {
        SourceLocation { span, ..self }
    }

    pub fn with_position(self, pos: usize) -> Self {
        SourceLocation {
            span: pos..pos,
            ..self
        }
    }

    pub fn get_value(&self) -> &T {
        &self.inner_value
    }

    pub fn into_value(self) -> T {
        self.inner_value
    }

    pub fn convert<U: From<T>>(self) -> SourceLocation<U> {
        SourceLocation {
            inner_value: self.inner_value.into(),
            span: self.span,
            source: self.source,
        }
    }

    pub fn map<U>(&self, new_value: U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: new_value,
            span: self.span.clone(),
            source: self.source.clone(),
        }
    }

    pub fn map_after<U>(&self, new_value: U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: new_value,
            span: self.span.end..self.span.end,
            source: self.source.clone(),
        }
    }
}

impl<T: std::fmt::Display> SourceLocation<T> {
    pub fn pretty_fmt(&self) -> String {
        match &self.source {
            SourceKind::None => format!("{}", self.inner_value),
            SourceKind::Filename(f) => format!("{}\n{:?} in {:?}", self.inner_value, self.span, f),
            SourceKind::String(s) => self.pretty_fmt_in_source(s),
        }
    }

    fn pretty_fmt_in_source(&self, src: &str) -> String {
        if self.span.len() == 0 {
            let pos = self.span.start;
            let line_start = find_start_of_line(src, pos);
            let line_end = find_end_of_line(src, pos);
            let line_number = line_number(src, pos);

            if pos >= src.len() {
                format!(
                    "{}\n{:5}  {}\n       {: <s$}^",
                    self.inner_value,
                    line_number,
                    &src[line_start..line_end],
                    "",
                    s = pos - line_start
                )
            } else {
                format!(
                    "{} `{}`\n{:5}  {}\n       {: <s$}^",
                    self.inner_value,
                    &src[pos..pos + 1],
                    line_number,
                    &src[line_start..line_end],
                    "",
                    s = pos - line_start,
                )
            }
        } else {
            let line_start = find_start_of_line(src, self.span.start);
            let line_end = find_end_of_line(src, line_start);
            let line_number = line_number(src, self.span.start);

            format!(
                "{} '{}'\n{:5}  {}\n       {: <s$}{:^<e$}",
                self.inner_value,
                &src[self.span.clone()],
                line_number,
                &src[line_start..line_end],
                "",
                "",
                s = self.span.start - line_start,
                e = self.span.len()
            )
        }
    }
}

impl<T> SourceLocation<T> {
    pub fn pretty_fmt_inline(&self) -> String {
        match &self.source {
            SourceKind::None => String::new(),
            SourceKind::Filename(_) => String::new(),
            SourceKind::String(src) => {
                if self.span.len() == 0 {
                    let pos = self.span.start;
                    let line_start = find_start_of_line(src, pos);
                    let line_end = find_end_of_line(src, pos);
                    format!("{}", &src[line_start..line_end])
                } else {
                    let line_end = find_end_of_line(src, self.span.start);
                    if line_end < self.span.end {
                        format!("{} ...", &src[self.span.start..line_end])
                    } else {
                        format!("{}", &src[self.span.clone()])
                    }
                }
            }
        }
    }
}

impl SourceLocation<Sexpr<'_>> {
    pub fn iter(&self) -> impl Iterator<Item = &SourceLocation<Sexpr>> {
        let mut cursor = self;
        (0..)
            .map(move |_| match cursor.get_value() {
                Sexpr::Pair(pair) => {
                    cursor = &pair.1;
                    Some(&pair.0)
                }
                _ => None,
            })
            .take_while(Option::is_some)
            .map(Option::unwrap)
    }
}

impl<T: PartialEq> PartialEq for SourceLocation<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner_value == other.inner_value
    }
}

impl<T> Deref for SourceLocation<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.get_value()
    }
}

impl CxR for SourceLocation<Sexpr<'_>> {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.get_value().car()
    }

    fn cdr(&self) -> Option<&Self> {
        self.get_value().cdr()
    }
}

impl<T> From<T> for SourceLocation<T> {
    fn from(x: T) -> SourceLocation<T> {
        SourceLocation::new(x)
    }
}

impl PartialEq<Sexpr<'_>> for SourceLocation<Sexpr<'_>> {
    fn eq(&self, other: &Sexpr) -> bool {
        self.get_value() == other
    }
}

impl<T: std::fmt::Display> std::fmt::Display for SourceLocation<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.get_value().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_fmt_inline_prints_span_of_string_exactly() {
        let loc = SourceLocation::new(()).in_string("abcdefg").with_span(2..5);
        assert_eq!(loc.pretty_fmt_inline(), "cde");
    }

    #[test]
    fn pretty_fmt_inline_prints_whole_line_if_span_is_empty() {
        let loc = SourceLocation::new(())
            .in_string("ab\ncde\nefg")
            .with_span(3..3);
        assert_eq!(loc.pretty_fmt_inline(), "cde");
    }

    #[test]
    fn pretty_fmt_inline_prints_first_line_if_spanned_multiple() {
        let loc = SourceLocation::new(())
            .in_string("ab\ncde\nefg")
            .with_span(3..8);
        assert_eq!(loc.pretty_fmt_inline(), "cde ...");
    }
}
