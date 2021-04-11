use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::sync::Arc;

use crate::shared_string::SharedStr;
use crate::str_utils::{find_end_of_line, find_start_of_line, line_number};
use crate::Sexpr;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct SourceKind {
    string: Option<SharedStr>,
    file: Option<Arc<PathBuf>>,
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
            source: SourceKind::default(),
        }
    }

    pub fn in_string(mut self, s: impl Into<SharedStr>) -> Self {
        self.source.string = Some(s.into());
        self
    }

    pub fn in_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.source.file = Some(Arc::new(path.into()));
        self
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

    pub fn map_value<U>(&self, new_value: U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: new_value,
            span: self.span.clone(),
            source: self.source.clone(),
        }
    }

    pub fn map<U>(&self, f: impl FnOnce(&T) -> U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: f(&self.inner_value),
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
        let mut result = String::new();

        if let Some(f) = &self.source.file {
            result = format!("In {:?},\n", f);
        }

        if let Some(src) = &self.source.string {
            result.push_str(&self.pretty_fmt_in_source(&src));
        } else {
            result = format!("{}{}", result, self.inner_value);
        }

        result
    }

    fn pretty_fmt_in_source(&self, src: &str) -> String {
        if self.span.is_empty() {
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
        if let Some(src) = &self.source.string {
            if self.span.is_empty() {
                let pos = self.span.start;
                let line_start = find_start_of_line(src, pos);
                let line_end = find_end_of_line(src, pos);
                src[line_start..line_end].to_string()
            } else {
                let line_end = find_end_of_line(src, self.span.start);
                if line_end < self.span.end {
                    format!("{} ...", &src[self.span.start..line_end])
                } else {
                    src[self.span.clone()].to_string()
                }
            }
        } else {
            String::new()
        }
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

impl<T> From<T> for SourceLocation<T> {
    fn from(x: T) -> SourceLocation<T> {
        SourceLocation::new(x)
    }
}

impl PartialEq<Sexpr> for SourceLocation<Sexpr> {
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
