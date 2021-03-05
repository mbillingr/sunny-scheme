use crate::frontend::environment::Env;
use crate::frontend::{
    ast::{Ast, AstNode},
    error::{Error, Result},
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

macro_rules! match_sexpr {
    ([$expr:tt: $($rules:tt)*]) => {
        _match_sexpr![$expr; $($rules)*]
    };

    ([$($first:tt)*] $($rest:tt)*) => {{
        match match_sexpr![[$($first)*]] {
            Some(r) => Some(r),
            None => match_sexpr![$($rest)*],
        }
    }};
}

macro_rules! _match_sexpr {
    ($expr:tt; _ => $action:block) => {{ let _ = $expr; Some($action) }};

    ($expr:tt; $x:ident => $action:block) => {{ let $x = $expr; Some($action) }};

    ($expr:tt; $x:ident: Symbol => $action:block) => {
        if let Some($x) = $expr.as_symbol() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; () => $action:block) => {
        if $expr.is_null() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ1:tt)? . $second:tt $(:$typ2:tt)?) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first $(:$typ1)? => {
                _match_sexpr![cdr; $second $(:$typ2)? => $action]
            }].flatten()
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ:tt)?) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![cdr; () => {
                _match_sexpr![car; $first $(:$typ)? => $action]
            }].flatten()
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $(:$typ:tt)?, $($rest:tt)*) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first $(:$typ)? => {
                _match_sexpr![cdr; ($($rest)*) => $action]
            }].flatten()
        } else {
            None
        }
    };
}

pub struct Begin;

impl SyntaxExpander for Begin {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_ . rest) => {
                Sequence.expand(rest, further, env)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
        _env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, arg) => { Ok(Ast::constant(arg.clone())) }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Assignment;

impl SyntaxExpander for Assignment {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, name: Symbol, value) => {
                let (depth, binding) = env.lookup_or_insert_global(name);
                let value = further.expand(value, further, env)?;
                binding.meaning_assignment(sexpr.map(()), depth, value)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Sequence;

impl SyntaxExpander for Sequence {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (expr) => { further.expand(expr, further, env) }]
            [sexpr: (expr . rest) => {
                let first = further.expand(expr, further, env)?;
                let rest = self.expand(rest, further, env)?;
                Ok(Ast::sequence(first, rest))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Branch;

impl SyntaxExpander for Branch {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, condition, consequence, alternative) => {
                let condition = further.expand(condition, further, env)?;
                let consequence = further.expand(consequence, further, env)?;
                let alternative = further.expand(alternative, further, env)?;
                Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
            }]
            [sexpr: (_, condition, consequence) => {
                let condition = further.expand(condition, further, env)?;
                let consequence = further.expand(consequence, further, env)?;
                let alternative = Ast::void();
                Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Lambda;

impl SyntaxExpander for Lambda {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_, params . body) => {
                let body_env = env.extend(params)?;
                let body = Sequence.expand(body, further, &body_env)?;

                Ok(Ast::lambda(sexpr.map(()), params.len(), body))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}
