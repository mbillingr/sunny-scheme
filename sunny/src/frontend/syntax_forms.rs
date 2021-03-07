use std::collections::HashMap;
use std::rc::Rc;

use log::warn;

use sunny_sexpr_parser::CxR;
use sunny_sexpr_parser::{RefExpr, Sexpr, SourceLocation};

use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_at, Error, Result},
    SyntaxExpander,
};

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

    ($expr:tt; $x:ident: List => $action:block) => {
        if $expr.is_pair() || $expr.is_null() {
            Some($action)
        } else {
            None
        }
    };

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

    ($expr:tt; $literal:expr => $action:block) => {
        if $expr.get_value() == &sunny_sexpr_parser::Sexpr::from($literal) {
            Some($action)
        } else {
            None
        }
    };
}

macro_rules! define_form {
    ($t:ident($xpr:ident, $env:ident): $([$($rules:tt)*])+) => {
        pub struct $t;
        impl SyntaxExpander for $t {
            fn expand(&self, $xpr: RefExpr, $env: &Env) -> Result<AstNode> {
                match_sexpr![
                    $([$xpr: $($rules)*])+
                ]
                .unwrap_or_else(|| Err($xpr.map_value(Error::InvalidForm)))
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////////////

define_form! {
    Expression(sexpr, env):
        [(f: Symbol . _) => {
            if let Some(sx) = env.lookup_syntax(f) {
                sx.expand(sexpr, env)
            } else {
                Expression.expand_application(sexpr, env)
            }
        }]
        [list: List => {
            Expression.expand_application(sexpr, env)
        }]
        [name: Symbol => {
            let (depth, binding) = env.lookup_or_insert_global(name);
            binding.expand_reference(sexpr.map_value(()), depth)
        }]
        [_ => {
            Ok(Ast::constant(sexpr.clone()))
        }]
}

impl Expression {
    fn expand_application(&self, sexpr: RefExpr, env: &Env) -> Result<AstNode> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.expand(a, env)?);
        }
        Ok(Ast::invoke(sexpr.map_value(()), args))
    }
}

define_form! {
    Begin(sexpr, env):
        [(_ . rest) => { Sequence.expand(rest, env) }]
}

define_form! {
    Quotation(sexpr, _env):
        [(_, value) => { Ok(Ast::constant(value.clone())) }]
}

define_form! {
    Assignment(sexpr, env):
        [(_, name: Symbol, value) => {
            let (depth, binding) = env.lookup_or_insert_global(name);
            let value = Expression.expand(value, env)?;
            binding.expand_assignment(sexpr.map_value(()), depth, value)
        }]
}

define_form! {
    Definition(sexpr, env):
        [(_, name: Symbol, value) => {
            let value = Expression.expand(value, env)?;
            let (depth, binding) = env.ensure_global_variable(name);
            binding.expand_assignment(sexpr.map_value(()), depth, value)
        }]
        [(_, (name: Symbol . args) . body) => {
            let body_env = env.extend(args)?;
            let body = Sequence.expand(body, &body_env)?;
            let function = Ast::lambda(sexpr.map_value(()), args.len(), body);

            let (depth, binding) = env.ensure_global_variable(name);
            binding.expand_assignment(sexpr.map_value(()), depth, function)
        }]
}

define_form! {
    Sequence(sexpr, env):
        [(expr) => { Expression.expand(expr, env) }]
        [(expr . rest) => {
            let first = Expression.expand(expr, env)?;
            let rest = Sequence.expand(rest, env)?;
            Ok(Ast::sequence(first, rest))
        }]
}

define_form! {
    Branch(sexpr, env):
        [(_, condition, consequence, alternative) => {
            let condition = Expression.expand(condition, env)?;
            let consequence = Expression.expand(consequence, env)?;
            let alternative = Expression.expand(alternative, env)?;
            Ok(Ast::ifexpr(sexpr.map_value(()), condition, consequence, alternative))
        }]
        [(_, condition, consequence) => {
            let condition = Expression.expand(condition, env)?;
            let consequence = Expression.expand(consequence, env)?;
            let alternative = Ast::void();
            Ok(Ast::ifexpr(sexpr.map_value(()), condition, consequence, alternative))
        }]
}

define_form! {
    Lambda(sexpr, env):
        [(_, params . body) => {
            let body_env = env.extend(params)?;
            let body = Sequence.expand(body, &body_env)?;

            Ok(Ast::lambda(sexpr.map_value(()), params.len(), body))
        }]
}

define_form! {
    Cons(sexpr, env):
       [(_, car, cdr) => {
            let car = Expression.expand(car, env)?;
            let cdr = Expression.expand(cdr, env)?;
            Ok(Ast::cons(sexpr.map_value(()), car, cdr))
       }]
}

define_form! {
    SyntaxDefinition(sexpr, env):
       [(_, keyword: Symbol, transformer_spec) => {
            let transformer = SyntaxTransformer.build(transformer_spec, env)?;
            env.outermost_env().insert_syntax_shared(keyword, transformer);
            Ok(Ast::void())
       }]
}

pub struct SyntaxTransformer;

impl SyntaxTransformer {
    fn build(&self, spec: RefExpr, _env: &Env) -> Result<Rc<dyn SyntaxExpander>> {
        match_sexpr![
            [spec: ("simple-macro", args, body) => { Ok(Rc::new(SimpleMacro::new(args, body)?) as Rc<dyn SyntaxExpander>) }]
        ].unwrap_or_else(|| Err(spec.map_value(Error::InvalidForm)))
    }
}

pub struct SimpleMacro {
    args: Vec<String>,
    template: SourceLocation<Sexpr>,
}

impl SyntaxExpander for SimpleMacro {
    fn expand(&self, sexpr: RefExpr, env: &Env) -> Result<AstNode> {
        let mut substitutions = HashMap::new();
        let mut more_args = sexpr
            .cdr()
            .ok_or_else(|| sexpr.map_value(Error::MissingArgument))?;

        for name in &self.args {
            let arg = more_args
                .car()
                .ok_or_else(|| more_args.map_value(Error::MissingArgument))?;
            more_args = more_args.cdr().unwrap();
            substitutions.insert(name, arg);
        }

        let new_sexpr = Sexpr::substitute(&self.template, &substitutions);
        Expression.expand(&new_sexpr, env)
    }
}

impl SimpleMacro {
    pub fn new(args: RefExpr, body: RefExpr) -> Result<Self> {
        let mut argnames = vec![];
        for arg in args.iter() {
            if let Some(name) = arg.as_symbol() {
                argnames.push(name.to_string());
            } else {
                return Err(arg.map_value(Error::ExpectedSymbol));
            }
        }

        Ok(SimpleMacro {
            args: argnames,
            template: body.clone(),
        })
    }
}

pub struct LibraryDefinition;

impl SyntaxExpander for LibraryDefinition {
    fn expand(&self, sexpr: RefExpr, _env: &Env) -> Result<AstNode> {
        match_sexpr![
            [sexpr: (_, libname: List . statements) => {
                let lib_env = base_environment();

                let mut exports = vec![];
                let mut body_parts = vec![];
                for stmt in statements.iter() {
                    match stmt.car().and_then(|s| s.as_symbol()) {
                        Some("import") => warn!("Ignoring (import ...) statement in library definition"),
                        Some("export") => {
                            for export_item in stmt.cdr().unwrap().iter() {
                                let export_name = export_item
                                    .as_symbol()
                                    .ok_or_else(|| error_at(export_item, Error::ExpectedSymbol))?;
                                let (_, binding) = lib_env.ensure_global(export_name);
                                if let EnvBinding::Variable(var_idx) = binding {
                                    exports.push((export_name.to_string(), var_idx));
                                } else {
                                    unimplemented!()
                                }
                            }
                        }
                        Some("begin") => {
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), &lib_env)?)
                        }
                        _ => return Err(error_at(stmt, Error::UnexpectedStatement)),
                    }
                }

                let mut body = body_parts.pop().expect("Empty library body");
                while let Some(prev_part) = body_parts.pop() {
                    body = Ast::sequence(prev_part, body);
                }

                let meaning_exports = Ast::export(exports);
                body = Ast::sequence(body, meaning_exports);

                Ok(Ast::module(body))
            }]
        ]
            .unwrap_or_else(|| Err(sexpr.map_value(Error::InvalidForm)))
    }
}
