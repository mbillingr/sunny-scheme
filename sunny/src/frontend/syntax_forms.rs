use std::collections::HashMap;
use std::rc::Rc;

use sunny_sexpr_parser::CxR;
use sunny_sexpr_parser::{RefExpr, Sexpr, SourceLocation};

use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::Env,
    error::{error_at, Error, Result},
    library::{libname_to_string, Export},
    syntactic_closure::SyntacticClosure,
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

    ($expr:tt; $x:ident: Obj<$t:ty> => $action:block) => {
        if let Some($x) = $expr.as_object::<$t>() {
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
        #[derive(Debug)]
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
        [() => {
            Err(sexpr.map(|_|Error::InvalidForm))
        }]
        [list: List => {
            Expression.expand_application(sexpr, env)
        }]
        [name: Symbol => {
            let context = sexpr.map_value(());
            env.ensure_variable(name);
            if let Some(full_name) = env.lookup_variable(name).unwrap().as_global() {
                Ok(Ast::fetch_global(context, full_name))
            } else {
                let offset = env.lookup_variable_index(name).unwrap();
                Ok(Ast::fetch(context, offset))
            }
        }]
        [sc: Obj<SyntacticClosure> => {
            sc.expand(&Expression)
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
            let context = sexpr.map_value(());
            env.ensure_variable(name);
            let value = Expression.expand(value, env)?;
            if let Some(full_name) = env.lookup_variable(name).unwrap().as_global() {
                Ok(Ast::store_global(context, full_name, value))
            } else {
                let offset = env.lookup_variable_index(name).unwrap();
                Ok(Ast::store(context, offset, value))
            }
        }]
}

define_form! {
    Definition(sexpr, env):
        [(_, name: Symbol, value) => {
            let value = Expression.expand(value, env)?;
            env.ensure_global_variable(name);
            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(sexpr.map_value(()), full_name, value))
        }]
        [(_, (name: Symbol . args) . body) => {
            let body_env = env.extend(args)?;
            let body = Sequence.expand(body, &body_env)?;
            let function = Ast::lambda(sexpr.map_value(()), args.len(), body);

            env.ensure_global_variable(name);
            let binding = env.lookup_global_variable(name).unwrap();
            let full_name = binding.as_global().unwrap();
            Ok(Ast::store_global(sexpr.map_value(()), full_name, function))
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
            env.add_global_binding(keyword, transformer);
            Ok(Ast::void())
       }]
}

define_form! {
    Import(sexpr, env):
       [(_, libname) => {
            let libstr = libname_to_string(libname);
            if let Some(lib) = env.find_library(&libstr) {
                for export in lib.exports() {
                    env.add_global_binding(export.export_name.to_string(), export.binding.clone());
                }
                Ok(Ast::void())
            } else {
                Err(error_at(libname, Error::UnknownLibrary))
            }
       }]
}

pub struct SyntaxTransformer;

impl SyntaxTransformer {
    fn build(&self, spec: RefExpr, env: &Env) -> Result<Rc<dyn SyntaxExpander>> {
        match_sexpr![
            [spec: ("simple-macro", args, body) => { Ok(Rc::new(SimpleMacro::new(args, body, env)?) as Rc<dyn SyntaxExpander>) }]
        ].unwrap_or_else(|| Err(spec.map_value(Error::InvalidForm)))
    }
}

#[derive(Debug)]
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
                .cloned()
                .map(|a| SyntacticClosure::new(a, env.clone()))
                .map(|sc| Sexpr::obj(sc))
                .map(SourceLocation::new)
                .ok_or_else(|| more_args.map_value(Error::MissingArgument))?;
            more_args = more_args.cdr().unwrap();
            substitutions.insert(name.as_str(), arg);
        }

        let new_sexpr = Sexpr::substitute(&self.template, &substitutions);
        Expression.expand(&new_sexpr, env)
    }
}

impl SimpleMacro {
    pub fn new(args: RefExpr, body: RefExpr, env: &Env) -> Result<Self> {
        let mut argnames = vec![];
        for arg in args.iter() {
            if let Some(name) = arg.as_symbol() {
                argnames.push(name.to_string());
            } else {
                return Err(arg.map_value(Error::ExpectedSymbol));
            }
        }

        let template =
            SourceLocation::new(Sexpr::obj(SyntacticClosure::new(body.clone(), env.clone())));

        Ok(SimpleMacro {
            args: argnames,
            template,
        })
    }
}

#[derive(Debug)]
pub struct LibraryDefinition;

impl SyntaxExpander for LibraryDefinition {
    fn expand(&self, sexpr: RefExpr, env: &Env) -> Result<AstNode> {
        match_sexpr![
            [sexpr: (_, libname . statements) => {
                let mut lib_env = base_environment(libname);
                lib_env.use_libraries_from(env);

                for stmt in statements.iter() {
                    match stmt.car().and_then(|s| s.as_symbol()) {
                        Some("import") => {Import.expand(stmt, &lib_env)?;}
                        Some("export") => {}
                        Some("begin") => {}
                        _ => return Err(error_at(stmt, Error::UnexpectedStatement)),
                    }
                }

                let mut body_parts = vec![];
                for stmt in statements.iter() {
                    match stmt.car().and_then(|s| s.as_symbol()) {
                        Some("begin") => {
                            body_parts.push(Sequence.expand(stmt.cdr().unwrap(), &lib_env)?)
                        }
                        _ => {}
                    }
                }

                let mut exports = vec![];
                for stmt in statements.iter() {
                    match stmt.car().and_then(|s| s.as_symbol()) {
                        Some("export") => {
                            for export_item in stmt.cdr().unwrap().iter() {
                                let export_name = export_item
                                    .as_symbol()
                                    .ok_or_else(|| error_at(export_item, Error::ExpectedSymbol))?;
                                let binding = lib_env.lookup_global_variable(export_name).ok_or_else(||error_at(export_item, Error::UndefinedExport))?;
                                exports.push(Export::new(export_name, binding));
                            }
                        }
                        _ => {}
                    }
                }

                let export_vars = exports.iter().filter(|exp| exp.binding.is_global()).cloned().collect();

                env.define_library(libname, exports);

                let mut body = body_parts.pop().unwrap_or_else(|| Ast::void());
                while let Some(prev_part) = body_parts.pop() {
                    body = Ast::sequence(prev_part, body);
                }

                Ok(Ast::module(libname, body, export_vars))
            }]
        ]
            .unwrap_or_else(|| Err(sexpr.map_value(Error::InvalidForm)))
    }
}
