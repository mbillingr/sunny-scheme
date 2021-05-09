use crate::frontend::{ast::AstNode, environment::Env, error::Result, SyntaxExpander};
use sexpr_generics::prelude::*;
use std::any::Any;
use std::collections::HashMap;
use sunny_scm::{Scm, ScmHasher, ScmObject, SourceMap};

#[derive(Clone)]
pub struct SyntacticClosure {
    sexpr: Scm,
    env: Env,
}

impl SyntacticClosure {
    pub fn new(expr: Scm, env: Env) -> Self {
        if let Some(sc) = expr.to_type::<SyntacticClosure>() {
            sc.clone()
        } else {
            SyntacticClosure { sexpr: expr, env }
        }
    }

    pub fn new_scm(expr: Scm, env: Env) -> Scm {
        if expr.is_of_type::<SyntacticClosure>() {
            expr
        } else {
            Scm::obj(SyntacticClosure { sexpr: expr, env })
        }
    }

    pub fn expand(
        &self,
        expander: &impl SyntaxExpander,
        src_map: &SourceMap,
        env: &Env,
    ) -> Result<AstNode> {
        let sc_env = env.prepare_sc_expansion(self.env.clone());
        expander.expand(&self.sexpr, src_map, &sc_env)
    }

    pub fn raw_expr(&self) -> &Scm {
        &self.sexpr
    }

    pub fn env(&self) -> &Env {
        &self.env
    }
}

impl ScmObject for SyntacticClosure {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        self.sexpr.as_dyn().is_eqv(other)
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        self.sexpr.as_dyn().is_equal(other)
    }

    fn eqv_hash(&self, _state: &mut ScmHasher) {
        unimplemented!()
    }

    fn equal_hash(&self, _state: &mut ScmHasher) {
        unimplemented!()
    }

    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm {
        let sexpr = self.sexpr.substitute(mapping);
        SyntacticClosure {
            sexpr,
            env: self.env.clone(),
        }
        .into()
    }

    fn quote(&self, _: &Scm) -> Scm {
        self.raw_expr().clone()
    }
}

impl std::fmt::Display for SyntacticClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Debug for SyntacticClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "<syntactic closure @{}: {}>",
            self.env.name(),
            self.sexpr
        )
    }
}
