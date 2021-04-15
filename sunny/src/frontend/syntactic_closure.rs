use crate::frontend::{ast::AstNode, environment::Env, error::Result, SyntaxExpander};
use std::any::Any;
use std::collections::HashMap;
use sunny_sexpr_parser::{Scm, ScmHasher, ScmObject, SourceMap};

pub struct SyntacticClosure {
    sexpr: Scm,
    env: Env,
}

impl SyntacticClosure {
    pub fn new(expr: Scm, env: Env) -> Self {
        SyntacticClosure { sexpr: expr, env }
    }

    pub fn expand(&self, expander: &impl SyntaxExpander, src_map: &SourceMap) -> Result<AstNode> {
        expander.expand(&self.sexpr, src_map, &self.env)
    }

    pub fn raw_expr(&self) -> &Scm {
        &self.sexpr
    }
}

impl ScmObject for SyntacticClosure {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn deep_hash(&self, _state: &mut ScmHasher) {
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
}

impl std::fmt::Display for SyntacticClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::fmt::Debug for SyntacticClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<syntactic closure @{:p}>", self)
    }
}
