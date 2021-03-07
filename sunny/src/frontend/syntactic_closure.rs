use crate::frontend::{ast::AstNode, environment::Env, error::Result, SyntaxExpander};
use std::collections::HashMap;
use std::rc::Rc;
use sunny_sexpr_parser::{AnySexprObject, Sexpr, SexprObject, SrcExpr};

pub struct SyntacticClosure {
    sexpr: SrcExpr,
    env: Env,
}

impl SyntacticClosure {
    pub fn new(expr: SrcExpr, env: Env) -> Self {
        SyntacticClosure { sexpr: expr, env }
    }

    pub fn expand(&self, expander: &impl SyntaxExpander) -> Result<AstNode> {
        expander.expand(&self.sexpr, &self.env)
    }
}

impl SexprObject for SyntacticClosure {
    fn substitute(&self, mapping: &HashMap<&str, SrcExpr>) -> Rc<dyn AnySexprObject> {
        let sexpr = Sexpr::substitute(&self.sexpr, mapping);
        Rc::new(SyntacticClosure {
            sexpr,
            env: self.env.clone(),
        })
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
