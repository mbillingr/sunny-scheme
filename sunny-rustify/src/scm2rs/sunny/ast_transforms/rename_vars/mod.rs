#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::rename_minus_vars;
}

pub fn rename_minus_vars(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let rename = args[0].clone();let node = args[1].clone();{
// (letrec ((renamed (quote ())) (renamed? (lambda (var) (memq var renamed))) (do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var))) (set! renamed (cons var renamed)))))) (transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var))))) (transform-children)))) (node (quote transform) transform))
{
// (let ((renamed (quote *uninitialized*)) (renamed? (quote *uninitialized*)) (do-rename! (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! renamed (quote ())) (set! renamed? (lambda (var) (memq var renamed))) (set! do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var))) (set! renamed (cons var renamed)))))) (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var))))) (transform-children))) (node (quote transform) transform)))
{let [renamed, renamed_p, do_minus_rename_i, transform, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let transform = transform.into_boxed();{let do_minus_rename_i = do_minus_rename_i.into_boxed();{let renamed_p = renamed_p.into_boxed();{let renamed = renamed.into_boxed();{renamed.set(Scm::Nil);Scm::anything();renamed_p.set({// Closure
let renamed = renamed.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var = args[0].clone();{
// (memq var renamed)
imports::memq(&[var.clone(),renamed.get()])}})});Scm::anything();do_minus_rename_i.set({// Closure
let renamed_p = renamed_p.clone();let rename = rename.clone();let renamed = renamed.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var = args[0].clone();if ({
// (renamed? var)
renamed_p.get().invoke(&[var.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (variable-set-name! var (rename (variable-name var)))
imports::variable_minus_set_minus_name_i(&[var.clone(),{
// (rename (variable-name var))
rename.clone().invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])}])};renamed.set({
// (cons var renamed)
imports::cons(&[var.clone(),renamed.get()])});Scm::anything()}}})});Scm::anything();transform.set({// Closure
let do_minus_rename_i = do_minus_rename_i.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node = args[0].clone();let transform_minus_children = args[1].clone();{{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote REFERENCE))
imports::eq_p(&[{
// (node (quote kind))
node.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("REFERENCE")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i.get().invoke(&[{
// (node (quote get-var))
node.clone().invoke(&[Scm::symbol("get-var")])}])}} else {Scm::symbol("*UNSPECIFIED*")}};{
// (transform-children)
transform_minus_children.clone().invoke(&[])}}})});Scm::anything();{
// (node (quote transform) transform)
node.clone().invoke(&[Scm::symbol("transform"),transform.get()])}}}}}}}}}}.into()
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (rename-vars rename node) ...)
            (/*NOP*/)
        }
    };
}
