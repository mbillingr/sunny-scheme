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
    {if args.len() != 2{panic!("invalid arity")}let rename__710 = args[0].clone();let node__714 = args[1].clone();{
// (letrec ((renamed (quote ())) (renamed? (lambda (var) (memq var renamed))) (do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var))))) (transform-children)))) (node (quote transform) transform))
{
// (let ((renamed (quote *uninitialized*)) (renamed? (quote *uninitialized*)) (do-rename! (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! renamed (quote ())) (set! renamed? (lambda (var) (memq var renamed))) (set! do-rename! (lambda (var) (if (renamed? var) (quote DONE) (begin (variable-set-name! var (rename (variable-name var) var)) (set! renamed (cons var renamed)))))) (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote REFERENCE)) (do-rename! (node (quote get-var)))) ((eq? (node (quote kind)) (quote ASSIGNMENT)) (do-rename! (node (quote get-var))))) (transform-children))) (node (quote transform) transform)))
{let [renamed__705, renamed_p__706, do_minus_rename_i__708, transform__711, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let transform__711 = transform__711.into_boxed();{let do_minus_rename_i__708 = do_minus_rename_i__708.into_boxed();{let renamed_p__706 = renamed_p__706.into_boxed();{let renamed__705 = renamed__705.into_boxed();{renamed__705.set(Scm::Nil);Scm::anything();renamed_p__706.set({// Closure
let renamed__705 = renamed__705.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var__707 = args[0].clone();{
// (memq var renamed)
imports::memq(&[var__707.clone(),renamed__705.get()])}})});Scm::anything();do_minus_rename_i__708.set({// Closure
let renamed_p__706 = renamed_p__706.clone();let rename__710 = rename__710.clone();let renamed__705 = renamed__705.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let var__709 = args[0].clone();if ({
// (renamed? var)
renamed_p__706.get().invoke(&[var__709.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (variable-set-name! var (rename (variable-name var) var))
imports::variable_minus_set_minus_name_i(&[var__709.clone(),{
// (rename (variable-name var) var)
rename__710.clone().invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__709.clone()])},var__709.clone()])}])};renamed__705.set({
// (cons var renamed)
imports::cons(&[var__709.clone(),renamed__705.get()])});Scm::anything()}}})});Scm::anything();transform__711.set({// Closure
let do_minus_rename_i__708 = do_minus_rename_i__708.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node__712 = args[0].clone();let transform_minus_children__713 = args[1].clone();{{
// (cond ...)
if ({
// (eq? (node (quote kind)) (quote REFERENCE))
imports::eq_p(&[{
// (node (quote kind))
node__712.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("REFERENCE")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i__708.get().invoke(&[{
// (node (quote get-var))
node__712.clone().invoke(&[Scm::symbol("get-var")])}])}} else if ({
// (eq? (node (quote kind)) (quote ASSIGNMENT))
imports::eq_p(&[{
// (node (quote kind))
node__712.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ASSIGNMENT")])}).is_true() {{
// (do-rename! (node (quote get-var)))
do_minus_rename_i__708.get().invoke(&[{
// (node (quote get-var))
node__712.clone().invoke(&[Scm::symbol("get-var")])}])}} else {Scm::symbol("*UNSPECIFIED*")}};{
// (transform-children)
transform_minus_children__713.clone().invoke(&[])}}})});Scm::anything();{
// (node (quote transform) transform)
node__714.clone().invoke(&[Scm::symbol("transform"),transform__711.get()])}}}}}}}}}}.into()
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
