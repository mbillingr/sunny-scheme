#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::make_minus_module_minus_tree_minus_leaf;
    pub use super::globals::make_minus_module_minus_tree_minus_node;
    pub use super::globals::module_minus_tree_minus_append_minus_child_i;
    pub use super::globals::module_minus_tree_minus_children;
    pub use super::globals::module_minus_tree_minus_find_minus_child;
    pub use super::globals::module_minus_tree_minus_insert_i;
    pub use super::globals::module_minus_tree_minus_leaf_p;
    pub use super::globals::module_minus_tree_minus_libobj;
    pub use super::globals::module_minus_tree_minus_name;
    pub use super::globals::module_minus_tree_minus_set_minus_children_i;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_insert_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-insert!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_append_minus_child_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-append-child!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_find_minus_child: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-find-child"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_set_minus_children_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-set-children!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_libobj: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-libobj"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_children: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-children"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_leaf_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-leaf?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_module_minus_tree_minus_leaf: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-module-tree-leaf"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_module_minus_tree_minus_node: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-module-tree-node"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    {
        (/*NOP*/);
        // (define (make-module-tree-node name) (cons name (quote ())))
        globals::make_minus_module_minus_tree_minus_node.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (quote ())))
                    {
                        // (cons name (quote ()))
                        imports::cons
                            .with(|value| value.get())
                            .invoke(&[name.clone(), Scm::Nil])
                    }
                })
            })
        });
        // (define (make-module-tree-leaf name lib) (cons name lib))
        globals::make_minus_module_minus_tree_minus_leaf.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let lib = args[1].clone();
                    // (letrec () (cons name lib))
                    {
                        // (cons name lib)
                        imports::cons
                            .with(|value| value.get())
                            .invoke(&[name.clone(), lib.clone()])
                    }
                })
            })
        });
        // (define (module-tree-leaf? node) (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node)))))
        globals::module_minus_tree_minus_leaf_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec () (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node)))))
                    {
                        // (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node))))
                        if (
                            // (pair? node)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[node.clone()])
                        )
                        .is_true()
                        {
                            if (
                                // (symbol? (car node))
                                imports::symbol_p.with(|value| value.get()).invoke(&[
                                    // (car node)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[node.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                if (
                                    // (not (null? (cdr node)))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (null? (cdr node))
                                        imports::null_p.with(|value| value.get()).invoke(&[
                                            // (cdr node)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[node.clone()]),
                                        ]),
                                    ])
                                )
                                .is_true()
                                {
                                    // (not (pair? (cdr node)))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (pair? (cdr node))
                                        imports::pair_p.with(|value| value.get()).invoke(&[
                                            // (cdr node)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[node.clone()]),
                                        ]),
                                    ])
                                } else {
                                    Scm::False
                                }
                            } else {
                                Scm::False
                            }
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (module-tree-name node) (car node))
        globals::module_minus_tree_minus_name.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec () (car node))
                    {
                        // (car node)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[node.clone()])
                    }
                })
            })
        });
        // (define (module-tree-children node) (cdr node))
        globals::module_minus_tree_minus_children.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec () (cdr node))
                    {
                        // (cdr node)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[node.clone()])
                    }
                })
            })
        });
        // (define (module-tree-libobj node) (cdr node))
        globals::module_minus_tree_minus_libobj.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec () (cdr node))
                    {
                        // (cdr node)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[node.clone()])
                    }
                })
            })
        });
        // (define (module-tree-set-children! node children) (set-cdr! node children))
        globals::module_minus_tree_minus_set_minus_children_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    let children = args[1].clone();
                    // (letrec () (set-cdr! node children))
                    {
                        // (set-cdr! node children)
                        imports::set_minus_cdr_i
                            .with(|value| value.get())
                            .invoke(&[node.clone(), children.clone()])
                    }
                })
            })
        });
        // (define (module-tree-find-child node name) (if (module-tree-leaf? node) (error "called (module-tree-find-child) on leaf node" name node)) (assq name (module-tree-children node)))
        globals::module_minus_tree_minus_find_minus_child.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    let name = args[1].clone();
                    // (letrec () (if (module-tree-leaf? node) (error "called (module-tree-find-child) on leaf node" name node)) (assq name (module-tree-children node)))
                    {
                        {
                            if (
                                // (module-tree-leaf? node)
                                globals::module_minus_tree_minus_leaf_p
                                    .with(|value| value.get())
                                    .invoke(&[node.clone()])
                            )
                            .is_true()
                            {
                                // (error "called (module-tree-find-child) on leaf node" name node)
                                imports::error.with(|value| value.get()).invoke(&[
                                    Scm::from("called (module-tree-find-child) on leaf node"),
                                    name.clone(),
                                    node.clone(),
                                ])
                            } else {
                                Scm::symbol("*UNSPECIFIED*")
                            };
                            // (assq name (module-tree-children node))
                            imports::assq.with(|value| value.get()).invoke(&[
                                name.clone(),
                                // (module-tree-children node)
                                globals::module_minus_tree_minus_children
                                    .with(|value| value.get())
                                    .invoke(&[node.clone()]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (module-tree-append-child! node child) (module-tree-set-children! node (cons child (module-tree-children node))))
        globals::module_minus_tree_minus_append_minus_child_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    let child = args[1].clone();
                    // (letrec () (module-tree-set-children! node (cons child (module-tree-children node))))
                    {
                        // (module-tree-set-children! node (cons child (module-tree-children node)))
                        globals::module_minus_tree_minus_set_minus_children_i
                            .with(|value| value.get())
                            .invoke(&[
                                node.clone(),
                                // (cons child (module-tree-children node))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    child.clone(),
                                    // (module-tree-children node)
                                    globals::module_minus_tree_minus_children
                                        .with(|value| value.get())
                                        .invoke(&[node.clone()]),
                                ]),
                            ])
                    }
                })
            })
        });
        // (define (module-tree-insert! tree libname libobj) (if (null? libname) (error "invalid insert")) (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))))))
        globals::module_minus_tree_minus_insert_i.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let tree = args[0].clone();let libname = args[1].clone();let libobj = args[2].clone();
// (letrec () (if (null? libname) (error "invalid insert")) (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))))))
{{if (
// (null? libname)
imports::null_p.with(|value| value.get()).invoke(&[libname.clone(),])).is_true() {
// (error "invalid insert")
imports::error.with(|value| value.get()).invoke(&[Scm::from("invalid insert"),])} else {Scm::symbol("*UNSPECIFIED*")};
// (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node)))))
{let [child, ] = [
// (module-tree-find-child tree (car libname))
globals::module_minus_tree_minus_find_minus_child.with(|value| value.get()).invoke(&[tree.clone(),
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(),]),]),];if (child.clone()).is_true() {
// (module-tree-insert! child (cdr libname) libobj)
globals::module_minus_tree_minus_insert_i.with(|value| value.get()).invoke(&[child.clone(),
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(),]),libobj.clone(),])} else {if (
// (null? (cdr libname))
imports::null_p.with(|value| value.get()).invoke(&[
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(),]),])).is_true() {
// (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
globals::module_minus_tree_minus_append_minus_child_i.with(|value| value.get()).invoke(&[tree.clone(),
// (make-module-tree-leaf (car libname) libobj)
globals::make_minus_module_minus_tree_minus_leaf.with(|value| value.get()).invoke(&[
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(),]),libobj.clone(),]),])} else {
// (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))
{let [new_minus_node, ] = [
// (make-module-tree-node (car libname))
globals::make_minus_module_minus_tree_minus_node.with(|value| value.get()).invoke(&[
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(),]),]),];{
// (module-tree-insert! new-node (cdr libname) libobj)
globals::module_minus_tree_minus_insert_i.with(|value| value.get()).invoke(&[new_minus_node.clone(),
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(),]),libobj.clone(),]);
// (module-tree-append-child! tree new-node)
globals::module_minus_tree_minus_append_minus_child_i.with(|value| value.get()).invoke(&[tree.clone(),new_minus_node.clone(),])}}}}}}}})}))
    };
}
