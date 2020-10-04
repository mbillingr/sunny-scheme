#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::module_tree::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::rust_minus_gen_minus_global_minus_defs;
    pub use super::globals::rust_minus_gen_minus_in_minus_module;
    pub use super::globals::rust_minus_gen_minus_in_minus_submodule;
    pub use super::globals::rust_minus_gen_minus_module_minus_tree;
    pub use super::globals::rust_minus_gen_minus_module_minus_tree_minus_list;
    pub use super::globals::rust_minus_gen_minus_modules;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_global_minus_defs: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-global-defs"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_in_minus_module: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-in-module"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_in_minus_submodule: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-in-submodule"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-module-tree"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-module-tree-list"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_modules: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION rust-gen-modules"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::rust::module::initialize();
    crate::sunny::rust::module_tree::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (rust-gen-global-defs module g) ...)
            globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let g = args[1].clone();if ({
// (null? g)
imports::null_p.with(|value| value.get()).invoke(&[g.clone()])}).is_true() {{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])}} else {{
// (cond ...)
if ({
// (import-variable? (cdar g))
imports::import_minus_variable_p.with(|value| value.get()).invoke(&[{
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),{
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone()])}])}} else if ({
// (keyword? (cdar g))
imports::keyword_p.with(|value| value.get()).invoke(&[{
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),{
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone()])}])}} else if ({
// (global-variable? (cdar g))
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[{
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone()])}])}).is_true() {{{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE " (caar g) "\"))}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),{
// (rustify-identifier (caar g))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone()])}])},Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE "),{
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone()])},Scm::from("\"))}")])};{
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),{
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone()])}])}}} else if ({
// (global-function? (cdar g))
imports::global_minus_function_p.with(|value| value.get()).invoke(&[{
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone()])}])}).is_true() {{{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL FUNCTION " (caar g) "\"))}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),{
// (rustify-identifier (caar g))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone()])}])},Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL FUNCTION "),{
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone()])},Scm::from("\"))}")])};{
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),{
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone()])}])}}} else {{
// (error "Unexpected entry in global environment" (car g))
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unexpected entry in global environment"),{
// (car g)
imports::car.with(|value| value.get()).invoke(&[g.clone()])}])}}}}})}))
        };
        {
            // (define (rust-gen-modules module libs) ...)
            globals::rust_minus_gen_minus_modules.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let module = args[0].clone();
                        let libs = args[1].clone();
                        {
                            // (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree)))
                            {
                                let module_minus_tree = {
                                    // (make-module-tree-node (quote root))
                                    imports::make_minus_module_minus_tree_minus_node
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("root")])
                                };
                                {
                                    {
                                        // (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
                                        imports::for_minus_each.with(|value| value.get()).invoke(&[
                                            {
                                                let module_minus_tree = module_minus_tree.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let lib = args[0].clone();
                                                    {
                                                        // (module-tree-insert! module-tree (car lib) (cdr lib))
                                                        imports::module_minus_tree_minus_insert_i
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module_minus_tree.clone(),
                                                                {
                                                                    // (car lib)
                                                                    imports::car
                                                                        .with(|value| value.get())
                                                                        .invoke(&[lib.clone()])
                                                                },
                                                                {
                                                                    // (cdr lib)
                                                                    imports::cdr
                                                                        .with(|value| value.get())
                                                                        .invoke(&[lib.clone()])
                                                                },
                                                            ])
                                                    }
                                                })
                                            },
                                            libs.clone(),
                                        ])
                                    };
                                    {
                                        // (rust-gen-module-tree-list module (module-tree-children module-tree))
                                        globals::rust_minus_gen_minus_module_minus_tree_minus_list
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), {
                                                // (module-tree-children module-tree)
                                                imports::module_minus_tree_minus_children
                                                    .with(|value| value.get())
                                                    .invoke(&[module_minus_tree.clone()])
                                            }])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (rust-gen-module-tree module node) ...)
            globals::rust_minus_gen_minus_module_minus_tree.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let node = args[1].clone();{{
// (println module "pub mod " (rustify-libname (module-tree-name node)) ";")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub mod "),{
// (rustify-libname (module-tree-name node))
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[{
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone()])}])},Scm::from(";")])};if ({
// (module-tree-leaf? node)
imports::module_minus_tree_minus_leaf_p.with(|value| value.get()).invoke(&[node.clone()])}).is_true() {{
// (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod)))
globals::rust_minus_gen_minus_in_minus_submodule.with(|value| value.get()).invoke(&[{
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone()])},module.clone(),{let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let submod = args[0].clone();{
// ((module-tree-libobj node) (quote gen-rust) submod)
{
// (module-tree-libobj node)
imports::module_minus_tree_minus_libobj.with(|value| value.get()).invoke(&[node.clone()])}.invoke(&[Scm::symbol("gen-rust"),submod.clone()])}})}])}} else {{
// (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) (rust-gen-module-tree-list submod (module-tree-children node))))
globals::rust_minus_gen_minus_in_minus_submodule.with(|value| value.get()).invoke(&[{
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone()])},module.clone(),{let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let submod = args[0].clone();{
// (rust-gen-module-tree-list submod (module-tree-children node))
globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| value.get()).invoke(&[submod.clone(),{
// (module-tree-children node)
imports::module_minus_tree_minus_children.with(|value| value.get()).invoke(&[node.clone()])}])}})}])}}}})}))
        };
        {
            // (define (rust-gen-module-tree-list module nodes) ...)
            globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let module = args[0].clone();
                        let nodes = args[1].clone();
                        {
                            // (for-each (lambda (child) (rust-gen-module-tree module child)) nodes)
                            imports::for_minus_each.with(|value| value.get()).invoke(&[
                                {
                                    let module = module.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let child = args[0].clone();
                                        {
                                            // (rust-gen-module-tree module child)
                                            globals::rust_minus_gen_minus_module_minus_tree
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), child.clone()])
                                        }
                                    })
                                },
                                nodes.clone(),
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (rust-gen-in-module name base-path body) ...)
            globals::rust_minus_gen_minus_in_minus_module.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let base_minus_path = args[1].clone();
                        let body = args[2].clone();
                        {
                            // (let ((module (open-module name base-path))) (body module) (close-module module))
                            {
                                let module = {
                                    // (open-module name base-path)
                                    imports::open_minus_module
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), base_minus_path.clone()])
                                };
                                {
                                    {
                                        // (body module)
                                        body.clone().invoke(&[module.clone()])
                                    };
                                    {
                                        // (close-module module)
                                        imports::close_minus_module
                                            .with(|value| value.get())
                                            .invoke(&[module.clone()])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (rust-gen-in-submodule name parent body) ...)
            globals::rust_minus_gen_minus_in_minus_submodule.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let parent = args[1].clone();
                        let body = args[2].clone();
                        {
                            // (let ((module (open-submodule name parent))) (body module) (close-module module))
                            {
                                let module = {
                                    // (open-submodule name parent)
                                    imports::open_minus_submodule
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), parent.clone()])
                                };
                                {
                                    {
                                        // (body module)
                                        body.clone().invoke(&[module.clone()])
                                    };
                                    {
                                        // (close-module module)
                                        imports::close_minus_module
                                            .with(|value| value.get())
                                            .invoke(&[module.clone()])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        }
    };
}
