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
    pub use super::rust_minus_gen_minus_global_minus_defs;
    pub use super::rust_minus_gen_minus_in_minus_module;
    pub use super::rust_minus_gen_minus_in_minus_submodule;
    pub use super::rust_minus_gen_minus_module_minus_tree;
    pub use super::rust_minus_gen_minus_module_minus_tree_minus_list;
    pub use super::rust_minus_gen_minus_modules;
}

pub fn rust_minus_gen_minus_global_minus_defs(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let module_5 = args[0].clone();let g_0 = args[1].clone();if ({
// (null? g)
imports::null_p(&[g_0.clone()])}).is_true() {{
// (println module)
imports::println(&[module_5.clone()])}} else {{
// (cond ...)
if ({
// (import-variable? (car g))
imports::import_minus_variable_p(&[{
// (car g)
imports::car(&[g_0.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module_5.clone(),{
// (cdr g)
imports::cdr(&[g_0.clone()])}])}} else if ({
// (keyword? (car g))
imports::keyword_p(&[{
// (car g)
imports::car(&[g_0.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module_5.clone(),{
// (cdr g)
imports::cdr(&[g_0.clone()])}])}} else if ({
// (global-variable? (car g))
imports::global_minus_variable_p(&[{
// (car g)
imports::car(&[g_0.clone()])}])}).is_true() {{{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (variable-name (car g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE " (variable-name (car g)) "\"))}")
imports::println(&[module_5.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g_0.clone()])}])},Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE "),{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g_0.clone()])}])},Scm::from("\"))}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module_5.clone(),{
// (cdr g)
imports::cdr(&[g_0.clone()])}])}}} else if ({
// (global-function? (car g))
imports::global_minus_function_p(&[{
// (car g)
imports::car(&[g_0.clone()])}])}).is_true() {{{
// (println module "pub fn " (variable-name (car g)) "(args: &[Scm]) -> Scm { ")
imports::println(&[module_5.clone(),Scm::from("pub fn "),{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g_0.clone()])}])},Scm::from("(args: &[Scm]) -> Scm { ")])};{
// ((global-function-get-value (car g)) (quote gen-rust) module)
{
// (global-function-get-value (car g))
imports::global_minus_function_minus_get_minus_value(&[{
// (car g)
imports::car(&[g_0.clone()])}])}.invoke(&[Scm::symbol("gen-rust"),module_5.clone()])};{
// (println module ".into()}")
imports::println(&[module_5.clone(),Scm::from(".into()}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module_5.clone(),{
// (cdr g)
imports::cdr(&[g_0.clone()])}])}}} else {{
// (error "Unexpected entry in global environment" (car g))
imports::error(&[Scm::from("Unexpected entry in global environment"),{
// (car g)
imports::car(&[g_0.clone()])}])}}}}}.into()
}
pub fn rust_minus_gen_minus_in_minus_module(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let name_29 = args[0].clone();
        let base_minus_path_1 = args[1].clone();
        let body_0 = args[2].clone();
        {
            // (let ((module (open-module name base-path))) (body module) (close-module module))
            {
                let module_9 = {
                    // (open-module name base-path)
                    imports::open_minus_module(&[name_29.clone(), base_minus_path_1.clone()])
                };
                {
                    {
                        // (body module)
                        body_0.clone().invoke(&[module_9.clone()])
                    };
                    {
                        // (close-module module)
                        imports::close_minus_module(&[module_9.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn rust_minus_gen_minus_in_minus_submodule(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let name_30 = args[0].clone();
        let parent_1 = args[1].clone();
        let body_1 = args[2].clone();
        {
            // (let ((module (open-submodule name parent))) (body module) (close-module module))
            {
                let module_10 = {
                    // (open-submodule name parent)
                    imports::open_minus_submodule(&[name_30.clone(), parent_1.clone()])
                };
                {
                    {
                        // (body module)
                        body_1.clone().invoke(&[module_10.clone()])
                    };
                    {
                        // (close-module module)
                        imports::close_minus_module(&[module_10.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn rust_minus_gen_minus_module_minus_tree(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let module_7 = args[0].clone();
        let node_7 = args[1].clone();
        {
            {
                // (println module "pub mod " (rustify-libname (module-tree-name node)) ";")
                imports::println(&[
                    module_7.clone(),
                    Scm::from("pub mod "),
                    {
                        // (rustify-libname (module-tree-name node))
                        imports::rustify_minus_libname(&[{
                            // (module-tree-name node)
                            imports::module_minus_tree_minus_name(&[node_7.clone()])
                        }])
                    },
                    Scm::from(";"),
                ])
            };
            if ({
                // (module-tree-leaf? node)
                imports::module_minus_tree_minus_leaf_p(&[node_7.clone()])
            })
            .is_true()
            {
                {
                    // (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod)))
                    Scm::func(rust_minus_gen_minus_in_minus_submodule).invoke(&[
                        {
                            // (module-tree-name node)
                            imports::module_minus_tree_minus_name(&[node_7.clone()])
                        },
                        module_7.clone(),
                        {
                            // Closure
                            let node_7 = node_7.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let submod_0 = args[0].clone();
                                {
                                    // ((module-tree-libobj node) (quote gen-rust) submod)
                                    {
                                        // (module-tree-libobj node)
                                        imports::module_minus_tree_minus_libobj(&[node_7.clone()])
                                    }
                                    .invoke(&[Scm::symbol("gen-rust"), submod_0.clone()])
                                }
                            })
                        },
                    ])
                }
            } else {
                {
                    // (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) (rust-gen-module-tree-list submod (module-tree-children node))))
                    Scm::func(rust_minus_gen_minus_in_minus_submodule).invoke(&[
                        {
                            // (module-tree-name node)
                            imports::module_minus_tree_minus_name(&[node_7.clone()])
                        },
                        module_7.clone(),
                        {
                            // Closure
                            let node_7 = node_7.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let submod_1 = args[0].clone();
                                {
                                    // (rust-gen-module-tree-list submod (module-tree-children node))
                                    Scm::func(rust_minus_gen_minus_module_minus_tree_minus_list)
                                        .invoke(&[submod_1.clone(), {
                                            // (module-tree-children node)
                                            imports::module_minus_tree_minus_children(&[
                                                node_7.clone()
                                            ])
                                        }])
                                }
                            })
                        },
                    ])
                }
            }
        }
    }
    .into()
}
pub fn rust_minus_gen_minus_module_minus_tree_minus_list(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let module_8 = args[0].clone();
        let nodes_0 = args[1].clone();
        {
            // (for-each (lambda (child) (rust-gen-module-tree module child)) nodes)
            imports::for_minus_each(&[
                {
                    // Closure
                    let module_8 = module_8.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let child_2 = args[0].clone();
                        {
                            // (rust-gen-module-tree module child)
                            rust_minus_gen_minus_module_minus_tree(&[
                                module_8.clone(),
                                child_2.clone(),
                            ])
                        }
                    })
                },
                nodes_0.clone(),
            ])
        }
    }
    .into()
}
pub fn rust_minus_gen_minus_modules(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let module_6 = args[0].clone();
        let libs_0 = args[1].clone();
        {
            // (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree)))
            {
                let module_minus_tree_0 = {
                    // (make-module-tree-node (quote root))
                    imports::make_minus_module_minus_tree_minus_node(&[Scm::symbol("root")])
                };
                {
                    {
                        // (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
                        imports::for_minus_each(&[
                            {
                                // Closure
                                let module_minus_tree_0 = module_minus_tree_0.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let lib_1 = args[0].clone();
                                    {
                                        // (module-tree-insert! module-tree (car lib) (cdr lib))
                                        imports::module_minus_tree_minus_insert_i(&[
                                            module_minus_tree_0.clone(),
                                            {
                                                // (car lib)
                                                imports::car(&[lib_1.clone()])
                                            },
                                            {
                                                // (cdr lib)
                                                imports::cdr(&[lib_1.clone()])
                                            },
                                        ])
                                    }
                                })
                            },
                            libs_0.clone(),
                        ])
                    };
                    {
                        // (rust-gen-module-tree-list module (module-tree-children module-tree))
                        Scm::func(rust_minus_gen_minus_module_minus_tree_minus_list).invoke(&[
                            module_6.clone(),
                            {
                                // (module-tree-children module-tree)
                                imports::module_minus_tree_minus_children(&[
                                    module_minus_tree_0.clone()
                                ])
                            },
                        ])
                    }
                }
            }
        }
    }
    .into()
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
            (/*NOP*/)
        };
        {
            // (define (rust-gen-modules module libs) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-gen-module-tree module node) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-gen-module-tree-list module nodes) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-gen-in-module name base-path body) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-gen-in-submodule name parent body) ...)
            (/*NOP*/)
        }
    };
}
