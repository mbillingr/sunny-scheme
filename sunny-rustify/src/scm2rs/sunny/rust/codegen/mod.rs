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
    {if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let g = args[1].clone();if ({
// (null? g)
imports::null_p(&[g.clone()])}).is_true() {{
// (println module)
imports::println(&[module.clone()])}} else {{
// (cond ...)
if ({
// (import-variable? (car g))
imports::import_minus_variable_p(&[{
// (car g)
imports::car(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
imports::cdr(&[g.clone()])}])}} else if ({
// (keyword? (car g))
imports::keyword_p(&[{
// (car g)
imports::car(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
imports::cdr(&[g.clone()])}])}} else if ({
// (global-variable? (car g))
imports::global_minus_variable_p(&[{
// (car g)
imports::car(&[g.clone()])}])}).is_true() {{{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (variable-name (car g))) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE " (variable-name (car g)) "\"))}")
imports::println(&[module.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),{
// (rustify-identifier (variable-name (car g)))
imports::rustify_minus_identifier(&[{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g.clone()])}])}])},Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE "),{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g.clone()])}])},Scm::from("\"))}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
imports::cdr(&[g.clone()])}])}}} else if ({
// (global-function? (car g))
imports::global_minus_function_p(&[{
// (car g)
imports::car(&[g.clone()])}])}).is_true() {{{
// (println module "pub fn " (rustify-identifier (variable-name (car g))) "(args: &[Scm]) -> Scm { ")
imports::println(&[module.clone(),Scm::from("pub fn "),{
// (rustify-identifier (variable-name (car g)))
imports::rustify_minus_identifier(&[{
// (variable-name (car g))
imports::variable_minus_name(&[{
// (car g)
imports::car(&[g.clone()])}])}])},Scm::from("(args: &[Scm]) -> Scm { ")])};{
// ((global-function-get-value (car g)) (quote gen-rust) module)
{
// (global-function-get-value (car g))
imports::global_minus_function_minus_get_minus_value(&[{
// (car g)
imports::car(&[g.clone()])}])}.invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ".into()}")
imports::println(&[module.clone(),Scm::from(".into()}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
imports::cdr(&[g.clone()])}])}}} else {{
// (error "Unexpected entry in global environment" (car g))
imports::error(&[Scm::from("Unexpected entry in global environment"),{
// (car g)
imports::car(&[g.clone()])}])}}}}}.into()
}
pub fn rust_minus_gen_minus_in_minus_module(args: &[Scm]) -> Scm {
    {
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
                    imports::open_minus_module(&[name.clone(), base_minus_path.clone()])
                };
                {
                    {
                        // (body module)
                        body.clone().invoke(&[module.clone()])
                    };
                    {
                        // (close-module module)
                        imports::close_minus_module(&[module.clone()])
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
        let name = args[0].clone();
        let parent = args[1].clone();
        let body = args[2].clone();
        {
            // (let ((module (open-submodule name parent))) (body module) (close-module module))
            {
                let module = {
                    // (open-submodule name parent)
                    imports::open_minus_submodule(&[name.clone(), parent.clone()])
                };
                {
                    {
                        // (body module)
                        body.clone().invoke(&[module.clone()])
                    };
                    {
                        // (close-module module)
                        imports::close_minus_module(&[module.clone()])
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
        let module = args[0].clone();
        let node = args[1].clone();
        {
            {
                // (println module "pub mod " (rustify-libname (module-tree-name node)) ";")
                imports::println(&[
                    module.clone(),
                    Scm::from("pub mod "),
                    {
                        // (rustify-libname (module-tree-name node))
                        imports::rustify_minus_libname(&[{
                            // (module-tree-name node)
                            imports::module_minus_tree_minus_name(&[node.clone()])
                        }])
                    },
                    Scm::from(";"),
                ])
            };
            if ({
                // (module-tree-leaf? node)
                imports::module_minus_tree_minus_leaf_p(&[node.clone()])
            })
            .is_true()
            {
                {
                    // (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod)))
                    Scm::func(rust_minus_gen_minus_in_minus_submodule).invoke(&[
                        {
                            // (module-tree-name node)
                            imports::module_minus_tree_minus_name(&[node.clone()])
                        },
                        module.clone(),
                        {
                            // Closure
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let submod = args[0].clone();
                                {
                                    // ((module-tree-libobj node) (quote gen-rust) submod)
                                    {
                                        // (module-tree-libobj node)
                                        imports::module_minus_tree_minus_libobj(&[node.clone()])
                                    }
                                    .invoke(&[Scm::symbol("gen-rust"), submod.clone()])
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
                            imports::module_minus_tree_minus_name(&[node.clone()])
                        },
                        module.clone(),
                        {
                            // Closure
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let submod = args[0].clone();
                                {
                                    // (rust-gen-module-tree-list submod (module-tree-children node))
                                    Scm::func(rust_minus_gen_minus_module_minus_tree_minus_list)
                                        .invoke(&[submod.clone(), {
                                            // (module-tree-children node)
                                            imports::module_minus_tree_minus_children(&[
                                                node.clone()
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
        let module = args[0].clone();
        let nodes = args[1].clone();
        {
            // (for-each (lambda (child) (rust-gen-module-tree module child)) nodes)
            imports::for_minus_each(&[
                {
                    // Closure
                    let module = module.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let child = args[0].clone();
                        {
                            // (rust-gen-module-tree module child)
                            rust_minus_gen_minus_module_minus_tree(&[module.clone(), child.clone()])
                        }
                    })
                },
                nodes.clone(),
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
        let module = args[0].clone();
        let libs = args[1].clone();
        {
            // (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree)))
            {
                let module_minus_tree = {
                    // (make-module-tree-node (quote root))
                    imports::make_minus_module_minus_tree_minus_node(&[Scm::symbol("root")])
                };
                {
                    {
                        // (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
                        imports::for_minus_each(&[
                            {
                                // Closure
                                let module_minus_tree = module_minus_tree.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let lib = args[0].clone();
                                    {
                                        // (module-tree-insert! module-tree (car lib) (cdr lib))
                                        imports::module_minus_tree_minus_insert_i(&[
                                            module_minus_tree.clone(),
                                            {
                                                // (car lib)
                                                imports::car(&[lib.clone()])
                                            },
                                            {
                                                // (cdr lib)
                                                imports::cdr(&[lib.clone()])
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
                        Scm::func(rust_minus_gen_minus_module_minus_tree_minus_list).invoke(&[
                            module.clone(),
                            {
                                // (module-tree-children module-tree)
                                imports::module_minus_tree_minus_children(&[
                                    module_minus_tree.clone()
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
