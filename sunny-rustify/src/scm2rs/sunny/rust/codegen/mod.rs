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
Scm::func(imports::null_p).invoke(&[g.clone()])}).is_true() {{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])}} else {{
// (cond ...)
if ({
// (import-variable? (cdar g))
Scm::func(imports::import_minus_variable_p).invoke(&[{
// (cdar g)
Scm::func(imports::cdar).invoke(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
Scm::func(imports::cdr).invoke(&[g.clone()])}])}} else if ({
// (keyword? (cdar g))
Scm::func(imports::keyword_p).invoke(&[{
// (cdar g)
Scm::func(imports::cdar).invoke(&[g.clone()])}])}).is_true() {{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
Scm::func(imports::cdr).invoke(&[g.clone()])}])}} else if ({
// (global-variable? (cdar g))
Scm::func(imports::global_minus_variable_p).invoke(&[{
// (cdar g)
Scm::func(imports::cdar).invoke(&[g.clone()])}])}).is_true() {{{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE " (caar g) "\"))}")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),{
// (rustify-identifier (caar g))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (caar g)
Scm::func(imports::caar).invoke(&[g.clone()])}])},Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL VARIABLE "),{
// (caar g)
Scm::func(imports::caar).invoke(&[g.clone()])},Scm::from("\"))}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
Scm::func(imports::cdr).invoke(&[g.clone()])}])}}} else if ({
// (global-function? (cdar g))
Scm::func(imports::global_minus_function_p).invoke(&[{
// (cdar g)
Scm::func(imports::cdar).invoke(&[g.clone()])}])}).is_true() {{{
// (println module "pub fn " (rustify-identifier (caar g)) "(args: &[Scm]) -> Scm { ")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("pub fn "),{
// (rustify-identifier (caar g))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (caar g)
Scm::func(imports::caar).invoke(&[g.clone()])}])},Scm::from("(args: &[Scm]) -> Scm { ")])};{
// ((global-function-get-value (cdar g)) (quote gen-rust) module)
{
// (global-function-get-value (cdar g))
Scm::func(imports::global_minus_function_minus_get_minus_value).invoke(&[{
// (cdar g)
Scm::func(imports::cdar).invoke(&[g.clone()])}])}.invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ".into()}")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from(".into()}")])};{
// (rust-gen-global-defs module (cdr g))
Scm::func(rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),{
// (cdr g)
Scm::func(imports::cdr).invoke(&[g.clone()])}])}}} else {{
// (error "Unexpected entry in global environment" (car g))
Scm::func(imports::error).invoke(&[Scm::from("Unexpected entry in global environment"),{
// (car g)
Scm::func(imports::car).invoke(&[g.clone()])}])}}}}}.into()
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
                    Scm::func(imports::open_minus_module)
                        .invoke(&[name.clone(), base_minus_path.clone()])
                };
                {
                    {
                        // (body module)
                        body.clone().invoke(&[module.clone()])
                    };
                    {
                        // (close-module module)
                        Scm::func(imports::close_minus_module).invoke(&[module.clone()])
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
                    Scm::func(imports::open_minus_submodule).invoke(&[name.clone(), parent.clone()])
                };
                {
                    {
                        // (body module)
                        body.clone().invoke(&[module.clone()])
                    };
                    {
                        // (close-module module)
                        Scm::func(imports::close_minus_module).invoke(&[module.clone()])
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
                Scm::func(imports::println).invoke(&[
                    module.clone(),
                    Scm::from("pub mod "),
                    {
                        // (rustify-libname (module-tree-name node))
                        Scm::func(imports::rustify_minus_libname).invoke(&[{
                            // (module-tree-name node)
                            Scm::func(imports::module_minus_tree_minus_name).invoke(&[node.clone()])
                        }])
                    },
                    Scm::from(";"),
                ])
            };
            if ({
                // (module-tree-leaf? node)
                Scm::func(imports::module_minus_tree_minus_leaf_p).invoke(&[node.clone()])
            })
            .is_true()
            {
                {
                    // (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod)))
                    Scm::func(rust_minus_gen_minus_in_minus_submodule).invoke(&[
                        {
                            // (module-tree-name node)
                            Scm::func(imports::module_minus_tree_minus_name).invoke(&[node.clone()])
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
                                        Scm::func(imports::module_minus_tree_minus_libobj)
                                            .invoke(&[node.clone()])
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
                            Scm::func(imports::module_minus_tree_minus_name).invoke(&[node.clone()])
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
                                            Scm::func(imports::module_minus_tree_minus_children)
                                                .invoke(&[node.clone()])
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
            Scm::func(imports::for_minus_each).invoke(&[
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
                            Scm::func(rust_minus_gen_minus_module_minus_tree)
                                .invoke(&[module.clone(), child.clone()])
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
                    Scm::func(imports::make_minus_module_minus_tree_minus_node)
                        .invoke(&[Scm::symbol("root")])
                };
                {
                    {
                        // (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
                        Scm::func(imports::for_minus_each).invoke(&[
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
                                        Scm::func(imports::module_minus_tree_minus_insert_i).invoke(
                                            &[
                                                module_minus_tree.clone(),
                                                {
                                                    // (car lib)
                                                    Scm::func(imports::car).invoke(&[lib.clone()])
                                                },
                                                {
                                                    // (cdr lib)
                                                    Scm::func(imports::cdr).invoke(&[lib.clone()])
                                                },
                                            ],
                                        )
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
                                Scm::func(imports::module_minus_tree_minus_children)
                                    .invoke(&[module_minus_tree.clone()])
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
