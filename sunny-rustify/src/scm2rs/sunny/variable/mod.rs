#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::table::exports::*;
}

pub mod exports {
    pub use super::boxed_minus_variable_p;
    pub use super::global_minus_add_minus_definition_i;
    pub use super::global_minus_function_minus_get_minus_value;
    pub use super::global_minus_function_minus_set_minus_value_i;
    pub use super::global_minus_function_p;
    pub use super::global_minus_variable_p;
    pub use super::import_minus_variable_p;
    pub use super::keyword_minus_handler;
    pub use super::keyword_minus_name;
    pub use super::keyword_p;
    pub use super::local_minus_boxify_i;
    pub use super::local_minus_variable_p;
    pub use super::make_minus_keyword;
    pub use super::new_minus_boxed;
    pub use super::new_minus_global;
    pub use super::new_minus_import;
    pub use super::new_minus_keyword;
    pub use super::new_minus_local;
    pub use super::replace_minus_var_i;
    pub use super::undefined_minus_global_minus_variable_p;
    pub use super::variable_minus_mutable_p;
    pub use super::variable_minus_name;
    pub use super::variable_minus_set_minus_mutable_i;
    pub use super::variable_minus_set_minus_name_i;
    pub use super::variable_p;
}

thread_local! {#[allow(non_upper_case_globals)] pub static BoxedVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE BoxedVariable"))}
thread_local! {#[allow(non_upper_case_globals)] pub static GlobalFunction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE GlobalFunction"))}
thread_local! {#[allow(non_upper_case_globals)] pub static GlobalVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE GlobalVariable"))}
thread_local! {#[allow(non_upper_case_globals)] pub static ImportedVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE ImportedVariable"))}
thread_local! {#[allow(non_upper_case_globals)] pub static Keyword: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE Keyword"))}
thread_local! {#[allow(non_upper_case_globals)] pub static LocalVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE LocalVariable"))}
thread_local! {#[allow(non_upper_case_globals)] pub static UndefinedGlobal: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE UndefinedGlobal"))}
thread_local! {#[allow(non_upper_case_globals)] pub static Variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE Variable"))}
pub fn boxed_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__13 = args[0].clone();
        {
            // (ancestor? obj BoxedVariable)
            imports::ancestor_p(&[obj__13.clone(), BoxedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_add_minus_definition_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__8 = args[0].clone();
        let val__0 = args[1].clone();
        {
            // (call-method var (quote add-definition!) val)
            imports::call_minus_method(&[
                var__8.clone(),
                Scm::symbol("add-definition!"),
                val__0.clone(),
            ])
        }
    }
    .into()
}
pub fn global_minus_function_minus_get_minus_value(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__9 = args[0].clone();
        {
            // (get-field var (quote value))
            imports::get_minus_field(&[var__9.clone(), Scm::symbol("value")])
        }
    }
    .into()
}
pub fn global_minus_function_minus_set_minus_value_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__10 = args[0].clone();
        let val__1 = args[1].clone();
        {
            // (set-field! var (quote value) val)
            imports::set_minus_field_i(&[var__10.clone(), Scm::symbol("value"), val__1.clone()])
        }
    }
    .into()
}
pub fn global_minus_function_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__10 = args[0].clone();
        {
            // (ancestor? obj GlobalFunction)
            imports::ancestor_p(&[obj__10.clone(), GlobalFunction.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__8 = args[0].clone();
        {
            // (ancestor? obj GlobalVariable)
            imports::ancestor_p(&[obj__8.clone(), GlobalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn import_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__11 = args[0].clone();
        {
            // (ancestor? obj ImportedVariable)
            imports::ancestor_p(&[obj__11.clone(), ImportedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn keyword_minus_handler(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw__1 = args[0].clone();
        {
            // (get-field kw (quote handler))
            imports::get_minus_field(&[kw__1.clone(), Scm::symbol("handler")])
        }
    }
    .into()
}
pub fn keyword_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw__0 = args[0].clone();
        {
            // (get-field kw (quote name))
            imports::get_minus_field(&[kw__0.clone(), Scm::symbol("name")])
        }
    }
    .into()
}
pub fn keyword_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__6 = args[0].clone();
        {
            // (and (table? obj) (ancestor? obj Keyword))
            if ({
                // (table? obj)
                imports::table_p(&[obj__6.clone()])
            })
            .is_true()
            {
                {
                    // (ancestor? obj Keyword)
                    imports::ancestor_p(&[obj__6.clone(), Keyword.with(|value| value.get())])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn local_minus_boxify_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__11 = args[0].clone();
        {
            // (call-method var (quote into-boxed!))
            imports::call_minus_method(&[var__11.clone(), Scm::symbol("into-boxed!")])
        }
    }
    .into()
}
pub fn local_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__12 = args[0].clone();
        {
            // (ancestor? obj LocalVariable)
            imports::ancestor_p(&[obj__12.clone(), LocalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn make_minus_keyword(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__6 = args[0].clone();
        let handler__0 = args[1].clone();
        {
            // (let ((keyword (clone Keyword))) (set-field! keyword (quote name) name) (set-field! keyword (quote handler) handler) keyword)
            {
                let keyword__0 = {
                    // (clone Keyword)
                    imports::clone(&[Keyword.with(|value| value.get())])
                };
                {
                    {
                        // (set-field! keyword (quote name) name)
                        imports::set_minus_field_i(&[
                            keyword__0.clone(),
                            Scm::symbol("name"),
                            name__6.clone(),
                        ])
                    };
                    {
                        // (set-field! keyword (quote handler) handler)
                        imports::set_minus_field_i(&[
                            keyword__0.clone(),
                            Scm::symbol("handler"),
                            handler__0.clone(),
                        ])
                    };
                    keyword__0.clone()
                }
            }
        }
    }
    .into()
}
pub fn new_minus_boxed(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__11 = args[0].clone();
        {
            // (call-method BoxedVariable (quote new) name)
            imports::call_minus_method(&[
                BoxedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__11.clone(),
            ])
        }
    }
    .into()
}
pub fn new_minus_global(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__9 = args[0].clone();
        {
            // (call-method UndefinedGlobal (quote new) name)
            imports::call_minus_method(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("new"),
                name__9.clone(),
            ])
        }
    }
    .into()
}
pub fn new_minus_import(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__8 = args[0].clone();
        {
            // (call-method ImportedVariable (quote new) name)
            imports::call_minus_method(&[
                ImportedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__8.clone(),
            ])
        }
    }
    .into()
}
pub fn new_minus_keyword(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__7 = args[0].clone();
        let handler__1 = args[1].clone();
        {
            // (make-keyword name handler)
            make_minus_keyword(&[name__7.clone(), handler__1.clone()])
        }
    }
    .into()
}
pub fn new_minus_local(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__10 = args[0].clone();
        {
            // (call-method LocalVariable (quote new) name)
            imports::call_minus_method(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__10.clone(),
            ])
        }
    }
    .into()
}
pub fn replace_minus_var_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__12 = args[0].clone();
        let new_minus_var__0 = args[1].clone();
        {
            // (replace-table! var new-var)
            imports::replace_minus_table_i(&[var__12.clone(), new_minus_var__0.clone()])
        }
    }
    .into()
}
pub fn undefined_minus_global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__9 = args[0].clone();
        {
            // (ancestor? obj UndefinedGlobal)
            imports::ancestor_p(&[obj__9.clone(), UndefinedGlobal.with(|value| value.get())])
        }
    }
    .into()
}
pub fn variable_minus_mutable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__6 = args[0].clone();
        {
            // (call-method var (quote mutable?))
            imports::call_minus_method(&[var__6.clone(), Scm::symbol("mutable?")])
        }
    }
    .into()
}
pub fn variable_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__4 = args[0].clone();
        {
            // (let ((name (get-field var (quote name)))) (if (string? name) name (symbol->string name)))
            {
                let name__4 = {
                    // (get-field var (quote name))
                    imports::get_minus_field(&[var__4.clone(), Scm::symbol("name")])
                };
                if ({
                    // (string? name)
                    imports::string_p(&[name__4.clone()])
                })
                .is_true()
                {
                    name__4.clone()
                } else {
                    {
                        // (symbol->string name)
                        imports::symbol_minus__g_string(&[name__4.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn variable_minus_set_minus_mutable_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__7 = args[0].clone();
        {
            // (call-method var (quote set-mutable!))
            imports::call_minus_method(&[var__7.clone(), Scm::symbol("set-mutable!")])
        }
    }
    .into()
}
pub fn variable_minus_set_minus_name_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__5 = args[0].clone();
        let name__5 = args[1].clone();
        {
            // (set-field! var (quote name) name)
            imports::set_minus_field_i(&[var__5.clone(), Scm::symbol("name"), name__5.clone()])
        }
    }
    .into()
}
pub fn variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__7 = args[0].clone();
        {
            // (ancestor? obj Variable)
            imports::ancestor_p(&[obj__7.clone(), Variable.with(|value| value.get())])
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
    crate::scheme::cxr::initialize();
    crate::scheme::write::initialize();
    crate::sunny::table::initialize();
    {
        (/*NOP*/);
        {
            // (define Keyword (make-table))
            Keyword.with(|value| {
                value.set({
                    // (make-table)
                    imports::make_minus_table(&[])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! Keyword (quote __name__) (quote Keyword))
            imports::set_minus_field_i(&[
                Keyword.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("Keyword"),
            ])
        };
        {
            // (define Variable (make-table))
            Variable.with(|value| {
                value.set({
                    // (make-table)
                    imports::make_minus_table(&[])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! Variable (quote __name__) (quote Variable))
            imports::set_minus_field_i(&[
                Variable.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("Variable"),
            ])
        };
        {
            // (set-field! Variable (quote mut) #f)
            imports::set_minus_field_i(&[
                Variable.with(|value| value.get()),
                Scm::symbol("mut"),
                Scm::False,
            ])
        };
        {
            // (set-field! Variable (quote mutable?) (lambda (self) (get-field self (quote mut))))
            imports::set_minus_field_i(&[
                Variable.with(|value| value.get()),
                Scm::symbol("mutable?"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self__5 = args[0].clone();
                        {
                            // (get-field self (quote mut))
                            imports::get_minus_field(&[self__5.clone(), Scm::symbol("mut")])
                        }
                    })
                },
            ])
        };
        {
            // (set-field! Variable (quote set-mutable!) (lambda (self) (set-field! self (quote mut) #t)))
            imports::set_minus_field_i(&[
                Variable.with(|value| value.get()),
                Scm::symbol("set-mutable!"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self__6 = args[0].clone();
                        {
                            // (set-field! self (quote mut) #t)
                            imports::set_minus_field_i(&[
                                self__6.clone(),
                                Scm::symbol("mut"),
                                Scm::True,
                            ])
                        }
                    })
                },
            ])
        };
        {
            // (define GlobalVariable (clone Variable))
            GlobalVariable.with(|value| {
                value.set({
                    // (clone Variable)
                    imports::clone(&[Variable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! GlobalVariable (quote __name__) (quote GlobalVariable))
            imports::set_minus_field_i(&[
                GlobalVariable.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("GlobalVariable"),
            ])
        };
        {
            // (set-field! GlobalVariable (quote mutable?) (lambda (self) #t))
            imports::set_minus_field_i(&[
                GlobalVariable.with(|value| value.get()),
                Scm::symbol("mutable?"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        Scm::True
                    })
                },
            ])
        };
        {
            // (set-field! GlobalVariable (quote add-definition!) (lambda (self value) (quote IGNORED)))
            imports::set_minus_field_i(&[
                GlobalVariable.with(|value| value.get()),
                Scm::symbol("add-definition!"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let value = args[1].clone();
                        Scm::symbol("IGNORED")
                    })
                },
            ])
        };
        {
            // (define GlobalFunction (clone Variable))
            GlobalFunction.with(|value| {
                value.set({
                    // (clone Variable)
                    imports::clone(&[Variable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! GlobalFunction (quote __name__) (quote GlobalFunction))
            imports::set_minus_field_i(&[
                GlobalFunction.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("GlobalFunction"),
            ])
        };
        {
            // (set-field! GlobalFunction (quote add-definition!) (lambda (self value) (set-parent! self GlobalVariable)))
            imports::set_minus_field_i(&[
                GlobalFunction.with(|value| value.get()),
                Scm::symbol("add-definition!"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self__7 = args[0].clone();
                        let value = args[1].clone();
                        {
                            // (set-parent! self GlobalVariable)
                            imports::set_minus_parent_i(&[
                                self__7.clone(),
                                GlobalVariable.with(|value| value.get()),
                            ])
                        }
                    })
                },
            ])
        };
        {
            // (define UndefinedGlobal (clone Variable))
            UndefinedGlobal.with(|value| {
                value.set({
                    // (clone Variable)
                    imports::clone(&[Variable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! UndefinedGlobal (quote __name__) (quote UndefinedGlobal))
            imports::set_minus_field_i(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("UndefinedGlobal"),
            ])
        };
        {
            // (set-field! UndefinedGlobal (quote new) (lambda (self name) (let ((var (clone UndefinedGlobal))) (set-field! var (quote name) name) var)))
            imports::set_minus_field_i(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("new"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let name__0 = args[1].clone();
                        {
                            // (let ((var (clone UndefinedGlobal))) (set-field! var (quote name) name) var)
                            {
                                let var__0 = {
                                    // (clone UndefinedGlobal)
                                    imports::clone(&[UndefinedGlobal.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__0.clone(),
                                            Scm::symbol("name"),
                                            name__0.clone(),
                                        ])
                                    };
                                    var__0.clone()
                                }
                            }
                        }
                    })
                },
            ])
        };
        {
            // (set-field! UndefinedGlobal (quote add-definition!) (lambda (self value) (cond ((eq? (value (quote kind)) (quote ABSTRACTION)) (set-parent! self GlobalFunction) (set-field! self (quote value) value)) ((eq? (value (quote kind)) (quote VARARG-ABSTRACTION)) (set-parent! self GlobalFunction) (set-field! self (quote value) value)) (else (set-parent! self GlobalVariable)))))
            imports::set_minus_field_i(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("add-definition!"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self__8 = args[0].clone();
                        let value__2 = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (eq? (value (quote kind)) (quote ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value__2.clone().invoke(&[Scm::symbol("kind")])
                                    },
                                    Scm::symbol("ABSTRACTION"),
                                ])
                            })
                            .is_true()
                            {
                                {
                                    {
                                        // (set-parent! self GlobalFunction)
                                        imports::set_minus_parent_i(&[
                                            self__8.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__8.clone(),
                                            Scm::symbol("value"),
                                            value__2.clone(),
                                        ])
                                    }
                                }
                            } else if ({
                                // (eq? (value (quote kind)) (quote VARARG-ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value__2.clone().invoke(&[Scm::symbol("kind")])
                                    },
                                    Scm::symbol("VARARG-ABSTRACTION"),
                                ])
                            })
                            .is_true()
                            {
                                {
                                    {
                                        // (set-parent! self GlobalFunction)
                                        imports::set_minus_parent_i(&[
                                            self__8.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__8.clone(),
                                            Scm::symbol("value"),
                                            value__2.clone(),
                                        ])
                                    }
                                }
                            } else {
                                {
                                    // (set-parent! self GlobalVariable)
                                    imports::set_minus_parent_i(&[
                                        self__8.clone(),
                                        GlobalVariable.with(|value| value.get()),
                                    ])
                                }
                            }
                        }
                    })
                },
            ])
        };
        {
            // (define ImportedVariable (clone Variable))
            ImportedVariable.with(|value| {
                value.set({
                    // (clone Variable)
                    imports::clone(&[Variable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! ImportedVariable (quote __name__) (quote ImportedVariable))
            imports::set_minus_field_i(&[
                ImportedVariable.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("ImportedVariable"),
            ])
        };
        {
            // (set-field! ImportedVariable (quote new) (lambda (self name) (let ((var (clone ImportedVariable))) (set-field! var (quote name) name) var)))
            imports::set_minus_field_i(&[
                ImportedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let name__1 = args[1].clone();
                        {
                            // (let ((var (clone ImportedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__1 = {
                                    // (clone ImportedVariable)
                                    imports::clone(&[ImportedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__1.clone(),
                                            Scm::symbol("name"),
                                            name__1.clone(),
                                        ])
                                    };
                                    var__1.clone()
                                }
                            }
                        }
                    })
                },
            ])
        };
        {
            // (define LocalVariable (clone Variable))
            LocalVariable.with(|value| {
                value.set({
                    // (clone Variable)
                    imports::clone(&[Variable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! LocalVariable (quote __name__) (quote LocalVariable))
            imports::set_minus_field_i(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("LocalVariable"),
            ])
        };
        {
            // (set-field! LocalVariable (quote into-boxed!) (lambda (self) (set-parent! self BoxedVariable)))
            imports::set_minus_field_i(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("into-boxed!"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self__9 = args[0].clone();
                        {
                            // (set-parent! self BoxedVariable)
                            imports::set_minus_parent_i(&[
                                self__9.clone(),
                                BoxedVariable.with(|value| value.get()),
                            ])
                        }
                    })
                },
            ])
        };
        {
            // (set-field! LocalVariable (quote new) (lambda (self name) (let ((var (clone LocalVariable))) (set-field! var (quote name) name) var)))
            imports::set_minus_field_i(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("new"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let name__2 = args[1].clone();
                        {
                            // (let ((var (clone LocalVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__2 = {
                                    // (clone LocalVariable)
                                    imports::clone(&[LocalVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__2.clone(),
                                            Scm::symbol("name"),
                                            name__2.clone(),
                                        ])
                                    };
                                    var__2.clone()
                                }
                            }
                        }
                    })
                },
            ])
        };
        {
            // (define BoxedVariable (clone LocalVariable))
            BoxedVariable.with(|value| {
                value.set({
                    // (clone LocalVariable)
                    imports::clone(&[LocalVariable.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! BoxedVariable (quote __name__) (quote BoxedVariable))
            imports::set_minus_field_i(&[
                BoxedVariable.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("BoxedVariable"),
            ])
        };
        {
            // (set-field! BoxedVariable (quote new) (lambda (self name) (let ((var (clone BoxedVariable))) (set-field! var (quote name) name) var)))
            imports::set_minus_field_i(&[
                BoxedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let name__3 = args[1].clone();
                        {
                            // (let ((var (clone BoxedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__3 = {
                                    // (clone BoxedVariable)
                                    imports::clone(&[BoxedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__3.clone(),
                                            Scm::symbol("name"),
                                            name__3.clone(),
                                        ])
                                    };
                                    var__3.clone()
                                }
                            }
                        }
                    })
                },
            ])
        };
        {
            // (define (variable-name var) ...)
            (/*NOP*/)
        };
        {
            // (define (variable-set-name! var name) ...)
            (/*NOP*/)
        };
        {
            // (define (keyword? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (make-keyword name handler) ...)
            (/*NOP*/)
        };
        {
            // (define (keyword-name kw) ...)
            (/*NOP*/)
        };
        {
            // (define (keyword-handler kw) ...)
            (/*NOP*/)
        };
        {
            // (define (variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (global-variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (undefined-global-variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (global-function? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (import-variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (local-variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (boxed-variable? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (variable-mutable? var) ...)
            (/*NOP*/)
        };
        {
            // (define (variable-set-mutable! var) ...)
            (/*NOP*/)
        };
        {
            // (define (global-add-definition! var val) ...)
            (/*NOP*/)
        };
        {
            // (define (global-function-get-value var) ...)
            (/*NOP*/)
        };
        {
            // (define (global-function-set-value! var val) ...)
            (/*NOP*/)
        };
        {
            // (define (local-boxify! var) ...)
            (/*NOP*/)
        };
        {
            // (define (new-keyword name handler) ...)
            (/*NOP*/)
        };
        {
            // (define (new-import name) ...)
            (/*NOP*/)
        };
        {
            // (define (new-global name) ...)
            (/*NOP*/)
        };
        {
            // (define (new-local name) ...)
            (/*NOP*/)
        };
        {
            // (define (new-boxed name) ...)
            (/*NOP*/)
        };
        {
            // (define (replace-var! var new-var) ...)
            (/*NOP*/)
        }
    };
}
