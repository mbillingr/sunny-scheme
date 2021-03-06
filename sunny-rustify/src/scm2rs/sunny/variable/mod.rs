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
        let obj_13 = args[0].clone();
        {
            // (ancestor? obj BoxedVariable)
            imports::ancestor_p(&[obj_13.clone(), BoxedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_add_minus_definition_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var_8 = args[0].clone();
        let val_0 = args[1].clone();
        {
            // (call-method var (quote add-definition!) val)
            imports::call_minus_method(&[
                var_8.clone(),
                Scm::symbol("add-definition!"),
                val_0.clone(),
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
        let var_9 = args[0].clone();
        {
            // (get-field var (quote value))
            imports::get_minus_field(&[var_9.clone(), Scm::symbol("value")])
        }
    }
    .into()
}
pub fn global_minus_function_minus_set_minus_value_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var_10 = args[0].clone();
        let val_1 = args[1].clone();
        {
            // (set-field! var (quote value) val)
            imports::set_minus_field_i(&[var_10.clone(), Scm::symbol("value"), val_1.clone()])
        }
    }
    .into()
}
pub fn global_minus_function_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_10 = args[0].clone();
        {
            // (ancestor? obj GlobalFunction)
            imports::ancestor_p(&[obj_10.clone(), GlobalFunction.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_8 = args[0].clone();
        {
            // (ancestor? obj GlobalVariable)
            imports::ancestor_p(&[obj_8.clone(), GlobalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn import_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_11 = args[0].clone();
        {
            // (ancestor? obj ImportedVariable)
            imports::ancestor_p(&[obj_11.clone(), ImportedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn keyword_minus_handler(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw_1 = args[0].clone();
        {
            // (get-field kw (quote handler))
            imports::get_minus_field(&[kw_1.clone(), Scm::symbol("handler")])
        }
    }
    .into()
}
pub fn keyword_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw_0 = args[0].clone();
        {
            // (get-field kw (quote name))
            imports::get_minus_field(&[kw_0.clone(), Scm::symbol("name")])
        }
    }
    .into()
}
pub fn keyword_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_6 = args[0].clone();
        {
            // (and (table? obj) (ancestor? obj Keyword))
            if ({
                // (table? obj)
                imports::table_p(&[obj_6.clone()])
            })
            .is_true()
            {
                {
                    // (ancestor? obj Keyword)
                    imports::ancestor_p(&[obj_6.clone(), Keyword.with(|value| value.get())])
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
        let var_11 = args[0].clone();
        {
            // (call-method var (quote into-boxed!))
            imports::call_minus_method(&[var_11.clone(), Scm::symbol("into-boxed!")])
        }
    }
    .into()
}
pub fn local_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_12 = args[0].clone();
        {
            // (ancestor? obj LocalVariable)
            imports::ancestor_p(&[obj_12.clone(), LocalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn make_minus_keyword(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_6 = args[0].clone();
        let handler_0 = args[1].clone();
        {
            // (let ((keyword (clone Keyword))) (set-field! keyword (quote name) name) (set-field! keyword (quote handler) handler) keyword)
            {
                let keyword_0 = {
                    // (clone Keyword)
                    imports::clone(&[Keyword.with(|value| value.get())])
                };
                {
                    {
                        // (set-field! keyword (quote name) name)
                        imports::set_minus_field_i(&[
                            keyword_0.clone(),
                            Scm::symbol("name"),
                            name_6.clone(),
                        ])
                    };
                    {
                        // (set-field! keyword (quote handler) handler)
                        imports::set_minus_field_i(&[
                            keyword_0.clone(),
                            Scm::symbol("handler"),
                            handler_0.clone(),
                        ])
                    };
                    keyword_0.clone()
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
        let name_11 = args[0].clone();
        {
            // (call-method BoxedVariable (quote new) name)
            imports::call_minus_method(&[
                BoxedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name_11.clone(),
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
        let name_9 = args[0].clone();
        {
            // (call-method UndefinedGlobal (quote new) name)
            imports::call_minus_method(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("new"),
                name_9.clone(),
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
        let name_8 = args[0].clone();
        {
            // (call-method ImportedVariable (quote new) name)
            imports::call_minus_method(&[
                ImportedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name_8.clone(),
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
        let name_7 = args[0].clone();
        let handler_1 = args[1].clone();
        {
            // (make-keyword name handler)
            make_minus_keyword(&[name_7.clone(), handler_1.clone()])
        }
    }
    .into()
}
pub fn new_minus_local(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_10 = args[0].clone();
        {
            // (call-method LocalVariable (quote new) name)
            imports::call_minus_method(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name_10.clone(),
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
        let var_12 = args[0].clone();
        let new_minus_var_0 = args[1].clone();
        {
            // (replace-table! var new-var)
            imports::replace_minus_table_i(&[var_12.clone(), new_minus_var_0.clone()])
        }
    }
    .into()
}
pub fn undefined_minus_global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_9 = args[0].clone();
        {
            // (ancestor? obj UndefinedGlobal)
            imports::ancestor_p(&[obj_9.clone(), UndefinedGlobal.with(|value| value.get())])
        }
    }
    .into()
}
pub fn variable_minus_mutable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var_6 = args[0].clone();
        {
            // (call-method var (quote mutable?))
            imports::call_minus_method(&[var_6.clone(), Scm::symbol("mutable?")])
        }
    }
    .into()
}
pub fn variable_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var_4 = args[0].clone();
        {
            // (let ((name (get-field var (quote name)))) (if (string? name) name (symbol->string name)))
            {
                let name_4 = {
                    // (get-field var (quote name))
                    imports::get_minus_field(&[var_4.clone(), Scm::symbol("name")])
                };
                if ({
                    // (string? name)
                    imports::string_p(&[name_4.clone()])
                })
                .is_true()
                {
                    name_4.clone()
                } else {
                    {
                        // (symbol->string name)
                        imports::symbol_minus__g_string(&[name_4.clone()])
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
        let var_7 = args[0].clone();
        {
            // (call-method var (quote set-mutable!))
            imports::call_minus_method(&[var_7.clone(), Scm::symbol("set-mutable!")])
        }
    }
    .into()
}
pub fn variable_minus_set_minus_name_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var_5 = args[0].clone();
        let name_5 = args[1].clone();
        {
            // (set-field! var (quote name) name)
            imports::set_minus_field_i(&[var_5.clone(), Scm::symbol("name"), name_5.clone()])
        }
    }
    .into()
}
pub fn variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_7 = args[0].clone();
        {
            // (ancestor? obj Variable)
            imports::ancestor_p(&[obj_7.clone(), Variable.with(|value| value.get())])
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
                        let self__7 = args[0].clone();
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
                        let self__8 = args[0].clone();
                        let value_2 = args[1].clone();
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
                        let self__9 = args[0].clone();
                        let value_3 = args[1].clone();
                        {
                            // (set-parent! self GlobalVariable)
                            imports::set_minus_parent_i(&[
                                self__9.clone(),
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
                        let self__10 = args[0].clone();
                        let name_0 = args[1].clone();
                        {
                            // (let ((var (clone UndefinedGlobal))) (set-field! var (quote name) name) var)
                            {
                                let var_0 = {
                                    // (clone UndefinedGlobal)
                                    imports::clone(&[UndefinedGlobal.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var_0.clone(),
                                            Scm::symbol("name"),
                                            name_0.clone(),
                                        ])
                                    };
                                    var_0.clone()
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
                        let self__11 = args[0].clone();
                        let value_4 = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (eq? (value (quote kind)) (quote ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value_4.clone().invoke(&[Scm::symbol("kind")])
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
                                            self__11.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__11.clone(),
                                            Scm::symbol("value"),
                                            value_4.clone(),
                                        ])
                                    }
                                }
                            } else if ({
                                // (eq? (value (quote kind)) (quote VARARG-ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value_4.clone().invoke(&[Scm::symbol("kind")])
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
                                            self__11.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__11.clone(),
                                            Scm::symbol("value"),
                                            value_4.clone(),
                                        ])
                                    }
                                }
                            } else {
                                {
                                    // (set-parent! self GlobalVariable)
                                    imports::set_minus_parent_i(&[
                                        self__11.clone(),
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
                        let self__12 = args[0].clone();
                        let name_1 = args[1].clone();
                        {
                            // (let ((var (clone ImportedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var_1 = {
                                    // (clone ImportedVariable)
                                    imports::clone(&[ImportedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var_1.clone(),
                                            Scm::symbol("name"),
                                            name_1.clone(),
                                        ])
                                    };
                                    var_1.clone()
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
                        let self__13 = args[0].clone();
                        {
                            // (set-parent! self BoxedVariable)
                            imports::set_minus_parent_i(&[
                                self__13.clone(),
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
                        let self__14 = args[0].clone();
                        let name_2 = args[1].clone();
                        {
                            // (let ((var (clone LocalVariable))) (set-field! var (quote name) name) var)
                            {
                                let var_2 = {
                                    // (clone LocalVariable)
                                    imports::clone(&[LocalVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var_2.clone(),
                                            Scm::symbol("name"),
                                            name_2.clone(),
                                        ])
                                    };
                                    var_2.clone()
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
                        let self__15 = args[0].clone();
                        let name_3 = args[1].clone();
                        {
                            // (let ((var (clone BoxedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var_3 = {
                                    // (clone BoxedVariable)
                                    imports::clone(&[BoxedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var_3.clone(),
                                            Scm::symbol("name"),
                                            name_3.clone(),
                                        ])
                                    };
                                    var_3.clone()
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
