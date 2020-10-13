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
        let obj__188 = args[0].clone();
        {
            // (ancestor? obj BoxedVariable)
            imports::ancestor_p(&[obj__188.clone(), BoxedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_add_minus_definition_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__191 = args[0].clone();
        let val__192 = args[1].clone();
        {
            // (call-method var (quote add-definition!) val)
            imports::call_minus_method(&[
                var__191.clone(),
                Scm::symbol("add-definition!"),
                val__192.clone(),
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
        let var__193 = args[0].clone();
        {
            // (get-field var (quote value))
            imports::get_minus_field(&[var__193.clone(), Scm::symbol("value")])
        }
    }
    .into()
}
pub fn global_minus_function_minus_set_minus_value_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__194 = args[0].clone();
        let val__195 = args[1].clone();
        {
            // (set-field! var (quote value) val)
            imports::set_minus_field_i(&[var__194.clone(), Scm::symbol("value"), val__195.clone()])
        }
    }
    .into()
}
pub fn global_minus_function_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__185 = args[0].clone();
        {
            // (ancestor? obj GlobalFunction)
            imports::ancestor_p(&[obj__185.clone(), GlobalFunction.with(|value| value.get())])
        }
    }
    .into()
}
pub fn global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__183 = args[0].clone();
        {
            // (ancestor? obj GlobalVariable)
            imports::ancestor_p(&[obj__183.clone(), GlobalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn import_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__186 = args[0].clone();
        {
            // (ancestor? obj ImportedVariable)
            imports::ancestor_p(&[obj__186.clone(), ImportedVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn keyword_minus_handler(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw__181 = args[0].clone();
        {
            // (get-field kw (quote handler))
            imports::get_minus_field(&[kw__181.clone(), Scm::symbol("handler")])
        }
    }
    .into()
}
pub fn keyword_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let kw__180 = args[0].clone();
        {
            // (get-field kw (quote name))
            imports::get_minus_field(&[kw__180.clone(), Scm::symbol("name")])
        }
    }
    .into()
}
pub fn keyword_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__176 = args[0].clone();
        {
            // (and (table? obj) (ancestor? obj Keyword))
            if ({
                // (table? obj)
                imports::table_p(&[obj__176.clone()])
            })
            .is_true()
            {
                {
                    // (ancestor? obj Keyword)
                    imports::ancestor_p(&[obj__176.clone(), Keyword.with(|value| value.get())])
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
        let var__196 = args[0].clone();
        {
            // (call-method var (quote into-boxed!))
            imports::call_minus_method(&[var__196.clone(), Scm::symbol("into-boxed!")])
        }
    }
    .into()
}
pub fn local_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__187 = args[0].clone();
        {
            // (ancestor? obj LocalVariable)
            imports::ancestor_p(&[obj__187.clone(), LocalVariable.with(|value| value.get())])
        }
    }
    .into()
}
pub fn make_minus_keyword(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__178 = args[0].clone();
        let handler__179 = args[1].clone();
        {
            // (let ((keyword (clone Keyword))) (set-field! keyword (quote name) name) (set-field! keyword (quote handler) handler) keyword)
            {
                let keyword__177 = {
                    // (clone Keyword)
                    imports::clone(&[Keyword.with(|value| value.get())])
                };
                {
                    {
                        // (set-field! keyword (quote name) name)
                        imports::set_minus_field_i(&[
                            keyword__177.clone(),
                            Scm::symbol("name"),
                            name__178.clone(),
                        ])
                    };
                    {
                        // (set-field! keyword (quote handler) handler)
                        imports::set_minus_field_i(&[
                            keyword__177.clone(),
                            Scm::symbol("handler"),
                            handler__179.clone(),
                        ])
                    };
                    keyword__177.clone()
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
        let name__202 = args[0].clone();
        {
            // (call-method BoxedVariable (quote new) name)
            imports::call_minus_method(&[
                BoxedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__202.clone(),
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
        let name__200 = args[0].clone();
        {
            // (call-method UndefinedGlobal (quote new) name)
            imports::call_minus_method(&[
                UndefinedGlobal.with(|value| value.get()),
                Scm::symbol("new"),
                name__200.clone(),
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
        let name__199 = args[0].clone();
        {
            // (call-method ImportedVariable (quote new) name)
            imports::call_minus_method(&[
                ImportedVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__199.clone(),
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
        let name__197 = args[0].clone();
        let handler__198 = args[1].clone();
        {
            // (make-keyword name handler)
            make_minus_keyword(&[name__197.clone(), handler__198.clone()])
        }
    }
    .into()
}
pub fn new_minus_local(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__201 = args[0].clone();
        {
            // (call-method LocalVariable (quote new) name)
            imports::call_minus_method(&[
                LocalVariable.with(|value| value.get()),
                Scm::symbol("new"),
                name__201.clone(),
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
        let var__203 = args[0].clone();
        let new_minus_var__204 = args[1].clone();
        {
            // (replace-table! var new-var)
            imports::replace_minus_table_i(&[var__203.clone(), new_minus_var__204.clone()])
        }
    }
    .into()
}
pub fn undefined_minus_global_minus_variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__184 = args[0].clone();
        {
            // (ancestor? obj UndefinedGlobal)
            imports::ancestor_p(&[obj__184.clone(), UndefinedGlobal.with(|value| value.get())])
        }
    }
    .into()
}
pub fn variable_minus_mutable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__189 = args[0].clone();
        {
            // (call-method var (quote mutable?))
            imports::call_minus_method(&[var__189.clone(), Scm::symbol("mutable?")])
        }
    }
    .into()
}
pub fn variable_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let var__172 = args[0].clone();
        {
            // (let ((name (get-field var (quote name)))) (if (string? name) name (symbol->string name)))
            {
                let name__173 = {
                    // (get-field var (quote name))
                    imports::get_minus_field(&[var__172.clone(), Scm::symbol("name")])
                };
                if ({
                    // (string? name)
                    imports::string_p(&[name__173.clone()])
                })
                .is_true()
                {
                    name__173.clone()
                } else {
                    {
                        // (symbol->string name)
                        imports::symbol_minus__g_string(&[name__173.clone()])
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
        let var__190 = args[0].clone();
        {
            // (call-method var (quote set-mutable!))
            imports::call_minus_method(&[var__190.clone(), Scm::symbol("set-mutable!")])
        }
    }
    .into()
}
pub fn variable_minus_set_minus_name_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__174 = args[0].clone();
        let name__175 = args[1].clone();
        {
            // (set-field! var (quote name) name)
            imports::set_minus_field_i(&[var__174.clone(), Scm::symbol("name"), name__175.clone()])
        }
    }
    .into()
}
pub fn variable_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__182 = args[0].clone();
        {
            // (ancestor? obj Variable)
            imports::ancestor_p(&[obj__182.clone(), Variable.with(|value| value.get())])
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
                        let self__158 = args[0].clone();
                        {
                            // (get-field self (quote mut))
                            imports::get_minus_field(&[self__158.clone(), Scm::symbol("mut")])
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
                        let self__159 = args[0].clone();
                        {
                            // (set-field! self (quote mut) #t)
                            imports::set_minus_field_i(&[
                                self__159.clone(),
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
                        let self__160 = args[0].clone();
                        let value = args[1].clone();
                        {
                            // (set-parent! self GlobalVariable)
                            imports::set_minus_parent_i(&[
                                self__160.clone(),
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
                        let name__162 = args[1].clone();
                        {
                            // (let ((var (clone UndefinedGlobal))) (set-field! var (quote name) name) var)
                            {
                                let var__161 = {
                                    // (clone UndefinedGlobal)
                                    imports::clone(&[UndefinedGlobal.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__161.clone(),
                                            Scm::symbol("name"),
                                            name__162.clone(),
                                        ])
                                    };
                                    var__161.clone()
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
                        let self__164 = args[0].clone();
                        let value__163 = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (eq? (value (quote kind)) (quote ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value__163.clone().invoke(&[Scm::symbol("kind")])
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
                                            self__164.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__164.clone(),
                                            Scm::symbol("value"),
                                            value__163.clone(),
                                        ])
                                    }
                                }
                            } else if ({
                                // (eq? (value (quote kind)) (quote VARARG-ABSTRACTION))
                                imports::eq_p(&[
                                    {
                                        // (value (quote kind))
                                        value__163.clone().invoke(&[Scm::symbol("kind")])
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
                                            self__164.clone(),
                                            GlobalFunction.with(|value| value.get()),
                                        ])
                                    };
                                    {
                                        // (set-field! self (quote value) value)
                                        imports::set_minus_field_i(&[
                                            self__164.clone(),
                                            Scm::symbol("value"),
                                            value__163.clone(),
                                        ])
                                    }
                                }
                            } else {
                                {
                                    // (set-parent! self GlobalVariable)
                                    imports::set_minus_parent_i(&[
                                        self__164.clone(),
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
                        let name__166 = args[1].clone();
                        {
                            // (let ((var (clone ImportedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__165 = {
                                    // (clone ImportedVariable)
                                    imports::clone(&[ImportedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__165.clone(),
                                            Scm::symbol("name"),
                                            name__166.clone(),
                                        ])
                                    };
                                    var__165.clone()
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
                        let self__167 = args[0].clone();
                        {
                            // (set-parent! self BoxedVariable)
                            imports::set_minus_parent_i(&[
                                self__167.clone(),
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
                        let name__169 = args[1].clone();
                        {
                            // (let ((var (clone LocalVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__168 = {
                                    // (clone LocalVariable)
                                    imports::clone(&[LocalVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__168.clone(),
                                            Scm::symbol("name"),
                                            name__169.clone(),
                                        ])
                                    };
                                    var__168.clone()
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
                        let name__171 = args[1].clone();
                        {
                            // (let ((var (clone BoxedVariable))) (set-field! var (quote name) name) var)
                            {
                                let var__170 = {
                                    // (clone BoxedVariable)
                                    imports::clone(&[BoxedVariable.with(|value| value.get())])
                                };
                                {
                                    {
                                        // (set-field! var (quote name) name)
                                        imports::set_minus_field_i(&[
                                            var__170.clone(),
                                            Scm::symbol("name"),
                                            name__171.clone(),
                                        ])
                                    };
                                    var__170.clone()
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
