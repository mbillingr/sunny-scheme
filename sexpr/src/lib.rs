//! Generic algorithms used with S-Expressions
//!
//! This crate provides generic algorithms and helper functions for
//! use with S-Expressions without assuming any particular
//! implementation of the S-Expression data structures.
//!
//! In the last couple of years I have worked with many different
//! implementations of S-Expressions. I used enums, I used trait
//! objects, I used external crates, I used raw pointers, ...
//! And every time I implemented the same algorithms again and again.
//! This must stop. Now!
//!
//! ## Example
//! By defining how to represent pairs and null-values you
//! automatically get list accessors. And by further defining how to
//! construct such values you get functions such as list `reverse` or
//! `append`.
//!
//! ```
//! use sexpr_generics::prelude::*;
//! use sexpr_generics::lists;
//! use sexpr_generics::list;
//!
//! /// Your custom value type
//! #[derive(Debug, PartialEq, Clone)]
//! enum Value {
//!     Null,
//!     Int(i32),
//!     Pair(Box<Value>, Box<Value>),
//! }
//!
//! impl Value {
//!     fn new_pair(first: Value, second: Value) -> Self {
//!         Value::Pair(Box::new(first), Box::new(second))
//!     }
//! }
//!
//! impl Nullable for Value {
//!     fn is_null(&self) -> bool {
//!         matches![self, Value::Null]
//!     }
//! }
//!
//! impl MaybePair for Value {
//!     type First = Self;
//!     type Second = Self;
//!
//!     fn first(&self) -> Option<&Self::First> {
//!         match self {
//!             Value::Pair(x, _) => Some(x),
//!             _ => None,
//!         }
//!     }
//!
//!     fn second(&self) -> Option<&Self::Second> {
//!         match self {
//!             Value::Pair(_, x) => Some(x),
//!             _ => None,
//!         }
//!     }
//! }
//!
//! let short_list = Value::new_pair(Value::Int(10),
//!                                  Value::new_pair(Value::Int(20),
//!                                                  Value::Null));
//! assert_eq!(lists::length(&short_list), 2);
//! assert_eq!(lists::get(&short_list, 0), Some(&Value::Int(10)));
//! assert_eq!(lists::get(&short_list, 1), Some(&Value::Int(20)));
//! assert_eq!(lists::get(&short_list, 2), None);
//!
//! // By implementing these traits for StatelessFactory you get
//! // access to the `lists` methods. If your type needs stateful
//! // construction you'd have to pass your own factory to the
//! // methods in `lists::factory`.
//!
//! impl NullFactory<Value> for StatelessFactory {
//!     fn null(&mut self) -> Value {
//!         Value::Null
//!     }
//! }
//!
//! impl PairFactory<Value> for StatelessFactory {
//!     fn pair(&mut self, first: Value, second: Value) -> Value {
//!         Value::new_pair(first, second)
//!     }
//! }
//!
//! // Now you can use functions that construct new lists, such as
//! // `reverse`. As a bonus you even get a macro for creating lists!
//! assert_eq!(lists::reverse(&short_list),
//!            list![Value::Int(20), Value::Int(10)])
//! ```

pub mod core_traits;
pub mod cxr;
pub mod equality;
pub mod factory_traits;
pub mod lists;
pub mod numbers;
pub mod prelude;
pub mod static_matcher;
