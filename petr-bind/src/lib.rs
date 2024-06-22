//! Binds symbols from the AST into a symbol map from ID to a node representation.

pub use binder::{Bind, Binder, FunctionId, Item, Scope, ScopeId, TypeId};
mod binder;
mod impls;
