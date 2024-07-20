//! Binds symbols from the AST into a symbol map from ID to a node representation.
//! The binder's only job is to create a data structure of all scopes and the symbols that those
//! scopes define. The resolver is then able to do scope-aware name resolution in the next step.

pub use binder::{Bind, Binder, BindingId, FunctionId, Item, ModuleId, Scope, ScopeId, ScopeKind};
pub use petr_ast::dependency::Dependency;
mod binder;
mod impls;
