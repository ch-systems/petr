//! given bindings, fully resolve an AST
//! This crate's job is to tee up the type checker for the next stage of compilation.

use petr_ast::Ast;
pub use petr_ast::{Intrinsic as IntrinsicName, Literal, Ty};
use petr_utils::{Identifier, SymbolInterner};
pub use resolved::QueryableResolvedItems;
use resolver::Resolver;
pub use resolver::{Expr, ExprKind, Function, FunctionCall, Intrinsic, ResolutionError, Type};

mod resolved;
mod resolver;

pub fn resolve_symbols(
    ast: petr_ast::Ast,
    interner: SymbolInterner,
    // TODO better type here
    dependencies: Vec<(
        /* Key */ String,
        /*Name from manifest*/ Identifier,
        /*Things this depends on*/ Vec<String>,
        Ast,
    )>,
) -> (Vec<ResolutionError>, QueryableResolvedItems) {
    let resolver = Resolver::new(ast, interner, dependencies);
    resolver.into_queryable()
}
