//! given bindings, fully resolve an AST
//! This crate's job is to tee up the type checker for the next stage of compilation.

pub use resolved::QueryableResolvedItems;
use resolver::Resolver;
pub use resolver::{Expr, ExprKind, Function, FunctionCall, Intrinsic, ResolutionError, Type};
pub use swim_ast::{Intrinsic as IntrinsicName, Literal, Ty};
use swim_utils::SymbolInterner;

mod resolved;
mod resolver;

pub fn resolve_symbols(
    ast: swim_ast::Ast,
    interner: SymbolInterner,
) -> (Vec<ResolutionError>, QueryableResolvedItems) {
    let resolver = Resolver::new_from_single_ast(ast, interner);
    resolver.into_queryable()
}
