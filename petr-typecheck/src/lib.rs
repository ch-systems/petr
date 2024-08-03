//! TODO:
//! - Effectual Types
//! - Formalize constraints:
//!     - UnifyEffects
//!     - SatisfiesEffects

mod error;

pub use constraint_generation::{unify_basic_math_op, FunctionSignature, TypeCheck, TypeChecker};
pub use error::TypeConstraintError;
pub use petr_bind::FunctionId;
use petr_resolve::QueryableResolvedItems;
pub use petr_resolve::{Intrinsic as ResolvedIntrinsic, IntrinsicName, Literal};
use petr_utils::{idx_map_key, IndexMap, SpannedItem, TypeId};
pub use solution::TypeSolution;
pub use typed_ast::*;
pub use types::*;

mod constraint_generation;
mod pretty_printing;
mod solution;
#[cfg(test)]
mod tests;
mod typed_ast;
mod types;

pub type TypeError = SpannedItem<TypeConstraintError>;
pub type TResult<T> = Result<T, TypeError>;

pub fn type_check(resolved: QueryableResolvedItems) -> Result<TypeSolution, Vec<SpannedItem<TypeConstraintError>>> {
    let mut type_checker = TypeChecker::new(resolved);
    type_checker.fully_type_check();

    type_checker.into_solution()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOrFunctionId {
    TypeId(TypeId),
    FunctionId(FunctionId),
}

impl From<TypeId> for TypeOrFunctionId {
    fn from(type_id: TypeId) -> Self {
        TypeOrFunctionId::TypeId(type_id)
    }
}

impl From<FunctionId> for TypeOrFunctionId {
    fn from(function_id: FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(function_id)
    }
}

impl From<&TypeId> for TypeOrFunctionId {
    fn from(type_id: &TypeId) -> Self {
        TypeOrFunctionId::TypeId(*type_id)
    }
}

impl From<&FunctionId> for TypeOrFunctionId {
    fn from(function_id: &FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(*function_id)
    }
}

idx_map_key!(TypeVariable);
