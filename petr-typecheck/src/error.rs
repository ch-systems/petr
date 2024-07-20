use miette::Diagnostic;
use thiserror::Error;

use crate::PetrType;

#[derive(Error, Debug, Diagnostic, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub enum TypeConstraintError {
    #[error("Failed to unify types: {0:?}, {1:?}")]
    UnificationFailure(PetrType, PetrType),
    #[error("Type {0:?} does not satisfy the constraints of type {1:?}")]
    FailedToSatisfy(PetrType, PetrType),
    #[error("Function {function} takes {expected:?} arguments, but got {got:?} arguments.")]
    ArgumentCountMismatch { function: String, expected: usize, got: usize },
    #[error("Type could not be inferrred")]
    UnknownInference,
    #[error("Internal compiler error: {0}")]
    Internal(String),
}
