use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Debug, Diagnostic, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub enum TypeConstraintError {
    #[error("failed to unify types: `{0}` and `{1}`")]
    UnificationFailure(String, String),
    #[error("type `{0}` does not satisfy the constraints of type {1}")]
    FailedToSatisfy(String, String),
    #[error("type `{1}` is not a subtype of sum type `{0:?}`")]
    NotSubtype(Vec<String>, String),
    #[error("Function {function} takes {expected:?} arguments, but got {got:?} arguments.")]
    ArgumentCountMismatch { function: String, expected: usize, got: usize },
    #[error("type could not be inferred")]
    UnknownInference,
    #[error("internal compiler error: {0}")]
    Internal(String),
    // TODO better errors here
    #[error("This type references itself in a circular way")]
    CircularType,
}
