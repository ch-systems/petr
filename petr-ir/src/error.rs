use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone)]
pub enum LoweringError {
    #[error("Internal compiler error: {0}")]
    Internal(String),
    #[error("Unable to infer type")]
    UnableToInferType,
}
