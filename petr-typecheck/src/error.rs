use miette::Diagnostic;
use thiserror::Error;

use crate::PetrType;

#[derive(Error, Debug, Diagnostic, PartialEq, Clone)]
pub enum TypeConstraintError {
    #[error("Failed to unify types: {0:?}, {1:?}")]
    UnificationFailure(PetrType, PetrType),
    #[error("Type {0:?} does not satisfy the constraints of type {1:?}")]
    FailedToSatisfy(PetrType, PetrType),
    #[error("Function {function} takes {expected:?} arguments, but got {got:?} arguments.")]
    ArgumentCountMismatch { function: String, expected: usize, got: usize },
}

/*
impl Diagnostic for TypeConstraintError {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.help.as_ref().map(|x| -> Box<dyn std::fmt::Display> { Box::new(x) })
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.kind.severity()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.kind.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.kind.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.kind.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.kind.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.kind.diagnostic_source()
    }
}
*/
