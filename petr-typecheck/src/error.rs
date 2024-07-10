use miette::Diagnostic;
use thiserror::Error;

use crate::PetrType;

#[derive(Error, Debug, PartialEq, Clone)]
pub struct TypeCheckError {
    kind: TypeCheckErrorKind,
    help: Option<String>,
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Error, Debug, Diagnostic, PartialEq, Clone)]
pub enum UnificationError {
    #[error("Failed to unify types: {0:?}, {1:?}")]
    Failure(PetrType, PetrType),
}

#[derive(Error, Debug, Diagnostic, PartialEq, Clone)]
pub enum TypeCheckErrorKind {
    #[error(transparent)]
    UnificationFailure(#[from] UnificationError),
    // TODO: decl span as well as callsite span
    #[error("Function {function} takes {expected:?} arguments, but got {got:?} arguments.")]
    ArgumentCountMismatch { function: String, expected: usize, got: usize },
}

impl TypeCheckErrorKind {
    pub fn into_err(self) -> TypeCheckError {
        self.into()
    }
}

impl TypeCheckError {
    pub fn with_help(
        mut self,
        help: impl Into<String>,
    ) -> Self {
        self.help = Some(help.into());
        self
    }
}

impl From<TypeCheckErrorKind> for TypeCheckError {
    fn from(kind: TypeCheckErrorKind) -> Self {
        Self { kind, help: None }
    }
}

impl Diagnostic for TypeCheckError {
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
