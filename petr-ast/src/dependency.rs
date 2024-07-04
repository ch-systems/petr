use crate::Ast;

/// Describes the AST of a dependency package
pub struct Dependency {
    /// The unique key that identifies this source package, agnostic of the alias given to it in
    /// the manifest file
    pub key:          String,
    /// The name of the package as given in the manifest file
    pub name:         petr_utils::Identifier,
    /// The keys of any packages that this package depends on
    pub dependencies: Vec<String>,
    /// The AST of the source code of this package
    pub ast:          Ast,
}
