//! given bindings, fully resolve an AST
//! This crate's job is to tee up the type checker for the next stage of compilation.

use std::rc::Rc;

use swim_ast::{
    Ast, AstNode, Commented, Expression, FunctionDeclaration, FunctionParameter, TypeDeclaration,
};
use swim_bind::{Binder, FunctionId, Item, ScopeId, TypeId};
use swim_utils::{Identifier, SpannedItem, SymbolId};

pub struct ResolutionError;

pub struct Resolver {
    binder: Binder,
    errs: Vec<ResolutionError>,
}

/*
struct Package {
    binder: Binder,
    ast: Ast,
}
*/

pub struct ResolvedNode {
    kind: ResolvedNodeKind,
}

impl ResolvedNode {
    pub fn new(kind: ResolvedNodeKind) -> Self {
        Self { kind }
    }
}

pub enum ResolvedNodeKind {
    Function(Function),
    Expr(Expr),
}

// TODO: refactor this into polytype
#[derive(Clone, Copy)]
pub enum Type {
    Integer,
    Bool,
    Unit,
    // like `Unit`, but doesn't throw additional type errors to prevent cascading errors.
    ErrorRecovery,
    Named(TypeId),
}

impl Resolve for swim_ast::Ty {
    type Resolved = Type;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<Type> {
        Some(match self {
            swim_ast::Ty::Int => Type::Integer,
            swim_ast::Ty::Bool => Type::Bool,
            swim_ast::Ty::Named(name) => {
                match resolver.binder.find_symbol_in_scope(name.id, scope_id) {
                    Some(Item::Type(id)) => Type::Named(*id),
                    Some(_) => {
                        todo!("push error -- symbol is not type");
                        return None;
                    }
                    None => {
                        todo!("push error -- symbol not found");
                        return None;
                    }
                }
            }
        })
    }
}

pub struct Function {
    pub name: Identifier,
    pub params: Vec<(Identifier, Type)>,
    pub return_type: Type,
    pub body: Expr,
}

pub struct Expr {
    kind: ExprKind,
    return_type: Type,
}

impl Expr {
    pub fn error_recovery() -> Self {
        Self {
            kind: ExprKind::Unit,
            return_type: Type::ErrorRecovery,
        }
    }

    pub fn new(kind: ExprKind, return_type: Type) -> Self {
        Self { kind, return_type }
    }
}

pub enum ExprKind {
    Literal(swim_ast::Literal),
    List(Box<[Expr]>),
    Unit,
}

impl Resolver {
    pub fn add_package(&mut self, package_name: Rc<str>, ast: Ast) -> Vec<ResolvedNode> {
        let binder = Binder::from_ast(&ast);
        let mut buf = Vec::new();
        // Iterate over the binder's scopes and resolve all symbols
        for (scope_id, scope) in binder.scope_iter() {
            for (name, item) in scope.iter() {
                use Item::*;
                match item {
                    Function(func) => self.resolve_function(*func, scope_id),
                    Expr(_expr) => todo!(),
                    Type(_) => todo!(),
                }
                .map(|item| buf.push(item));
            }
        }
        buf
        /*
        let package = Package { binder, ast };
        self.packages.insert(package_name, package);
        */
    }

    fn resolve_function(&mut self, func: FunctionId, scope_id: ScopeId) -> Option<ResolvedNode> {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body
        let func = self.binder.get_function(func).clone();
        func.resolve(self, scope_id)
    }
}

pub trait Resolve {
    type Resolved;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<Self::Resolved>;
}

impl Resolve for AstNode {
    type Resolved = ResolvedNode;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        match self {
            swim_ast::AstNode::FunctionDeclaration(decl) => decl.resolve(resolver, scope_id),
            swim_ast::AstNode::TypeDeclaration(decl) => decl.resolve(resolver, scope_id),
        }
    }
}

impl Resolve for FunctionDeclaration {
    type Resolved = ResolvedNode;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body

        let mut params_buf = Vec::with_capacity(self.parameters.len());

        for FunctionParameter { name, ty } in self.parameters.iter() {
            let ty = ty.resolve(resolver, scope_id).unwrap_or(Type::Unit);
            params_buf.push((*name, ty));
        }

        let return_type = self
            .return_type
            .resolve(resolver, scope_id)
            .unwrap_or(Type::Unit);

        let body = self
            .body
            .resolve(resolver, scope_id)
            .unwrap_or(Expr::error_recovery());

        Some(ResolvedNode::new(ResolvedNodeKind::Function(Function {
            name: self.name.clone(),
            params: params_buf,
            return_type,
            body,
        })))
    }
}

impl Resolve for Expression {
    type Resolved = Expr;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<Expr> {
        Some(match self {
            Expression::Literal(x) => Expr::new(ExprKind::Literal(*x), literal_return_type(x)),
            Expression::List(list) => {
                let list: Vec<Expr> = list
                    .elements
                    .into_iter()
                    .map(|x| {
                        x.resolve(resolver, scope_id)
                            .unwrap_or_else(Expr::error_recovery)
                    })
                    .collect();
                let ty = list
                    .get(0)
                    .as_ref()
                    .map(|x| x.return_type)
                    .unwrap_or(Type::ErrorRecovery);
                // TODO: do list combination type if list of unit, which functions like a block
                Expr::new(ExprKind::List(list.into_boxed_slice()), ty)
            }
            Expression::Operator(_) => todo!(),
            Expression::Variable(_) => todo!(),
            Expression::TypeConstructor => todo!(),
        })
    }
}

fn literal_return_type(x: &swim_ast::Literal) -> Type {
    match x {
        swim_ast::Literal::Integer(_) => Type::Integer,
    }
}

impl<T: Resolve> Resolve for Commented<T> {
    type Resolved = T::Resolved;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<Self::Resolved> {
        self.item().resolve(resolver, scope_id)
    }
}

impl<T: Resolve> Resolve for SpannedItem<T> {
    type Resolved = T::Resolved;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<Self::Resolved> {
        self.item().resolve(resolver, scope_id)
    }
}

impl Resolve for TypeDeclaration {
    type Resolved = ResolvedNode;
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        todo!()
    }
}

/*
pub fn resolve(&mut self) -> Vec<ResolvedNode> {
    // bind the AST
    self.ast.nodes
        .into_iter()
        .map(|node| match node {
            swim_ast::AstNode::FunctionDeclaration(decl) => decl.resolve(&mut self),
            swim_ast::AstNode::TypeDeclaration(decl) => decl.bind(&mut self.binder),
        })
        .collect()
        }*/

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};
    use swim_ast::Literal;
    use swim_utils::render_error;
    fn check(input: impl Into<String>, expect: Expect) {
        let input = input.into();
        let parser = swim_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter()
                .for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let binder = Binder::from_ast(&ast);
        let result = pretty_print_resolution(&binder, &interner);
        expect.assert_eq(&result);
    }

    fn pretty_print_resolution(binder: &Binder, interner: &swim_utils::SymbolInterner) -> String {
        let mut result = String::new();
        for (scope_id, scope) in binder.scope_iter() {
            result.push_str(&format!("Scope {:?}:\n", scope_id));
            for (name, item) in scope.iter() {
                let symbol = interner.get(*name);
                let item_str = match item {
                    Item::Function(func) => format!("Function {:?}", binder.get_function(func)),
                    Item::Expr(expr) => format!("Expression {:?}", expr),
                    Item::Type(ty) => format!("Type {:?}", ty),
                };
                result.push_str(&format!("  {}: {}\n", symbol, item_str));
            }
        }
        result
    }

    #[test]
    fn func_returns_list() {
        check(
            r#"
            function foo(a in 'A) returns 'int [1, 2, 3]
            "#,
            expect![[r#"
            "#]],
        );
    }
}
