//! given bindings, fully resolve an AST
//! This crate's job is to tee up the type checker for the next stage of compilation.

use std::{collections::BTreeMap, rc::Rc};

use swim_ast::{Ast, AstNode, Commented, Expression, FunctionDeclaration, FunctionParameter};
use swim_bind::{Binder, FunctionId, Item, ScopeId, TypeId};
use swim_utils::{Identifier, SpannedItem, SymbolId};

pub struct ResolutionError;

pub struct Resolver {
    resolved_functions: BTreeMap<FunctionId, Function>,
    resolved_types: BTreeMap<TypeId, TypeDeclaration>,
    errs: Vec<ResolutionError>,
}

/*
struct Package {
    binder: Binder,
    ast: Ast,
}
*/

#[derive(Debug)]
pub struct TypeDeclaration {
    name: Identifier,
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

impl Type {
    fn to_string(&self) -> String {
        match self {
            Type::Integer => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "()".to_string(),
            Type::ErrorRecovery => "<error>".to_string(),
            Type::Named(id) => format!("named type {}", id),
        }
    }
}

impl Resolve for swim_ast::Ty {
    type Resolved = Type;
    fn resolve(&self, resolver: &mut Resolver, binder: &Binder, scope_id: ScopeId) -> Option<Type> {
        Some(match self {
            swim_ast::Ty::Int => Type::Integer,
            swim_ast::Ty::Bool => Type::Bool,
            swim_ast::Ty::Named(name) => match binder.find_symbol_in_scope(name.id, scope_id) {
                Some(Item::Type(id)) => Type::Named(*id),
                Some(_) => {
                    todo!("push error -- symbol is not type");
                    return None;
                }
                None => {
                    todo!("push error -- {} symbol not found", name.id);
                    return None;
                }
            },
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

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Expr {{ kind: {:?}, return_type: {} }}",
            self.kind,
            self.return_type.to_string()
        )
    }
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

impl std::fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Literal(lit) => write!(f, "Literal({:?})", lit),
            ExprKind::List(exprs) => write!(f, "List({:?})", exprs),
            ExprKind::Unit => write!(f, "Unit"),
        }
    }
}

impl Resolver {
    // TODO: one for dependencies/packages which creates more scopes in the same binder
    pub fn new_from_single_ast(ast: Ast) -> Self {
        let binder = Binder::from_ast(&ast);
        let mut resolver = Self {
            errs: Vec::new(),
            resolved_functions: BTreeMap::new(),
            resolved_types: BTreeMap::new(),
        };
        resolver.add_package(ast, &binder);
        resolver
    }
    pub fn add_package(&mut self, ast: Ast, binder: &Binder) {
        // Iterate over the binder's scopes and resolve all symbols
        let scopes_and_ids = binder
            .scope_iter()
            .map(|(x, y)| (x, y.clone()))
            .collect::<Vec<_>>();
        for (scope_id, scope) in scopes_and_ids {
            for (name, item) in scope.iter() {
                use Item::*;
                match item {
                    Function(func) => self.resolve_function(binder, *func, scope_id),
                    Expr(_expr) => todo!(),
                    Type(ty) => self.resolve_type(binder, *ty, scope_id),
                }
            }
        }
        /*
        let package = Package { binder, ast };
        self.packages.insert(package_name, package);
        */
    }

    fn resolve_type(&mut self, binder: &Binder, ty: TypeId, scope_id: ScopeId) {
        let ty_decl = binder.get_type(ty).clone();
        let Some(resolved) = ty_decl.resolve(self, binder, scope_id) else {
            return;
        };
        self.resolved_types.insert(ty, resolved);
    }

    fn resolve_function(&mut self, binder: &Binder, func_id: FunctionId, scope_id: ScopeId) {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body
        let func = binder.get_function(func_id).clone();
        let Some(func) = func.resolve(self, binder, scope_id) else {
            return;
        };
        self.resolved_functions.insert(func_id, func);
    }
}

pub trait Resolve {
    type Resolved;
    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved>;
}

impl Resolve for FunctionDeclaration {
    type Resolved = Function;
    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body

        let mut params_buf = Vec::with_capacity(self.parameters.len());

        for FunctionParameter { name, ty } in self.parameters.iter() {
            let ty = ty.resolve(resolver, binder, scope_id).unwrap_or(Type::Unit);
            params_buf.push((*name, ty));
        }

        let return_type = self
            .return_type
            .resolve(resolver, binder, scope_id)
            .unwrap_or(Type::Unit);

        let body = self
            .body
            .resolve(resolver, binder, scope_id)
            .unwrap_or(Expr::error_recovery());

        Some(Function {
            name: self.name.clone(),
            params: params_buf,
            return_type,
            body,
        })
    }
}

impl Resolve for Expression {
    type Resolved = Expr;
    fn resolve(&self, resolver: &mut Resolver, binder: &Binder, scope_id: ScopeId) -> Option<Expr> {
        Some(match self {
            Expression::Literal(x) => Expr::new(ExprKind::Literal(*x), literal_return_type(x)),
            Expression::List(list) => {
                let list: Vec<Expr> = list
                    .elements
                    .into_iter()
                    .map(|x| {
                        x.resolve(resolver, binder, scope_id)
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
            // TODO
            Expression::TypeConstructor => Expr::error_recovery(),
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
    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        self.item().resolve(resolver, binder, scope_id)
    }
}

impl<T: Resolve> Resolve for SpannedItem<T> {
    type Resolved = T::Resolved;
    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        self.item().resolve(resolver, binder, scope_id)
    }
}

impl Resolve for swim_ast::TypeDeclaration {
    type Resolved = TypeDeclaration;
    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        // when resolving a type declaration,
        // we just need to resolve all the inner types from the fields
        let mut field_types = Vec::new();

        // for variant in variants
        // for field in variant's fields
        // resolve the field type
        for variant in self.variants.iter() {
            for field in variant.item().fields.iter() {
                if let Some(field_type) = field.resolve(resolver, binder, scope_id) {
                    field_types.push(field_type);
                } else {
                    // Handle the error case where the field type could not be resolved
                    return None;
                }
            }
        }

        Some(TypeDeclaration { name: self.name })
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
        let resolver = Resolver::new_from_single_ast(ast);
        let result = pretty_print_resolution(&resolver, &interner);
        expect.assert_eq(&result);
    }

    fn pretty_print_resolution(
        resolver: &Resolver,
        interner: &swim_utils::SymbolInterner,
    ) -> String {
        let mut result = String::new();
        result.push_str("_____FUNCTIONS_____\n");
        for (func_id, func) in &resolver.resolved_functions {
            result.push_str(&format!(
                "Function: {} (id {:?})\n",
                interner.get(func.name.id),
                func_id
            ));
            result.push_str("Params:\n");
            for (name, ty) in &func.params {
                let name = interner.get(name.id);
                let ty = ty.to_string();
                result.push_str(&format!("  {}: {}\n", name, ty));
            }
            let ty = func.return_type.to_string();
            result.push_str(&format!("Return Type: {}\n", ty));
            result.push_str("Body:\n");
            result.push_str(&format!("  {:?}\n\n", func.body));
        }

        result.push_str("_____TYPES_____\n");
        for (type_id, ty_decl) in &resolver.resolved_types {
            result.push_str(&format!(
                "Type {} (id {:?}): {:?}\n\n",
                interner.get(ty_decl.name.id),
                ty_decl,
                type_id
            ));
        }

        result
    }

    #[test]
    fn func_returns_list() {
        check(
            r#"
            function foo(a in 'int) returns 'int [1, 2, 3]
            "#,
            expect![[r#"
                _____FUNCTIONS_____
                Function: foo (id FunctionId(0))
                Params:
                  a: int
                Return Type: int
                Body:
                  Expr { kind: List([Expr { kind: Literal(Integer(1)), return_type: int }, Expr { kind: Literal(Integer(2)), return_type: int }, Expr { kind: Literal(Integer(3)), return_type: int }]), return_type: int }

                _____TYPES_____
            "#]],
        );
    }
    #[test]
    fn func_returns_named_type() {
        check(
            r#"
            type MyType = a | b
            function foo(a in 'MyType) returns 'MyType [1, 2, 3]
            "#,
            expect![[r#"
                _____FUNCTIONS_____
                Function: a (id FunctionId(0))
                Params:
                Return Type: named type typeid0
                Body:
                  Expr { kind: Unit, return_type: <error> }

                Function: b (id FunctionId(1))
                Params:
                Return Type: named type typeid0
                Body:
                  Expr { kind: Unit, return_type: <error> }

                Function: foo (id FunctionId(2))
                Params:
                  a: named type typeid0
                Return Type: named type typeid0
                Body:
                  Expr { kind: List([Expr { kind: Literal(Integer(1)), return_type: int }, Expr { kind: Literal(Integer(2)), return_type: int }, Expr { kind: Literal(Integer(3)), return_type: int }]), return_type: int }

                _____TYPES_____
                Type MyType (id TypeDeclaration { name: Identifier { id: SymbolId(0) } }): TypeId(0)

            "#]],
        );
    }
}
