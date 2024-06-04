//! given bindings, fully resolve an AST
//! This crate's job is to tee up the type checker for the next stage of compilation.

pub use resolved::QueryableResolvedItems;
pub use resolver::{Expr, ExprKind, Function, FunctionCall, Type};
pub use swim_ast::{Literal, Ty};

mod resolved {
    use std::{collections::BTreeMap, rc::Rc};

    use swim_ast::{Ast, AstNode, Commented, Expression, FunctionDeclaration, FunctionParameter};
    use swim_bind::{Binder, FunctionId, Item, ScopeId, TypeId};
    use swim_utils::{Identifier, SpannedItem, SymbolId};

    use crate::resolver::{Function, Resolver, TypeDeclaration};
    /// Contains things that have already been resolved.
    /// Resolved items cannot be queried during resolution. This is because the resolution
    /// stage should only query the binder, then the type checking stage can query
    /// the [QueryableResolved] to get the resolved items -- [QueryableResolvedItems] is the
    /// immutable result of resolution, and resolved items can no longer be mutated.
    pub(crate) struct ResolvedItems {
        resolved_functions: BTreeMap<FunctionId, Function>,
        resolved_types: BTreeMap<TypeId, TypeDeclaration>,
    }

    impl ResolvedItems {
        pub fn insert_function(&mut self, id: FunctionId, function: Function) {
            self.resolved_functions.insert(id, function);
        }

        pub fn insert_type(&mut self, id: TypeId, type_decl: TypeDeclaration) {
            self.resolved_types.insert(id, type_decl);
        }

        pub(crate) fn new() -> Self {
            Self {
                resolved_functions: Default::default(),
                resolved_types: Default::default(),
            }
        }

        pub fn into_queryable(self) -> QueryableResolvedItems {
            QueryableResolvedItems {
                resolved_functions: self.resolved_functions,
                resolved_types: self.resolved_types,
            }
        }
    }

    pub struct QueryableResolvedItems {
        resolved_functions: BTreeMap<FunctionId, Function>,
        resolved_types: BTreeMap<TypeId, TypeDeclaration>,
    }

    impl QueryableResolvedItems {
        pub fn new_from_single_ast(ast: Ast) -> Self {
            let mut resolver = Resolver::new_from_single_ast(ast);
            resolver.into_queryable()
        }

        pub fn get_function(&self, id: FunctionId) -> &Function {
            self.resolved_functions
                .get(&id)
                .expect("function IDs should always correspond to resolved functions")
        }

        pub fn get_type(&self, id: TypeId) -> &TypeDeclaration {
            self.resolved_types
                .get(&id)
                .expect("type IDs should always correspond to resolved types")
        }

        pub fn functions(&self) -> impl Iterator<Item = (&FunctionId, &Function)> {
            self.resolved_functions.iter()
        }

        pub fn types(&self) -> impl Iterator<Item = (&TypeId, &TypeDeclaration)> {
            self.resolved_types.iter()
        }
    }
}

mod resolver {
    use std::{collections::BTreeMap, rc::Rc};

    use swim_ast::{Ast, AstNode, Commented, Expression, FunctionDeclaration, FunctionParameter};
    use swim_bind::{Binder, FunctionId, Item, ScopeId, TypeId};
    use swim_utils::{Identifier, SpannedItem, SymbolId, SymbolInterner};

    use crate::resolved::{QueryableResolvedItems, ResolvedItems};
    pub struct ResolutionError;

    pub struct Resolver {
        resolved: ResolvedItems,
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
        pub name: Identifier,
    }

    // TODO: refactor this into polytype
    #[derive(Clone, Copy, Debug)]
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
        fn resolve(
            &self,
            resolver: &mut Resolver,
            binder: &Binder,
            scope_id: ScopeId,
        ) -> Option<Type> {
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

    #[derive(Clone)]
    pub struct FunctionCall {
        pub function: FunctionId,
        pub args: Vec<Expr>,
    }

    #[derive(Clone)]
    pub struct Function {
        pub name: Identifier,
        pub params: Vec<(Identifier, Type)>,
        pub return_type: Type,
        pub body: Expr,
    }

    #[derive(Clone)]
    pub struct Expr {
        pub kind: ExprKind,
    }

    impl Expr {
        pub fn error_recovery() -> Self {
            Self {
                kind: ExprKind::ErrorRecovery,
            }
        }

        pub fn new(kind: ExprKind) -> Self {
            Self { kind }
        }
    }

    #[derive(Clone)]
    pub enum ExprKind {
        Literal(swim_ast::Literal),
        List(Box<[Expr]>),
        FunctionCall(FunctionCall),
        Variable(Type),
        Unit,
        ErrorRecovery,
    }

    impl Resolver {
        // TODO: one for dependencies/packages which creates more scopes in the same binder
        pub fn new_from_single_ast(ast: Ast) -> Self {
            let binder = Binder::from_ast(&ast);
            let mut resolver = Self {
                errs: Vec::new(),
                resolved: ResolvedItems::new(),
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
                        Function(func, func_scope) => {
                            self.resolve_function(binder, *func, *func_scope)
                        }
                        Expr(_expr) => todo!(),
                        Type(ty) => self.resolve_type(binder, *ty, scope_id),
                        FunctionParameter(_ty) => {
                            // I don't think we have to do anything here but not sure
                        }
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
            self.resolved.insert_type(ty, resolved);
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
            self.resolved.insert_function(func_id, func);
        }

        pub fn into_queryable(self) -> QueryableResolvedItems {
            self.resolved.into_queryable()
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

            // functions exist in their own scope with their own variables
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
        fn resolve(
            &self,
            resolver: &mut Resolver,
            binder: &Binder,
            scope_id: ScopeId,
        ) -> Option<Expr> {
            Some(match self {
                Expression::Literal(x) => Expr::new(ExprKind::Literal(*x)),
                Expression::List(list) => {
                    let list: Vec<Expr> = list
                        .elements
                        .into_iter()
                        .map(|x| {
                            x.resolve(resolver, binder, scope_id)
                                .unwrap_or_else(Expr::error_recovery)
                        })
                        .collect();
                    // TODO: do list combination type if list of unit, which functions like a block
                    Expr::new(ExprKind::List(list.into_boxed_slice()))
                }
                Expression::Operator(_) => todo!("resolve into a function call to stdlib"),
                Expression::FunctionCall(decl) => {
                    let resolved_call = decl.resolve(resolver, binder, scope_id)?;

                    Expr::new(ExprKind::FunctionCall(resolved_call))
                }
                Expression::Variable(var) => {
                    let Some(Item::FunctionParameter(ty)) =
                        binder.find_symbol_in_scope(var.id, scope_id)
                    else {
                        todo!("push error and return")
                    };
                    let ty = ty
                        .resolve(resolver, binder, scope_id)
                        .unwrap_or(Type::ErrorRecovery);
                    Expr::new(ExprKind::Variable(ty))
                }
                // TODO
                Expression::TypeConstructor => Expr::error_recovery(),
            })
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

    impl Resolve for swim_ast::FunctionCall {
        type Resolved = FunctionCall;

        fn resolve(
            &self,
            resolver: &mut Resolver,
            binder: &Binder,
            scope_id: ScopeId,
        ) -> Option<Self::Resolved> {
            let func_name = self.func_name;
            let Some(Item::Function(resolved_id, _func_scope)) =
                binder.find_symbol_in_scope(func_name.id, scope_id)
            else {
                todo!("push error");
                panic!()
            };

            let args = self
                .args
                .iter()
                .map(|x| {
                    x.resolve(resolver, binder, scope_id)
                        .unwrap_or_else(Expr::error_recovery)
                })
                .collect();

            Some(FunctionCall {
                function: *resolved_id,
                args,
            })
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
        mod pretty_printing {
            use swim_utils::{PrettyPrint, SymbolInterner};

            use crate::{resolved::QueryableResolvedItems, resolver::Type};

            use super::{Expr, ExprKind};
            impl Expr {
                pub fn to_string(
                    &self,
                    resolver: &QueryableResolvedItems,
                    interner: &SymbolInterner,
                ) -> String {
                    self.kind.to_string(resolver, interner)
                }
            }

            impl Type {
                pub fn to_string(
                    &self,
                    resolver: &QueryableResolvedItems,
                    interner: &SymbolInterner,
                ) -> String {
                    match self {
                        Type::Integer => "int".to_string(),
                        Type::Bool => "bool".to_string(),
                        Type::Unit => "()".to_string(),
                        Type::ErrorRecovery => "<error>".to_string(),
                        Type::Named(id) => {
                            format!(
                                "named type {}",
                                interner.get(resolver.get_type(*id).name.id)
                            )
                        }
                    }
                }
            }

            impl ExprKind {
                pub fn to_string(
                    &self,
                    resolver: &QueryableResolvedItems,
                    interner: &SymbolInterner,
                ) -> String {
                    match self {
                        ExprKind::Literal(lit) => format!("Literal({:?})", lit),
                        ExprKind::List(exprs) => format!(
                            "[{}]",
                            exprs
                                .iter()
                                .map(|x| x.to_string(&resolver, &interner))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                        ExprKind::FunctionCall(call) => {
                            format!("FunctionCall({})", call.function)
                        }
                        ExprKind::Unit => format!("Unit"),
                        ExprKind::ErrorRecovery => format!("<error>"),
                        ExprKind::Variable(_) => todo!(),
                    }
                }
            }
        }
        use crate::{resolved::QueryableResolvedItems, resolver::Type};

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
            let queryable = resolver.resolved.into_queryable();
            let result = pretty_print_resolution(&queryable, &interner);
            expect.assert_eq(&result);
        }

        fn pretty_print_resolution(
            queryable: &QueryableResolvedItems,
            interner: &swim_utils::SymbolInterner,
        ) -> String {
            let mut result = String::new();
            result.push_str("_____FUNCTIONS_____\n");
            for (func_id, func) in queryable.functions() {
                result.push_str(&format!(
                    "#{} {}",
                    Into::<usize>::into(*func_id),
                    interner.get(func.name.id),
                ));
                result.push_str("(");
                for (name, ty) in &func.params {
                    let name = interner.get(name.id);
                    let ty = ty.to_string(&queryable, &interner);
                    result.push_str(&format!("  {}: {}, ", name, ty));
                }
                result.push_str(") ");
                let ty = func.return_type.to_string(&queryable, &interner);
                result.push_str(&format!("-> {} ", ty));
                result.push_str(&format!(
                    "  {:?}\n",
                    func.body.to_string(&queryable, &interner)
                ));
            }

            result.push_str("_____TYPES_____\n");
            for (type_id, ty_decl) in queryable.types() {
                result.push_str(&format!(
                    "#{} {}",
                    Into::<usize>::into(*type_id),
                    interner.get(ty_decl.name.id),
                ));
                result.push_str("\n\n");
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
                #0 foo(  a: int, ) -> int   Expr { kind: List([Expr { kind: Literal(Integer(1)), return_type: int }, Expr { kind: Literal(Integer(2)), return_type: int }, Expr { kind: Literal(Integer(3)), return_type: int }]), return_type: int }
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
                #0 a() -> named type typeid0   Expr { kind: Unit, return_type: <error> }
                #1 b() -> named type typeid0   Expr { kind: Unit, return_type: <error> }
                #2 foo(  a: named type typeid0, ) -> named type typeid0   Expr { kind: List([Expr { kind: Literal(Integer(1)), return_type: int }, Expr { kind: Literal(Integer(2)), return_type: int }, Expr { kind: Literal(Integer(3)), return_type: int }]), return_type: int }
                _____TYPES_____
                #0 MyType

            "#]],
            );
        }

        #[test]
        fn func_returns_named_type_declared_after_use() {
            check(
                r#"
            function foo(a in 'MyType) returns 'MyType [
                1,
                2,
                3
            ]
            type MyType = a | b
            "#,
                expect![[r#"
                _____FUNCTIONS_____
                #0 foo(  a: named type typeid0, ) -> named type typeid0   Expr { kind: List([Expr { kind: Literal(Integer(1)), return_type: int }, Expr { kind: Literal(Integer(2)), return_type: int }, Expr { kind: Literal(Integer(3)), return_type: int }]), return_type: int }
                #1 a() -> named type typeid0   Expr { kind: Unit, return_type: <error> }
                #2 b() -> named type typeid0   Expr { kind: Unit, return_type: <error> }
                _____TYPES_____
                #0 MyType

            "#]],
            )
        }

        #[test]
        fn call_func_before_decl() {
            check(
                r#"
            function foo() returns 'MyType ~bar(5)
            function bar(a in 'MyType) returns 'MyType [
                1,
                2,
                3
            ]
            type MyType = a | b
            "#,
                expect![[r#""#]],
            )
        }

        #[test]
        fn call_func_in_list_before_decl() {
            check(
                r#"
            function foo() returns 'MyType ~bar(5)
            function bar(a in 'MyType) returns 'MyType [
                1,
                2,
                3
            ]
            type MyType = a | b
            "#,
                expect![[r#""#]],
            )
        }
    }
}
