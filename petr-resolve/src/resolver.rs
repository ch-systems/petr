// TODO:
// use a push_error API which sets more diagnostic fields, similar to how the parser does it

use std::rc::Rc;

use petr_ast::{Ast, Commented, Expression, FunctionDeclaration, FunctionParameter, OperatorExpression};
use petr_bind::{Binder, Dependency, FunctionId, Item, ScopeId};
use petr_utils::{Identifier, Path, Span, SpannedItem, SymbolInterner, TypeId};
use thiserror::Error;

use crate::resolved::{QueryableResolvedItems, ResolvedItems};
#[derive(Debug, Error)]
pub enum ResolutionError {
    #[error("Function parameter not found: {0}")]
    FunctionParameterNotFound(String),
    #[error("Symbol not found: {0}")]
    NotFound(String),
    #[error("Could not find implementation for operator: {0} at {1}")]
    OperatorImplementationNotFound(String, String),
}

pub(crate) struct Resolver {
    pub resolved: ResolvedItems,
    pub interner: SymbolInterner,
    pub errs:     Vec<ResolutionError>,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub name:     Identifier,
    pub variants: Box<[TypeVariant]>,
}

#[derive(Debug, Clone)]
pub struct TypeVariant {
    pub name:   Identifier,
    pub fields: Box<[TypeField]>,
}

#[derive(Debug, Clone)]
pub struct TypeField {
    pub name: Identifier,
    pub ty:   Type,
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Integer,
    Bool,
    Unit,
    String,
    // like `Unit`, but doesn't throw additional type errors to prevent cascading errors.
    ErrorRecovery,
    Named(TypeId),
    Generic(Identifier),
}

impl Resolve for petr_ast::Ty {
    type Resolved = Type;

    fn resolve(
        &self,
        _resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Type> {
        Some(match self {
            petr_ast::Ty::Int => Type::Integer,
            petr_ast::Ty::Bool => Type::Bool,
            petr_ast::Ty::String => Type::String,
            petr_ast::Ty::Unit => Type::Unit,
            petr_ast::Ty::Named(name) => match binder.find_symbol_in_scope(name.id, scope_id) {
                Some(Item::Type(id)) => Type::Named(*id),
                Some(something) => {
                    let name = _resolver.interner.get(name.id);
                    // this makes sense, the type constructor is getting resolved instead of the ty
                    // find_symbol_in_scope could take in what it is looking for as a parameter,
                    // _or_ we could have a special case when a function body is just a type
                    // constructorjkkj
                    todo!("push error -- symbol {name} is not type, it is a {something:?}");
                    // return None;
                },
                None => Type::Generic(*name),
            },
        })
    }
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub function: FunctionId,
    pub args:     Vec<Expr>,
    pub span:     Span,
}

impl FunctionCall {
    pub fn span(&self) -> petr_utils::Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name:        Identifier,
    pub params:      Vec<(Identifier, Type)>,
    pub return_type: Type,
    pub body:        Expr,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn error_recovery(span: Span) -> Self {
        Self {
            kind: ExprKind::ErrorRecovery,
            span,
        }
    }

    pub fn new(
        kind: ExprKind,
        span: Span,
    ) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Literal(petr_ast::Literal),
    List(Box<[Expr]>),
    FunctionCall(FunctionCall),
    Variable { name: Identifier, ty: Type },
    Intrinsic(Intrinsic),
    Unit,
    // the `id` is the id of the type declaration that defined this constructor
    TypeConstructor(TypeId, Box<[Expr]>),
    ErrorRecovery,
    ExpressionWithBindings { bindings: Vec<Binding>, expression: Box<Expr> },
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub name:       Identifier,
    pub expression: Expr,
}

#[derive(Clone, Debug)]
pub struct Intrinsic {
    pub intrinsic: petr_ast::Intrinsic,
    pub args:      Box<[Expr]>,
}

impl Resolver {
    #[cfg(test)]
    pub fn new_from_single_ast(
        ast: Ast,
        interner: SymbolInterner,
    ) -> Self {
        let binder = Binder::from_ast(&ast);
        let mut resolver = Self {
            errs: Vec::new(),
            resolved: ResolvedItems::new(),
            interner,
        };
        resolver.add_package(&binder);
        resolver
    }

    pub fn new(
        ast: Ast,
        mut interner: SymbolInterner,
        dependencies: Vec<Dependency>,
    ) -> Self {
        let binder = Binder::from_ast_and_deps(&ast, dependencies, &mut interner);
        let mut resolver = Self {
            errs: Vec::new(),
            resolved: ResolvedItems::new(),
            interner,
        };
        resolver.add_package(&binder);
        resolver
    }

    pub fn add_package(
        &mut self,
        binder: &Binder,
    ) {
        // Iterate over the binder's scopes and resolve all symbols
        let scopes_and_ids = binder.scope_iter().collect::<Vec<_>>();
        for (scope_id, scope) in scopes_and_ids {
            for (_name, item) in scope.iter() {
                self.resolve_item(item.item(), binder, scope_id)
            }
        }
    }

    fn resolve_item(
        &mut self,
        item: &Item,
        binder: &Binder,
        scope_id: ScopeId,
    ) {
        use Item::*;
        match item {
            Function(func, func_scope) => self.resolve_function(binder, *func, *func_scope),
            Type(ty) => self.resolve_type(binder, *ty, scope_id),
            FunctionParameter(_ty) => {
                // I don't think we have to do anything here but not sure
            },
            Binding(a) => {
                let binding = binder.get_binding(*a);
                let resolved_expr = match binding.val.resolve(self, binder, scope_id) {
                    Some(o) => o,
                    None => {
                        // TODO i think this is incorrect
                        let name = self.interner.get(binding.name.id);
                        self.errs.push(ResolutionError::NotFound(name.to_string()));
                        return;
                    },
                };
                self.resolved.bindings.insert(
                    *a,
                    crate::resolver::Binding {
                        name:       binding.name,
                        expression: resolved_expr,
                    },
                );
            },
            // TODO not sure if we can skip this, or if we should resolve it during imports
            Module(id) => {
                let module = binder.get_module(*id);
                let scope_id = module.root_scope;
                let scope = binder.iter_scope(scope_id);
                for (_name, item) in scope {
                    self.resolve_item(item.item(), binder, scope_id);
                }
            },
            Import { .. } => { // do nothing?
                 // find the module that the import refers to
                 // the first ident is either a top-level module or one that is in this scope
                 // let mut path_iter = path.iter();
                 // let Some(first_item) = binder.find_symbol_in_scope(
                 //     path_iter.next().expect("import with no items was parsed -- should be an invariant").id,
                 //     scope_id,
                 // ) else {
                 //     todo!("push import item not found error")
                 // };

                // let first_item = match first_item {
                //     Item::Module(id) => id,
                //     _ => todo!("push error -- import path is not a module"),
                // };

                // let mut rover = binder.get_module(*first_item);
                // // iterate over the rest of the path to find the path item
                // for (ix, item) in path_iter.enumerate() {
                //     let is_last = ix == path.len() - 1;
                //     let Some(next_symbol) = binder.find_symbol_in_scope(item.id, rover.root_scope) else { todo!("push item not found err") };

                //     match next_item {
                //         Item::Module(id) => rover = binder.get_module(id),
                //         otherwise if is_last => {
                //             let alias = alias.unwrap_or_else(|| item.name);

                //         },
                //         _ => todo!("push error -- import path item is not a module"),
                //     }
                // }

                // todo!()
            },
        }
    }

    fn resolve_type(
        &mut self,
        binder: &Binder,
        ty: TypeId,
        scope_id: ScopeId,
    ) {
        let ty_decl = binder.get_type(ty).clone();
        let Some(resolved) = ty_decl.resolve(self, binder, scope_id) else {
            return;
        };
        self.resolved.insert_type(ty, resolved);
    }

    fn resolve_function(
        &mut self,
        binder: &Binder,
        func_id: FunctionId,
        scope_id: ScopeId,
    ) {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body
        let func = binder.get_function(func_id).clone();
        println!("resolving function: {}", func.item().name.id);
        let Some(func) = func.resolve(self, binder, scope_id) else {
            return;
        };
        self.resolved.insert_function(func_id, func);
    }

    pub fn into_queryable(self) -> (Vec<ResolutionError>, QueryableResolvedItems) {
        (
            self.errs,
            QueryableResolvedItems::new(self.resolved.resolved_functions, self.resolved.resolved_types, self.interner),
        )
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

impl Resolve for SpannedItem<FunctionDeclaration> {
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

        let mut params_buf = Vec::with_capacity(self.item().parameters.len());

        // functions exist in their own scope with their own variables
        for FunctionParameter { name, ty } in self.item().parameters.iter() {
            let ty = ty.resolve(resolver, binder, scope_id).unwrap_or(Type::Unit);
            params_buf.push((*name, ty));
        }

        let return_type = self.item().return_type.resolve(resolver, binder, scope_id).unwrap_or(Type::Unit);

        let body = match self.item().body.resolve(resolver, binder, scope_id) {
            Some(x) => x,
            // need to use an error recovery func here, so the FunctionId still exists in the
            // resolved function map.
            // If we were to return `None` and not resolve the function, then calls to this
            // function would hold a reference `FunctionId(x)` which would not exist anymore.
            None => Expr::error_recovery(self.span()),
        };

        Some(Function {
            name: self.item().name,
            params: params_buf,
            return_type,
            body,
        })
    }
}

impl Resolve for SpannedItem<Expression> {
    type Resolved = Expr;

    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Expr> {
        Some(match self.item() {
            Expression::Literal(x) => Expr::new(ExprKind::Literal(x.clone()), self.span()),
            Expression::List(list) => {
                let list: Vec<Expr> = list
                    .elements
                    .iter()
                    .map(|x| match x.resolve(resolver, binder, scope_id) {
                        Some(x) => x,
                        None => todo!("error recovery"),
                    })
                    .collect();
                // TODO: do list combination type if list of unit, which functions like a block
                Expr::new(ExprKind::List(list.into_boxed_slice()), self.span())
            },
            Expression::Operator(op) => {
                let OperatorExpression { lhs, rhs, op } = *op.clone();
                // resolves to a call to stdlib
                use petr_ast::Operator::*;
                let func = match op.item() {
                    Plus => "add",
                    Minus => "sub",
                    Star => "mul",
                    Slash => "div",
                };
                let path = ["std", "ops", func];

                let func_path = Path {
                    identifiers: path
                        .iter()
                        .map(|x| Identifier {
                            id:   resolver.interner.insert(Rc::from(*x)),
                            span: self.span(),
                        })
                        .collect(),
                };

                let Some(either::Left(function)) = func_path.resolve(resolver, binder, scope_id) else {
                    resolver
                        .errs
                        .push(ResolutionError::OperatorImplementationNotFound(func.to_string(), path.join(".")));
                    return None;
                };

                let call = FunctionCall {
                    function,
                    args: vec![lhs.resolve(resolver, binder, scope_id)?, rhs.resolve(resolver, binder, scope_id)?],
                    span: self.span(),
                };

                Expr::new(ExprKind::FunctionCall(call), self.span())
            },
            Expression::FunctionCall(decl) => {
                let resolved_call = self.span().with_item(decl).resolve(resolver, binder, scope_id)?;

                Expr::new(ExprKind::FunctionCall(resolved_call), self.span())
            },
            Expression::Variable(var) => {
                let item = match binder.find_symbol_in_scope(var.id, scope_id) {
                    Some(item @ Item::FunctionParameter(_) | item @ Item::Binding(_)) => item,
                    a => todo!("variable references non-variable item: {a:?}"),
                    /*
                    None => {
                        let var_name = resolver.interner.get(var.id);
                        todo!();
                        // resolver.errs.push(ResolutionError::NotFound(var_name.to_string()));
                        return None;
                    },
                    */
                };
                match item {
                    Item::Binding(binding_id) => {
                        let binding = binder.get_binding(*binding_id);
                        //                        let expr = binding.resolve(resolver, binder, scope_id).expect("TODO errs");
                        //                        resolver.resolved.bindings.insert(*binding_id, expr.clone());

                        Expr::new(
                            ExprKind::Variable {
                                name: *var,
                                // I Think this works for inference -- instantiating a new generic
                                // type. Should revisit for soundness.
                                ty:   Type::Generic(binding.name),
                            },
                            self.span(),
                        )
                    },
                    Item::FunctionParameter(ty) => {
                        let ty = match ty.resolve(resolver, binder, scope_id) {
                            Some(ty) => ty,

                            None => {
                                todo!("not found err");
                                // Type::ErrorRecovery
                            },
                        };

                        Expr::new(ExprKind::Variable { name: *var, ty }, self.span())
                    },
                    _ => unreachable!(),
                }
            },
            Expression::TypeConstructor(parent_type_id, args) => {
                // Type constructor expressions themselves don't actually do anything.
                // The function parameters and return types
                // of the function are what get type checked -- there is no fn body, and this
                // TypeConstructor expression is what represents that.
                let resolved_args = args
                    .iter()
                    .map(|x| match x.resolve(resolver, binder, scope_id) {
                        Some(x) => x,
                        None => todo!("error recov"),
                    })
                    .collect::<Vec<_>>();
                Expr::new(ExprKind::TypeConstructor(*parent_type_id, resolved_args.into_boxed_slice()), self.span())
            },
            Expression::IntrinsicCall(intrinsic) => {
                let resolved = intrinsic.resolve(resolver, binder, scope_id)?;
                Expr::new(ExprKind::Intrinsic(resolved), self.span())
            },
            Expression::Binding(bound_expression) => {
                let scope_id = binder.get_expr_scope(bound_expression.expr_id).expect("invariant: scope should exist");
                let mut bindings: Vec<Binding> = Vec::with_capacity(bound_expression.bindings.len());
                for binding in &bound_expression.bindings {
                    let rhs = binding.val.resolve(resolver, binder, scope_id)?;
                    bindings.push(Binding {
                        name:       binding.name,
                        expression: rhs,
                    });
                }
                let expression = bound_expression.expression.resolve(resolver, binder, scope_id)?;
                Expr::new(
                    ExprKind::ExpressionWithBindings {
                        expression: Box::new(expression),
                        bindings,
                    },
                    self.span(),
                )
            },
        })
    }
}

impl Resolve for petr_ast::IntrinsicCall {
    type Resolved = Intrinsic;

    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        let args = self
            .args
            .iter()
            .map(|x| match x.resolve(resolver, binder, scope_id) {
                Some(x) => x,
                None => todo!("error recov"),
            })
            .collect();
        Some(Intrinsic {
            intrinsic: self.intrinsic.clone(),
            args,
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

impl Resolve for SpannedItem<&petr_ast::FunctionCall> {
    type Resolved = FunctionCall;

    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        let resolved_id = match self.item().func_name.resolve(resolver, binder, scope_id) {
            Some(either::Either::Left(func)) => func,
            Some(either::Either::Right(_ty)) => {
                todo!("push error -- tried to call ty as func");
            },
            _ => todo!("not found error"),
        };

        let args = self
            .item()
            .args
            .iter()
            .map(|x| match x.resolve(resolver, binder, scope_id) {
                Some(x) => x,
                None => todo!("Error recov"),
            })
            .collect();

        Some(FunctionCall {
            function: resolved_id,
            args,
            span: self.span(),
        })
    }
}

impl Resolve for petr_utils::Path {
    // TODO globs
    type Resolved = either::Either<FunctionId, TypeId>;

    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        let mut path_iter = self.identifiers.iter();
        let Some(first_item) = ({
            let item = path_iter.next().expect("import with no items was parsed -- should be an invariant");
            binder.find_symbol_in_scope(item.id, scope_id)
        }) else {
            let name = self.identifiers.iter().map(|x| resolver.interner.get(x.id)).collect::<Vec<_>>().join(".");
            resolver.errs.push(ResolutionError::NotFound(name));
            return None;
        };

        let first_item = match first_item {
            Item::Module(id) if self.identifiers.len() > 1 => id,
            Item::Function(f, _) if self.identifiers.len() == 1 => return Some(either::Either::Left(*f)),
            Item::Type(t) if self.identifiers.len() == 1 => return Some(either::Either::Right(*t)),
            Item::Import { path, alias: _ } if self.identifiers.len() == 1 => return path.resolve(resolver, binder, scope_id),
            a => todo!("push error -- import path is not a module {a:?}"),
        };

        let mut rover = binder.get_module(*first_item);
        // iterate over the rest of the path to find the path item
        for (ix, item) in path_iter.enumerate() {
            let is_last = ix == self.identifiers.len() - 2; // -2 because we advanced the iter by one already
            let Some(next_symbol) = binder.find_symbol_in_scope(item.id, rover.root_scope) else {
                let name = resolver.interner.get(item.id);
                resolver.errs.push(ResolutionError::NotFound(name.to_string()));
                return None;
            };

            match next_symbol {
                Item::Module(id) => rover = binder.get_module(*id),
                Item::Function(func, _scope) if is_last => return Some(either::Either::Left(*func)),
                Item::Type(ty) if is_last => return Some(either::Either::Right(*ty)),
                Item::Import { path, alias: _ } => match path.resolve(resolver, binder, scope_id) {
                    Some(either::Left(func)) => return Some(either::Left(func)),
                    Some(either::Right(ty)) => return Some(either::Right(ty)),
                    None => todo!("push error -- import not found"),
                },
                a => todo!("push error -- import path item is not a module, it is a {a:?}"),
            }
        }

        todo!("import of module not supported yet, must be type or function")
    }
}

impl Resolve for petr_ast::TypeDeclaration {
    type Resolved = TypeDeclaration;

    fn resolve(
        &self,
        resolver: &mut Resolver,
        binder: &Binder,
        scope_id: ScopeId,
    ) -> Option<Self::Resolved> {
        // when resolving a type declaration,
        // we just need to resolve all the inner types from the fields

        // for variant in variants
        // for field in variant's fields
        // resolve the field type
        let mut variants = Vec::with_capacity(self.variants.len());
        for variant in self.variants.iter() {
            let mut field_types = Vec::with_capacity(variant.item().fields.len());
            for field in variant.item().fields.iter() {
                if let Some(field_type) = field.item().ty.resolve(resolver, binder, scope_id) {
                    field_types.push(TypeField {
                        name: field.item().name,
                        ty:   field_type,
                    });
                } else {
                    // TODO Handle the error case where the field type could not be resolved
                    return None;
                }
            }
            variants.push(TypeVariant {
                name:   variant.item().name,
                fields: field_types.into_boxed_slice(),
            });
        }

        Some(TypeDeclaration {
            name:     self.name,
            variants: variants.into_boxed_slice(),
        })
    }
}

#[cfg(test)]
mod tests {
    mod pretty_printing {

        use super::{Expr, ExprKind};
        use crate::{resolved::QueryableResolvedItems, resolver::Type};
        impl Expr {
            pub fn to_string(
                &self,
                resolver: &QueryableResolvedItems,
            ) -> String {
                self.kind.to_string(resolver)
            }
        }

        impl Type {
            pub fn to_string(
                &self,
                resolver: &QueryableResolvedItems,
            ) -> String {
                match self {
                    Type::Integer => "int".to_string(),
                    Type::Bool => "bool".to_string(),
                    Type::Unit => "()".to_string(),
                    Type::String => "string".to_string(),
                    Type::ErrorRecovery => "<error>".to_string(),
                    Type::Named(id) => {
                        format!("named type {}", resolver.interner.get(resolver.get_type(*id).name.id))
                    },
                    Type::Generic(a) => format!("generic type {}", resolver.interner.get(a.id)),
                }
            }
        }

        impl ExprKind {
            pub fn to_string(
                &self,
                resolver: &QueryableResolvedItems,
            ) -> String {
                match self {
                    ExprKind::Literal(lit) => format!("Literal({:?})", lit),
                    ExprKind::List(exprs) => format!("[{}]", exprs.iter().map(|x| x.to_string(resolver)).collect::<Vec<_>>().join(", ")),
                    ExprKind::FunctionCall(call) => {
                        format!("FunctionCall({})", call.function)
                    },
                    ExprKind::Unit => "Unit".to_string(),
                    ExprKind::ErrorRecovery => "<error>".to_string(),
                    ExprKind::Variable { name, ty } => format!("{}: {}", resolver.interner.get(name.id), ty.to_string(resolver)),
                    ExprKind::Intrinsic(x) => format!(
                        "@{}({})",
                        x.intrinsic,
                        x.args.iter().map(|x| x.to_string(resolver)).collect::<Vec<_>>().join(", ")
                    ),
                    ExprKind::TypeConstructor(..) => "Type constructor".into(),
                    ExprKind::ExpressionWithBindings { .. } => todo!(),
                }
            }
        }
    }
    use expect_test::{expect, Expect};
    use petr_utils::render_error;

    use super::*;
    use crate::resolved::QueryableResolvedItems;
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = petr_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let resolver = Resolver::new_from_single_ast(ast, interner);
        let (errs, queryable) = resolver.into_queryable();
        assert!(errs.is_empty());
        let result = pretty_print_resolution(&queryable);
        expect.assert_eq(&result);
    }

    fn check_multiple(
        inputs: Vec<impl Into<String>>,
        expect: Expect,
    ) {
        let inputs: Vec<_> = inputs
            .into_iter()
            .enumerate()
            .map(|(i, input)| (format!("test{}", i + 1), input.into()))
            .collect();
        let parser = petr_parse::Parser::new(inputs);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let resolver = Resolver::new_from_single_ast(ast, interner);
        let (errs, queryable) = resolver.into_queryable();
        assert!(errs.is_empty());
        let result = pretty_print_resolution(&queryable);
        expect.assert_eq(&result);
    }

    fn pretty_print_resolution(queryable: &QueryableResolvedItems) -> String {
        let mut result = String::new();
        result.push_str("_____FUNCTIONS_____\n");
        for (func_id, func) in queryable.functions() {
            result.push_str(&format!("#{} {}", Into::<usize>::into(func_id), queryable.interner.get(func.name.id),));
            result.push('(');
            for (name, ty) in &func.params {
                let name = queryable.interner.get(name.id);
                let ty = ty.to_string(queryable);
                result.push_str(&format!("  {}: {}, ", name, ty));
            }
            result.push_str(") ");
            let ty = func.return_type.to_string(queryable);
            result.push_str(&format!("-> {} ", ty));
            result.push_str(&format!("  {:?}\n", func.body.to_string(queryable)));
        }

        result.push_str("_____TYPES_____\n");
        for (type_id, ty_decl) in queryable.types() {
            result.push_str(&format!("#{} {}", Into::<usize>::into(type_id), queryable.interner.get(ty_decl.name.id),));
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
                    #0 foo(  a: int, ) -> int   "[Literal(Integer(1)), Literal(Integer(2)), Literal(Integer(3))]"
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
                #0 a() -> named type MyType   "Type constructor"
                #1 b() -> named type MyType   "Type constructor"
                #2 foo(  a: named type MyType, ) -> named type MyType   "[Literal(Integer(1)), Literal(Integer(2)), Literal(Integer(3))]"
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
                #0 foo(  a: named type MyType, ) -> named type MyType   "[Literal(Integer(1)), Literal(Integer(2)), Literal(Integer(3))]"
                #1 a() -> named type MyType   "Type constructor"
                #2 b() -> named type MyType   "Type constructor"
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
            expect![[r#"
                _____FUNCTIONS_____
                #0 foo() -> named type MyType   "FunctionCall(functionid1)"
                #1 bar(  a: named type MyType, ) -> named type MyType   "[Literal(Integer(1)), Literal(Integer(2)), Literal(Integer(3))]"
                #2 a() -> named type MyType   "Type constructor"
                #3 b() -> named type MyType   "Type constructor"
                _____TYPES_____
                #0 MyType

            "#]],
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
            expect![[r#"
                _____FUNCTIONS_____
                #0 foo() -> named type MyType   "FunctionCall(functionid1)"
                #1 bar(  a: named type MyType, ) -> named type MyType   "[Literal(Integer(1)), Literal(Integer(2)), Literal(Integer(3))]"
                #2 a() -> named type MyType   "Type constructor"
                #3 b() -> named type MyType   "Type constructor"
                _____TYPES_____
                #0 MyType

            "#]],
        )
    }
    #[test]
    fn import_something_from_another_file() {
        check_multiple(
            vec![
                r#"
                Function exported_func(a in 'int) returns 'int a
                "#,
                r#"
                import test1.exported_func

                function foo() returns 'int ~exported_func(5)

               "#,
            ],
            expect![[r#"
                _____FUNCTIONS_____
                #0 exported_func(  a: int, ) -> int   "a: int"
                #1 foo() -> int   "FunctionCall(functionid0)"
                _____TYPES_____
            "#]],
        )
    }

    #[test]
    fn import_something_from_another_file_with_alias() {
        check_multiple(
            vec![
                r#"
                Function exported_func(a in 'int) returns 'int a
                "#,
                r#"
                import test1.exported_func as bar 

                function foo() returns 'int ~bar(5)

               "#,
            ],
            expect![[r#"
                _____FUNCTIONS_____
                #0 exported_func(  a: int, ) -> int   "a: int"
                #1 foo() -> int   "FunctionCall(functionid0)"
                _____TYPES_____
            "#]],
        )
    }
}
