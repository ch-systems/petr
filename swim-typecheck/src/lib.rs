mod error;

use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use error::{TypeCheckError, TypeCheckErrorKind};
use polytype::{tp, Type};
pub use swim_bind::FunctionId;
use swim_bind::TypeId;
use swim_resolve::{Expr, ExprKind, QueryableResolvedItems, Ty};
pub use swim_resolve::{Intrinsic, IntrinsicName, Literal};
use swim_utils::{idx_map_key, Identifier, IndexMap, SymbolInterner};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOrFunctionId {
    TypeId(TypeId),
    FunctionId(FunctionId),
}

impl From<TypeId> for TypeOrFunctionId {
    fn from(type_id: TypeId) -> Self {
        TypeOrFunctionId::TypeId(type_id)
    }
}

impl From<FunctionId> for TypeOrFunctionId {
    fn from(function_id: FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(function_id)
    }
}

impl From<&TypeId> for TypeOrFunctionId {
    fn from(type_id: &TypeId) -> Self {
        TypeOrFunctionId::TypeId(*type_id)
    }
}

impl From<&FunctionId> for TypeOrFunctionId {
    fn from(function_id: &FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(*function_id)
    }
}

pub struct TypeChecker {
    ctx: polytype::Context,
    type_map: BTreeMap<TypeOrFunctionId, TypeVariable>,
    typed_functions: BTreeMap<FunctionId, Function>,
    // typed_types: IndexMap<TypedTypeId, TypeVariable>,
    errors: Vec<TypeCheckError>,
    resolved: QueryableResolvedItems,
}

pub type TypeVariable = Type<&'static str>;

pub enum SwimType {
    Unit,
    Integer,
    Boolean,
}

impl TypeChecker {
    /// realizes a polytype into a swim type
    /// TODO very inefficient to do this one at a time, should realize
    /// all types at once during lowering
    pub fn realize_type(
        &self,
        ty: &TypeVariable,
    ) -> SwimType {
        let ty = ty.clone();
        let int_ty = tp!(int);
        let bool_ty = tp!(bool);
        let unit_ty = tp!(unit);
        match ty {
            ty if ty == int_ty => SwimType::Integer,
            ty if ty == bool_ty => SwimType::Boolean,
            ty if ty == unit_ty => SwimType::Unit,
            _ => todo!(),
        }
    }
    fn fully_resolve(&mut self) {
        // TODO collects on these iters is not ideal
        for (id, _) in self.resolved.types() {
            let ty = self.fresh_ty_var();
            self.type_map.insert(id.into(), ty);
            // todo!("type decl map?"); need to store the function types of type constructors
            // and the parent type id here
        }
        for (id, func) in self.resolved.functions() {
            let typed_function = func.type_check(self);
            self.type_map.insert(
                id.into(),
                TypeChecker::arrow_type(
                    [
                        typed_function.params.iter().map(|(_, b)| b.clone()).collect(),
                        vec![typed_function.return_ty.clone()],
                    ]
                    .concat(),
                ),
            );
            self.typed_functions.insert(id, typed_function);
            //            self.function_name_map.insert(func.name, typed_function_id);
        }
    }

    fn resolve_from_entry(
        &mut self,
        entry_point: FunctionId,
        resolved: &swim_resolve::QueryableResolvedItems,
    ) {
        // TODO -- should entry point be a str?
        // find function with entry point name and type check its body as a function call
        // Find the function with the entry point name
        let function = self
            .type_map
            .keys()
            .find_map(|key| match key {
                TypeOrFunctionId::FunctionId(func_id) if *func_id == entry_point => Some(resolved.get_function(*func_id)),
                _ => None,
            })
            .expect("Entry point function not found");

        // Type check the function body as a function call
        let mut function_call = swim_resolve::FunctionCall {
            function: entry_point,
            args: vec![], // Populate with actual arguments if necessary
        };
        function_call.type_check(self);
        ()
    }

    pub fn new(resolved: QueryableResolvedItems) -> Self {
        let mut ctx = polytype::Context::default();
        let mut type_checker = TypeChecker {
            ctx,
            type_map: Default::default(),
            errors: Default::default(),
            typed_functions: Default::default(),
            resolved,
        };

        // for (id, _ty) in type_checker.resolved_items.types() {
        //     let ty = type_checker.fresh_ty_var();
        //     type_checker.type_map.insert(id.into(), ty);
        // }

        // for (id, func) in resolved.functions() {
        //     let mut func_type = Vec::with_capacity(func.params.len() + 1);
        //     for (_, ty) in &func.params {
        //         func_type.push(type_checker.to_type_var(ty));
        //     }
        //     func_type.push(type_checker.to_type_var(&func.return_type));
        //     let func_type = TypeChecker::arrow_type(func_type);
        //     type_checker.type_map.insert(id.into(), func_type);
        // }

        type_checker.fully_resolve();
        type_checker
    }

    pub fn fresh_ty_var(&mut self) -> TypeVariable {
        self.ctx.new_variable()
    }

    fn arrow_type(tys: Vec<TypeVariable>) -> TypeVariable {
        assert!(!tys.is_empty(), "arrow_type: tys is empty");

        if tys.len() == 1 {
            return tys[0].clone();
        }

        let mut ty = Type::arrow(tys[0].clone(), tys[1].clone());

        for i in 2..tys.len() {
            ty = Type::arrow(ty, tys[i].clone());
        }

        ty
    }

    pub fn to_type_var(
        &mut self,
        ty: &swim_resolve::Type,
    ) -> TypeVariable {
        match ty {
            swim_resolve::Type::Integer => tp!(int),
            swim_resolve::Type::Bool => tp!(bool),
            swim_resolve::Type::Unit => tp!(unit),
            swim_resolve::Type::String => tp!(string),
            swim_resolve::Type::ErrorRecovery => {
                // unifies to anything, fresh var
                self.fresh_ty_var()
            },
            swim_resolve::Type::Named(ty_id) => self.type_map.get(&ty_id.into()).expect("type did not exist in type map").clone(),
            swim_resolve::Type::Generic(generic_name) => {
                // TODO I think this needs to be a qualifier
                self.fresh_ty_var()
            },
        }
    }

    pub fn get_type(
        &self,
        key: impl Into<TypeOrFunctionId>,
    ) -> &TypeVariable {
        self.type_map.get(&key.into()).expect("type did not exist in type map")
    }

    fn convert_literal_to_type(
        &self,
        literal: &swim_resolve::Literal,
    ) -> TypeVariable {
        use swim_resolve::Literal::*;
        match literal {
            Integer(_) => tp!(int),
            Boolean(_) => tp!(bool),
            String(_) => tp!(string),
        }
    }

    fn push_error(
        &mut self,
        e: impl Into<TypeCheckErrorKind>,
    ) {
        let kind = e.into();
        self.errors.push(kind.into());
    }

    pub fn unify(
        &mut self,
        ty1: &TypeVariable,
        ty2: &TypeVariable,
    ) {
        match self.ctx.unify(ty1, ty2) {
            Ok(_) => (),
            Err(e) => self.push_error(e),
        }
    }

    fn get_untyped_function(
        &self,
        function: FunctionId,
    ) -> &swim_resolve::Function {
        self.resolved.get_function(function)
    }

    /// Given a symbol ID, look it up in the interner and realize it as a
    /// string.
    fn realize_symbol(
        &self,
        id: swim_utils::SymbolId,
    ) -> Rc<str> {
        self.resolved.interner.get(id)
    }

    pub fn get_function(
        &self,
        id: &FunctionId,
    ) -> &Function {
        self.typed_functions.get(id).expect("invariant: should exist")
    }

    // TODO unideal clone
    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, Function)> {
        self.typed_functions.iter().map(|(a, b)| (*a, b.clone())).collect::<Vec<_>>().into_iter()
    }
}

#[derive(Clone)]
pub enum TypedExpr {
    FunctionCall {
        func: FunctionId,
        args: Vec<(Identifier, TypedExpr)>,
        ty: TypeVariable,
    },
    Literal {
        value: Literal,
        ty: TypeVariable,
    },
    List {
        elements: Vec<TypedExpr>,
        ty: TypeVariable,
    },
    Unit,
    Variable {
        ty: TypeVariable,
        // TODO name?
    },
    Intrinsic {
        ty: TypeVariable,
        intrinsic: Intrinsic,
    },
    // TODO put a span here?
    ErrorRecovery,
}

impl TypedExpr {
    pub fn ty(&self) -> TypeVariable {
        use TypedExpr::*;
        match self {
            FunctionCall { ty, .. } => ty.clone(),
            Literal { ty, .. } => ty.clone(),
            List { ty, .. } => ty.clone(),
            Unit => tp!(unit),
            Variable { ty, .. } => ty.clone(),
            Intrinsic { ty, .. } => ty.clone(),
            ErrorRecovery => tp!(error),
        }
    }
}

impl TypeCheck for Expr {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        match &self.kind {
            ExprKind::Literal(lit) => {
                let ty = ctx.convert_literal_to_type(&lit);
                TypedExpr::Literal { value: lit.clone(), ty }
            },
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    TypedExpr::List {
                        elements: vec![],
                        ty: tp!(list(tp!(unit))),
                    }
                } else {
                    todo!(" exprs.first().unwrap().kind.return_type()")
                }
            },
            ExprKind::FunctionCall(call) => {
                // unify args with params
                // return the func return type
                let func_decl = ctx.get_untyped_function(call.function).clone();
                if call.args.len() != func_decl.params.len() {
                    ctx.push_error(TypeCheckErrorKind::ArgumentCountMismatch {
                        expected: func_decl.params.len(),
                        got: call.args.len(),
                        function: ctx.realize_symbol(func_decl.name.id).to_string(),
                    });
                    return TypedExpr::ErrorRecovery;
                }
                let mut args = Vec::with_capacity(call.args.len());

                for (arg, (param_name, param)) in call.args.iter().zip(func_decl.params.iter()) {
                    let arg_ty = arg.type_check(ctx);
                    let param_ty = ctx.to_type_var(param);
                    ctx.unify(&arg_ty.ty(), &param_ty);
                    args.push((*param_name, arg_ty));
                }
                TypedExpr::FunctionCall {
                    func: call.function,
                    args,
                    ty: ctx.to_type_var(&func_decl.return_type),
                }
            },
            ExprKind::Unit => TypedExpr::Unit,
            ExprKind::ErrorRecovery => TypedExpr::ErrorRecovery,
            ExprKind::Variable(item) => TypedExpr::Variable { ty: ctx.to_type_var(item) },
            ExprKind::Intrinsic(intrinsic) => intrinsic.type_check(ctx),
        }
    }
}

impl TypeCheck for Intrinsic {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        use swim_resolve::IntrinsicName::*;
        match self.intrinsic {
            Puts => {
                if self.args.len() != 1 {
                    todo!("puts arg len check");
                }
                // puts takes a single string and returns unit
                let type_of_arg = self.args[0].type_check(ctx);
                ctx.unify(&tp!(string), &type_of_arg.ty());
                TypedExpr::Intrinsic {
                    intrinsic: self.clone(),
                    ty: tp!(unit),
                }
            },
        }
    }
}

trait TypeCheck {
    type Output;
    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output;
}

#[derive(Clone)]
pub struct Function {
    pub params: Vec<(Identifier, TypeVariable)>,
    pub body: TypedExpr,
    pub return_ty: TypeVariable,
}

impl TypeCheck for swim_resolve::Function {
    type Output = Function;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        let params = self.params.iter().map(|(name, ty)| (*name, ctx.to_type_var(ty))).collect::<Vec<_>>();

        // unify types within the body with the parameter
        let body = self.body.type_check(ctx);

        let declared_return_type = ctx.to_type_var(&self.return_type);

        ctx.unify(&declared_return_type, &body.ty());

        Function {
            params,
            return_ty: declared_return_type,
            body,
        }
        // in a scope that contains the above names to type variables, check the body
        // TODO: introduce scopes here, like in the binder, except with type variables
    }
}

impl TypeCheck for swim_resolve::FunctionCall {
    type Output = ();

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        // use polytype::Type::substitute to sub in the types of the arg exprs
        // and then unify with the function's type
        // get the function type
        let func_type = ctx.get_type(self.function).clone();
        let arg_types = self.args.iter().map(|arg| arg.type_check(ctx)).collect::<Vec<_>>();

        let arg_type = TypeChecker::arrow_type(arg_types.iter().map(TypedExpr::ty).collect());

        ctx.unify(&func_type, &arg_type);

        /*
        let mut substitutions = Default::default();
        for (arg, ty) in func_type.iter().zip(arg_types) {
            if ctx.is_concrete(ty) {
            } else {
                substitutions.insert(arg.clone(), ty);
            }
        }

        func_type.substitute(arg_types);
        */
    }
}

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};
    use swim_resolve::{resolve_symbols, QueryableResolvedItems};
    use swim_utils::{render_error, SourceId, SymbolInterner};

    use super::*;
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = swim_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let resolved = resolve_symbols(ast, interner);
        let type_checker = TypeChecker::new(resolved);
        let res = pretty_print_type_checker(&source_map, type_checker);

        expect.assert_eq(&res);
    }

    fn pretty_print_type_checker(
        source_map: &IndexMap<SourceId, (&str, &str)>,
        type_checker: TypeChecker,
    ) -> String {
        let mut s = String::new();
        for (id, ty) in &type_checker.type_map {
            let text = match id {
                TypeOrFunctionId::TypeId(id) => {
                    let ty = type_checker.resolved.get_type(*id);

                    let name = type_checker.resolved.interner.get(ty.name.id);
                    format!("type {}", name)
                },
                TypeOrFunctionId::FunctionId(id) => {
                    let func = type_checker.resolved.get_function(*id);

                    let name = type_checker.resolved.interner.get(func.name.id);
                    format!("function {}", name)
                },
            };
            s.push_str(&text);
            s.push_str(" → ");
            s.push_str(&ty.to_string());
            s.push('\n');
        }

        if !type_checker.errors.is_empty() {
            s.push_str("\nErrors:\n");
            for error in type_checker.errors {
                s.push_str(&format!("{}\n", miette::Report::new(error)));
            }
        }
        s
    }

    #[test]
    fn identity_resolution_concrete_type() {
        check(
            r#"
            function foo(x in 'int) returns 'int x
            "#,
            expect![[r#"
                function foo → int → int
            "#]],
        );
    }

    /* TODO this is maybe good for generic syntax
    #[test]
    fn identity_resolution_generic() {
        check(
            r#"
            function foo(x in 'A) returns 'A x
            "#,
            expect![[r#""#]],
        );
    }
    */

    #[test]
    fn identity_resolution_custom_type() {
        check(
            r#"
            type MyType = A | B
            function foo(x in 'MyType) returns 'MyType x
            "#,
            expect![[r#"
                type MyType → t0
                function A → t0
                function B → t0
                function foo → t0 → t0
            "#]],
        );
    }

    #[test]
    fn literal_unification_fail() {
        check(
            r#"
            function foo() returns 'int 5
            function bar() returns 'bool 5
            "#,
            expect![[r#"
                function foo → int
                function bar → bool

                Errors:
                Failed to unify types: Failure(bool, int)
            "#]],
        );
    }

    #[test]
    fn literal_unification_success() {
        check(
            r#"
            function foo() returns 'int 5
            function bar() returns 'bool true
            "#,
            expect![[r#"
                function foo → int
                function bar → bool
            "#]],
        );
    }

    #[test]
    fn pass_zero_arity_func_to_intrinsic() {
        check(
            r#"
        function string_literal() returns 'string
          "This is a string literal."

        function my_func() returns 'unit
          @puts(~string_literal)"#,
            expect![[r#"
                function string_literal → string
                function my_func → unit
            "#]],
        );
    }

    #[test]
    fn pass_literal_string_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts("test")"#,
            expect![[r#"
                function my_func → unit
            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_literal_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts(bool)"#,
            expect![[r#"
                function my_func → unit

                Errors:
                Failed to unify types: Failure(string, error)
            "#]],
        );
    }

    #[test]
    fn intrinsic_and_return_ty_dont_match() {
        check(
            r#"
        function my_func() returns 'bool
          @puts("test")"#,
            expect![[r#"
                function my_func → bool

                Errors:
                Failed to unify types: Failure(bool, unit)
            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_fn_call_to_intrinsic() {
        check(
            r#"
        function bool_literal() returns 'bool
            true

        function my_func() returns 'unit
          @puts(~bool_literal)"#,
            expect![[r#"
                function bool_literal → bool
                function my_func → unit

                Errors:
                Failed to unify types: Failure(string, bool)
            "#]],
        );
    }

    // TODO this will work when generics work
    #[test]
    fn multiple_calls_to_fn_dont_unify_params_themselves() {
        check(
            r#"
        function bool_literal(a in 'A, b in 'B) returns 'bool
            true

        function my_func() returns 'bool
            ~bool_literal(1, 2)

        {- should not unify the parameter types of bool_literal -}
        function my_second_func() returns 'bool
            ~bool_literal(true, false)
        "#,
            expect![[r#"
                function bool_literal → (t0 → t1) → bool
                function my_func → bool
                function my_second_func → bool
            "#]],
        );
    }
}
