mod error;

use std::{collections::BTreeMap, rc::Rc};

use error::{TypeCheckError, TypeCheckErrorKind};
pub use petr_bind::FunctionId;
use petr_bind::TypeId;
use petr_resolve::{Expr, ExprKind, QueryableResolvedItems};
pub use petr_resolve::{Intrinsic as ResolvedIntrinsic, IntrinsicName, Literal};
use petr_utils::{Identifier, SymbolId};
use polytype::{tp, Type};

// TODO return QueryableTypeChecked instead of type checker
// Clean up API so this is the only function exposed
pub fn type_check(resolved: QueryableResolvedItems) -> (Vec<TypeCheckError>, TypeChecker) {
    let mut type_checker = TypeChecker::new(resolved);
    type_checker.fully_type_check();
    (type_checker.errors.clone(), type_checker)
}

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
    errors: Vec<TypeCheckError>,
    resolved: QueryableResolvedItems,
    variable_scope: Vec<BTreeMap<Identifier, TypeVariable>>,
}

pub type TypeVariable = Type<&'static str>;

/// TODO get rid of this type and use polytype directly
pub enum PetrType {
    Unit,
    Integer,
    Boolean,
    String,
    Variable(usize),
}

impl TypeChecker {
    /// realizes a polytype into a petr type
    /// TODO very inefficient to do this one at a time, should realize
    /// all types at once during lowering
    pub fn realize_type(
        &self,
        ty: &TypeVariable,
    ) -> PetrType {
        let ty = ty.clone();
        let int_ty = tp!(int);
        let bool_ty = tp!(bool);
        let unit_ty = tp!(unit);
        let string_ty = tp!(string);
        match ty {
            ty if ty == int_ty => PetrType::Integer,
            ty if ty == bool_ty => PetrType::Boolean,
            ty if ty == unit_ty => PetrType::Unit,
            ty if ty == string_ty => PetrType::String,
            Type::Variable(otherwise) => PetrType::Variable(otherwise),
            other => todo!("{other:?}"),
        }
    }

    pub fn get_symbol(
        &self,
        id: SymbolId,
    ) -> Rc<str> {
        self.resolved.interner.get(id).clone()
    }

    fn with_type_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.variable_scope.push(Default::default());
        let res = f(self);
        self.variable_scope.pop();
        res
    }

    fn generic_type(
        &mut self,
        id: &Identifier,
    ) -> TypeVariable {
        for scope in self.variable_scope.iter().rev() {
            if let Some(ty) = scope.get(id) {
                return ty.clone();
            }
        }
        let fresh_ty = self.fresh_ty_var();
        self.variable_scope
            .last_mut()
            .expect("looked for generic when no scope existed")
            .insert(*id, fresh_ty.clone());
        fresh_ty
    }

    fn find_variable(
        &self,
        id: Identifier,
    ) -> Option<TypeVariable> {
        for scope in self.variable_scope.iter().rev() {
            if let Some(ty) = scope.get(&id) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn fully_type_check(&mut self) {
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
        }
    }

    pub fn new(resolved: QueryableResolvedItems) -> Self {
        let ctx = polytype::Context::default();
        let mut type_checker = TypeChecker {
            ctx,
            type_map: Default::default(),
            errors: Default::default(),
            typed_functions: Default::default(),
            resolved,
            variable_scope: Default::default(),
        };

        type_checker.fully_type_check();
        type_checker
    }

    pub fn insert_variable(
        &mut self,
        id: Identifier,
        ty: TypeVariable,
    ) {
        self.variable_scope
            .last_mut()
            .expect("inserted variable when no scope existed")
            .insert(id, ty);
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

        for item in tys.iter().skip(2) {
            ty = Type::arrow(ty, item.clone());
        }

        ty
    }

    pub fn to_type_var(
        &mut self,
        ty: &petr_resolve::Type,
    ) -> TypeVariable {
        match ty {
            petr_resolve::Type::Integer => tp!(int),
            petr_resolve::Type::Bool => tp!(bool),
            petr_resolve::Type::Unit => tp!(unit),
            petr_resolve::Type::String => tp!(string),
            petr_resolve::Type::ErrorRecovery => {
                // unifies to anything, fresh var
                self.fresh_ty_var()
            },
            petr_resolve::Type::Named(ty_id) => self.type_map.get(&ty_id.into()).expect("type did not exist in type map").clone(),
            petr_resolve::Type::Generic(generic_name) => {
                // TODO I think this needs to be a qualifier
                // polytype has support for qualifying polymorphic types
                // but instead I'm going to do the lazy thing and instantiate the generic
                // with a fresh type variable
                self.generic_type(generic_name)
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
        literal: &petr_resolve::Literal,
    ) -> TypeVariable {
        use petr_resolve::Literal::*;
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
    ) -> &petr_resolve::Function {
        self.resolved.get_function(function)
    }

    /// Given a symbol ID, look it up in the interner and realize it as a
    /// string.
    fn realize_symbol(
        &self,
        id: petr_utils::SymbolId,
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
pub enum Intrinsic {
    Puts(Box<TypedExpr>),
    Add(Box<TypedExpr>, Box<TypedExpr>),
}

impl std::fmt::Debug for Intrinsic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Intrinsic::Puts(expr) => write!(f, "@puts({:?})", expr),
            Intrinsic::Add(lhs, rhs) => write!(f, "@add({:?}, {:?})", lhs, rhs),
        }
    }
}

#[derive(Clone)]
pub enum TypedExpr {
    FunctionCall {
        func: FunctionId,
        args: Vec<(Identifier, TypedExpr)>,
        ty:   TypeVariable,
    },
    Literal {
        value: Literal,
        ty:    TypeVariable,
    },
    List {
        elements: Vec<TypedExpr>,
        ty:       TypeVariable,
    },
    Unit,
    Variable {
        ty:   TypeVariable,
        name: Identifier,
    },
    Intrinsic {
        ty:        TypeVariable,
        intrinsic: Intrinsic,
    },
    // TODO put a span here?
    ErrorRecovery,
    ExprWithBindings {
        bindings:   Vec<(Identifier, TypedExpr)>,
        expression: Box<TypedExpr>,
    },
}

impl std::fmt::Debug for TypedExpr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        use TypedExpr::*;
        match self {
            FunctionCall { func, args, .. } => {
                write!(f, "function call to {} with args: ", func)?;
                for (name, arg) in args {
                    write!(f, "{}: {:?}, ", name.id, arg)?;
                }
                Ok(())
            },
            Literal { value, .. } => write!(f, "literal: {}", value),
            List { elements, .. } => {
                write!(f, "list: [")?;
                for elem in elements {
                    write!(f, "{:?}, ", elem)?;
                }
                write!(f, "]")
            },
            Unit => write!(f, "unit"),
            Variable { name, .. } => write!(f, "variable: {}", name.id),
            Intrinsic { intrinsic, .. } => write!(f, "intrinsic: {:?}", intrinsic),
            ErrorRecovery => write!(f, "error recovery"),
            ExprWithBindings { bindings, expression } => {
                write!(f, "bindings: ")?;
                for (name, expr) in bindings {
                    write!(f, "{}: {:?}, ", name.id, expr)?;
                }
                write!(f, "expression: {:?}", expression)
            },
        }
    }
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
            ExprWithBindings { expression, .. } => expression.ty(),
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
                let ty = ctx.convert_literal_to_type(lit);
                TypedExpr::Literal { value: lit.clone(), ty }
            },
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    TypedExpr::List {
                        elements: vec![],
                        ty:       tp!(list(tp!(unit))),
                    }
                } else {
                    let type_checked_exprs = exprs.iter().map(|expr| expr.type_check(ctx)).collect::<Vec<_>>();
                    // unify the type of the first expr against everything else in the list
                    let first_ty = type_checked_exprs[0].ty();
                    for expr in type_checked_exprs.iter().skip(1) {
                        ctx.unify(&first_ty, &expr.ty());
                    }
                    TypedExpr::List {
                        elements: type_checked_exprs,
                        ty:       tp!(list(first_ty)),
                    }
                }
            },
            ExprKind::FunctionCall(call) => {
                // unify args with params
                // return the func return type
                let func_decl = ctx.get_untyped_function(call.function).clone();
                if call.args.len() != func_decl.params.len() {
                    ctx.push_error(TypeCheckErrorKind::ArgumentCountMismatch {
                        expected: func_decl.params.len(),
                        got:      call.args.len(),
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
            ExprKind::Variable { name, ty } => {
                // look up variable in scope
                // find its expr return type
                let var_ty = ctx.find_variable(*name).expect("variable not found in scope");
                let ty = ctx.to_type_var(ty);

                ctx.unify(&var_ty, &ty);

                TypedExpr::Variable { ty, name: *name }
            },
            ExprKind::Intrinsic(intrinsic) => intrinsic.type_check(ctx),
            ExprKind::TypeConstructor => {
                // type constructor expressions take inputs that should line up with a type decl and return a type
                todo!()
            },
            ExprKind::ExpressionWithBindings { bindings, expression } => {
                // for each binding, type check the rhs
                ctx.with_type_scope(|ctx| {
                    let mut type_checked_bindings = Vec::with_capacity(bindings.len());
                    for binding in bindings {
                        let binding_ty = binding.expression.type_check(ctx);
                        ctx.insert_variable(binding.name, binding_ty.ty());
                        type_checked_bindings.push((binding.name, binding_ty));
                    }

                    TypedExpr::ExprWithBindings {
                        bindings:   type_checked_bindings,
                        expression: Box::new(expression.type_check(ctx)),
                    }
                })
            },
        }
    }
}

impl TypeCheck for ResolvedIntrinsic {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        use petr_resolve::IntrinsicName::*;
        match self.intrinsic {
            Puts => {
                if self.args.len() != 1 {
                    todo!("puts arg len check");
                }
                // puts takes a single string and returns unit
                let arg = self.args[0].type_check(ctx);
                ctx.unify(&tp!(string), &arg.ty());
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Puts(Box::new(arg)),
                    ty:        tp!(unit),
                }
            },
            Add => {
                if self.args.len() != 2 {
                    todo!("add arg len check");
                }
                let arg1 = self.args[0].type_check(ctx);
                let arg2 = self.args[1].type_check(ctx);
                ctx.unify(&arg1.ty(), &tp!(int));
                ctx.unify(&arg2.ty(), &tp!(int));
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Add(Box::new(arg1), Box::new(arg2)),
                    ty:        tp!(int),
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
    pub name:      Identifier,
    pub params:    Vec<(Identifier, TypeVariable)>,
    pub body:      TypedExpr,
    pub return_ty: TypeVariable,
}

impl TypeCheck for petr_resolve::Function {
    type Output = Function;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        ctx.with_type_scope(|ctx| {
            let params = self.params.iter().map(|(name, ty)| (*name, ctx.to_type_var(ty))).collect::<Vec<_>>();

            for (name, ty) in &params {
                ctx.insert_variable(*name, ty.clone());
            }

            // unify types within the body with the parameter
            let body = self.body.type_check(ctx);

            let declared_return_type = ctx.to_type_var(&self.return_type);

            ctx.unify(&declared_return_type, &body.ty());

            Function {
                name: self.name,
                params,
                return_ty: declared_return_type,
                body,
            }
        })
        // in a scope that contains the above names to type variables, check the body
        // TODO: introduce scopes here, like in the binder, except with type variables
    }
}

impl TypeCheck for petr_resolve::FunctionCall {
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
    }
}

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};
    use petr_resolve::resolve_symbols;
    use petr_utils::{render_error, SymbolInterner};

    use super::*;
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
        let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
        assert!(errs.is_empty(), "can't typecheck: unresolved symbols");
        let type_checker = TypeChecker::new(resolved);
        let res = pretty_print_type_checker(type_checker);

        expect.assert_eq(&res);
    }

    fn pretty_print_type_checker(type_checker: TypeChecker) -> String {
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
            match id {
                TypeOrFunctionId::TypeId(_) => (),
                TypeOrFunctionId::FunctionId(func) => {
                    let func = type_checker.typed_functions.get(func).unwrap();
                    let body = &func.body;
                    s.push_str(&pretty_print_typed_expr(&type_checker.resolved.interner, body));
                    s.push('\n');
                },
            }

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

    fn pretty_print_typed_expr(
        interner: &SymbolInterner,
        typed_expr: &TypedExpr,
    ) -> String {
        match typed_expr {
            TypedExpr::ExprWithBindings { bindings, expression } => {
                let mut s = String::new();
                for (name, expr) in bindings {
                    let ident = interner.get(name.id);
                    s.push_str(&format!("{ident}: {:?} ({}),\n", expr, expr.ty()));
                }
                s.push_str(&format!("{:?} ({})", pretty_print_typed_expr(interner, expression), expression.ty()));
                s
            },
            TypedExpr::Variable { name, ty } => {
                let name = interner.get(name.id);
                format!("variable: {name} ({ty})")
            },

            TypedExpr::FunctionCall { func, args, ty } => {
                let mut s = String::new();
                s.push_str(&format!("function call to {} with args: ", func));
                for (name, arg) in args {
                    let name = interner.get(name.id);
                    s.push_str(&format!("{name}: {:?}, ", arg.ty()));
                }
                s.push_str(&format!("returns {ty}"));
                s
            },
            otherwise => format!("{:?}", otherwise),
        }
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

    // TODO this is maybe good for generic syntax
    #[test]
    fn identity_resolution_generic() {
        check(
            r#"
            function foo(x in 'A) returns 'A x
            "#,
            expect![[r#"
                function foo → t0 → t0
            "#]],
        );
    }

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
    #[test]
    fn list_different_types_type_err() {
        check(
            r#"
                function my_list() returns 'list [ 1, true ]
            "#,
            expect![[r#"
                function my_list → t0

                Errors:
                Failed to unify types: Failure(int, bool)
            "#]],
        );
    }

    #[test]
    fn incorrect_number_of_args() {
        check(
            r#"
                function add(a in 'int, b in 'int) returns 'int a

                function add_five(a in 'int) returns 'int ~add(5)
            "#,
            expect![[r#"
                function add → (int → int) → int
                function add_five → int → int

                Errors:
                Function add takes 2 arguments, but got 1 arguments.
                Failed to unify types: Failure(int, error)
            "#]],
        );
    }

    #[test]
    fn infer_let_bindings() {
        check(
            r#"
            function hi(x in 'int, y in 'int) returns 'int
    let a = x,
        b = y,
        c = 20,
        d = 30,
        e = 42,
    a
function main() returns 'int ~hi(1, 2)"#,
            expect![[r#"
                function hi → (int → int) → int
                a: variable: symbolid2 (int),
                b: variable: symbolid4 (int),
                c: literal: 20 (int),
                d: literal: 30 (int),
                e: literal: 42 (int),
                "variable: a (int)" (int)

                function main → int
                function call to functionid0 with args: x: Constructed("int", []), y: Constructed("int", []), 

            "#]],
        )
    }
}
