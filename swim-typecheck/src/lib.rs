mod error {
    use std::os;

    use miette::Diagnostic;
    use thiserror::Error;

    #[derive(Error, Debug, PartialEq)]
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
    #[derive(Error, Debug, Diagnostic, PartialEq)]
    pub enum TypeCheckErrorKind {
        #[error("Failed to unify types: {0}")]
        UnificationFailure(#[from] polytype::UnificationError),
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
            self.help
                .as_ref()
                .map(|x| -> Box<dyn std::fmt::Display> { Box::new(x) })
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
}

use std::collections::BTreeMap;

use error::{TypeCheckError, TypeCheckErrorKind};
use polytype::{tp, Type};
use swim_bind::{FunctionId, TypeId};
use swim_resolve::{Expr, ExprKind, Intrinsic, Literal, QueryableResolvedItems, Ty};
use swim_utils::{Identifier, IndexMap};

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
    errors: Vec<TypeCheckError>,
}

pub type TypeVariable = Type<&'static str>;

impl TypeChecker {
    fn fully_resolve(
        &mut self,
        resolved: &QueryableResolvedItems,
    ) {
        // for each function, resolve its body
        let mut funcs_to_check = vec![];
        for (id, ty) in &self.type_map {
            match id {
                TypeOrFunctionId::FunctionId(func_id) => {
                    let function = resolved.get_function(*func_id);
                    let mut function = function.clone();
                    funcs_to_check.push(function);
                },
                _ => {},
            }
        }

        for mut func in funcs_to_check {
            func.type_check(self);
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
                TypeOrFunctionId::FunctionId(func_id) if *func_id == entry_point => {
                    Some(resolved.get_function(*func_id))
                },
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

    fn from_resolved(resolved: &swim_resolve::QueryableResolvedItems) -> Self {
        let mut ctx = polytype::Context::default();
        let mut type_checker = TypeChecker {
            ctx,
            type_map: Default::default(),
            errors: Default::default(),
        };

        for (id, _ty) in resolved.types() {
            let ty = type_checker.fresh_ty_var();
            type_checker.type_map.insert(id.into(), ty);
        }

        for (id, func) in resolved.functions() {
            let mut func_type = Vec::with_capacity(func.params.len() + 1);
            for (_, ty) in &func.params {
                func_type.push(type_checker.to_type_var(ty));
            }
            func_type.push(type_checker.to_type_var(&func.return_type));
            let func_type = TypeChecker::arrow_type(func_type);
            type_checker.type_map.insert(id.into(), func_type);
        }

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
            swim_resolve::Type::Named(ty_id) => self
                .type_map
                .get(&ty_id.into())
                .expect("type did not exist in type map")
                .clone(),
        }
    }

    pub fn get_type(
        &self,
        key: impl Into<TypeOrFunctionId>,
    ) -> &TypeVariable {
        self.type_map
            .get(&key.into())
            .expect("type did not exist in type map")
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
}

impl TypeCheck for Expr {
    type Output = TypeVariable;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        match &self.kind {
            ExprKind::Literal(lit) => ctx.convert_literal_to_type(&lit),
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    tp!(list(tp!(unit)))
                } else {
                    todo!(" exprs.first().unwrap().kind.return_type()")
                }
            },
            ExprKind::FunctionCall(call) => {
                todo!()
            },
            ExprKind::Unit => tp!(unit),
            ExprKind::ErrorRecovery => ctx.fresh_ty_var(),
            ExprKind::Variable(item) => ctx.to_type_var(item),
            ExprKind::Intrinsic(intrinsic) => intrinsic.type_check(ctx),
        }
    }
}

impl TypeCheck for Intrinsic {
    type Output = TypeVariable;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        use swim_resolve::IntrinsicName::*;
        match self.intrinsic {
            Puts => {
                if self.args.len() != 1 {
                    todo!("puts arg len check");
                    return ctx.fresh_ty_var();
                }
                // puts takes a single string and returns unit
                let type_of_arg = self.args[0].type_check(ctx);
                ctx.unify(&tp!(string), &type_of_arg);
                tp!(unit)
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

pub struct Function {
    params: Vec<(Identifier, TypeVariable)>,
    return_ty: TypeVariable,
}

impl TypeCheck for swim_resolve::Function {
    type Output = Function;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        let params = self
            .params
            .iter()
            .map(|(name, ty)| (*name, ctx.to_type_var(ty)));

        // unify types within the body with the parameter
        let return_type_of_body_expr = self.body.type_check(ctx);

        let declared_return_type = ctx.to_type_var(&self.return_type);

        ctx.unify(&declared_return_type, &return_type_of_body_expr);

        Function {
            params: params.collect(),
            return_ty: declared_return_type,
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
        let arg_types = self
            .args
            .iter()
            .map(|arg| arg.type_check(ctx))
            .collect::<Vec<_>>();

        let arg_type = TypeChecker::arrow_type(arg_types);

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
    use miette::Error;
    use swim_resolve::QueryableResolvedItems;
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
            errs.into_iter()
                .for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let resolved = QueryableResolvedItems::new_from_single_ast(ast);
        let mut type_checker = TypeChecker::from_resolved(&resolved);
        type_checker.fully_resolve(&resolved);
        let res = pretty_print_type_checker(&interner, &source_map, type_checker, &resolved);

        expect.assert_eq(&res);
    }

    fn pretty_print_type_checker(
        interner: &SymbolInterner,
        source_map: &IndexMap<SourceId, (&str, &str)>,
        type_checker: TypeChecker,
        resolved: &QueryableResolvedItems,
    ) -> String {
        let mut s = String::new();
        for (id, ty) in &type_checker.type_map {
            let text = match id {
                TypeOrFunctionId::TypeId(id) => {
                    let ty = resolved.get_type(*id);

                    let name = interner.get(ty.name.id);
                    format!("type {}", name)
                },
                TypeOrFunctionId::FunctionId(id) => {
                    let func = resolved.get_function(*id);

                    let name = interner.get(func.name.id);
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
            expect![[r#""#]],
        );
    }

    #[test]
    fn pass_literal_string_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts("test")"#,
            expect![[r#""#]],
        );
    }

    #[test]
    fn pass_wrong_type_literal_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts(bool)"#,
            expect![[r#""#]],
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
            expect![[r#""#]],
        );
    }
}
