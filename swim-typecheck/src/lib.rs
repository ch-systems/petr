use std::collections::BTreeMap;

use polytype::{tp, Type};
use swim_bind::{FunctionId, TypeId};
use swim_resolve::QueryableResolvedItems;
use swim_utils::IndexMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TypeOrFunctionId {
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

struct TypeChecker {
    ctx: polytype::Context,
    type_map: BTreeMap<TypeOrFunctionId, TypeVariable>,
}

type TypeVariable = Type<&'static str>;

impl TypeChecker {
    fn fully_resolve(&mut self, resolved: &QueryableResolvedItems) {
        // for each function, resolve its body
        let mut funcs_to_check = vec![];
        for (id, ty) in &self.type_map {
            match id {
                TypeOrFunctionId::FunctionId(func_id) => {
                    let function = resolved.get_function(*func_id);
                    let mut function = function.clone();
                    funcs_to_check.push(function);
                }
                _ => {}
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
                }
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

    pub fn to_type_var(&mut self, ty: &swim_resolve::Type) -> TypeVariable {
        match ty {
            swim_resolve::Type::Integer => tp!(int),
            swim_resolve::Type::Bool => tp!(bool),
            swim_resolve::Type::Unit => tp!(unit),
            swim_resolve::Type::ErrorRecovery => {
                // unifies to anything, fresh var
                self.fresh_ty_var()
            }
            swim_resolve::Type::Named(ty_id) => self
                .type_map
                .get(&ty_id.into())
                .expect("type did not exist in type map")
                .clone(),
        }
    }

    pub fn get_type(&self, key: impl Into<TypeOrFunctionId>) -> &TypeVariable {
        self.type_map
            .get(&key.into())
            .expect("type did not exist in type map")
    }

    pub fn unify(&mut self, ty1: &TypeVariable, ty2: &TypeVariable) {
        match self.ctx.unify(ty1, ty2) {
            Ok(_) => (),
            Err(_) => todo!("push to errs"),
        }
    }
}

trait TypeCheck {
    type Output;
    fn type_check(&mut self, ctx: &mut TypeChecker) -> Self::Output;
}

impl TypeCheck for swim_resolve::Function {
    type Output = ();

    fn type_check(&mut self, ctx: &mut TypeChecker) -> Self::Output {
        let parameter_types = self.params.iter().map(|(name, ty)| ctx.to_type_var(ty));

        // in a scope that contains the above names to type variables, check the body
        // TODO: introduce scopes here, like in the binder, except with type variables

        todo!()
    }
}

impl TypeCheck for swim_resolve::FunctionCall {
    type Output = ();

    fn type_check(&mut self, ctx: &mut TypeChecker) -> Self::Output {
        // use polytype::Type::substitute to sub in the types of the arg exprs
        // and then unify with the function's type
        // get the function type
        let func_type = ctx.get_type(self.function).clone();
        let arg_types = self
            .args
            .iter()
            .map(|arg| ctx.to_type_var(&arg.return_type()))
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

    use super::*;
    use expect_test::{expect, Expect};
    use swim_resolve::QueryableResolvedItems;
    use swim_utils::{render_error, SourceId, SymbolInterner};
    fn check(input: impl Into<String>, expect: Expect) {
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
        let res = pretty_print_type_checker(&interner, &source_map, &type_checker, &resolved);

        expect.assert_eq(&res);
    }

    fn pretty_print_type_checker(
        interner: &SymbolInterner,
        source_map: &IndexMap<SourceId, (&str, &str)>,
        type_checker: &TypeChecker,
        resolved: &QueryableResolvedItems,
    ) -> String {
        let mut s = String::new();
        for (id, ty) in &type_checker.type_map {
            let text = match id {
                TypeOrFunctionId::TypeId(id) => {
                    let ty = resolved.get_type(*id);

                    let name = interner.get(ty.name.id);
                    format!("type {}", name)
                }
                TypeOrFunctionId::FunctionId(id) => {
                    let func = resolved.get_function(*id);

                    let name = interner.get(func.name.id);
                    format!("function {}", name)
                }
            };
            s.push_str(&text);
            s.push_str(" → ");
            s.push_str(&ty.to_string());
            s.push('\n');
        }
        s
    }

    #[test]
    fn identity_resolution_concrete() {
        check(
            r#"
            function foo(x in 'int) returns 'int x
            "#,
            expect![[r#""#]],
        );
    }

    #[test]
    fn identity_resolution_generic() {
        check(
            r#"
            function foo(x in 'A) returns 'A x
            "#,
            expect![[r#""#]],
        );
    }

    #[test]
    fn identity_resolution_custom_type() {
        check(
            r#"
            type MyType = A | B
            function foo(x in 'MyType) returns 'MyType x
            "#,
            expect![[r#""#]],
        );
    }

    #[test]
    fn concrete_unification() {
        check(
            r#"
            function foo() returns 'int 5
            function bar() returns 'bool 5
            "#,
            expect![[r#""#]],
        );
    }
}

/*
use swim_ast::{Ast, AstNode, FunctionDeclaration};
pub struct TypeChecker {
    ast: Ast,
    ctx: polytype::Context,
}

pub struct TypedAst {
    nodes: Vec<AstNode>,
}

/*
pub struct Typed<T> {
    item: T,
    ty: Type,
}

*/

pub enum AstNode {
    FunctionDeclaration(TypedFunctionDeclaration),
    TypeDeclaration(TypedTypeDeclaration),
}

pub struct FunctionDeclaration {
    ty: Type,
    name: Identifier,
    args: Vec<TypedArgument>,
    ret_ty: Type,
}

pub struct TypeDeclaration {
    name: Identifier,
    variants: Vec<TypeVariant>,
}

pub struct Type {
    ty: polytype::TypeScheme,
}

impl TypeChecker {
    pub fn type_check(&mut self, node: AstNode) -> Typed<AstNode> {
        let ty = match node {
            AstNode::FunctionDeclaration(decl) => decl.type_check(&mut self.ctx),
            AstNode::TypeDeclaration(_) => todo!(),
        };
        Typed { item: node, ty }
    }
}

pub trait TypeCheckable {
    fn type_check(&self, ctx: &mut polytype::Context) -> Type;
}

impl TypeCheckable for FunctionDeclaration {
    fn type_check(&self, ctx: &mut polytype::Context) -> Type {
        let ty = ctx.new_type();
        ctx.bind(self.name, ty.clone());
        let ret_ty = self.ret_ty.type_check(ctx);
        let arg_tys = self.args.iter().map(|arg| arg.type_check(ctx)).collect();
        let ty = polytype::TypeScheme::new(arg_tys, ret_ty);
        Type { ty }
    }
}

*/
