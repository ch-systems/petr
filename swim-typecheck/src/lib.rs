use std::collections::BTreeMap;

use polytype::{tp, Type};
use swim_bind::TypeId;
use swim_utils::IndexMap;

struct TypeChecker {
    ctx: polytype::Context,
    type_map: BTreeMap<TypeId, TypeVariable>,
}

type TypeVariable = Type<&'static str>;

impl TypeChecker {
    fn from_resolved(resolved: swim_resolve::QueryableResolvedItems) -> Self {
        let mut ctx = polytype::Context::default();
        let mut type_map: BTreeMap<_, _> = Default::default();

        for (id, _ty) in resolved.types() {
            let ty = ctx.new_variable();
            type_map.insert(*id, ty);
        }

        Self { ctx, type_map }
    }
    fn fresh_ty_var(&mut self) -> TypeVariable {
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
                .get(&ty_id)
                .expect("type did not exist in type map")
                .clone(),
        }
    }
}

trait TypeCheck {
    type Output;
    fn type_check(&self, ctx: &mut TypeChecker) -> Self::Output;
}

impl TypeCheck for swim_resolve::Function {
    type Output = ();

    fn type_check(&self, ctx: &mut TypeChecker) -> Self::Output {
        // find the type variables for the parameters
        let parameter_types = self.params.iter().map(|(name, ty)| ctx.to_type_var(ty));

        // in a scope that contains the above names to type variables, check the body
        // TODO: introduce scopes here, like in the binder, except with type variables

        todo!()
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
