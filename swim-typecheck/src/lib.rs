//! Given an `Ast`, produce a type-checked `TypedAst`.
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
