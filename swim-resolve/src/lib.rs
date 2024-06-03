//! given bindings, fully resolve an AST

use std::{collections::BTreeMap, rc::Rc};

use swim_ast::{Ast, AstNode, Commented, FunctionParameter, TypeDeclaration};
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

struct ResolvedNode {
    kind: ResolvedNodeKind,
}

pub enum ResolvedNodeKind {
    Function(Function),
    Expr(Expr),
}

// TODO: refactor this into polytype
pub enum Type {
    Int,
    Bool,
    Unit,
    Named(TypeId),
}

impl Resolve for swim_ast::Ty {
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        let ty = match self {
            swim_ast::Ty::Int => Type::Int,
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
        };
        todo!("should trait return associated type, or should everything be a resolved node?");
        todo!("if we keep resolved nodes, then we have a TON of flexibility in resolving and handling items -- too much? Extra error handling down the line")
    }
}

pub struct Function {
    pub name: Identifier,
    pub params: Vec<(Identifier, Type)>,
    pub return_type: Type,
    pub body: Expr,
}

pub struct Expr;

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
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode>;
}

impl Resolve for AstNode {
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        match self {
            swim_ast::AstNode::FunctionDeclaration(decl) => decl.resolve(resolver, scope_id),
            swim_ast::AstNode::TypeDeclaration(decl) => decl.resolve(resolver, scope_id),
        }
    }
}

impl Resolve for swim_ast::FunctionDeclaration {
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        // when resolving a function declaration,
        // the symbols that need resolution are:
        // - the names of the types in parameters
        // - the return type
        // - the body

        let mut params_buf = Vec::with_capacity(self.parameters.len());

        for FunctionParameter { name, ty } in self.parameters.iter() {
            let ty = ty.resolve(resolver, scope_id).unwrap_or(Type::Unit);
            params_buf.push((name, ty));
        }

        todo!()
    }
}

impl<T: Resolve> Resolve for Commented<T> {
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        self.item().resolve(resolver)
    }
}

impl<T: Resolve> Resolve for SpannedItem<T> {
    fn resolve(&self, resolver: &mut Resolver, scope_id: ScopeId) -> Option<ResolvedNode> {
        self.item().resolve(resolver)
    }
}

impl Resolve for TypeDeclaration {
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
