use petr_bind::FunctionId;
use petr_resolve::{Expr, ExprKind, Literal};
use petr_utils::{Identifier, Span};

use crate::{
    constraint_generation::{GenerateTypeConstraints, TypeConstraintContext},
    types::SpecificType,
    TypeVariable,
};

#[derive(Clone)]
pub enum Intrinsic {
    Puts(Box<TypedExpr>),
    Add(Box<TypedExpr>, Box<TypedExpr>),
    Multiply(Box<TypedExpr>, Box<TypedExpr>),
    Divide(Box<TypedExpr>, Box<TypedExpr>),
    Subtract(Box<TypedExpr>, Box<TypedExpr>),
    Malloc(Box<TypedExpr>),
    SizeOf(Box<TypedExpr>),
    Equals(Box<TypedExpr>, Box<TypedExpr>),
}

impl std::fmt::Debug for Intrinsic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Intrinsic::Puts(expr) => write!(f, "@puts({:?})", expr),
            Intrinsic::Add(lhs, rhs) => write!(f, "@add({:?}, {:?})", lhs, rhs),
            Intrinsic::Multiply(lhs, rhs) => write!(f, "@multiply({:?}, {:?})", lhs, rhs),
            Intrinsic::Divide(lhs, rhs) => write!(f, "@divide({:?}, {:?})", lhs, rhs),
            Intrinsic::Subtract(lhs, rhs) => write!(f, "@subtract({:?}, {:?})", lhs, rhs),
            Intrinsic::Malloc(size) => write!(f, "@malloc({:?})", size),
            Intrinsic::SizeOf(expr) => write!(f, "@sizeof({:?})", expr),
            Intrinsic::Equals(lhs, rhs) => write!(f, "@equal({:?}, {:?})", lhs, rhs),
        }
    }
}

#[derive(Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub span: Span,
}

impl TypedExpr {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub enum TypedExprKind {
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
    ErrorRecovery(Span),
    ExprWithBindings {
        bindings:   Vec<(Identifier, TypedExpr)>,
        expression: Box<TypedExpr>,
    },
    TypeConstructor {
        ty:   TypeVariable,
        args: Box<[TypedExpr]>,
    },
    If {
        condition:   Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
}

impl std::fmt::Debug for TypedExpr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        use TypedExprKind::*;
        match &self.kind {
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
            ErrorRecovery(span) => {
                write!(f, "error recovery {span:?}")
            },
            ExprWithBindings { bindings, expression } => {
                write!(f, "bindings: ")?;
                for (name, expr) in bindings {
                    write!(f, "{}: {:?}, ", name.id, expr)?;
                }
                write!(f, "expression: {:?}", expression)
            },
            TypeConstructor { ty, .. } => write!(f, "type constructor: {:?}", ty),
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {:?} then {:?} else {:?}", condition, then_branch, else_branch)
            },
        }
    }
}

impl GenerateTypeConstraints for Expr {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeConstraintContext,
    ) -> Self::Output {
        let kind = match &self.kind {
            ExprKind::Literal(lit) => {
                let ty = ctx.convert_literal_to_type(lit);
                TypedExprKind::Literal { value: lit.clone(), ty }
            },
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    let ty = ctx.unit();
                    TypedExprKind::List { elements: vec![], ty }
                } else {
                    let type_checked_exprs = exprs.iter().map(|expr| expr.type_check(ctx)).collect::<Vec<_>>();
                    // unify the type of the first expr against everything else in the list
                    let first_ty = ctx.expr_ty(&type_checked_exprs[0]);
                    for expr in type_checked_exprs.iter().skip(1) {
                        let second_ty = ctx.expr_ty(expr);
                        ctx.unify(first_ty, second_ty, expr.span());
                    }
                    let first_ty = ctx.types().get(first_ty).clone();
                    TypedExprKind::List {
                        elements: type_checked_exprs,
                        ty:       ctx.insert_type::<SpecificType>(&SpecificType::List(Box::new(first_ty))),
                    }
                }
            },
            ExprKind::FunctionCall(call) => (*call).type_check(ctx),
            ExprKind::Unit => TypedExprKind::Unit,
            ExprKind::ErrorRecovery => TypedExprKind::ErrorRecovery(self.span),
            ExprKind::Variable { name, ty } => {
                // look up variable in scope
                // find its expr return type
                let var_ty = ctx.find_variable(*name).expect("variable not found in scope");
                let ty = ctx.to_type_var(ty);

                ctx.unify(var_ty, ty, name.span());

                TypedExprKind::Variable { ty, name: *name }
            },
            ExprKind::Intrinsic(intrinsic) => return self.span.with_item(intrinsic.clone()).type_check(ctx),
            ExprKind::TypeConstructor(parent_type_id, args) => {
                // This ExprKind only shows up in the body of type constructor functions, and
                // is basically a noop. The surrounding function decl will handle type checking for
                // the type constructor.
                let args = args.iter().map(|arg| arg.type_check(ctx)).collect::<Vec<_>>();
                let ty = ctx.get_type(*parent_type_id);
                TypedExprKind::TypeConstructor {
                    ty:   *ty,
                    args: args.into_boxed_slice(),
                }
            },
            ExprKind::ExpressionWithBindings { bindings, expression } => {
                // for each binding, type check the rhs
                ctx.with_type_scope(|ctx| {
                    let mut type_checked_bindings = Vec::with_capacity(bindings.len());
                    for binding in bindings {
                        let binding_ty = binding.expression.type_check(ctx);
                        let binding_expr_return_ty = ctx.expr_ty(&binding_ty);
                        ctx.insert_variable(binding.name, binding_expr_return_ty);
                        type_checked_bindings.push((binding.name, binding_ty));
                    }

                    TypedExprKind::ExprWithBindings {
                        bindings:   type_checked_bindings,
                        expression: Box::new(expression.type_check(ctx)),
                    }
                })
            },
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = condition.type_check(ctx);
                let condition_ty = ctx.expr_ty(&condition);
                ctx.unify(ctx.bool(), condition_ty, condition.span());

                let then_branch = then_branch.type_check(ctx);
                let then_ty = ctx.expr_ty(&then_branch);

                let else_branch = else_branch.type_check(ctx);
                let else_ty = ctx.expr_ty(&else_branch);

                ctx.unify(else_ty, then_ty, else_branch.span());

                TypedExprKind::If {
                    condition:   Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                }
            },
        };

        TypedExpr { kind, span: self.span }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name:      Identifier,
    pub params:    Vec<(Identifier, TypeVariable)>,
    pub body:      TypedExpr,
    pub return_ty: TypeVariable,
}
