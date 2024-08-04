use petr_ast::{Commented, Expression, ExpressionWithBindings, FunctionDeclaration, ImportStatement, TypeDeclaration};
use petr_utils::{Identifier, SpannedItem, TypeId};

use crate::{binder::ScopeKind, Bind, Binder, FunctionId, ScopeId};

impl Bind for SpannedItem<&TypeDeclaration> {
    type Output = Option<(Identifier, TypeId)>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        binder.insert_type(self)
    }
}

impl Bind for SpannedItem<TypeDeclaration> {
    type Output = Option<(Identifier, TypeId)>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        binder.insert_type(&self.span().with_item(self.item()))
    }
}

impl Bind for Expression {
    // the scope that the expression lives in
    type Output = ();

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        // only lists get their own scope for now
        match self {
            Expression::List(list) => {
                list.bind(binder);
            },
            Expression::Binding(ExpressionWithBindings {
                bindings,
                expression,
                expr_id,
            }) => binder.with_scope(ScopeKind::ExpressionWithBindings, |binder, scope_id| {
                for binding in bindings.iter() {
                    binder.insert_binding_into_current_scope(binding.name.id, binding.name.span().with_item(binding.clone()));
                }
                expression.bind(binder);
                binder.insert_expression(*expr_id, scope_id);
            }),
            Expression::Literal(_) => (),
            Expression::Operator(op) => {
                op.lhs.bind(binder);
                op.rhs.bind(binder);
            },
            Expression::FunctionCall(f) => {
                // bind all of the arguments
                for arg in f.args.iter() {
                    arg.bind(binder);
                }
            },
            Expression::Variable(_) => (),
            Expression::IntrinsicCall(i) => {
                // bind all of the arguments
                for arg in i.args.iter() {
                    arg.bind(binder);
                }
            },
            Expression::TypeConstructor(_, expr) => {
                for arg in expr.iter() {
                    arg.bind(binder);
                }
            },
            Expression::If(i) => {
                i.condition.bind(binder);
                i.then_branch.bind(binder);
                if let Some(ref else_branch) = i.else_branch {
                    else_branch.bind(binder);
                }
            },
        }
    }
}

impl Bind for petr_ast::List {
    type Output = ();

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        for item in self.elements.iter() {
            item.bind(binder);
        }
    }
}

impl<T: Bind> Bind for Commented<T> {
    type Output = T::Output;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        self.item().bind(binder)
    }
}

impl<T: Bind> Bind for SpannedItem<T> {
    type Output = T::Output;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        self.item().bind(binder)
    }
}

impl Bind for SpannedItem<FunctionDeclaration> {
    type Output = Option<(Identifier, (FunctionId, ScopeId))>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        binder.insert_function(&self.span().with_item(self.item()))
    }
}

impl Bind for SpannedItem<&FunctionDeclaration> {
    type Output = Option<(Identifier, (FunctionId, ScopeId))>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        binder.insert_function(self)
    }
}

impl Bind for ImportStatement {
    type Output = Option<(Identifier, crate::binder::ImportStatement)>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        // the alias, if any, or the last path element if there is no alias
        let name = self.alias.unwrap_or_else(|| *self.path.iter().last().expect("should never be empty"));

        let import = crate::binder::ImportStatement {
            path:  self.path.clone(),
            alias: self.alias,
        };

        binder.insert_import_into_current_scope(name.id, name.span.with_item(import.clone()));

        if self.is_exported() {
            Some((name, import))
        } else {
            None
        }
    }
}
