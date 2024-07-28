use petr_ast::{Commented, Expression, ExpressionWithBindings, FunctionDeclaration, ImportStatement, TypeDeclaration};
use petr_utils::{Identifier, SpannedItem, TypeId};

use crate::{binder::ScopeKind, Bind, Binder, FunctionId, Item, ScopeId};

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
            _ => (),
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
    type Output = Option<(Identifier, Item)>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        let item = Item::Import {
            path:  self.path.clone(),
            alias: self.alias,
        };

        // the alias, if any, or the last path element if there is no alias
        let name = self.alias.unwrap_or_else(|| *self.path.iter().last().expect("should never be empty"));

        binder.insert_import_into_current_scope(name.id, name.span.with_item(item.clone()));

        if self.is_exported() {
            Some((name, item))
        } else {
            None
        }
    }
}
