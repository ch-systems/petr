use petr_ast::{Commented, Expression, ExpressionWithBindings, FunctionDeclaration, ImportStatement, TypeDeclaration};
use petr_utils::{Identifier, SpannedItem};

use crate::{binder::ScopeKind, Bind, Binder, Item};

impl Bind for TypeDeclaration {
    type Output = Option<(Identifier, Item)>;

    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output {
        binder.insert_type(self)
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
            }) => {
                binder.with_scope(ScopeKind::ExpressionWithBindings, |binder, scope_id| {
                    for binding in bindings.iter() {
                        let binding_id = binder.insert_binding(binding.clone());
                        binder.insert_into_current_scope(binding.name.id, Item::Binding(binding_id));
                    }
                    // TODO: functions get inserted as Items with scopes, so we should probably
                    // insert bound expressions as an Item with their own scope, not sure how yet.
                    expression.bind(binder);
                    binder.insert_expression(*expr_id, scope_id);
                    //
                })
            },
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
impl Bind for FunctionDeclaration {
    type Output = Option<(Identifier, Item)>;

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
        let name = self
            .alias
            .unwrap_or_else(|| *self.path.identifiers.last().expect("should never be empty"));

        binder.insert_into_current_scope(name.id, item.clone());

        if self.is_exported() {
            Some((name, item))
        } else {
            None
        }
    }
}
