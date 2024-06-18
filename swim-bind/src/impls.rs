use swim_ast::{Commented, Expression, ExpressionWithBindings, FunctionDeclaration, ImportStatement, TypeDeclaration};
use swim_utils::{Identifier, SpannedItem};

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
    type Output = ();

    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        // only lists get their own scope for now
        match self {
            Expression::List(list) => {
                for item in list.elements.iter() {
                    item.bind(binder);
                }
            },
            Expression::Binding(ExpressionWithBindings { bindings, expression }) => {
                binder.with_scope(ScopeKind::ExpressionWithBindings, |binder, _scope_id| {
                    for binding in bindings.iter() {
                        let binding_id = binder.insert_binding(*expression.clone());
                        binder.insert_into_current_scope(binding.name.id, Item::Binding(binding_id));
                    }
                    expression.bind(binder);
                });
            },
            _ => self.bind(binder),
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
        let name = self.alias.unwrap_or_else(|| *self.path.last().expect("should never be empty"));

        binder.insert_into_current_scope(name.id, item.clone());

        if self.is_exported() {
            Some((name, item))
        } else {
            None
        }
    }
}
