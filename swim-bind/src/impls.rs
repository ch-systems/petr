use swim_ast::{Commented, Expression, ExpressionWithBindings, FunctionDeclaration, TypeDeclaration};
use swim_utils::SpannedItem;

use crate::{Bind, Binder, Item};

impl Bind for TypeDeclaration {
    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        binder.insert_type(self);
    }
}

impl Bind for Expression {
    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        // only lists get their own scope for now
        match self {
            Expression::List(list) => {
                binder.with_scope(|binder, _scope_id| {
                    for item in list.elements.iter() {
                        item.bind(binder);
                    }
                });
            },
            Expression::Binding(ExpressionWithBindings { bindings, expression }) => {
                binder.with_scope(|binder, _scope_id| {
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
    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        self.item().bind(binder)
    }
}

impl<T: Bind> Bind for SpannedItem<T> {
    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        self.item().bind(binder)
    }
}
impl Bind for FunctionDeclaration {
    fn bind(
        &self,
        binder: &mut Binder,
    ) {
        binder.insert_function(self);
    }
}
