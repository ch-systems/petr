//! Binds symbols from the AST into a symbol map from ID to a node representation.

mod impls {

    use crate::{Bind, Binder};

    use swim_ast::{
        Ast, Commented, Expression, FunctionDeclaration, Ty, TypeDeclaration, TypeVariant,
    };
    use swim_utils::{idx_map_key, IndexMap, SpannedItem, SymbolId, SymbolInterner};

    impl Bind for TypeDeclaration {
        fn bind(&self, binder: &mut Binder) {
            binder.insert_type(self);
        }
    }

    impl Bind for Expression {
        fn bind(&self, binder: &mut Binder) {
            // only lists get their own scope for now
            match self {
                Expression::List(list) => {
                    binder.with_scope(|binder, scope_id| {
                        for item in list.elements.iter() {
                            item.bind(binder);
                        }
                    });
                }
                _ => self.bind(binder),
            }
        }
    }

    impl<T: Bind> Bind for Commented<T> {
        fn bind(&self, binder: &mut Binder) {
            self.item().bind(binder)
        }
    }

    impl<T: Bind> Bind for SpannedItem<T> {
        fn bind(&self, binder: &mut Binder) {
            self.item().bind(binder)
        }
    }
    impl Bind for FunctionDeclaration {
        fn bind(&self, binder: &mut Binder) {
            binder.insert_function(self);
        }
    }
}

pub use binder::{Bind, Binder, FunctionId, Item, Scope, ScopeId, TypeId};
mod binder {
    use std::collections::BTreeMap;

    use swim_ast::{Ast, Expression, FunctionDeclaration, FunctionParameter, Ty, TypeDeclaration};
    use swim_utils::{idx_map_key, IndexMap, SpannedItem, SymbolId, SymbolInterner};

    idx_map_key!(
        /// The ID type of a Scope in the Binder.
        ScopeId
    );

    idx_map_key!(
        /// The ID type of an Expr.
       ExprId
    );

    idx_map_key!(
        /// The ID type of an Expr.
        FunctionId
    );

    idx_map_key!(
        /// The ID type of a variable binding.
       BindingId
    );

    idx_map_key!(
        /// The ID type of a type declaration.
        TypeId
    );

    idx_map_key!(
        /// The ID type of a bound node.
       NodeId
    );

    /// Tracks what scope an item is in.
    /// The resolver will iterate over these to resolve symbols.
    #[derive(Clone, Debug)]
    pub struct Node {
        pub item: Item,
        pub scope: ScopeId,
    }

    #[derive(Clone, Debug)]
    pub enum Item {
        Expr(ExprId),
        Function(FunctionId),
        Type(TypeId),
    }

    pub struct Binder {
        scopes: IndexMap<ScopeId, Scope<Item>>,
        scope_chain: Vec<ScopeId>,
        //    bindings: IndexMap<BindingId, Binding>,
        functions: IndexMap<FunctionId, FunctionDeclaration>,
        types: IndexMap<TypeId, TypeDeclaration>,
        /// The processed nodes that have been bound. They no longer contain things like
        /// `SpannedItem` or `Commented` -- they're further from the language syntactically than
        /// the AST, which directly represents the syntax.
        /// Not a fully semantic representation, but a step closer. Serves to tee up the resolver for
        /// an easy job.
        nodes: IndexMap<NodeId, Node>,
    }

    pub struct Scope<T> {
        parent: Option<ScopeId>,
        items: BTreeMap<SymbolId, T>,
    }

    impl<T> Scope<T> {
        pub fn new() -> Self {
            Self {
                parent: None,
                items: BTreeMap::new(),
            }
        }

        pub fn insert(&mut self, k: SymbolId, v: T) {
            self.items.insert(k, v);
        }

        pub fn parent(&self) -> Option<ScopeId> {
            self.parent
        }

        pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &T)> {
            self.items.iter()
        }
    }

    impl Binder {
        fn new() -> Self {
            Self {
                scopes: IndexMap::default(),
                scope_chain: Vec::new(),
                functions: IndexMap::default(),
                types: IndexMap::default(),
                nodes: IndexMap::default(),
            }
        }

        pub fn get_function(&self, function_id: FunctionId) -> &FunctionDeclaration {
            self.functions.get(function_id)
        }

        pub fn get_type(&self, type_id: TypeId) -> &TypeDeclaration {
            self.types.get(type_id)
        }

        /// Searches for a symbol in a scope or any of its parents
        pub fn find_symbol_in_scope(&self, name: SymbolId, scope_id: ScopeId) -> Option<&Item> {
            let scope = self.scopes.get(scope_id);
            if let Some(item) = scope.items.get(&name) {
                return Some(item);
            }

            if let Some(parent_id) = scope.parent() {
                return self.find_symbol_in_scope(name, parent_id);
            }

            None
        }

        /// Iterate over all scopes in the binder.
        pub fn scope_iter(&self) -> impl Iterator<Item = (ScopeId, &Scope<Item>)> {
            self.scopes.iter()
        }

        pub fn insert_into_current_scope(&mut self, name: SymbolId, item: Item) {
            let scope_id = self
                .scope_chain
                .last()
                .expect("there's always at least one scope");
            self.scopes.get_mut(*scope_id).insert(name, item);
        }

        fn push_scope(&mut self) -> ScopeId {
            let parent_id = self.scope_chain.last().cloned();

            let id = self.scopes.insert(Scope {
                parent: parent_id,
                ..Scope::new()
            });

            self.scope_chain.push(id);

            id
        }

        fn pop_scope(&mut self) {
            let _ = self.scope_chain.pop();
        }

        pub fn with_scope<F, R>(&mut self, f: F) -> R
        where
            F: FnOnce(&mut Self, ScopeId) -> R,
        {
            let id = self.push_scope();
            let res = f(self, id);
            self.pop_scope();
            res
        }

        pub(crate) fn insert_type(&mut self, ty_decl: &TypeDeclaration) {
            // insert a function binding for every constructor
            // and a type binding for the parent type
            let type_id = self.types.insert(ty_decl.clone());
            self.insert_into_current_scope(ty_decl.name.id, Item::Type(type_id));

            ty_decl.variants.iter().for_each(|variant| {
                let span = variant.span();
                let variant = variant.item();
                let fields_as_parameters = variant
                    .fields
                    .iter()
                    .map(|field| FunctionParameter {
                        // TODO: don't just use the parent variant name
                        name: variant.name,
                        ty: *field,
                    })
                    .collect::<Vec<_>>();

                let function = FunctionDeclaration {
                    name: variant.name.clone(),
                    parameters: fields_as_parameters.into_boxed_slice(),
                    return_type: Ty::Named(ty_decl.name),
                    body: span.with_item(Expression::TypeConstructor),
                    visibility: ty_decl.visibility,
                };

                let function_id = self.functions.insert(function);
                self.insert_into_current_scope(variant.name.id, Item::Function(function_id));
            });
        }

        pub(crate) fn insert_function(&mut self, arg: &FunctionDeclaration) {
            let function_id = self.functions.insert(arg.clone());
            self.insert_into_current_scope(arg.name.id, Item::Function(function_id));
        }
    }

    impl Binder {
        pub fn from_ast(ast: &Ast) -> Self {
            let mut binder = Self::new();

            binder.with_scope(|binder, scope_id| {
                for node in &ast.nodes {
                    match node.item() {
                        swim_ast::AstNode::FunctionDeclaration(decl) => decl.bind(binder),
                        swim_ast::AstNode::TypeDeclaration(decl) => decl.bind(binder),
                    }
                }
            });

            binder
        }
    }

    pub trait Bind {
        fn bind(&self, binder: &mut Binder);
    }

    #[cfg(test)]
    mod tests {
        fn check(input: impl Into<String>, expect: Expect) {
            let input = input.into();
            let parser = swim_parse::Parser::new(vec![("test", input)]);
            let (ast, errs, interner, source_map) = parser.into_result();
            if !errs.is_empty() {
                errs.into_iter()
                    .for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
                panic!("fmt failed: code didn't parse");
            }
            let binder = Binder::from_ast(&ast);
            let result = pretty_print_bindings(&binder, &interner);
            expect.assert_eq(&result);
        }

        use expect_test::{expect, Expect};
        use swim_utils::render_error;

        use super::*;
        fn pretty_print_bindings(binder: &Binder, interner: &SymbolInterner) -> String {
            let mut result = String::new();
            for (scope_id, scope) in binder.scopes.iter() {
                result.push_str(&format!("Scope {:?}:\n", scope_id));
                for (symbol_id, item) in &scope.items {
                    let symbol_name = interner.get(*symbol_id);
                    let item_description = match item {
                        Item::Expr(expr_id) => format!("Expr {:?}", expr_id),
                        Item::Function(function_id) => format!("Function {:?}", function_id),
                        Item::Type(type_id) => format!("Type {:?}", type_id),
                    };
                    result.push_str(&format!("  {}: {}\n", symbol_name, item_description));
                }
            }
            result
        }

        #[test]
        fn bind_type_decl() {
            check(
                "type trinary_boolean = true | false | maybe ",
                expect![[r#"
                    Scope ScopeId(0):
                      trinary_boolean: Type TypeId(0)
                      true: Function FunctionId(0)
                      false: Function FunctionId(1)
                      maybe: Function FunctionId(2)
                "#]],
            );
        }
        #[test]
        fn bind_function_decl() {
            check(
                "function add(a in 'Int, b in  'Int) returns 'Int + a b",
                expect![[r#"
                    Scope ScopeId(0):
                      add: Function FunctionId(0)
                    "#]],
            );
        }

        #[test]
        fn bind_list_new_scope() {
            check(
                "function add(a in 'Int, b in  'Int) returns 'Int [ 1, 2, 3, 4, 5, 6 ]",
                expect![[r#"
                    Scope ScopeId(0):
                      add: Function FunctionId(0)
                    "#]],
            );
        }
    }
}
