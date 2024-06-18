use std::collections::BTreeMap;

use swim_ast::{Ast, Expression, FunctionDeclaration, FunctionParameter, Ty, TypeDeclaration};
use swim_utils::{idx_map_key, Identifier, IndexMap, Path, SymbolId};
// TODO:
// i don't know if type cons needs a scope. Might be good to remove that.

idx_map_key!(
    /// The ID type of a Scope in the Binder.
    ScopeId
);

idx_map_key!(
    /// The ID type of a functoin parameter
    FunctionParameterId
);

idx_map_key!(
    /// The ID type of an Expr.
   ExprId
);

idx_map_key!(
    /// The ID type of a function.
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
    /// The ID type of a module.
   ModuleId
);

#[derive(Clone, Debug)]
pub enum Item {
    Binding(BindingId),
    // the `ScopeId` is the scope of the function body
    Function(FunctionId, ScopeId),
    Type(TypeId),
    FunctionParameter(Ty),
    Module(ModuleId),
    Import { path: Box<[Identifier]>, alias: Option<Identifier> },
}

pub struct Binder {
    scopes:      IndexMap<ScopeId, Scope<Item>>,
    scope_chain: Vec<ScopeId>,
    bindings:    IndexMap<BindingId, Expression>,
    functions:   IndexMap<FunctionId, FunctionDeclaration>,
    types:       IndexMap<TypeId, TypeDeclaration>,
    modules:     IndexMap<ModuleId, Module>,
    root_scope:  ScopeId,
}

#[derive(Debug)]
pub struct Module {
    pub root_scope: ScopeId,
    pub exports:    BTreeMap<Identifier, Item>,
}

pub struct Scope<T> {
    parent: Option<ScopeId>,
    items:  BTreeMap<SymbolId, T>,
    kind:   ScopeKind,
}

/// Mainly just used for debugging what generated this scope.
#[derive(Clone, Copy, Debug)]
pub enum ScopeKind {
    Module(Identifier),
    Function,
    Root,
    TypeConstructor,
    // an expression with `let`s
    ExpressionWithBindings,
}

impl<T> Scope<T> {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            parent: None,
            items: BTreeMap::new(),
            kind,
        }
    }

    pub fn insert(
        &mut self,
        k: SymbolId,
        v: T,
    ) {
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
        let mut scopes = IndexMap::default();
        let root_scope = Scope {
            parent: None,
            items:  Default::default(),
            kind:   ScopeKind::Root,
        };
        let root_scope = scopes.insert(root_scope);
        Self {
            scopes,
            scope_chain: vec![root_scope],
            root_scope,
            functions: IndexMap::default(),
            types: IndexMap::default(),
            bindings: IndexMap::default(),
            modules: IndexMap::default(),
        }
    }

    pub fn get_function(
        &self,
        function_id: FunctionId,
    ) -> &FunctionDeclaration {
        self.functions.get(function_id)
    }

    pub fn get_type(
        &self,
        type_id: TypeId,
    ) -> &TypeDeclaration {
        self.types.get(type_id)
    }

    /// Searches for a symbol in a scope or any of its parents
    pub fn find_symbol_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<&Item> {
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

    pub fn insert_into_current_scope(
        &mut self,
        name: SymbolId,
        item: Item,
    ) {
        let scope_id = self.scope_chain.last().expect("there's always at least one scope");
        self.scopes.get_mut(*scope_id).insert(name, item);
    }

    fn push_scope(
        &mut self,
        kind: ScopeKind,
    ) -> ScopeId {
        let parent_id = self.scope_chain.last().cloned();

        let id = self.scopes.insert(Scope {
            parent: parent_id,
            ..Scope::new(kind)
        });

        self.scope_chain.push(id);

        id
    }

    fn pop_scope(&mut self) {
        let _ = self.scope_chain.pop();
    }

    pub fn with_scope<F, R>(
        &mut self,
        kind: ScopeKind,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self, ScopeId) -> R,
    {
        let id = self.push_scope(kind);
        let res = f(self, id);
        self.pop_scope();
        res
    }

    /// TODO (https://github.com/sezna/swim/issues/33)
    pub(crate) fn insert_type(
        &mut self,
        ty_decl: &TypeDeclaration,
    ) -> Option<(Identifier, Item)> {
        // insert a function binding for every constructor
        // and a type binding for the parent type
        let type_id = self.types.insert(ty_decl.clone());
        let type_item = Item::Type(type_id);
        self.insert_into_current_scope(ty_decl.name.id, type_item.clone());

        ty_decl.variants.iter().for_each(|variant| {
            let span = variant.span();
            let variant = variant.item();
            let (fields_as_parameters, func_scope) = self.with_scope(ScopeKind::TypeConstructor, |_, scope| {
                (
                    variant
                        .fields
                        .iter()
                        .map(|field| {
                            swim_ast::FunctionParameter {
                                // TODO: don't just use the parent variant name
                                name: variant.name,
                                ty:   *field,
                            }
                        })
                        .collect::<Vec<_>>(),
                    scope,
                )
            });

            let function = FunctionDeclaration {
                name:        variant.name,
                parameters:  fields_as_parameters.into_boxed_slice(),
                return_type: Ty::Named(ty_decl.name),
                body:        span.with_item(Expression::TypeConstructor),
                visibility:  ty_decl.visibility,
            };

            let function_id = self.functions.insert(function);
            self.insert_into_current_scope(variant.name.id, Item::Function(function_id, func_scope));
        });
        if ty_decl.is_exported() {
            Some((ty_decl.name, type_item))
        } else {
            None
        }
    }

    pub(crate) fn insert_function(
        &mut self,
        arg: &FunctionDeclaration,
    ) -> Option<(Identifier, Item)> {
        let function_id = self.functions.insert(arg.clone());
        let func_body_scope = self.with_scope(ScopeKind::Function, |binder, function_body_scope| {
            for param in arg.parameters.iter() {
                binder.insert_into_current_scope(param.name.id, Item::FunctionParameter(param.ty));
            }
            function_body_scope
        });
        let item = Item::Function(function_id, func_body_scope);
        self.insert_into_current_scope(arg.name.id, item.clone());
        if arg.is_exported() {
            Some((arg.name, item))
        } else {
            None
        }
    }

    pub(crate) fn insert_binding(
        &mut self,
        binding: Expression,
    ) -> BindingId {
        self.bindings.insert(binding)
    }

    pub fn from_ast(ast: &Ast) -> Self {
        let mut binder = Self::new();

        ast.modules.iter().for_each(|module| {
            let module_scope = binder.create_scope_from_path(&module.name);
            binder.with_specified_scope(module_scope, |binder, scope_id| {
                let exports = module.nodes.iter().filter_map(|node| match node.item() {
                    swim_ast::AstNode::FunctionDeclaration(decl) => decl.bind(binder),
                    swim_ast::AstNode::TypeDeclaration(decl) => decl.bind(binder),
                    swim_ast::AstNode::ImportStatement(stmt) => stmt.bind(binder),
                });
                let exports = BTreeMap::from_iter(exports);
                let module_id = binder.modules.insert(Module {
                    root_scope: scope_id,
                    exports,
                });
            });
        });

        binder
    }

    fn create_scope_from_path(
        &mut self,
        path: &Path,
    ) -> ScopeId {
        // given a path, create a scope for each segment. The last scope is returned.
        // e.g. for the path "a.b.c", create scopes for "a", "b", and "c", and return the scope for "c"
        let mut current_scope_id = *self.scope_chain.last().expect("there's always one scope: invariant");
        for segment in path.identifiers.iter() {
            let next_scope = self.create_orphan_scope(ScopeKind::Module(*segment));
            let module = Module {
                root_scope: next_scope,
                exports:    BTreeMap::new(),
            };
            let module_id = self.modules.insert(module);
            self.insert_into_specified_scope(current_scope_id, *segment, Item::Module(module_id));
            current_scope_id = next_scope
        }
        current_scope_id
    }

    pub fn insert_into_specified_scope(
        &mut self,
        scope: ScopeId,
        name: Identifier,
        item: Item,
    ) {
        let scope = self.scopes.get_mut(scope);
        scope.insert(name.id, item);
    }

    pub fn get_module(
        &self,
        id: ModuleId,
    ) -> &Module {
        self.modules.get(id)
    }

    pub fn get_binding(
        &self,
        binding_id: BindingId,
    ) -> &Expression {
        self.bindings.get(binding_id)
    }

    pub fn create_orphan_scope(
        &mut self,
        kind: ScopeKind,
    ) -> ScopeId {
        self.scopes.insert(Scope::new(kind))
    }

    // // TODO(sezna) replace "scope_chain.last().expect()" with "self.current_scope()" which doesn't return an option
    // fn insert_module_into_current_scope(
    //     &mut self,
    //     name: swim_utils::Path,
    //     module_item: Item,
    // ) {
    //     let mut current_parent_id = *self.scope_chain.last().expect("there's always at least one scope in the chain");
    //     for (ix, part) in name.identifiers.iter().enumerate() {
    //         let is_last = ix == name.identifiers.len() - 1;
    //         let this_scope_id = self.create_orphan_scope(ScopeKind::Module(*part));
    //         let this_scope = self.scopes.get_mut(this_scope_id);
    //         let module = if is_last {
    //             module_item.clone()
    //         } else {
    //             Item::Module(self.modules.insert(Module {
    //                 root_scope: this_scope_id,
    //                 exports:    BTreeMap::new(),
    //             }))
    //         };
    //         this_scope.parent = Some(current_parent_id);
    //         let current_parent = self.scopes.get_mut(current_parent_id);
    //         current_parent.items.insert(part.id, module);
    //         current_parent_id = this_scope_id;
    //     }
    // }

    fn with_specified_scope<F, R>(
        &mut self,
        scope: ScopeId,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self, ScopeId) -> R,
    {
        let old_scope_chain = self.scope_chain.clone();
        self.scope_chain = vec![self.root_scope, scope];
        let res = f(self, scope);
        self.scope_chain = old_scope_chain;
        res
    }
}

pub trait Bind {
    type Output;
    fn bind(
        &self,
        binder: &mut Binder,
    ) -> Self::Output;
}

#[cfg(test)]
mod tests {
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = swim_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let binder = Binder::from_ast(&ast);
        let result = pretty_print_bindings(&binder, &interner);
        expect.assert_eq(&result);
    }

    use expect_test::{expect, Expect};
    use swim_utils::{render_error, SymbolInterner};

    use super::*;
    fn pretty_print_bindings(
        binder: &Binder,
        interner: &SymbolInterner,
    ) -> String {
        let mut result = String::new();
        result.push_str("__Scopes__\n");
        for (scope_id, scope) in binder.scopes.iter() {
            result.push_str(&format!(
                "{}: {} (parent {}):\n",
                Into::<usize>::into(scope_id),
                match scope.kind {
                    ScopeKind::Module(name) => format!("Module {}", interner.get(name.id)),
                    ScopeKind::Function => "Function".into(),
                    ScopeKind::Root => "Root".into(),
                    ScopeKind::TypeConstructor => "Type Cons".into(),
                    ScopeKind::ExpressionWithBindings => "Expr w/ Bindings".into(),
                },
                scope.parent.map(|x| x.to_string()).unwrap_or_else(|| "none".into())
            ));
            for (symbol_id, item) in &scope.items {
                let symbol_name = interner.get(*symbol_id);
                let item_description = match item {
                    Item::Binding(bind_id) => format!("Binding {:?}", bind_id),
                    Item::Function(function_id, _function_scope) => {
                        format!("Function {:?}", function_id)
                    },
                    Item::Type(type_id) => format!("Type {:?}", type_id),
                    Item::FunctionParameter(param) => {
                        format!("FunctionParameter {:?}", param)
                    },
                    Item::Module(a) => {
                        format!("Module {:?}", binder.modules.get(*a))
                    },
                    Item::Import { path, alias } => todo!(),
                };
                result.push_str(&format!("  {}: {}\n", symbol_name, item_description));
            }
        }
        result
    }

    #[test]
    fn bind_type_decl() {
        check(
            "type trinary_boolean = True | False | maybe ",
            expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent none):
                  trinary_boolean: Type TypeId(0)
                  True: Function FunctionId(0)
                  False: Function FunctionId(1)
                  maybe: Function FunctionId(2)
                2: Type Cons (parent scopeid1):
                3: Type Cons (parent scopeid1):
                4: Type Cons (parent scopeid1):
            "#]],
        );
    }
    #[test]
    fn bind_function_decl() {
        check(
            "function add(a in 'Int, b in 'Int) returns 'Int + 1 2",
            expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent none):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3) })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3) })
            "#]],
        );
    }

    #[test]
    fn bind_list_new_scope() {
        check(
            "function add(a in 'Int, b in  'Int) returns 'Int [ 1, 2, 3, 4, 5, 6 ]",
            expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent none):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3) })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3) })
            "#]],
        );
    }
}
