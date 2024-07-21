use std::{collections::BTreeMap, rc::Rc};

use petr_ast::{dependency::Dependency, Ast, Binding, ExprId, Expression, FunctionDeclaration, Ty, TypeDeclaration};
use petr_utils::{idx_map_key, Identifier, IndexMap, Path, SpannedItem, SymbolId, SymbolInterner};
// TODO:
// - i don't know if type cons needs a scope. Might be good to remove that.
// - replace "scope_chain.last().expect()" with "self.current_scope()" which doesn't return an option

idx_map_key!(
    /// The ID type of a Scope in the Binder.
    ScopeId
);

idx_map_key!(
    /// The ID type of a functoin parameter
    FunctionParameterId
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
    /// The ID type of a module.
   ModuleId
);

#[derive(Clone, Debug)]
pub enum Item {
    Binding(BindingId),
    // the `ScopeId` is the scope of the function body
    Function(FunctionId, ScopeId),
    Type(petr_utils::TypeId),
    FunctionParameter(Ty),
    Module(ModuleId),
    Import { path: Path, alias: Option<Identifier> },
}

pub struct Binder {
    scopes:      IndexMap<ScopeId, Scope<SpannedItem<Item>>>,
    scope_chain: Vec<ScopeId>,
    /// Some expressions define their own scopes, like expressions with bindings
    // TODO rename to expr_scopes
    exprs: BTreeMap<ExprId, ScopeId>,
    bindings:    IndexMap<BindingId, Binding>,
    functions:   IndexMap<FunctionId, SpannedItem<FunctionDeclaration>>,
    types:       IndexMap<petr_utils::TypeId, TypeDeclaration>,
    modules:     IndexMap<ModuleId, Module>,
    root_scope:  ScopeId,
}

#[derive(Debug)]
pub struct Module {
    pub root_scope: ScopeId,
    pub exports:    BTreeMap<Identifier, Item>,
}

pub struct Scope<T> {
    /// A `Scope` always has a parent, unless it is the root scope of the user code.
    /// All scopes are descendents of one single root scope.
    parent: Option<ScopeId>,
    /// A mapping of the symbols that were declared in this scope. Note that any scopes that are
    /// children of this scope inherit these symbols as well.
    items:  BTreeMap<SymbolId, T>,
    #[allow(dead_code)]
    // this will be read but is also very useful for debugging
    kind: ScopeKind,
}

/// Not used in the compiler heavily yet, but extremely useful for understanding what kind of scope
/// you are in.
#[derive(Clone, Copy, Debug)]
pub enum ScopeKind {
    /// A module scope. This is the top level scope for a module.
    Module(Identifier),
    /// A function scope. This is the scope of a function body. Notably, function scopes are where
    /// all the function parameters are declared.
    Function,
    /// The root scope of the user code. There is only ever one ScopeKind::Root in a compilation.
    /// All scopes are descendents of the root.
    Root,
    /// This might not be needed -- the scope within a type constructor function.
    TypeConstructor,
    /// For a let... expression, this is the scope of the expression and its bindings.
    ExpressionWithBindings,
}

impl<T> Scope<T> {
    pub fn insert(
        &mut self,
        k: SymbolId,
        v: T,
    ) {
        // TODO: error handling and/or shadowing rules for this
        if self.items.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
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
            exprs: BTreeMap::new(),
        }
    }

    pub fn current_scope_id(&self) -> ScopeId {
        *self.scope_chain.last().expect("there's always at least one scope")
    }

    pub fn get_function(
        &self,
        function_id: FunctionId,
    ) -> &SpannedItem<FunctionDeclaration> {
        self.functions.get(function_id)
    }

    pub fn get_type(
        &self,
        type_id: petr_utils::TypeId,
    ) -> &TypeDeclaration {
        self.types.get(type_id)
    }

    /// Searches for a symbol in a scope or any of its parents
    pub fn find_symbol_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<&Item> {
        self.find_spanned_symbol_in_scope(name, scope_id).map(|item| item.item())
    }

    /// Searches for a symbol in a scope or any of its parents
    pub fn find_spanned_symbol_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<&SpannedItem<Item>> {
        let scope = self.scopes.get(scope_id);
        if let Some(item) = scope.items.get(&name) {
            return Some(item);
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_spanned_symbol_in_scope(name, parent_id);
        }

        None
    }

    /// Iterate over all scopes in the binder.
    pub fn scope_iter(&self) -> impl Iterator<Item = (ScopeId, &Scope<SpannedItem<Item>>)> {
        self.scopes.iter()
    }

    pub fn insert_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<Item>,
    ) {
        let scope_id = self.current_scope_id();
        self.scopes.get_mut(scope_id).insert(name, item);
    }

    fn push_scope(
        &mut self,
        kind: ScopeKind,
    ) -> ScopeId {
        let id = self.create_scope(kind);

        self.scope_chain.push(id);

        id
    }

    pub fn get_scope(
        &self,
        scope: ScopeId,
    ) -> &Scope<SpannedItem<Item>> {
        self.scopes.get(scope)
    }

    pub fn get_scope_kind(
        &self,
        scope: ScopeId,
    ) -> ScopeKind {
        self.scopes.get(scope).kind
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

    /// TODO (https://github.com/sezna/petr/issues/33)
    pub(crate) fn insert_type(
        &mut self,
        ty_decl: &SpannedItem<&TypeDeclaration>,
    ) -> Option<(Identifier, Item)> {
        // insert a function binding for every constructor
        // and a type binding for the parent type
        let type_id = self.types.insert((*ty_decl.item()).clone());
        let type_item = Item::Type(type_id);
        self.insert_into_current_scope(ty_decl.item().name.id, ty_decl.span().with_item(type_item.clone()));

        ty_decl.item().variants.iter().for_each(|variant| {
            let span = variant.span();
            let variant = variant.item();
            let (fields_as_parameters, _func_scope) = self.with_scope(ScopeKind::TypeConstructor, |_, scope| {
                (
                    variant
                        .fields
                        .iter()
                        .map(|field| petr_ast::FunctionParameter {
                            name: field.item().name,
                            ty:   field.item().ty,
                        })
                        .collect::<Vec<_>>(),
                    scope,
                )
            });
            // type constructors just access the arguments of the construction function directly
            let type_constructor_exprs = variant
                .fields
                .iter()
                .map(|field| field.span().with_item(Expression::Variable(field.item().name)))
                .collect::<Vec<_>>();

            let function = FunctionDeclaration {
                name:        variant.name,
                parameters:  fields_as_parameters.into_boxed_slice(),
                return_type: Ty::Named(ty_decl.item().name),
                body:        span.with_item(Expression::TypeConstructor(type_id, type_constructor_exprs.into_boxed_slice())),
                visibility:  ty_decl.item().visibility,
            };

            self.insert_function(&ty_decl.span().with_item(&function));
        });
        if ty_decl.item().is_exported() {
            Some((ty_decl.item().name, type_item))
        } else {
            None
        }
    }

    pub(crate) fn insert_function(
        &mut self,
        func: &SpannedItem<&FunctionDeclaration>,
    ) -> Option<(Identifier, Item)> {
        let span = func.span();
        let func = func.item();
        let function_id = self.functions.insert(span.with_item((*func).clone()));
        let func_body_scope = self.with_scope(ScopeKind::Function, |binder, function_body_scope| {
            for param in func.parameters.iter() {
                binder.insert_into_current_scope(param.name.id, param.name.span().with_item(Item::FunctionParameter(param.ty)));
            }

            func.body.bind(binder);
            function_body_scope
        });
        let item = Item::Function(function_id, func_body_scope);
        self.insert_into_current_scope(func.name.id, span.with_item(item.clone()));
        if func.is_exported() {
            Some((func.name, item))
        } else {
            None
        }
    }

    pub(crate) fn insert_binding(
        &mut self,
        binding: Binding,
    ) -> BindingId {
        self.bindings.insert(binding)
    }

    // TODO add optional prefix here:
    // if Some(p) then this is a dependency, and p should be prepended to the path of each module
    // If None then this is user code, and no prefix is needed
    pub fn from_ast(ast: &Ast) -> Self {
        let mut binder = Self::new();

        ast.modules.iter().for_each(|module| {
            let module_scope = binder.create_scope_from_path(&module.name);
            binder.with_specified_scope(module_scope, |binder, scope_id| {
                let exports = module.nodes.iter().filter_map(|node| match node.item() {
                    petr_ast::AstNode::FunctionDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                    petr_ast::AstNode::TypeDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                    petr_ast::AstNode::ImportStatement(stmt) => stmt.bind(binder),
                });
                let exports = BTreeMap::from_iter(exports);
                // we don't need to track this module ID -- it just needs to exist,
                // and all modules are iterated over in later stages of the compiler.
                // So we can safely ignore the return value here.
                let _module_id = binder.modules.insert(Module {
                    root_scope: scope_id,
                    exports,
                });
            });
        });

        binder
    }

    pub fn from_ast_and_deps(
        ast: &Ast,
        dependencies: Vec<Dependency>,
        interner: &mut SymbolInterner,
    ) -> Self {
        let mut binder = Self::new();

        for Dependency {
            key: _,
            name,
            dependencies: _,
            ast: dep_ast,
        } in dependencies
        {
            let span = dep_ast.span_pointing_to_beginning_of_ast();
            let id = interner.insert(Rc::from(name));
            let name = Identifier { id, span };
            let dep_scope = binder.create_scope_from_path(&Path::new(vec![name]));
            binder.with_specified_scope(dep_scope, |binder, _scope_id| {
                for module in dep_ast.modules {
                    let module_scope = binder.create_scope_from_path(&module.name);
                    binder.with_specified_scope(module_scope, |binder, scope_id| {
                        let exports = module.nodes.iter().filter_map(|node| match node.item() {
                            petr_ast::AstNode::FunctionDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                            petr_ast::AstNode::TypeDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                            petr_ast::AstNode::ImportStatement(stmt) => stmt.bind(binder),
                        });
                        let exports = BTreeMap::from_iter(exports);
                        // TODO do I need to track this module id?
                        let _module_id = binder.modules.insert(Module {
                            root_scope: scope_id,
                            exports,
                        });
                    });
                }
            })
        }

        for module in &ast.modules {
            let module_scope = binder.create_scope_from_path(&module.name);
            binder.with_specified_scope(module_scope, |binder, scope_id| {
                let exports = module.nodes.iter().filter_map(|node| match node.item() {
                    petr_ast::AstNode::FunctionDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                    petr_ast::AstNode::TypeDeclaration(decl) => node.span().with_item(decl.item()).bind(binder),
                    petr_ast::AstNode::ImportStatement(stmt) => stmt.bind(binder),
                });
                let exports = BTreeMap::from_iter(exports);
                // TODO do I need to track this module id?
                let _module_id = binder.modules.insert(Module {
                    root_scope: scope_id,
                    exports,
                });
            });
        }

        binder
    }

    /// given a path, create a scope for each segment. The last scope is returned.
    /// e.g. for the path "a.b.c", create scopes for "a", "b", and "c", and return the scope for "c"
    fn create_scope_from_path(
        &mut self,
        path: &Path,
    ) -> ScopeId {
        let mut current_scope_id = self.current_scope_id();
        for segment in path.iter() {
            // if this scope already exists,
            // just use that pre-existing ID
            if let Some(Item::Module(module_id)) = self.find_symbol_in_scope(segment.id, current_scope_id) {
                current_scope_id = self.modules.get(*module_id).root_scope;
                continue;
            }

            let next_scope = self.create_scope(ScopeKind::Module(*segment));
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
        scope.insert(name.id, name.span.with_item(item));
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
    ) -> &Binding {
        self.bindings.get(binding_id)
    }

    pub fn create_scope(
        &mut self,
        kind: ScopeKind,
    ) -> ScopeId {
        let scope = Scope {
            parent: Some(self.current_scope_id()),
            items: BTreeMap::new(),
            kind,
        };
        self.scopes.insert(scope)
    }

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

    pub fn iter_scope(
        &self,
        scope: ScopeId,
    ) -> impl Iterator<Item = (&SymbolId, &SpannedItem<Item>)> {
        self.scopes.get(scope).items.iter()
    }

    pub fn insert_expression(
        &mut self,
        id: ExprId,
        scope: ScopeId,
    ) {
        self.exprs.insert(id, scope);
    }

    pub fn get_expr_scope(
        &self,
        id: ExprId,
    ) -> Option<ScopeId> {
        self.exprs.get(&id).copied()
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
        let parser = petr_parse::Parser::new(vec![("test", input)]);
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
    use petr_utils::{render_error, SymbolInterner};

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
                let item_description = match item.item() {
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
                    Item::Import { .. } => todo!(),
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
                1: Module test (parent scopeid0):
                  trinary_boolean: Type TypeId(0)
                  True: Function FunctionId(0)
                  False: Function FunctionId(1)
                  maybe: Function FunctionId(2)
                2: Type Cons (parent scopeid1):
                3: Function (parent scopeid1):
                4: Type Cons (parent scopeid1):
                5: Function (parent scopeid1):
                6: Type Cons (parent scopeid1):
                7: Function (parent scopeid1):
            "#]],
        );
    }
    #[test]
    fn bind_function_decl() {
        check(
            "fn add(a in 'Int, b in 'Int) returns 'Int + 1 2",
            expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent scopeid0):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(13), length: 3 } } })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(24), length: 3 } } })
            "#]],
        );
    }

    #[test]
    fn bind_list_new_scope() {
        check(
            "fn add(a in 'Int, b in  'Int) returns 'Int [ 1, 2, 3, 4, 5, 6 ]",
            expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent scopeid0):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(13), length: 3 } } })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(25), length: 3 } } })
            "#]],
        );
    }
}
