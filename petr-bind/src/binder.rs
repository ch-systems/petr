use std::{collections::BTreeMap, rc::Rc};

use petr_ast::{dependency::Dependency, Ast, Binding, ExprId, Expression, FunctionDeclaration, Ty, TypeDeclaration};
use petr_utils::{idx_map_key, Identifier, IndexMap, Path, Span, SpannedItem, SymbolId, SymbolInterner};

#[cfg(test)]
mod tests;

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

#[derive(Clone)]
pub enum Item {
    Binding(Binding),
    // the `ScopeId` is the scope of the function body
    Function(FunctionId, ScopeId),
    Type(petr_utils::TypeId),
    FunctionParameter(Ty),
    Module(ModuleId),
    Import { path: Path, alias: Option<Identifier> },
}

pub struct Binder {
    scopes:      IndexMap<ScopeId, Scope>,
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
    pub root_scope:         ScopeId,
    pub exported_functions: BTreeMap<SymbolId, (FunctionId, ScopeId)>,
    pub exported_types:     BTreeMap<SymbolId, petr_utils::TypeId>,
}

#[derive(Default)]
pub struct Scope {
    /// A `Scope` always has a parent, unless it is the root scope of the user code.
    /// All scopes are descendents of one single root scope.
    parent: Option<ScopeId>,
    /// A mapping of the symbols that were declared in this scope. Note that any scopes that are
    /// children of this scope inherit these symbols as well.
    functions: BTreeMap<SymbolId, (FunctionId, ScopeId)>,
    bindings: BTreeMap<SymbolId, Binding>,
    function_params: BTreeMap<SymbolId, Ty>,
    types: BTreeMap<SymbolId, petr_utils::TypeId>,
    modules: BTreeMap<SymbolId, ModuleId>,
    imports: BTreeMap<SymbolId, ImportStatement>,
    spans: BTreeMap<SymbolId, petr_utils::Span>,
    #[allow(dead_code)]
    // this will be read but is also very useful for debugging
    kind: ScopeKind,
}

#[derive(Clone)]
pub struct ImportStatement {
    pub path:  Path,
    pub alias: Option<Identifier>,
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

impl Default for ScopeKind {
    fn default() -> Self {
        Self::Root
    }
}

impl Scope {
    // TODO: for all insert_ functions, return an error on overriding of names instead of
    // panicking. Or define shadowing rules or something.
    pub fn insert_function(
        &mut self,
        k: SymbolId,
        span: Span,
        v: (FunctionId, ScopeId),
    ) {
        if self.functions.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }

        self.spans.insert(k, span);
    }

    pub fn insert_type(
        &mut self,
        k: SymbolId,
        span: Span,
        v: petr_utils::TypeId,
    ) {
        if self.types.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
        self.spans.insert(k, span);
    }

    pub fn insert_binding(
        &mut self,
        k: SymbolId,
        span: Span,
        v: Binding,
    ) {
        if self.bindings.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
        self.spans.insert(k, span);
    }

    pub fn insert_module(
        &mut self,
        k: SymbolId,
        span: Span,
        v: ModuleId,
    ) {
        if self.modules.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
        self.spans.insert(k, span);
    }

    pub fn insert_function_parameter(
        &mut self,
        k: SymbolId,
        span: Span,
        v: Ty,
    ) {
        if self.function_params.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
        self.spans.insert(k, span);
    }

    pub fn insert_import(
        &mut self,
        k: SymbolId,
        span: Span,
        v: ImportStatement,
    ) {
        if self.imports.insert(k, v).is_some() {
            todo!("throw error for overriding symbol name {k}")
        }
        self.spans.insert(k, span);
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    /*
    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &T)> {
        self.items.iter()
    }
    */
}

impl Binder {
    fn new() -> Self {
        let mut scopes = IndexMap::default();
        let root_scope = Scope::default();
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

    /// does not check parent scopes
    pub fn find_module_in_single_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<ModuleId> {
        let scope = self.scopes.get(scope_id);
        if let Some(module_id) = scope.modules.get(&name) {
            return Some(*module_id);
        }
        None
    }

    pub fn find_module_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<ModuleId> {
        let scope = self.scopes.get(scope_id);
        if let Some(module_id) = scope.modules.get(&name) {
            return Some(*module_id);
        }
        if let Some(parent_id) = scope.parent() {
            return self.find_module_in_scope(name, parent_id);
        }
        None
    }

    pub fn find_type_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<petr_utils::TypeId> {
        let scope = self.scopes.get(scope_id);
        if let Some(id) = scope.types.get(&name) {
            return Some(*id);
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_type_in_scope(name, parent_id);
        }
        None
    }

    pub fn find_function_parameter_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<Ty> {
        let scope = self.scopes.get(scope_id);
        if let Some(id) = scope.function_params.get(&name) {
            return Some(id.clone());
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_function_parameter_in_scope(name, parent_id);
        }
        None
    }

    pub fn find_binding_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<Binding> {
        let scope = self.scopes.get(scope_id);
        if let Some(id) = scope.bindings.get(&name) {
            return Some(id.clone());
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_binding_in_scope(name, parent_id);
        }
        None
    }

    pub fn find_function_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<(FunctionId, ScopeId)> {
        let scope = self.scopes.get(scope_id);
        if let Some(entry) = scope.functions.get(&name) {
            return Some(*entry);
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_function_in_scope(name, parent_id);
        }
        None
    }

    pub fn find_import_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<ImportStatement> {
        let scope = self.scopes.get(scope_id);
        if let Some(entry) = scope.imports.get(&name) {
            return Some(entry.clone());
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_import_in_scope(name, parent_id);
        }
        None
    }

    /// Given a name, tries to figure out what it is by calling all of the find_ methods
    pub fn find_symbol_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<Item> {
        if let Some((id, scope)) = self.find_function_in_scope(name, scope_id) {
            return Some(Item::Function(id, scope));
        }

        if let Some(id) = self.find_type_in_scope(name, scope_id) {
            return Some(Item::Type(id));
        }

        if let Some(id) = self.find_binding_in_scope(name, scope_id) {
            return Some(Item::Binding(id));
        }

        if let Some(id) = self.find_module_in_scope(name, scope_id) {
            return Some(Item::Module(id));
        }

        if let Some(item) = self.find_import_in_scope(name, scope_id) {
            return Some(Item::Import {
                path:  item.path,
                alias: item.alias,
            });
        }

        None
    }

    /// Searches for a symbol in a scope or any of its parents
    pub fn find_spanned_func_in_scope(
        &self,
        name: SymbolId,
        scope_id: ScopeId,
    ) -> Option<SpannedItem<&(FunctionId, ScopeId)>> {
        let scope = self.scopes.get(scope_id);
        if let Some(item) = scope.functions.get(&name) {
            let span = scope.spans.get(&name).expect("function should have a span");
            return Some(span.with_item(item));
        }

        if let Some(parent_id) = scope.parent() {
            return self.find_spanned_func_in_scope(name, parent_id);
        }

        None
    }

    /// Iterate over all scopes in the binder.
    pub fn scope_iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter()
    }

    pub fn insert_function_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<(FunctionId, ScopeId)>,
    ) {
        let scope_id = self.current_scope_id();
        self.scopes.get_mut(scope_id).insert_function(name, item.span(), *item.item());
    }

    pub fn insert_binding_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<Binding>,
    ) {
        let scope_id = self.current_scope_id();
        self.scopes.get_mut(scope_id).insert_binding(name, item.span(), item.into_item());
    }

    pub fn insert_import_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<ImportStatement>,
    ) {
        let scope_id = self.current_scope_id();
        println!("inserting import {name} into current scope {scope_id}");
        self.scopes.get_mut(scope_id).insert_import(name, item.span(), item.into_item());
    }

    pub fn insert_function_parameter_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<Ty>,
    ) {
        let scope_id = self.current_scope_id();
        self.scopes
            .get_mut(scope_id)
            .insert_function_parameter(name, item.span(), item.into_item());
    }

    pub fn insert_type_into_current_scope(
        &mut self,
        name: SymbolId,
        item: SpannedItem<petr_utils::TypeId>,
    ) {
        let scope_id = self.current_scope_id();
        self.scopes.get_mut(scope_id).insert_type(name, item.span(), *item.item());
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
    ) -> &Scope {
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
    ) -> Option<(Identifier, petr_utils::TypeId)> {
        // insert a function binding for every constructor
        // and a type binding for the parent type
        let type_id = self.types.insert((*ty_decl.item()).clone());
        self.insert_type_into_current_scope(ty_decl.item().name.id, ty_decl.span().with_item(type_id));

        for variant in &ty_decl.item().variants {
            let span = variant.span();
            let variant = match variant.item() {
                petr_ast::TypeVariantOrLiteral::Variant(v) => v,
                // we don't need to create type constructor functions for constant literal types
                petr_ast::TypeVariantOrLiteral::Literal(_) => continue,
            };
            let (fields_as_parameters, _func_scope) = self.with_scope(ScopeKind::TypeConstructor, |_, scope| {
                (
                    variant
                        .fields
                        .iter()
                        .map(|field| petr_ast::FunctionParameter {
                            name: field.item().name,
                            ty:   field.item().ty.clone(),
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
        }

        // if there is a constant literal variant, then we introduce a type constructor that is
        // just for constant literal variants
        let constant_literals = ty_decl
            .item()
            .variants
            .iter()
            .filter_map(|variant| match variant.item() {
                petr_ast::TypeVariantOrLiteral::Literal(lit) => Some(lit),
                _ => None,
            })
            .collect::<Vec<_>>();

        if !constant_literals.is_empty() {
            let constants_as_sum_type = Ty::Sum(
                constant_literals
                    .into_iter()
                    .map(|lit| Ty::Literal(lit.clone()))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );

            let param_name = Identifier {
                id:   ty_decl.item().name.id,
                span: ty_decl.span(),
            };

            let type_constructor_expr = ty_decl.span().with_item(Expression::Variable(param_name));

            let function = FunctionDeclaration {
                name:        ty_decl.item().name,
                parameters:  vec![petr_ast::FunctionParameter {
                    name: param_name,
                    ty:   constants_as_sum_type,
                }]
                .into_boxed_slice(),
                return_type: Ty::Named(ty_decl.item().name),
                body:        ty_decl
                    .span()
                    .with_item(Expression::TypeConstructor(type_id, vec![type_constructor_expr].into_boxed_slice())),
                visibility:  ty_decl.item().visibility,
            };

            self.insert_function(&ty_decl.span().with_item(&function));
        }

        if ty_decl.item().is_exported() {
            Some((ty_decl.item().name, type_id))
        } else {
            None
        }
    }

    pub(crate) fn insert_function(
        &mut self,
        func: &SpannedItem<&FunctionDeclaration>,
    ) -> Option<(Identifier, (FunctionId, ScopeId))> {
        let span = func.span();
        let func = func.item();
        let function_id = self.functions.insert(span.with_item((*func).clone()));
        let function_body_scope = self.with_scope(ScopeKind::Function, |binder, function_body_scope| {
            for param in func.parameters.iter() {
                binder.insert_function_parameter_into_current_scope(param.name.id, param.name.span().with_item(param.ty.clone()));
            }

            func.body.bind(binder);
            function_body_scope
        });
        self.insert_function_into_current_scope(func.name.id, span.with_item((function_id, function_body_scope)));
        if func.is_exported() {
            Some((func.name, (function_id, function_body_scope)))
        } else {
            None
        }
    }

    // TODO add optional prefix here:
    // if Some(p) then this is a dependency, and p should be prepended to the path of each module
    // If None then this is user code, and no prefix is needed
    pub fn from_ast(ast: &Ast) -> Self {
        let mut binder = Self::new();

        ast.modules.iter().for_each(|module| {
            let module_scope = binder.create_scope_from_path(&module.name);
            let mut exported_functions = BTreeMap::default();
            let mut exported_types = BTreeMap::default();
            binder.with_specified_scope(module_scope, |binder, scope_id| {
                for item in module.nodes.iter() {
                    match item.item() {
                        petr_ast::AstNode::FunctionDeclaration(decl) => {
                            if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                exported_functions.insert(k.id, v);
                            }
                        },
                        petr_ast::AstNode::TypeDeclaration(decl) => {
                            if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                exported_types.insert(k.id, v);
                            }
                        },
                        petr_ast::AstNode::ImportStatement(stmt) => {
                            println!("got an import");
                            if let Some(_item) = stmt.bind(binder) {
                                todo!()
                            }
                        },
                    }
                }
                // we don't need to track this module ID -- it just needs to exist,
                // and all modules are iterated over in later stages of the compiler.
                // So we can safely ignore the return value here.
                let _module_id = binder.modules.insert(Module {
                    root_scope: scope_id,
                    exported_functions,
                    exported_types,
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
                // TODO deduplicate module export logic with above func
                for module in &dep_ast.modules {
                    let mut exported_functions = BTreeMap::default();
                    let mut exported_types = BTreeMap::default();
                    let module_scope = binder.create_scope_from_path(&module.name);
                    binder.with_specified_scope(module_scope, |binder, scope_id| {
                        for item in module.nodes.iter() {
                            match item.item() {
                                petr_ast::AstNode::FunctionDeclaration(decl) => {
                                    if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                        exported_functions.insert(k.id, v);
                                    }
                                },
                                petr_ast::AstNode::TypeDeclaration(decl) => {
                                    if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                        exported_types.insert(k.id, v);
                                    }
                                },
                                petr_ast::AstNode::ImportStatement(stmt) => {
                                    if let Some(_item) = stmt.bind(binder) {
                                        todo!()
                                    }
                                },
                            }
                        }
                        // we don't need to track this module ID -- it just needs to exist,
                        // and all modules are iterated over in later stages of the compiler.
                        // So we can safely ignore the return value here.
                        let _module_id = binder.modules.insert(Module {
                            root_scope: scope_id,
                            exported_functions,
                            exported_types,
                        });
                    });
                }
            })
        }

        ast.modules.iter().for_each(|module| {
            let module_scope = binder.create_scope_from_path(&module.name);
            let mut exported_functions = BTreeMap::default();
            let mut exported_types = BTreeMap::default();
            binder.with_specified_scope(module_scope, |binder, scope_id| {
                for item in module.nodes.iter() {
                    match item.item() {
                        petr_ast::AstNode::FunctionDeclaration(decl) => {
                            if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                exported_functions.insert(k.id, v);
                            }
                        },
                        petr_ast::AstNode::TypeDeclaration(decl) => {
                            if let Some((k, v)) = item.span().with_item(decl.item()).bind(binder) {
                                exported_types.insert(k.id, v);
                            }
                        },
                        petr_ast::AstNode::ImportStatement(stmt) => {
                            if let Some(_item) = stmt.bind(binder) {
                                todo!()
                            }
                        },
                    }
                }
                // we don't need to track this module ID -- it just needs to exist,
                // and all modules are iterated over in later stages of the compiler.
                // So we can safely ignore the return value here.
                let _module_id = binder.modules.insert(Module {
                    root_scope: scope_id,
                    exported_functions,
                    exported_types,
                });
            });
        });

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
            if let Some(module_id) = self.find_module_in_single_scope(segment.id, current_scope_id) {
                current_scope_id = self.modules.get(module_id).root_scope;
                continue;
            }

            let next_scope = self.create_scope(ScopeKind::Module(*segment));
            let module = Module {
                root_scope:         next_scope,
                exported_functions: BTreeMap::default(),
                exported_types:     BTreeMap::default(),
            };
            let module_id = self.modules.insert(module);
            self.insert_module_into_specified_scope(current_scope_id, *segment, module_id);
            current_scope_id = next_scope
        }
        current_scope_id
    }

    pub fn insert_module_into_specified_scope(
        &mut self,
        scope: ScopeId,
        name: Identifier,
        item: ModuleId,
    ) {
        let scope = self.scopes.get_mut(scope);
        scope.insert_module(name.id, name.span, item);
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
            kind,
            ..Default::default()
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
    ) -> impl Iterator<Item = (&SymbolId, SpannedItem<Item>)> {
        let scope: &Scope = self.scopes.get(scope);
        let func_items = scope
            .functions
            .iter()
            .map(|(k, v)| (k, scope.spans.get(k).unwrap().with_item(Item::Function(v.0, v.1))));

        let type_items = scope
            .types
            .iter()
            .map(|(k, v)| (k, scope.spans.get(k).unwrap().with_item(Item::Type(*v))));

        func_items.into_iter().chain(type_items)
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
