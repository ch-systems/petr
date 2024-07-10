use std::collections::BTreeMap;

use petr_bind::{BindingId, FunctionId, TypeId};
use petr_utils::SymbolInterner;

use crate::resolver::{Binding, Function, TypeDeclaration};
/// Contains things that have already been resolved.
/// Resolved items cannot be queried during resolution. This is because the resolution
/// stage should only query the binder, then the type checking stage can query
/// the [QueryableResolved] to get the resolved items -- [QueryableResolvedItems] is the
/// immutable result of resolution, and resolved items can no longer be mutated.
pub(crate) struct ResolvedItems {
    pub resolved_functions: BTreeMap<FunctionId, Function>,
    pub resolved_types:     BTreeMap<TypeId, TypeDeclaration>,
    pub bindings:           BTreeMap<BindingId, Binding>,
}

impl ResolvedItems {
    pub fn insert_function(
        &mut self,
        id: FunctionId,
        function: Function,
    ) {
        self.resolved_functions.insert(id, function);
    }

    pub fn insert_type(
        &mut self,
        id: TypeId,
        type_decl: TypeDeclaration,
    ) {
        self.resolved_types.insert(id, type_decl);
    }

    pub(crate) fn new() -> Self {
        Self {
            resolved_functions: Default::default(),
            resolved_types:     Default::default(),
            bindings:           Default::default(),
        }
    }
}

pub struct QueryableResolvedItems {
    resolved_functions: BTreeMap<FunctionId, Function>,
    resolved_types:     BTreeMap<TypeId, TypeDeclaration>,
    pub interner:       SymbolInterner,
}

impl QueryableResolvedItems {
    pub fn new(
        resolved_functions: BTreeMap<FunctionId, Function>,
        resolved_types: BTreeMap<TypeId, TypeDeclaration>,
        interner: SymbolInterner,
    ) -> Self {
        Self {
            resolved_functions,
            resolved_types,
            interner,
        }
    }

    pub fn get_function(
        &self,
        id: FunctionId,
    ) -> &Function {
        self.resolved_functions
            .get(&id)
            .expect("function IDs should always correspond to resolved functions")
    }

    pub fn get_type(
        &self,
        id: TypeId,
    ) -> &TypeDeclaration {
        self.resolved_types.get(&id).expect("type IDs should always correspond to resolved types")
    }

    // TODO  The cloning of the below iterators (`functions` and `types`) is not ideal.
    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, Function)> {
        self.resolved_functions
            .iter()
            .map(|(id, decl)| (*id, decl.clone()))
            .collect::<Vec<_>>()
            .into_iter()
    }

    pub fn types(&self) -> impl Iterator<Item = (TypeId, TypeDeclaration)> {
        self.resolved_types.iter().map(|(id, decl)| (*id, *decl)).collect::<Vec<_>>().into_iter()
    }
}
