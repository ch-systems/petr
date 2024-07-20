//! types used across multiple petr crates

use std::rc::Rc;

#[cfg(feature = "debug")]
use lazy_static::lazy_static;

use crate::{idx_map_key, IndexMap};
#[cfg(feature = "debug")]
lazy_static! {
    pub static ref SYMBOL_INTERNER: std::sync::RwLock<Vec<String>> = std::sync::RwLock::new(Vec::new());
}

idx_map_key!(
    /// The ID type of a type declaration.
    TypeId
);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Identifier {
    pub id:   SymbolId,
    pub span: crate::Span,
}

// ignore the span when comparing with ord and partialord
impl PartialOrd for Identifier {
    fn partial_cmp(
        &self,
        other: &Self,
    ) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Identifier {
    fn cmp(
        &self,
        other: &Self,
    ) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl Identifier {
    pub fn span(&self) -> crate::Span {
        self.span
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path {
    pub identifiers: Box<[Identifier]>,
}

impl Path {
    pub fn new(identifiers: Vec<Identifier>) -> Self {
        Self {
            identifiers: identifiers.into_boxed_slice(),
        }
    }
}

impl Path {
    pub fn len(&self) -> usize {
        self.identifiers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.identifiers.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Identifier> {
        self.identifiers.iter()
    }
}

#[cfg(not(feature = "debug"))]
idx_map_key!(
    /// The ID of an ident in the symbol interner
    SymbolId
);

#[cfg(feature = "debug")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
#[doc = r"The ID of an ident in the symbol interner"]
pub struct SymbolId(usize);

#[cfg(feature = "debug")]
impl From<usize> for SymbolId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}
#[cfg(feature = "debug")]
impl From<SymbolId> for usize {
    fn from(o: SymbolId) -> usize {
        o.0
    }
}
#[cfg(feature = "debug")]
impl std::fmt::Display for SymbolId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let name = crate::SYMBOL_INTERNER.read().unwrap().get(self.0).unwrap().to_string();
        write!(f, "{}", name)
    }
}

#[derive(Default, Debug, Clone)]
pub struct SymbolInterner {
    symbol_map: IndexMap<SymbolId, Rc<str>>,
}

impl SymbolInterner {
    pub fn get_path(
        &self,
        path: &Path,
    ) -> Vec<Rc<str>> {
        path.iter().map(|id| self.get(id.id)).collect()
    }

    #[cfg(not(feature = "debug"))]
    pub fn insert(
        &mut self,
        v: Rc<str>,
    ) -> SymbolId {
        match self.symbol_map.contains_value(v.clone()) {
            Some(k) => k,
            None => self.symbol_map.insert(v),
        }
    }

    #[cfg(feature = "debug")]
    pub fn insert(
        &mut self,
        v: Rc<str>,
    ) -> SymbolId {
        match self.symbol_map.contains_value(v.clone()) {
            Some(k) => k,
            None => {
                let id = self.symbol_map.insert(v.clone());
                crate::SYMBOL_INTERNER.write().unwrap().insert(id.0, v.to_string());
                id
            },
        }
    }

    pub fn get(
        &self,
        id: SymbolId,
    ) -> Rc<str> {
        self.symbol_map.get(id).clone()
    }
}
