//! types used across multiple swim crates

use std::rc::Rc;

use crate::IndexMap;

#[derive(Clone, Copy)]
pub struct Identifier {
    pub id: SymbolKey,
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolKey(usize);

impl From<usize> for SymbolKey {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<SymbolKey> for usize {
    fn from(value: SymbolKey) -> Self {
        value.0
    }
}

#[derive(Default, Debug, Clone)]
pub struct SymbolInterner {
    symbol_map: IndexMap<SymbolKey, Rc<str>>,
}

impl SymbolInterner {
    pub fn insert(&mut self, v: Rc<str>) -> SymbolKey {
        match self.symbol_map.contains_value(v.clone()) {
            Some(k) => k,
            None => self.symbol_map.insert(v),
        }
    }

    pub fn get(&self, id: SymbolKey) -> Rc<str> {
        self.symbol_map.get(id).clone()
    }
}
