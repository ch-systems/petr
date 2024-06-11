//! types used across multiple swim crates

use std::rc::Rc;

use crate::{idx_map_key, IndexMap};

#[derive(Clone, Copy, Debug)]
pub struct Identifier {
    pub id: SymbolId,
}

idx_map_key!(
    /// The ID of an ident in the symbol interner
    SymbolId
);

#[derive(Default, Debug, Clone)]
pub struct SymbolInterner {
    symbol_map: IndexMap<SymbolId, Rc<str>>,
}

impl SymbolInterner {
    pub fn insert(&mut self,
                  v: Rc<str>)
                  -> SymbolId {
        match self.symbol_map.contains_value(v.clone()) {
            | Some(k) => k,
            | None => self.symbol_map.insert(v),
        }
    }

    pub fn get(&self,
               id: SymbolId)
               -> Rc<str> {
        self.symbol_map.get(id).clone()
    }
}
