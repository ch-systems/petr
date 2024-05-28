#![allow(dead_code)]

mod lexer;
mod parser;

use swim_utils::IndexMap;

#[derive(Debug)]
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

#[derive(Default)]
pub struct SymbolInterner<'a> {
    symbol_map: IndexMap<SymbolKey, &'a str>,
}

impl<'a> SymbolInterner<'a> {
    pub fn insert(&mut self, v: &'a str) -> SymbolKey {
        match self.symbol_map.contains_value(v) {
            Some(k) => k,
            None => self.symbol_map.insert(v),
        }
    }
}
