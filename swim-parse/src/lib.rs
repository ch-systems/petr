#![allow(dead_code)]

mod lexer;
pub mod parser;

mod comments {
    /// If a node is commentable, it will have its comments set here.
    pub trait Commentable {
        fn set_comments(&mut self, comments: Vec<SpannedItem<Comment>>);
    }
}

use std::rc::Rc;

use swim_utils::IndexMap;

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
    pub fn insert(&mut self, v: &str) -> SymbolKey {
        let v: Rc<str> = Rc::from(v);
        match self.symbol_map.contains_value(v.clone()) {
            Some(k) => k,
            None => self.symbol_map.insert(v),
        }
    }

    pub fn get(&self, id: SymbolKey) -> Rc<str> {
        self.symbol_map.get(id).clone()
    }
}
