#![allow(dead_code)]

pub mod parser;
pub use parser::Parser;

pub mod comments {

    use crate::parser::{ast::Comment, Parse};

    // TODO:
    // make Commented a struct with a comment field
    // impl Spanned for Commented<SpannedItem<T>> and Commentable for SpannedItem<Commented<T>>
    // impl Spanned for most AST nodes

    pub struct Commented<T>(T, Vec<Comment>);

    impl<T> Parse for Commented<T>
    where
        T: Parse,
    {
        fn parse(p: &mut crate::parser::Parser) -> Option<Self> {
            let item: T = p.parse()?;
            let comments = p.drain_comments();
            Some(Commented(item, comments))
        }
    }

    impl<T> Commented<T> {
        pub fn comments(&self) -> &[Comment] {
            &self.1[..]
        }
    }

    impl<T> Commented<T> {
        pub fn item(&self) -> &T {
            &self.0
        }
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
