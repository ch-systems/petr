#![allow(dead_code)]

mod lexer;
pub mod parser;

mod comments {
    use std::rc::Rc;

    use crate::parser::{ast::Comment, Parse};
    use swim_utils::SpannedItem;

    /// If a node is commentable, it will have its comments set here.
    pub trait Commentable {
        fn set_comments(self, comments: Vec<SpannedItem<Comment>>) -> Self;
        fn get_comments(&self) -> Rc<str>;
    }

    pub struct Commented<T>(T);

    impl<T> Parse for Commented<T>
    where
        T: Parse + Commentable,
    {
        fn parse(p: &mut crate::parser::Parser) -> Option<Self> {
            p.parse()
                .map(|item: T| item.set_comments(p.drain_comments()))
                .map(Commented)
        }
    }

    impl<T> Commented<T>
    where
        T: Commentable,
    {
        pub fn get_comments(&self) -> Rc<str> {
            self.0.get_comments()
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
