#![allow(dead_code)]
use std::marker::PhantomData;

use swim_utils::{SourceId, Span, SpannedItem};

pub struct IndexMap<K, V> {
    _key: PhantomData<K>,
    entries: Vec<V>,
}

impl<K, V> IndexMap<K, V> {
    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

impl<K, V> Default for IndexMap<K, V>
where
    K: From<usize> + Into<usize>,
{
    fn default() -> Self {
        Self {
            entries: Default::default(),
            _key: PhantomData,
        }
    }
}

impl<K, V> IndexMap<K, V>
where
    K: From<usize> + Into<usize>,
{
    pub fn insert(&mut self, value: V) -> K {
        let key = self.entries.len();
        self.entries.push(value);
        key.into()
    }

    pub fn get(&self, k: K) -> &V {
        &self.entries[k.into()]
    }

    fn get_mut(&mut self, k: K) -> &mut V {
        self.entries
            .get_mut(k.into())
            .expect("IDs are handed out by insertion, so this should never fail")
    }
}

impl<K, V> IndexMap<K, V>
where
    K: From<usize>,
    V: PartialEq,
{
    pub fn contains_value(&self, value: V) -> Option<K> {
        self.entries
            .iter()
            .position(|v| *v == value)
            .map(Into::into)
    }
}

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

pub struct Parser<'a> {
    interner: SymbolInterner<'a>,
}

use logos::Logos;
#[derive(Debug, Logos, PartialEq)]
#[logos(skip r"[ \t]+")]
enum Token {
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Divide,
    #[token("*")]
    Multiply,
    #[regex("[_a-zA-Z][_a-zA-Z0-9]{0,30}")]
    Identifier,
    Eof,
}

pub type LexedSources<'a, T: miette::SourceCode> =
    IndexMap<SourceId, (&'a T, logos::Lexer<'a, Token>)>;

pub struct Lexer<'a, T: miette::SourceCode> {
    sources: LexedSources<'a, T>,
    source: SourceId,
}

impl<'a, T: miette::SourceCode> Lexer<'a, T> {
    pub fn span(&self) -> Span {
        Span::new(self.source, self.current_lexer().span().into())
    }

    pub fn advance(&mut self) -> SpannedItem<Token> {
        let pre_advance_span = self.span();
        let current_lexer = self.current_lexer_mut();

        match current_lexer.next() {
            None => match self.advance_lexer() {
                Some(_) => return self.advance(),
                None => pre_advance_span.with_item(Token::Eof),
            },
            Some(tok) => self
                .span()
                .with_item(tok.expect("TODO: handle lexer failure")),
        }
    }

    fn current_lexer_mut(&mut self) -> &mut logos::Lexer<'a, Token> {
        &mut self.sources.get_mut(self.source).1
    }

    fn current_lexer(&self) -> &logos::Lexer<'a, Token> {
        &self.sources.get(self.source).1
    }

    /// advances to the next lexer, returning a reference to it if there is one
    fn advance_lexer(&mut self) -> Option<&mut logos::Lexer<'a, Token>> {
        let mut current_lexer = self.current_lexer();
        if Into::<usize>::into(self.source) == self.sources.len() - 1 {
            return None;
        }
        self.source = (Into::<usize>::into(self.source) + 1usize).into();
        Some(self.current_lexer_mut())
    }
}

/*
pub struct Lexer<'a> {
    interner: SymbolInterner<'a>,
    peek: Option<Spanned<Token>>,
    input: &'a str,
    stream: Chars<'a>,
    offset: usize,
}


impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let interner = SymbolInterner::default();

        Self {
            interner,
            stream: input.chars(),
            offset: 0,
            peek: None,
            input,
        }
    }

    pub fn advance(&mut self) -> Spanned<Token> {
        if let Some(peek) = self.peek.take() {
            return peek;
        }
        let lo = self.offset;
        let mut buf = vec![];
        let tok = loop {
            if let Some(tok) = self.stream.next() {
                self.offset += 1;
                match tok {
                    '*' => break Token::Op(Operator::Multiply),
                    '/' => break Token::Op(Operator::Divide),
                    '+' => break Token::Op(Operator::Plus),
                    '-' => break Token::Op(Operator::Minus),
                    ')' => break Token::OpenParens,
                    '(' => break Token::CloseParens,
                    tok if tok.is_whitespace() => {
                        break Token::Ident(self.interner.insert(&self.input[lo..self.offset]))
                    }
                    tok => buf.push(tok),
                };
            } else {
                break Token::Eof;
            }
        };

        let span = miette::SourceSpan::new(lo, self.offset - lo);

        Spanned(tok, span.into())

    }
}

pub enum Token {
    Op(Operator),
    OpenParens,
    CloseParens,
    Ident(SymbolKey),
    Eof,
}

pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}
*/
