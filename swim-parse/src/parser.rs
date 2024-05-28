#[cfg(test)]
mod tests;

mod ast;

use crate::{
    lexer::{Lexer, Token},
    SymbolInterner,
};
use miette::Diagnostic;
use swim_utils::SpannedItem;
use thiserror::Error;

use self::ast::{AstNode, AST};

#[derive(Error, Debug, Diagnostic, PartialEq)]
pub enum ParseError {
    #[error("Unmatched parenthesis")]
    UnmatchedParenthesis,
    #[error("Expected identifier, found {0}")]
    ExpectedIdentifier(String),
    #[error("Expected token {0}, found {1}")]
    ExpectedToken(Token, Token),
    #[error("Expected one of tokens {0:?}, found {1}")]
    ExpectedOneOf(Vec<Token>, Token),
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    interner: SymbolInterner<'a>,
    lexer: Lexer<'a>,
    errors: Vec<SpannedItem<ParseError>>,
}

impl<'a> Parser<'a> {
    pub fn new(sources: impl IntoIterator<Item = &'a str>) -> Self {
        Self {
            interner: SymbolInterner::default(),
            lexer: Lexer::new(sources),
            errors: Vec::new(),
        }
    }

    /// consume tokens until a node is produced
    pub fn into_result(mut self) -> (AST, Vec<SpannedItem<ParseError>>, SymbolInterner<'a>) {
        let nodes: Vec<SpannedItem<AstNode>> = self.many::<SpannedItem<AstNode>>();
        (AST::new(nodes), self.errors, self.interner)
    }

    pub fn many<P: Parse>(&mut self) -> Vec<P> {
        let mut buf = Vec::new();
        loop {
            if let Some(parsed_item) = P::parse(self) {
                buf.push(parsed_item);
            } else {
                break;
            }
        }
        buf
    }

    /// parses a sequence separated by `separator`
    /// e.g. if separator is `Token::Comma`, can parse `a, b, c, d`
    /// NOTE: this parses one or more items.
    pub fn sequence<P: Parse>(&mut self, separator: Token) -> Vec<P> {
        let mut buf = vec![];
        loop {
            let item = P::parse(self);
            match item {
                Some(item) => buf.push(item),
                None => return buf,
            }
            if *self.lexer.peek().item() == separator {
                self.lexer.advance();
            } else {
                return buf;
            }
        }
    }

    pub fn token(&mut self, tok: Token) -> Option<SpannedItem<Token>> {
        if *self.lexer.peek().item() == tok {
            Some(self.lexer.advance())
        } else {
            self.errors.push(
                self.lexer
                    .span()
                    .with_item(ParseError::ExpectedToken(tok, *self.lexer.peek().item())),
            );
            None
        }
    }

    pub fn parse<P: Parse>(&mut self) -> Option<P> {
        P::parse(self)
    }

    fn one_of<const N: usize>(&mut self, toks: [Token; N]) -> Option<SpannedItem<Token>> {
        match self.lexer.peek().item() {
            tok if toks.contains(tok) => self.token(*tok),
            _ => None,
        }
    }
}

pub trait Parse: Sized {
    fn parse(p: &mut Parser) -> Option<Self>;
}

impl<T> Parse for SpannedItem<T>
where
    T: Parse,
{
    fn parse(p: &mut Parser) -> Option<Self> {
        let before_span = p.lexer.span();
        let result = T::parse(p)?;
        let after_span = p.lexer.span();

        // i think this should be `hi` to `hi`, not 100% though
        Some(before_span.hi_to_hi(after_span).with_item(result))
    }
}
