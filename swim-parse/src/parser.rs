#[cfg(test)]
mod tests;

pub mod ast;

use crate::{
    lexer::{Lexer, Token},
    SymbolInterner,
};
use ast::Comment;
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
    interner: SymbolInterner,
    lexer: Lexer<'a>,
    errors: Vec<SpannedItem<ParseError>>,
    comments: Vec<SpannedItem<Comment>>,
    peek: Option<SpannedItem<Token>>,
}

impl<'a> Parser<'a> {
    pub fn peek(&mut self) -> SpannedItem<Token> {
        if let Some(ref peek) = self.peek {
            *peek
        } else {
            let item = self.advance();
            self.peek = Some(item);
            item
        }
    }
    pub fn new(sources: impl IntoIterator<Item = &'a str>) -> Self {
        Self {
            interner: SymbolInterner::default(),
            lexer: Lexer::new(sources),
            errors: Default::default(),
            comments: Default::default(),
            peek: None,
        }
    }

    pub fn drain_comments(&mut self) -> Vec<Comment> {
        self.comments
            .drain(..)
            .map(|spanned_item| spanned_item.into_item())
            .collect()
    }

    /// consume tokens until a node is produced
    pub fn into_result(mut self) -> (AST, Vec<SpannedItem<ParseError>>, SymbolInterner) {
        let nodes: Vec<SpannedItem<AstNode>> = self.many::<SpannedItem<AstNode>>();
        (AST::new(nodes), self.errors, self.interner)
    }

    pub fn interner(&self) -> &SymbolInterner {
        &self.interner
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
            if *self.peek().item() == separator {
                self.advance();
            } else {
                return buf;
            }
        }
    }

    pub fn advance(&mut self) -> SpannedItem<Token> {
        if let Some(tok) = self.peek.take() {
            return tok;
        }
        let next_tok = self.lexer.advance();
        match *next_tok.item() {
            Token::Comment => {
                if let Some(comment) = self.parse::<SpannedItem<Comment>>() {
                    self.comments.push(comment);
                }
                self.advance()
            }
            _ => next_tok,
        }
    }

    pub fn token(&mut self, tok: Token) -> Option<SpannedItem<Token>> {
        let peeked_token = self.peek();
        if *peeked_token.item() == tok {
            Some(self.advance())
        } else {
            let span = self.lexer.span();
            self.errors
                .push(span.with_item(ParseError::ExpectedToken(tok, *peeked_token.item())));
            None
        }
    }

    pub fn parse<P: Parse>(&mut self) -> Option<P> {
        P::parse(self)
    }

    fn one_of<const N: usize>(&mut self, toks: [Token; N]) -> Option<SpannedItem<Token>> {
        match self.peek().item() {
            tok if toks.contains(tok) => self.token(*tok),
            _ => None,
        }
    }

    pub fn errors(&self) -> &[SpannedItem<ParseError>] {
        &self.errors
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
