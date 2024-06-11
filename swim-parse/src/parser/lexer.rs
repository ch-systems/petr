//! TODO this should be a submodule of parser, to prevent its api from being used directly.
//! Parser's `.advance()` method should be used
#[cfg(test)]
mod tests;

use logos::Logos;
use swim_utils::{SourceId, Span, SpannedItem};

use crate::IndexMap;
#[derive(Debug, Logos, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t]+")]
pub enum Token {
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[regex("[0-9]+")]
    Integer,
    #[regex("[_a-zA-Z][_a-zA-Z0-9]{0,30}")]
    Identifier,
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    String,
    #[regex(r#"(\{\-)[^-}]*(\-\})"#)]
    Comment,
    #[token("function")]
    FunctionKeyword,
    #[token("in")]
    InKeyword,
    #[token("∈")]
    IsInSymbol,
    #[token("'")]
    TyMarker,
    #[token(",")]
    Comma,
    #[token("returns")]
    ReturnsKeyword,
    #[token("type")]
    TypeKeyword,
    #[token("\n")]
    Newline,
    #[token("=")]
    Equals,
    #[token("|")]
    Pipe,
    #[token("from")]
    FromKeyword,
    #[token("to")]
    ToKeyword,
    #[token("Function")]
    ExportFunctionKeyword,
    #[token("Type")]
    ExportTypeKeyword,
    #[token("~")]
    Tilde,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r#"\@[_a-zA-Z][_a-zA-Z0-9]{0,30}"#)]
    Intrinsic,
    Eof,
}
impl Token {
    pub(crate) fn is_operator(&self) -> bool {
        use Token::*;
        match self {
            | Plus | Minus | Slash | Star => true,
            | _ => false,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self,
           f: &mut std::fmt::Formatter<'_>)
           -> std::fmt::Result {
        use Token::*;
        match self {
            | OpenParen => write!(f, "("),
            | CloseParen => write!(f, ")"),
            | OpenBracket => write!(f, "["),
            | CloseBracket => write!(f, "]"),
            | Plus => write!(f, "+"),
            | Minus => write!(f, "-"),
            | Slash => write!(f, "/"),
            | Star => write!(f, "*"),
            | Integer => write!(f, "integer"),
            | Identifier => write!(f, "identifier"),
            | FunctionKeyword => write!(f, "function"),
            | InKeyword => write!(f, ":"),
            | IsInSymbol => write!(f, "∈"),
            | TyMarker => write!(f, "'"),
            | Comma => write!(f, ","),
            | ReturnsKeyword => write!(f, "returns"),
            | Eof => write!(f, "EOF"),
            | Comment => write!(f, "{{- comment -}}"),
            | Newline => write!(f, "newline"),
            | TypeKeyword => write!(f, "type"),
            | Equals => write!(f, "="),
            | Pipe => write!(f, "|"),
            | FromKeyword => write!(f, "from"),
            | ToKeyword => write!(f, "to"),
            | ExportFunctionKeyword => write!(f, "Function"),
            | ExportTypeKeyword => write!(f, "Type"),
            | Tilde => write!(f, "~"),
            | True => write!(f, "true"),
            | False => write!(f, "false"),
            | String => write!(f, "string"),
            | Intrinsic => write!(f, "@intrinsic"),
        }
    }
}

pub type LexedSources = IndexMap<SourceId, logos::Lexer<'static, Token>>;

pub struct Lexer {
    sources: LexedSources,
    source:  SourceId,
}

impl Lexer {
    pub fn new(sources: impl IntoIterator<Item = &'static str>) -> Self {
        let mut map: IndexMap<_, _> = Default::default();
        let sources = sources.into_iter();
        for source in sources {
            let lexer = Token::lexer(source);
            map.insert(lexer);
        }
        Self { sources: map,
               source:  0.into(), }
    }

    pub fn span(&self) -> Span {
        Span::new(self.source, self.current_lexer().span().into())
    }

    pub fn slice(&self) -> &str {
        self.current_lexer().slice()
    }

    pub(crate) fn advance(&mut self) -> SpannedItem<Token> {
        let pre_advance_span = self.span();
        let current_lexer = self.current_lexer_mut();

        match current_lexer.next() {
            | None => match self.advance_lexer() {
                | Some(_) => self.advance(),
                | None => pre_advance_span.with_item(Token::Eof),
            },
            | Some(tok) => self.span()
                               .with_item(tok.expect("TODO: handle lexer failure")),
        }
    }

    fn current_lexer_mut(&mut self) -> &mut logos::Lexer<'static, Token> {
        self.sources.get_mut(self.source)
    }

    fn current_lexer(&self) -> &logos::Lexer<'static, Token> {
        self.sources.get(self.source)
    }

    /// advances to the next lexer, returning a reference to it if there is one
    fn advance_lexer(&mut self) -> Option<&mut logos::Lexer<'static, Token>> {
        if Into::<usize>::into(self.source) == self.sources.len() - 1 {
            return None;
        }
        self.source = (Into::<usize>::into(self.source) + 1usize).into();
        Some(self.current_lexer_mut())
    }
}
