#[cfg(test)]
mod tests;

mod lexer;
use std::rc::Rc;

use lexer::Lexer;
pub use lexer::Token;
use miette::Diagnostic;
use swim_ast::{Ast, AstNode, Comment, List};
use swim_utils::{IndexMap, SourceId, Span, SpannedItem, SymbolId, SymbolInterner};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub struct ParseError {
    kind: ParseErrorKind,
    help: Option<String>,
}

impl From<ParseErrorKind> for ParseError {
    fn from(kind: ParseErrorKind) -> Self {
        Self { kind, help: None }
    }
}

impl ParseError {
    pub fn with_help(mut self,
                     help: Option<impl Into<String>>)
                     -> Self {
        self.help = help.map(Into::into);
        self
    }
}

impl Diagnostic for ParseError {
    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.help
            .as_ref()
            .map(|x| -> Box<dyn std::fmt::Display> { Box::new(x) })
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.kind.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.kind.severity()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.kind.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.kind.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.kind.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.kind.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.kind.diagnostic_source()
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self,
           f: &mut std::fmt::Formatter<'_>)
           -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
pub enum ParseErrorKind {
    #[error("Unmatched parenthesis")]
    UnmatchedParenthesis,
    #[error("Expected identifier, found {0}")]
    ExpectedIdentifier(String),
    #[error("Expected token {0}, found {1}")]
    ExpectedToken(Token, Token),
    #[error("Expected one of tokens {}; found {1}", format_toks(.0))]
    ExpectedOneOf(Vec<Token>, Token),
}
impl ParseErrorKind {
    pub fn into_err(self) -> ParseError {
        self.into()
    }
}

fn format_toks(toks: &[Token]) -> String {
    let mut buf = toks.iter()
                      .take(toks.len() - 1)
                      .map(|t| format!("{}", t))
                      .collect::<Vec<_>>()
                      .join(", ");
    if toks.len() == 2 {
        buf.push_str(&format!(" or {}", toks.last().unwrap()));
    } else if toks.len() > 2 {
        buf.push_str(&format!(", or {}", toks.last().unwrap()));
    }
    buf
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser {
    interner:   SymbolInterner,
    lexer:      Lexer,
    errors:     Vec<SpannedItem<ParseError>>,
    comments:   Vec<SpannedItem<Comment>>,
    peek:       Option<SpannedItem<Token>>,
    // the tuple is the file name and content
    source_map: IndexMap<SourceId, (&'static str, &'static str)>,
    help:       Vec<String>,
}

impl Parser {
    pub fn push_error(&mut self,
                      err: SpannedItem<ParseErrorKind>) {
        if self.help.is_empty() {
            return self.errors.push(err.map(|err| err.into_err()));
        }
        let mut help_text = Vec::with_capacity(self.help.len());
        let mut indentation = 0;
        for help in &self.help {
            let text = format!("{}{}{help}",
                               "  ".repeat(indentation),
                               if indentation == 0 { "" } else { "↪ " });
            help_text.push(text);
            indentation += 1;
        }
        let err = err.map(|err| err.into_err().with_help(Some(help_text.join("\n"))));
        self.errors.push(err);
    }

    pub fn slice(&self) -> &str {
        self.lexer.slice()
    }

    pub fn intern(&mut self,
                  internee: Rc<str>)
                  -> SymbolId {
        self.interner.insert(internee)
    }

    pub fn span(&self) -> Span {
        self.lexer.span()
    }

    pub fn peek(&mut self) -> SpannedItem<Token> {
        if let Some(ref peek) = self.peek {
            *peek
        } else {
            let item = self.advance();
            self.peek = Some(item);
            item
        }
    }

    pub fn new<'a>(sources: impl IntoIterator<Item = (impl Into<String>, impl Into<String>)>)
                   -> Self {
        // TODO we hold two copies of the source for now: one in source_maps, and one outside the parser
        // for the lexer to hold on to and not have to do self-referential pointers.
        let sources = sources.into_iter()
                             .map(|(name, source)| -> (&'static str, &'static str) {
                                 let name = name.into();
                                 let source = source.into();
                                 (Box::leak(name.into_boxed_str()),
                                  Box::leak(source.into_boxed_str()))
                             })
                             .collect::<Vec<_>>();
        let sources_for_lexer = sources.iter().map(|(_, source)| *source);
        Self { interner:   SymbolInterner::default(),
               lexer:      Lexer::new(sources_for_lexer),
               errors:     Default::default(),
               comments:   Default::default(),
               peek:       None,
               source_map: {
                   let mut source_map = IndexMap::default();
                   for (name, source) in sources.into_iter() {
                       source_map.insert((name, source));
                   }
                   source_map
               },
               help:       Default::default(), }
    }

    pub fn drain_comments(&mut self) -> Vec<Comment> {
        self.comments
            .drain(..)
            .map(|spanned_item| spanned_item.into_item())
            .collect()
    }

    /// consume tokens until a node is produced
    pub fn into_result(
        mut self)
        -> (Ast,
            Vec<SpannedItem<ParseError>>,
            SymbolInterner,
            IndexMap<SourceId, (&'static str, &'static str)>) {
        let nodes: Vec<SpannedItem<AstNode>> = self.many::<SpannedItem<AstNode>>();
        // drop the lexers from the source map
        (Ast::new(nodes), self.errors, self.interner, self.source_map)
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
    /// NOTE: this parses zero or more items. Will not reject zero items.
    pub fn sequence_zero_or_more<P: Parse>(&mut self,
                                           separator: Token)
                                           -> Option<Vec<P>> {
        let mut buf = vec![];
        loop {
            let item = P::parse(self);
            match item {
                Some(item) => buf.push(item),
                None => {
                    break;
                },
            }
            if *self.peek().item() == separator {
                self.advance();
            } else {
                break;
            }
        }
        Some(buf)
    }

    /// parses a sequence separated by `separator`
    /// e.g. if separator is `Token::Comma`, can parse `a, b, c, d`
    /// NOTE: this parses one or more items. Will reject zero items.
    pub fn sequence<P: Parse>(&mut self,
                              separator: Token)
                              -> Option<Vec<P>> {
        let mut buf = vec![];
        loop {
            let item = P::parse(self);
            match item {
                Some(item) => buf.push(item),
                None => {
                    break;
                },
            }
            if *self.peek().item() == separator {
                self.advance();
            } else {
                break;
            }
        }
        if buf.is_empty() {
            None
        } else {
            Some(buf)
        }
    }

    pub fn advance(&mut self) -> SpannedItem<Token> {
        if let Some(tok) = self.peek.take() {
            return tok;
        }
        let next_tok = self.lexer.advance();
        match *next_tok.item() {
            Token::Newline => self.advance(),
            Token::Comment => {
                if let Some(comment) = self.parse::<SpannedItem<Comment>>() {
                    self.comments.push(comment);
                }
                self.advance()
            },
            _ => next_tok,
        }
    }

    /// doesn't push the error to the error list and doesn't advance if the token is not found
    pub fn try_token(&mut self,
                     tok: Token)
                     -> Option<SpannedItem<Token>> {
        let peeked_token = self.peek();
        if *peeked_token.item() == tok {
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn token(&mut self,
                 tok: Token)
                 -> Option<SpannedItem<Token>> {
        let peeked_token = self.peek();
        if *peeked_token.item() == tok {
            Some(self.advance())
        } else {
            let span = self.lexer.span();
            self.push_error(span.with_item(ParseErrorKind::ExpectedToken(tok,
                                                                         *peeked_token.item())));
            None
        }
    }

    pub fn parse<P: Parse>(&mut self) -> Option<P> {
        P::parse(self)
    }

    pub fn one_of<const N: usize>(&mut self,
                                  toks: [Token; N])
                                  -> Option<SpannedItem<Token>> {
        match self.peek().item() {
            tok if toks.contains(tok) => self.token(*tok),
            tok => {
                let span = self.lexer.span();
                if N == 1 {
                    self.push_error(span.with_item(ParseErrorKind::ExpectedToken(toks[0], *tok)));
                } else {
                    self.push_error(span.with_item(ParseErrorKind::ExpectedOneOf(toks.to_vec(),
                                                                                 *tok)));
                }
                None
            },
        }
    }

    pub fn errors(&self) -> &[SpannedItem<ParseError>] {
        &self.errors
    }

    pub fn with_help<F, T>(&mut self,
                           help_text: impl Into<String>,
                           f: F)
                           -> T
        where F: Fn(&mut Parser) -> T
    {
        self.push_help(help_text);
        let res = f(self);
        self.pop_help();
        res
    }

    fn push_help(&mut self,
                 arg: impl Into<String>) {
        self.help.push(arg.into())
    }

    fn pop_help(&mut self) {
        let _ = self.help.pop();
    }
}

pub trait Parse: Sized {
    fn parse(p: &mut Parser) -> Option<Self>;
}

impl<T> Parse for SpannedItem<T> where T: Parse
{
    fn parse(p: &mut Parser) -> Option<Self> {
        let before_span = p.lexer.span();
        let result = T::parse(p)?;
        let after_span = p.lexer.span();

        // i think this should be `hi` to `hi`, not 100% though
        Some(before_span.hi_to_hi(after_span).with_item(result))
    }
}

impl Parse for List {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.try_token(Token::OpenBracket)?;
        let elements = p.sequence(Token::Comma)?;
        p.token(Token::CloseBracket)?;
        Some(List { elements: elements.into_boxed_slice(), })
    }
}
