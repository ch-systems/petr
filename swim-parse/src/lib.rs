#![allow(dead_code)]

mod lexer;

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
mod parser {
    #[cfg(test)]
    mod tests {
        use expect_test::expect;

        use super::Parser;

        fn check<T: AsRef<str>>(sources: Vec<T>, expected: expect_test::Expect) {
            let parser = Parser::new(sources.iter().map(|s| s.as_ref()));
            let (ast, errs) = parser.into_result();

            expected.assert_eq(&format!("{ast:#?}\n\n{errs:#?}"));
        }
        #[test]
        fn prefix_operator_expression() {
            check(vec!["function addToFive() + 4 1"], expect![""])
        }
    }
    mod ast {
        use super::{Parse, ParseError, Parser};
        use crate::{lexer::Token, SymbolKey};
        use swim_utils::{Span, SpannedItem};

        #[derive(Debug)]
        pub struct AST {
            nodes: Vec<SpannedItem<AstNode>>,
        }

        impl AST {
            pub fn new(nodes: Vec<SpannedItem<AstNode>>) -> AST {
                Self { nodes }
            }
        }

        #[derive(Debug)]
        pub enum AstNode {
            FunctionDeclaration(FunctionDeclaration),
        }

        impl Parse for AstNode {
            fn parse(p: &mut Parser) -> Option<Self> {
                let tok = p.lexer.advance();
                match tok.item() {
                    Token::FunctionKeyword => {
                        let name = p.parse()?;
                        p.token(Token::OpenParen)?;
                        let parameters = p
                            .sequence::<FunctionParameter>(Token::Comma)
                            .into_boxed_slice();
                        p.token(Token::CloseParen)?;
                        p.token(Token::ReturnsKeyword)?;
                        let return_type = p.parse()?;
                        let body = p.parse()?;
                        Some(AstNode::FunctionDeclaration(FunctionDeclaration {
                            name,
                            parameters,
                            return_type,
                            body,
                        }))
                    }
                    a => todo!("unimplemented parse: {a:?}"),
                }
            }
        }

        #[derive(Debug)]
        pub struct FunctionDeclaration {
            name: Identifier,
            parameters: Box<[FunctionParameter]>,
            return_type: Ty,
            body: SpannedItem<Expression>,
        }

        #[derive(Debug)]
        pub enum Expression {
            Literal(Literal),
            Operator(Box<OperatorExpression>),
        }

        impl Parse for Expression {
            fn parse(p: &mut Parser) -> Option<Self> {
                match p.lexer.peek().item() {
                    item if item.is_operator() => {
                        let op: SpannedItem<Operator> = p.parse()?;
                        // parse prefix notation operator expression
                        let lhs: SpannedItem<Expression> = p.parse()?;
                        let rhs: SpannedItem<Expression> = p.parse()?;
                        Some(Expression::Operator(Box::new(OperatorExpression {
                            lhs,
                            rhs,
                            op,
                        })))
                    }
                    Token::Integer => Some(Expression::Literal(p.parse()?)),
                    _ => todo!(),
                }
            }
        }

        #[derive(Debug)]
        pub enum Literal {
            Integer(i64),
        }

        impl Parse for Literal {
            fn parse(p: &mut Parser) -> Option<Self> {
                let tok = p.lexer.advance();
                match tok.item() {
                    Token::Integer => Some(Literal::Integer(
                        p.lexer
                            .slice()
                            .parse()
                            .expect("lexer should have verified this"),
                    )),
                    _ => {
                        p.errors.push(
                            p.lexer
                                .span()
                                .with_item(ParseError::ExpectedToken(Token::Integer, *tok.item())),
                        );
                        None
                    }
                }
            }
        }

        #[derive(Debug)]
        pub struct OperatorExpression {
            lhs: SpannedItem<Expression>,
            rhs: SpannedItem<Expression>,
            op: SpannedItem<Operator>,
        }

        #[derive(Debug)]
        pub struct FunctionParameter {
            name: Identifier,
            ty: Ty,
        }

        impl Parse for FunctionParameter {
            fn parse(p: &mut Parser) -> Option<Self> {
                todo!()
            }
        }

        #[derive(Debug)]
        pub struct Ty {
            ty_name: Identifier,
        }

        impl Parse for Ty {
            // TODO types are not just idents,
            // they can be more than that
            fn parse(p: &mut Parser) -> Option<Self> {
                p.token(Token::TyMarker)?;
                let ident = Identifier::parse(p)?;
                Some(Self { ty_name: ident })
            }
        }

        #[derive(Debug)]
        pub enum Operator {
            Plus,
            Minus,
            Star,
            Slash,
        }

        impl Parse for Operator {
            fn parse(p: &mut Parser) -> Option<Self> {
                let tok = p.lexer.advance();
                match tok.item() {
                    Token::Plus => Some(Operator::Plus),
                    Token::Minus => Some(Operator::Minus),
                    Token::Star => Some(Operator::Star),
                    Token::Slash => Some(Operator::Slash),
                    _ => {
                        p.errors
                            .push(p.lexer.span().with_item(ParseError::ExpectedOneOf(
                                vec![Token::Plus, Token::Minus, Token::Star, Token::Slash],
                                *tok.item(),
                            )));
                        None
                    }
                }
            }
        }

        #[derive(Debug)]
        pub struct Identifier {
            id: SymbolKey,
            span: Span,
        }

        impl Parse for Identifier {
            fn parse(p: &mut Parser) -> Option<Self> {
                let identifier = p.lexer.advance();
                if *identifier.item() != Token::Identifier {
                    p.errors.push(
                        p.lexer
                            .span()
                            .with_item(ParseError::ExpectedIdentifier(p.lexer.slice().to_string())),
                    );
                }
                let id = p.interner.insert(p.lexer.slice());
                let span = p.lexer.span();
                Some(Identifier { id, span })
            }
        }
    }
    use crate::{
        lexer::{Lexer, Token},
        SymbolInterner,
    };
    use miette::Diagnostic;
    use swim_utils::SpannedItem;
    use thiserror::Error;

    use self::ast::{AstNode, AST};

    #[derive(Error, Debug, Diagnostic)]
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
        pub fn into_result(mut self) -> (AST, Vec<SpannedItem<ParseError>>) {
            let nodes: Vec<SpannedItem<AstNode>> = self.many::<SpannedItem<AstNode>>();
            (AST::new(nodes), self.errors)
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
        /// NOTE: this parses zero or more items.
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
}
