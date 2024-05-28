#![allow(dead_code)]

mod lexer;

use swim_utils::IndexMap;

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
    mod ast {
        use swim_utils::{Span, SpannedItem};

        use crate::SymbolKey;

        use super::{Parse, Parser};

        pub struct AST {
            nodes: Vec<SpannedItem<AstNode>>,
        }

        impl AST {
            pub fn new(nodes: Vec<SpannedItem<AstNode>>) -> AST {
                Self { nodes }
            }
        }

        pub enum AstNode {
            FunctionDeclaration(FunctionDeclaration),
        }

        impl Parse for AstNode {
            fn parse(p: &mut Parser) -> Option<Self> {
                todo!()
            }
        }

        pub struct FunctionDeclaration {
            parameters: Box<[FunctionParameter]>,
            body: SpannedItem<Expression>,
        }

        pub enum Expression {
            OperatorExpression(Box<OperatorExpression>),
        }

        pub struct OperatorExpression {
            lhs: Expression,
            rhs: Expression,
            op: SpannedItem<Operator>,
            span: Span,
        }

        pub struct FunctionParameter {
            name: Ident,
            ty: Ident,
        }

        pub enum Operator {
            Plus,
            Minus,
            Star,
            Slash,
        }

        pub struct Ident {
            id: SymbolKey,
            span: Span,
        }
    }
    use crate::{lexer::Lexer, SymbolInterner};
    use miette::Diagnostic;
    use swim_utils::SpannedItem;
    use thiserror::Error;

    use self::ast::{AstNode, AST};

    #[derive(Error, Debug, Diagnostic)]
    pub enum ParseError {
        #[error("Unmatched parenthesis")]
        UnmatchedParenthesis,
    }

    type Result<T> = std::result::Result<T, ParseError>;

    pub struct Parser<'a> {
        interner: SymbolInterner<'a>,
        lexer: Lexer<'a>,
        errors: Vec<ParseError>,
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
        pub fn parse(mut self) -> (AST, Vec<ParseError>) {
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

            Some(before_span.join(after_span).with_item(result))
        }
    }
}
