pub mod pretty_print;

use std::rc::Rc;

use super::{Parse, ParseError, ParseErrorKind, Parser};
use crate::{comments::Commented, parser::Token, SymbolKey};
use swim_utils::{Span, SpannedItem};

// NOTE:
// for scopes, track scopes in a tree to know which scopes are parents of which ones
// then, store all names in a mapping from scope id to name
// use the tree to find the scopes that are "in scope" by traversing up
// and then search those scopes

pub struct AST {
    nodes: Vec<SpannedItem<AstNode>>,
}

impl AST {
    pub fn new(nodes: Vec<SpannedItem<AstNode>>) -> AST {
        Self { nodes }
    }
}

impl AST {
    pub fn nodes(&self) -> &Vec<SpannedItem<AstNode>> {
        &self.nodes
    }
}

pub enum AstNode {
    FunctionDeclaration(Commented<FunctionDeclaration>),
    TypeDeclaration(Commented<TypeDeclaration>),
}

pub struct TypeDeclaration {
    name: Identifier,
    variants: Box<[TypeVariant]>,
}
impl TypeDeclaration {
    pub fn variants(&self) -> &[TypeVariant] {
        &self.variants
    }
    pub fn name(&self) -> Identifier {
        self.name
    }
}

pub struct TypeVariant {
    name: Identifier,
    fields: Box<[Ty]>,
}

impl TypeVariant {
    pub fn name(&self) -> Identifier {
        self.name
    }

    pub fn fields(&self) -> &[Ty] {
        &self.fields
    }
}

impl Parse for TypeDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing type declaration",
            |p| -> Option<Self> {
                p.token(Token::TypeKeyword)?;
                let name = p.parse()?;
                if *p.peek().item() != Token::Equals {
                    // if there's no equals, then this is a type with no variants
                    return Some(Self {
                        name,
                        variants: vec![].into_boxed_slice(),
                    });
                }
                p.token(Token::Equals)?;
                let variants = p.sequence::<TypeVariant>(Token::Pipe)?;
                Some(Self {
                    name,
                    variants: variants.into_boxed_slice(),
                })
            },
        )
    }
}

impl Parse for TypeVariant {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing type variant",
            |p| -> Option<Self> {
                let mut buf = vec![];
                let name = p.parse()?;
                loop {
                    let peek = *p.peek().item();
                    if peek == Token::TyMarker {
                        let field: Ty = p.parse()?;
                        buf.push(field);
                    } else {
                        break;
                    }
                }
                Some(Self {
                    name,
                    fields: buf.into_boxed_slice(),
                })
            },
        )
    }
}

impl Parse for AstNode {
    fn parse(p: &mut Parser) -> Option<Self> {
        match p.peek().item() {
            Token::FunctionKeyword => Some(AstNode::FunctionDeclaration(p.parse()?)),
            Token::TypeKeyword => Some(AstNode::TypeDeclaration(p.parse()?)),
            Token::Eof => None,
            a => {
                let span = p.peek().span();
                p.push_error(span.with_item(ParseErrorKind::ExpectedOneOf(
                    vec![Token::FunctionKeyword, Token::TypeKeyword, Token::Eof],
                    *a,
                )));
                None
            }
        }
    }
}

pub struct FunctionDeclaration {
    name: Identifier,
    parameters: Box<[FunctionParameter]>,
    return_type: Ty,
    body: SpannedItem<Expression>,
}

impl Parse for FunctionDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing function declaration",
            |p| -> Option<Self> {
                p.token(Token::FunctionKeyword)?;
                let name = p.parse()?;
                p.token(Token::OpenParen)?;
                let parameters = if *p.peek().item() == Token::CloseParen {
                    vec![].into_boxed_slice()
                } else {
                    p.sequence(Token::Comma)?.into_boxed_slice()
                };
                p.token(Token::CloseParen)?;
                p.token(Token::ReturnsKeyword)?;
                let return_type = p.parse()?;
                let body = p.parse()?;
                Some(Self {
                    name,
                    parameters,
                    return_type,
                    body,
                })
            },
        )
    }
}
impl FunctionDeclaration {
    pub fn name(&self) -> Identifier {
        self.name
    }

    pub fn parameters(&self) -> &Box<[FunctionParameter]> {
        &self.parameters
    }

    pub fn return_type(&self) -> Ty {
        self.return_type
    }

    pub fn body(&self) -> &Expression {
        self.body.item()
    }
}

pub enum Expression {
    Literal(Literal),
    Block(Box<BlockExpr>),
    Operator(Box<OperatorExpression>),
    Variable(VariableExpression),
}

pub struct VariableExpression {
    name: Identifier,
}

impl VariableExpression {
    pub fn name(&self) -> Identifier {
        self.name
    }
}

impl Parse for VariableExpression {
    fn parse(p: &mut Parser) -> Option<Self> {
        let id = Identifier::parse(p)?;
        Some(VariableExpression { name: id })
    }
}

pub struct BlockExpr {
    contents: Vec<Binding>,
    return_expr: Expression,
}

pub struct Binding {
    name: Identifier,
    value: Expression,
}

impl Parse for Expression {
    fn parse(p: &mut Parser) -> Option<Self> {
        match p.peek().item() {
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
            // TODO might not want to do variables this way
            // may have to advance and peek to see if its a fn call etc
            Token::Identifier => Some(Expression::Variable(p.parse()?)),
            a => todo!("need to parse expr {a:?}"),
        }
    }
}

pub enum Literal {
    Integer(i64),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Integer(i) => i.to_string(),
        }
    }
}

impl Parse for Literal {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Integer => Some(Literal::Integer(
                p.lexer
                    .slice()
                    .parse()
                    .expect("lexer should have verified this"),
            )),
            _ => {
                p.push_error(
                    p.lexer
                        .span()
                        .with_item(ParseErrorKind::ExpectedToken(Token::Integer, *tok.item())),
                );
                None
            }
        }
    }
}

pub struct OperatorExpression {
    lhs: SpannedItem<Expression>,
    rhs: SpannedItem<Expression>,
    op: SpannedItem<Operator>,
}

impl OperatorExpression {
    pub fn lhs(&self) -> &SpannedItem<Expression> {
        &self.lhs
    }

    pub fn rhs(&self) -> &SpannedItem<Expression> {
        &self.rhs
    }

    pub fn op(&self) -> &SpannedItem<Operator> {
        &self.op
    }
}

pub struct FunctionParameter {
    name: Identifier,
    ty: Ty,
}

impl FunctionParameter {
    pub fn name(&self) -> Identifier {
        self.name
    }
    pub fn ty(&self) -> Ty {
        self.ty
    }
}

impl Parse for FunctionParameter {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing function parameter",
            |p| -> Option<Self> {
                let name: Identifier = p.parse()?;
                p.one_of([Token::InKeyword, Token::IsInSymbol])?;
                let ty: Ty = p.parse()?;
                Some(FunctionParameter { name, ty })
            },
        )
    }
}

#[derive(Clone, Copy)]
pub struct Ty {
    ty_name: Identifier,
}

impl Ty {
    pub fn name(&self) -> Identifier {
        self.ty_name
    }
}

impl Parse for Ty {
    // TODO types are not just idents,
    // they can be more than that
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing type", |p| -> Option<Self> {
            p.token(Token::TyMarker)?;
            let ident = Identifier::parse(p)?;
            Some(Self { ty_name: ident })
        })
    }
}

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
}

impl Parse for Operator {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Plus => Some(Operator::Plus),
            Token::Minus => Some(Operator::Minus),
            Token::Star => Some(Operator::Star),
            Token::Slash => Some(Operator::Slash),
            _ => {
                p.push_error(p.lexer.span().with_item(ParseErrorKind::ExpectedOneOf(
                    vec![Token::Plus, Token::Minus, Token::Star, Token::Slash],
                    *tok.item(),
                )));
                None
            }
        }
    }
}

impl Operator {
    pub fn as_str(&self) -> &'static str {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Star => "*",
            Operator::Slash => "/",
        }
    }
}

#[derive(Clone, Copy)]
pub struct Identifier {
    id: SymbolKey,
    span: Span,
}
impl Identifier {
    pub fn id(&self) -> SymbolKey {
        self.id
    }
}

impl Parse for Identifier {
    fn parse(p: &mut Parser) -> Option<Self> {
        let identifier = p.advance();
        if *identifier.item() != Token::Identifier {
            p.push_error(p.lexer.span().with_item(ParseErrorKind::ExpectedIdentifier(
                p.lexer.slice().to_string(),
            )));
        }
        let id = p.interner.insert(p.lexer.slice());
        let span = p.lexer.span();
        Some(Identifier { id, span })
    }
}

pub struct Comment {
    content: Rc<str>,
}

impl Comment {
    pub fn new(item: impl AsRef<str>) -> Self {
        Self {
            content: Rc::from(item.as_ref()),
        }
    }
    pub fn content(&self) -> Rc<str> {
        self.content.clone()
    }
}

impl Parse for Comment {
    fn parse(p: &mut Parser) -> Option<Self> {
        // because it matched in the lexer, we know the first two and last two chars
        // are {- and -}, so we slice those out.
        let slice = p.lexer.slice();
        let content = &slice[2..slice.len() - 2];
        let content = Rc::from(content.trim());
        Some(Comment { content })
    }
}
