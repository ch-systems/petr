pub(crate) mod pretty_print;

use std::rc::Rc;

use super::{Parse, ParseError, Parser};
use crate::{lexer::Token, SymbolKey};
use swim_utils::{Span, SpannedItem};

// NOTE:
// for scopes, track scopes in a tree to know which scopes are parents of which ones
// then, store all names in a mapping from scope id to name
// use the tree to find the scopes that are "in scope" by traversing up
// and then search those scopes

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
        match p.peek().item() {
            Token::FunctionKeyword => Some(AstNode::FunctionDeclaration(p.parse()?)),
            Token::Eof => return None,
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

impl Parse for FunctionDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.token(Token::FunctionKeyword)?;
        let name = p.parse()?;
        p.token(Token::OpenParen)?;
        let parameters = if *p.peek().item() == Token::CloseParen {
            vec![].into_boxed_slice()
        } else {
            p.sequence::<FunctionParameter>(Token::Comma)
                .into_boxed_slice()
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
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Block(Box<BlockExpr>),
    Operator(Box<OperatorExpression>),
    Variable(VariableExpression),
}

#[derive(Debug)]
pub struct VariableExpression {
    name: Identifier,
}

impl Parse for VariableExpression {
    fn parse(p: &mut Parser) -> Option<Self> {
        let id = Identifier::parse(p)?;
        Some(VariableExpression { name: id })
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    contents: Vec<Binding>,
    return_expr: Expression,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
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
        let name: Identifier = p.parse()?;
        p.one_of([Token::InKeyword, Token::IsInSymbol])?;
        let ty: Ty = p.parse()?;
        Some(FunctionParameter { name, ty })
    }
}

#[derive(Debug, Clone, Copy)]
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
        let tok = p.advance();
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

#[derive(Debug, Clone, Copy)]
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

pub struct Comment {
    content: Rc<str>,
}

impl Parse for Comment {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.token(Token::Comment)?;
        let content = Rc::from(p.lexer.slice());
        Some(Comment { content })
    }
}
