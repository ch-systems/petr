use std::rc::Rc;

use crate::comments::Commented;
use swim_utils::{Identifier, SpannedItem};

// NOTE:
// for scopes, track scopes in a tree to know which scopes are parents of which ones
// then, store all names in a mapping from scope id to name
// use the tree to find the scopes that are "in scope" by traversing up
// and then search those scopes

pub struct Ast {
    pub nodes: Vec<SpannedItem<AstNode>>,
}

impl Ast {
    pub fn new(nodes: Vec<SpannedItem<AstNode>>) -> Ast {
        Self { nodes }
    }
}

pub enum AstNode {
    FunctionDeclaration(Commented<FunctionDeclaration>),
    TypeDeclaration(Commented<TypeDeclaration>),
}

pub struct TypeDeclaration {
    pub name: Identifier,
    pub variants: Box<[TypeVariant]>,
}

pub struct TypeVariant {
    pub name: Identifier,
    pub fields: Box<[Ty]>,
}

pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Box<[FunctionParameter]>,
    pub return_type: Ty,
    pub body: SpannedItem<Expression>,
}

pub enum Expression {
    Literal(Literal),
    List(List),
    Operator(Box<OperatorExpression>),
    Variable(VariableExpression),
}

pub struct VariableExpression {
    pub name: Identifier,
}

pub struct List {
    pub elements: Box<[Commented<SpannedItem<Expression>>]>,
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

pub struct OperatorExpression {
    pub lhs: SpannedItem<Expression>,
    pub rhs: SpannedItem<Expression>,
    pub op: SpannedItem<Operator>,
}

pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Ty,
}

#[derive(Clone, Copy)]
pub struct Ty {
    pub ty_name: Identifier,
}

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
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

pub struct Comment {
    pub content: Rc<str>,
}

impl Comment {
    pub fn new(item: impl AsRef<str>) -> Self {
        Self {
            content: Rc::from(item.as_ref()),
        }
    }
}
