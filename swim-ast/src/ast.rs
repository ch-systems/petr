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

#[derive(Clone)]
pub struct TypeDeclaration {
    pub name: Identifier,
    pub variants: Box<[SpannedItem<TypeVariant>]>,
    pub visibility: Visibility,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Local,
    Exported,
}

#[derive(Clone)]
pub struct TypeVariant {
    pub name: Identifier,
    pub fields: Box<[Ty]>,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Box<[FunctionParameter]>,
    pub return_type: Ty,
    pub body: SpannedItem<Expression>,
    pub visibility: Visibility,
}

#[derive(Clone)]
pub enum Expression {
    Literal(Literal),
    List(List),
    Operator(Box<OperatorExpression>),
    FunctionCall(FunctionCall),
    Variable(Identifier),
    TypeConstructor,
}

#[derive(Clone)]
pub struct FunctionCall {
    pub func_name: Identifier,
    pub args: Box<[Expression]>,
    // used for the formatter, primarily
    pub args_were_parenthesized: bool,
}

#[derive(Clone)]
pub struct VariableExpression {
    pub name: Identifier,
}
#[derive(Clone)]
pub struct List {
    pub elements: Box<[Commented<SpannedItem<Expression>>]>,
}

#[derive(Clone, Copy, Debug)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Integer(i) => i.to_string(),
            Literal::Boolean(b) => b.to_string(),
        }
    }
}

#[derive(Clone)]
pub struct OperatorExpression {
    pub lhs: SpannedItem<Expression>,
    pub rhs: SpannedItem<Expression>,
    pub op: SpannedItem<Operator>,
}

#[derive(Clone, Debug, Copy)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Ty,
}

#[derive(Clone, Copy, Debug)]
pub enum Ty {
    Int,
    Bool,
    Named(Identifier),
}

#[derive(Clone)]
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

#[derive(Clone)]
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
