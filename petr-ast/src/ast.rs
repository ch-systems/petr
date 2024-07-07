use std::rc::Rc;

use petr_utils::{Identifier, Path, SpannedItem};

use crate::comments::Commented;

// todo rename to parse tree or parsed program
pub struct Ast {
    pub modules: Vec<Module>,
}

impl std::fmt::Debug for Ast {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        writeln!(f, "AST")?;
        for module in self.modules.iter() {
            let path = module.name.iter().map(|x| format!("{}", x.id)).collect::<Vec<_>>().join(".");
            writeln!(f, "Module: {path}")?;
            for node in module.nodes.iter() {
                match node.item() {
                    AstNode::FunctionDeclaration(fun) => writeln!(f, "  Function: {}", fun.item().name.id)?,
                    AstNode::TypeDeclaration(ty) => writeln!(f, "  Type: {}", ty.item().name.id)?,
                    AstNode::ImportStatement(i) => writeln!(
                        f,
                        "  Import: {}",
                        i.item().path.iter().map(|x| format!("{}", x.id)).collect::<Vec<_>>().join(".")
                    )?,
                }
            }
        }
        Ok(())
    }
}

pub struct Module {
    pub name:  Path,
    pub nodes: Vec<SpannedItem<AstNode>>,
}

impl Ast {
    pub fn new(nodes: Vec<Module>) -> Ast {
        Self { modules: nodes }
    }
}

pub enum AstNode {
    FunctionDeclaration(Commented<FunctionDeclaration>),
    TypeDeclaration(Commented<TypeDeclaration>),
    ImportStatement(Commented<ImportStatement>),
}

pub struct ImportStatement {
    pub path:       Path,
    pub alias:      Option<Identifier>,
    pub visibility: Visibility,
}
impl ImportStatement {
    pub fn is_exported(&self) -> bool {
        self.visibility == Visibility::Exported
    }
}

#[derive(Clone)]
pub struct TypeDeclaration {
    pub name:       Identifier,
    pub variants:   Box<[SpannedItem<TypeVariant>]>,
    pub visibility: Visibility,
}
impl TypeDeclaration {
    pub fn is_exported(&self) -> bool {
        self.visibility == Visibility::Exported
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Local,
    Exported,
}

#[derive(Clone)]
pub struct TypeVariant {
    pub name:   Identifier,
    pub fields: Box<[TypeField]>,
}

#[derive(Clone)]
pub struct TypeField {
    pub name: Identifier,
    pub ty:   Ty,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub name:        Identifier,
    pub parameters:  Box<[FunctionParameter]>,
    pub return_type: Ty,
    pub body:        SpannedItem<Expression>,
    pub visibility:  Visibility,
}
impl FunctionDeclaration {
    pub fn is_exported(&self) -> bool {
        self.visibility == Visibility::Exported
    }
}

#[derive(Clone)]
pub enum Expression {
    Literal(Literal),
    List(List),
    Operator(Box<OperatorExpression>),
    FunctionCall(FunctionCall),
    Variable(Identifier),
    IntrinsicCall(IntrinsicCall),
    Binding(ExpressionWithBindings),
    TypeConstructor(Box<[Expression]>),
}

#[derive(Clone)]
pub struct ExpressionWithBindings {
    pub bindings:   Vec<Binding>,
    pub expression: Box<Expression>,
    pub expr_id:    ExprId,
}

#[derive(Clone, Debug, PartialOrd, Ord, Eq, PartialEq, Copy)]
pub struct ExprId(pub usize);

#[derive(Clone)]
pub struct Binding {
    pub name: Identifier,
    pub val:  Expression,
}

#[derive(Clone)]
pub struct IntrinsicCall {
    pub intrinsic: Intrinsic,
    pub args:      Box<[Expression]>,
}

impl std::fmt::Display for Intrinsic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Intrinsic::Puts => write!(f, "puts"),
            Intrinsic::Add => write!(f, "add"),
            Intrinsic::Subtract => write!(f, "subtract"),
            Intrinsic::Multiply => write!(f, "multiply"),
            Intrinsic::Divide => write!(f, "divide"),
            Intrinsic::Malloc => write!(f, "malloc"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Intrinsic {
    /// intrinsic for `libc` puts
    Puts,
    Add,
    Subtract,
    Multiply,
    Divide,
    Malloc,
}

#[derive(Clone)]
pub struct FunctionCall {
    pub func_name: Path,
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

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    String(Rc<str>),
}

impl std::fmt::Display for Literal {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Clone)]
pub struct OperatorExpression {
    pub lhs: SpannedItem<Expression>,
    pub rhs: SpannedItem<Expression>,
    pub op:  SpannedItem<Operator>,
}

#[derive(Clone, Debug, Copy)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty:   Ty,
}

#[derive(Clone, Copy, Debug)]
pub enum Ty {
    Int,
    Bool,
    Named(Identifier),
    String,
    Unit,
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
