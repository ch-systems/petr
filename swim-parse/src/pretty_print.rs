//! Pretty-print the AST for tests and ease of development.

use swim_ast::*;
use swim_utils::{Identifier, SpannedItem, SymbolInterner};

pub trait PrettyPrint {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String;
}

impl PrettyPrint for Ast {
    fn pretty_print(&self, interner: &SymbolInterner, _indentation: usize) -> String {
        let mut buf = String::new();
        for node in &self.nodes {
            buf.push_str(&node.pretty_print(interner, 0));
        }
        buf
    }
}

impl PrettyPrint for AstNode {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        let mut string = match self {
            AstNode::FunctionDeclaration(node) => node.pretty_print(interner, indentation),
            AstNode::TypeDeclaration(ty) => ty.pretty_print(interner, indentation),
        };
        let indentation_str = "  ".repeat(indentation);
        string = format!("{indentation_str}{string}");
        let indentation_str = format!("\n{indentation_str}");
        string = string.replace('\n', &indentation_str);
        string
    }
}

impl PrettyPrint for TypeDeclaration {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        let TypeDeclaration {
            name,
            variants,
            visibility,
        } = self;
        format!(
            "{}{}type {} =\n{}",
            "  ".repeat(indentation),
            if *visibility == Visibility::Exported {
                "exported "
            } else {
                ""
            },
            name.pretty_print(interner, 0),
            variants
                .iter()
                .map(|field| field.pretty_print(interner, indentation + 1))
                .collect::<Vec<_>>()
                .join(" |\n"),
        )
    }
}

impl PrettyPrint for TypeVariant {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        format!(
            "{}{}({})",
            "  ".repeat(indentation),
            self.name.pretty_print(interner, 0),
            self.fields
                .iter()
                .map(|field| field.pretty_print(interner, 0))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

impl PrettyPrint for Identifier {
    fn pretty_print(&self, interner: &SymbolInterner, _: usize) -> String {
        interner.get(self.id).to_string()
    }
}

impl PrettyPrint for Ty {
    fn pretty_print(&self, interner: &SymbolInterner, _: usize) -> String {
        let name = match self {
            Ty::Bool => "bool".to_string(),
            Ty::Int => "int".to_string(),
            Ty::Named(name) => name.pretty_print(interner, 0),
        };
        format!("'{name}")
    }
}

impl PrettyPrint for FunctionParameter {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        format!(
            "{}{} ∈ {}",
            "  ".repeat(indentation),
            self.name.pretty_print(interner, 0),
            self.ty.pretty_print(interner, 0)
        )
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        match self {
            Expression::Literal(Literal::Integer(i)) => i.to_string(),
            Expression::List(list) => list.pretty_print(interner, indentation),
            Expression::Operator(op) => op.pretty_print(interner, indentation),
            Expression::Variable(v) => {
                format!("var({})", v.name.pretty_print(interner, indentation))
            }
            Expression::TypeConstructor => format!("type constructor"),
        }
    }
}

impl PrettyPrint for List {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        format!(
            "[{}]",
            self.elements
                .iter()
                .map(|item| item.pretty_print(interner, indentation))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl PrettyPrint for OperatorExpression {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        let lhs = self.lhs.pretty_print(interner, indentation);
        let rhs = self.rhs.pretty_print(interner, indentation);
        let op = match self.op.item() {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Star => "*",
            Operator::Slash => "/",
        };
        format!("{}({} {})", op, lhs, rhs)
    }
}

impl<T> PrettyPrint for SpannedItem<T>
where
    T: PrettyPrint,
{
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        self.item().pretty_print(interner, indentation)
    }
}

impl<T> PrettyPrint for Commented<T>
where
    T: PrettyPrint,
{
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        let comments = self.comments();
        format!(
            "{}{}",
            if comments.is_empty() {
                String::new()
            } else {
                format!(
                    "{}\n",
                    comments
                        .iter()
                        .map(|comment| comment.pretty_print(interner, indentation))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            },
            self.item().pretty_print(interner, indentation)
        )
    }
}

impl PrettyPrint for Comment {
    fn pretty_print(&self, _interner: &SymbolInterner, indentation: usize) -> String {
        format!("{}{{- {} -}}", "  ".repeat(indentation), self.content)
    }
}

impl PrettyPrint for FunctionDeclaration {
    fn pretty_print(&self, interner: &SymbolInterner, indentation: usize) -> String {
        let FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            visibility,
        } = self;
        format!(
            "{}{}Func {}({}{}{}) -> {} {}",
            "  ".repeat(indentation),
            if *visibility == Visibility::Exported {
                "exported "
            } else {
                ""
            },
            name.pretty_print(interner, 0),
            if parameters.is_empty() { "" } else { "\n" },
            parameters
                .iter()
                .map(|param| param.pretty_print(interner, indentation + 1))
                .collect::<Vec<_>>()
                .join(",\n"),
            if parameters.is_empty() { "" } else { "\n" },
            return_type.pretty_print(interner, indentation),
            body.pretty_print(interner, indentation)
        )
    }
}
