#![allow(warnings)]

use std::collections::BTreeMap;

use petr_typecheck::TypedExpr;
use petr_utils::Identifier;

trait LoweredFunction {
    type LoweredExpr;
    fn new(
        name: String,
        args: Vec<String>,
        body: Vec<Self::LoweredExpr>,
        returns: Option<Self::LoweredExpr>,
    ) -> Self;
}

trait Lowerer {
    type LoweredExpr;
    type Error;
    type LoweredFunction: LoweredFunction<LoweredExpr = Self::LoweredExpr>;

    fn lower_expr(
        &mut self,
        expr: &TypedExpr,
    ) -> Result<Self::LoweredExpr, Self::Error>;

    fn commit_lowered_function(
        &mut self,
        lowered_function: Self::LoweredFunction,
    ) -> Result<(), Self::Error>;

    /// Mutates `self` to push a new function declaration.
    fn lower_function(
        &mut self,
        function: &petr_typecheck::Function,
    ) -> Result<(), Self::Error> {
        let name = function.name.clone();
        let args = function.params.iter().map(|arg| arg.0.clone()).collect();
        let body = function.body.iter().map(|stmt| self.lower_expr(stmt)).collect::<Result<Vec<_>, _>>()?;

        let returns = if let Some(expr) = &function.returns {
            Some(self.lower_expr(expr)?)
        } else {
            None
        };

        let lowered_function = Self::LoweredFunction::new(name, args, body, returns);
        self.commit_lowered_function(lowered_function)

        Ok(())
    }
}

struct LoweredFunction {
    name:    String,
    args:    Vec<String>,
    body:    Vec<JsStatement>,
    returns: Option<JsExpr>,
}

struct JsFunction {
    name:    String,
    args:    Vec<String>,
    body:    Vec<JsStatement>,
    returns: Option<JsExpr>,
}

enum JsExpr {
    Literal(JsLiteral),
    List(Vec<JsExpr>),
    // `String` is the var name
    Variable(Identifier),
    IIFE {
        body:       Vec<JsStatement>,
        return_val: Box<JsExpr>,
    },
    Ternary {
        condition:   Box<JsExpr>,
        then_branch: Box<JsExpr>,
        else_branch: Box<JsExpr>,
    },
    Call {
        function: String,
        args:     Vec<JsExpr>,
    },
}

enum JsStatement {
    Binding(Identifier, JsExpr),
    // an expression can become a statement if it is just used with a semicolon after it
    Expression(JsExpr),
}

enum JsLiteral {
    Integer(i64),
    Boolean(bool),
    String(String),
}

/// Lowers Petr into Javascript.
struct JsLowerer {
    functions: BTreeMap<String, JsFunction>,
}

impl Lowerer for JsLowerer {
    type Error = ();
    type LoweredExpr = JsExpr;
    type LoweredFunction;

    fn lower_expr(
        &mut self,
        expr: &TypedExpr,
    ) -> Result<Self::LoweredExpr, Self::Error> {
        use petr_typecheck::{Literal::*, TypedExprKind::*};
        match &expr.kind {
            FunctionCall { func, args, ty } => todo!(),
            Literal { value, .. } => Ok(match value {
                Integer(i) => JsExpr::Literal(JsLiteral::Integer(*i)),
                Boolean(b) => JsExpr::Literal(JsLiteral::Boolean(*b)),
                String(s) => JsExpr::Literal(JsLiteral::String(s.clone())),
            }),
            List { elements, ty } => {
                let elements = elements.iter().map(|e| self.lower_expr(e)).collect::<Result<Vec<_>, _>>()?;
                Ok(JsExpr::List(elements))
            },
            Unit => Ok(JsExpr::List(vec![])),
            Variable { name, .. } => Ok(JsExpr::Variable(name.clone())),
            Intrinsic { ty, intrinsic } => self.lower_intrinsic(intrinsic),
            ErrorRecovery(_) => panic!("lowering shouldn't be performed on an AST with errors"),
            ExprWithBindings { bindings, expression } => {
                let mut body = vec![];
                for binding in bindings {
                    body.push(JsStatement::Binding(binding.0, self.lower_expr(&binding.1)?));
                }

                let return_val = Box::new(self.lower_expr(expression)?);
                Ok(JsExpr::IIFE { body, return_val })
            },
            TypeConstructor { ty, args } => todo!(),
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = Box::new(self.lower_expr(condition)?);
                let then_branch = Box::new(self.lower_expr(then_branch)?);
                let else_branch = Box::new(self.lower_expr(else_branch)?);
                Ok(JsExpr::Ternary {
                    condition,
                    then_branch,
                    else_branch,
                })
            },
        }
    }

    fn commit_lowered_function(
        &mut self,
        lowered_function: Self::LoweredFunction,
    ) -> Result<(), Self::Error> {
        todo!()
    }
}

impl JsLowerer {
    fn lower_intrinsic(
        &mut self,
        intrinsic: &petr_typecheck::Intrinsic,
    ) -> Result<JsExpr, ()> {
        use petr_typecheck::Intrinsic::*;
        match intrinsic {
            Puts(expr) => Ok(JsExpr::Call {
                function: "console.log".to_string(),
                args:     vec![self.lower_expr(expr)?],
            }),
            _ => todo!(),
        }
    }
}
