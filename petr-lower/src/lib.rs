use std::collections::BTreeMap;

use petr_typecheck::TypedExpr;

trait Lowerer {
    type Target;
    type Error;

    fn lower_expr(
        &mut self,
        expr: &TypedExpr,
    ) -> Result<Self::Target, Self::Error>;
}

struct JsFunction {
    name: String,
    args: Vec<String>,
    body: Vec<Js>,
}

enum Js {
    Return(Box<Js>),
    Assign(String, Box<Js>),
    Call(String, Vec<String>),
    Literal(JsLiteral),
}

enum JsLiteral {
    String(String),
    Int(i64),
    Float(f64),
}

/// Lowers Petr into Javascript.
struct JSLowerer {
    functions: BTreeMap<String, JsFunction>,
}

struct JsLoweringError {
    kind: JsLoweringErrorKind,
}

enum JsLoweringErrorKind {}

impl Lowerer for JSLowerer {
    type Error = JsLoweringError;
    type Target = Js;

    fn lower_expr(
        &mut self,
        expr: &TypedExpr,
    ) -> Result<Self::Target, Self::Error> {
        use petr_typecheck::TypedExprKind::*;
        match &expr.kind {
            FunctionCall { func, args, ty } => todo!(),
            Literal { value, ty } => todo!(),
            List { elements, ty } => todo!(),
            Unit => todo!(),
            Variable { ty, name } => todo!(),
            Intrinsic { ty, intrinsic } => todo!(),
            ErrorRecovery(_) => todo!(),
            ExprWithBindings { bindings, expression } => todo!(),
            TypeConstructor { ty, args } => todo!(),
            If {
                condition,
                then_branch,
                else_branch,
            } => todo!(),
        }
    }
}
