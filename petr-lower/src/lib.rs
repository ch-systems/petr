use std::collections::BTreeMap;

trait Lowerer {
    type Target;
    fn lower_expr(&mut self, expr: &TypedExpr) -> Result<Self::Target>;
}


struct JsFunction {
    name: String,
    args: Vec<String>,
    body: Vec<JsStatement>,
}

enum Js{
    Return(Box<Js>),
    Assign(String, JsExpr),
    Call(String, Vec<String>),
}

/// Lowers Petr into Javascript.
struct JSLowerer  {
    functions: BTreeMap<String, JsFunction>;
}

