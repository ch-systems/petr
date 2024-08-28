#![allow(warnings)]

use std::{collections::BTreeMap, fmt::Display, rc::Rc};

use petr_typecheck::TypedExpr;
use petr_utils::{Identifier, SymbolId, SymbolInterner};

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use petr_api::{render_error, resolve_symbols, type_check};

    use crate::{JsLowerer, Lowerer as _};

    fn check(
        sources: Vec<(&str, String)>,
        expect: Expect,
    ) {
        let parser = petr_parse::Parser::new(sources);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("build failed: code didn't parse");
        }
        let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
        if !errs.is_empty() {
            dbg!(&errs);
            panic!("build failed: resolution");
        }
        let solution = match type_check(resolved) {
            Ok(s) => s,
            Err(type_errs) => {
                type_errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
                panic!("build failed: code didn't type check");
            },
        };

        // unideal clone, but this is a test
        // TODO: improve typesolution -> Lowerer API
        let mut lowerer = JsLowerer::from_interner(solution.interner.clone());
        for (id, function) in solution.functions() {
            lowerer.lower_function(function).unwrap();
        }

        let js_source = lowerer.produce_target();
        expect.assert_eq(&js_source.to_string());

        // TODO: execute JS and assert on the result in this test harness
    }

    #[test]
    fn returns_42() {
        check(
            vec![("test", "fn main() returns 'int 42".to_string())],
            expect![[r#"
                fn main() {
                    return 42;
                }
            "#]],
        );
    }
}

trait LoweredFunction {
    type LoweredExpr;
    fn new(
        name: Rc<str>,
        args: Vec<Rc<str>>,
        body: Self::LoweredExpr,
    ) -> Self;
}

trait SymbolRealizer {
    fn realize_symbol_id(
        &self,
        id: SymbolId,
    ) -> Rc<str>;
}

trait Lowerer: SymbolRealizer {
    type LoweredExpr;
    type Error;
    type LoweredFunction: LoweredFunction<LoweredExpr = Self::LoweredExpr>;
    type Target: Display;

    fn lower_expr(
        &mut self,
        expr: &TypedExpr,
    ) -> Result<Self::LoweredExpr, Self::Error>;

    fn commit_lowered_function(
        &mut self,
        lowered_function: Self::LoweredFunction,
    ) -> Result<(), Self::Error>;

    /// Produce the target language source code.
    fn produce_target(self) -> Self::Target;

    /// Mutates `self` to push a new function declaration.
    fn lower_function(
        &mut self,
        function: &petr_typecheck::Function,
    ) -> Result<(), Self::Error> {
        let name = function.name.clone();
        let args = function.params.iter().map(|arg| self.realize_symbol_id(arg.0.id)).collect();
        let body = self.lower_expr(&function.body)?;
        let realized_name = self.realize_symbol_id(function.name.id);

        let lowered_function = Self::LoweredFunction::new(realized_name, args, body);
        self.commit_lowered_function(lowered_function);

        Ok(())
    }
}

impl LoweredFunction for LoweredJsFunction {
    type LoweredExpr = JsExpr;

    fn new(
        name: Rc<str>,
        args: Vec<Rc<str>>,
        body: Self::LoweredExpr,
    ) -> Self {
        Self { name, args, body }
    }
}

struct LoweredJsFunction {
    name: Rc<str>,
    args: Vec<Rc<str>>,
    body: JsExpr,
}

struct JsFunction {
    name: String,
    args: Vec<String>,
    body: JsExpr,
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

enum Js {
    Return(Box<Js>),
    Assign(String, Box<Js>),
    Call(String, Vec<String>),
    Literal(JsLiteral),
}

enum JsLiteral {
    String(String),
    Integer(i64),
    Boolean(bool),
    Float(f64),
}

/// Lowers Petr into Javascript.
struct JsLowerer {
    functions:       BTreeMap<String, JsFunction>,
    symbol_interner: SymbolInterner,
}

impl JsLowerer {
    fn from_interner(interner: SymbolInterner) -> Self {
        Self {
            functions:       Default::default(),
            symbol_interner: interner,
        }
    }
}

impl SymbolRealizer for JsLowerer {
    fn realize_symbol_id(
        &self,
        id: SymbolId,
    ) -> Rc<str> {
        self.symbol_interner.get(id)
    }
}

/// Simple wrapper type to denote that a string is JS source
struct JsSource(String);

impl Display for JsSource {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Lowerer for JsLowerer {
    type Error = ();
    type LoweredExpr = JsExpr;
    type LoweredFunction = LoweredJsFunction;
    type Target = JsSource;

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

    fn produce_target(self) -> Self::Target {
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
struct JsLoweringError {
    kind: JsLoweringErrorKind,
}

enum JsLoweringErrorKind {}
