fn check(
    input: impl Into<String>,
    expect: Expect,
) {
    let input = input.into();
    let parser = petr_parse::Parser::new(vec![("test", input)]);
    let (ast, errs, interner, source_map) = parser.into_result();
    if !errs.is_empty() {
        errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
        panic!("fmt failed: code didn't parse");
    }
    let binder = Binder::from_ast(&ast);
    let result = pretty_print_bindings(&binder, &interner);
    expect.assert_eq(&result);
}

use expect_test::{expect, Expect};
use petr_utils::{render_error, SymbolInterner};

use super::*;
fn pretty_print_bindings(
    binder: &Binder,
    interner: &SymbolInterner,
) -> String {
    let mut result = String::new();
    result.push_str("__Scopes__\n");
    for (scope_id, scope) in binder.scopes.iter() {
        result.push_str(&format!(
            "{}: {} (parent {}):\n",
            Into::<usize>::into(scope_id),
            match scope.kind {
                ScopeKind::Module(name) => format!("Module {}", interner.get(name.id)),
                ScopeKind::Function => "Function".into(),
                ScopeKind::Root => "Root".into(),
                ScopeKind::TypeConstructor => "Type Cons".into(),
                ScopeKind::ExpressionWithBindings => "Expr w/ Bindings".into(),
            },
            scope.parent.map(|x| x.to_string()).unwrap_or_else(|| "none".into())
        ));

        for (func, (func_id, _scope)) in scope.functions {
            let func_name = interner.get(func);
            result.push_str(&format!("  {}: Function {}\n", func_name, func_id));
        }

        for (ty, t) in scope.types {
            let ty_name = interner.get(ty);
            result.push_str(&format!("  {}: Type {:?}\n", ty_name, t));
        }

        for (id, module) in scope.modules {
            let module_name = interner.get(id);
            result.push_str(&format!("  {}: Module {:?}\n", module_name, module));
        }

        for (id, binding) in scope.bindings {
            let binding_name = interner.get(id);
            result.push_str(&format!("  {}: Binding\n", binding_name));
        }

        for (id, param) in &scope.function_params {
            let param_name = interner.get(id);
            result.push_str(&format!("  {}: FunctionParameter {:?}\n", param_name, param));
        }
    }
    result
}

#[test]
fn bind_type_decl() {
    check(
        "type trinary_boolean = True | False | maybe ",
        expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent scopeid0):
                  trinary_boolean: Type TypeId(0)
                  True: Function FunctionId(0)
                  False: Function FunctionId(1)
                  maybe: Function FunctionId(2)
                2: Type Cons (parent scopeid1):
                3: Function (parent scopeid1):
                4: Type Cons (parent scopeid1):
                5: Function (parent scopeid1):
                6: Type Cons (parent scopeid1):
                7: Function (parent scopeid1):
            "#]],
    );
}
#[test]
fn bind_function_decl() {
    check(
        "fn add(a in 'Int, b in 'Int) returns 'Int + 1 2",
        expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent scopeid0):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(13), length: 3 } } })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(24), length: 3 } } })
            "#]],
    );
}

#[test]
fn bind_list_new_scope() {
    check(
        "fn add(a in 'Int, b in  'Int) returns 'Int [ 1, 2, 3, 4, 5, 6 ]",
        expect![[r#"
                __Scopes__
                0: Root (parent none):
                  test: Module Module { root_scope: ScopeId(1), exports: {} }
                1: Module test (parent scopeid0):
                  add: Function FunctionId(0)
                2: Function (parent scopeid1):
                  a: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(13), length: 3 } } })
                  b: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(25), length: 3 } } })
            "#]],
    );
}
