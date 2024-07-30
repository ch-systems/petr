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

fn check_with_stdlib(
    input: impl Into<String>,
    expect: Expect,
) {
    let input = input.into();
    let mut sources = petr_stdlib::stdlib();
    sources.push(("test", &input));
    let parser = petr_parse::Parser::new(sources);
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

        for (func, (func_id, _scope)) in &scope.functions {
            let func_name = interner.get(*func);
            result.push_str(&format!("  {}: Function {}\n", func_name, func_id));
        }

        for (ty, t) in &scope.types {
            let ty_name = interner.get(*ty);
            result.push_str(&format!("  {}: Type {:?}\n", ty_name, t));
        }

        for (id, module) in &scope.modules {
            let module_name = interner.get(*id);
            result.push_str(&format!("  {}: Module {:?}\n", module_name, module));
        }

        for id in scope.bindings.keys() {
            let binding_name = interner.get(*id);
            result.push_str(&format!("  {}: Binding\n", binding_name));
        }

        for (id, param) in &scope.function_params {
            let param_name = interner.get(*id);
            result.push_str(&format!("  {}: FunctionParameter {:?}\n", param_name, param));
        }

        for id in scope.imports.keys() {
            let import_name = interner.get(*id);
            result.push_str(&format!("  {id}: Import {}\n", import_name));
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
              test: Module ModuleId(0)
            1: Module test (parent scopeid0):
              True: Function functionid0
              False: Function functionid1
              maybe: Function functionid2
              trinary_boolean: Type TypeId(0)
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
              test: Module ModuleId(0)
            1: Module test (parent scopeid0):
              add: Function functionid0
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
              test: Module ModuleId(0)
            1: Module test (parent scopeid0):
              add: Function functionid0
            2: Function (parent scopeid1):
              a: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(13), length: 3 } } })
              b: FunctionParameter Named(Identifier { id: SymbolId(3), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(25), length: 3 } } })
        "#]],
    );
}

#[test]
fn imports_work() {
    check_with_stdlib(
        "import std.ops.add fn main() returns 'int ~add(2, 3)",
        expect![[r#"
            __Scopes__
            0: Root (parent none):
              std: Module ModuleId(0)
              test: Module ModuleId(7)
            1: Module std (parent scopeid0):
              ops: Module ModuleId(1)
              io: Module ModuleId(3)
              mem: Module ModuleId(5)
            2: Module ops (parent scopeid0):
              add: Function functionid0
              sub: Function functionid1
              mult: Function functionid2
              div: Function functionid3
              eq: Function functionid4
            3: Function (parent scopeid2):
              lhs: FunctionParameter Int
              rhs: FunctionParameter Int
            4: Function (parent scopeid2):
              lhs: FunctionParameter Int
              rhs: FunctionParameter Int
            5: Function (parent scopeid2):
              lhs: FunctionParameter Int
              rhs: FunctionParameter Int
            6: Function (parent scopeid2):
              lhs: FunctionParameter Int
              rhs: FunctionParameter Int
            7: Function (parent scopeid2):
              lhs: FunctionParameter Named(Identifier { id: SymbolId(10), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(307), length: 1 } } })
              rhs: FunctionParameter Named(Identifier { id: SymbolId(10), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(318), length: 1 } } })
            8: Module io (parent scopeid0):
              print: Function functionid5
            9: Function (parent scopeid8):
              content: FunctionParameter String
            10: Module mem (parent scopeid0):
              Unsized: Function functionid6
              Sized: Function functionid7
              malloc: Function functionid8
              size_of: Function functionid9
              Ptr: Type TypeId(0)
            11: Type Cons (parent scopeid10):
            12: Function (parent scopeid10):
              address: FunctionParameter Int
            13: Type Cons (parent scopeid10):
            14: Function (parent scopeid10):
              address: FunctionParameter Int
              size: FunctionParameter Int
            15: Function (parent scopeid10):
              size: FunctionParameter Int
            16: Expr w/ Bindings (parent scopeid15):
              allocated: Binding
            17: Function (parent scopeid10):
              expr: FunctionParameter Named(Identifier { id: SymbolId(10), span: Span { source: SourceId(2), span: SourceSpan { offset: SourceOffset(246), length: 1 } } })
            18: Module test (parent scopeid0):
              main: Function functionid10
              symbolid2: Import add
            19: Function (parent scopeid18):
        "#]],
    );
}
