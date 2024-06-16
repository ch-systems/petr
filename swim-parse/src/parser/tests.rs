use expect_test::expect;
use swim_utils::{render_error, PrettyPrint};

use super::Parser;

fn check<T: Into<String>>(
    sources: Vec<T>,
    expected: expect_test::Expect,
) {
    let parser = Parser::new(sources.into_iter().map(|s| ("test", s)));
    let (ast, errs, interner, source_map) = parser.into_result();

    let pretty_printed_ast = ast.pretty_print(&interner, 0);
    let errs = errs
        .into_iter()
        .map(|err| format!("{:?}", render_error(&source_map, err)))
        .collect::<Vec<_>>()
        .join("\n");
    let errors_str = if errs.is_empty() {
        String::new()
    } else {
        format!("\n\nErrors\n____\n{errs}")
    };

    expected.assert_eq(&format!("AST\n____\n{pretty_printed_ast}\n{errors_str}"));
}

#[test]
fn prefix_operator_expression() {
    check(
        vec!["function addToFive() returns 'Integer + 4 1"],
        expect![[r#"
            AST
            ____
            module test =
            Func addToFive() -> 'Integer +(4 1)
        "#]],
    )
}

#[test]
fn parse_parameters() {
    check(
        vec!["function addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b"],
        expect![[r#"
            AST
            ____
            module test =
            Func addTwoNums(
              a ∈ 'Integer,
              b ∈ 'Integer
            ) -> 'Integer +(var(a) var(b))
        "#]],
    )
}

#[test]
fn parse_parameters_in_keyword_identical_to_symbol() {
    let parser_one = Parser::new(vec![("test", "function addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b")]);
    let parser_two = Parser::new(vec![("test", "function addTwoNums(a in 'Integer, b in 'Integer) returns 'Integer + a b")]);
    let (ast_one, errs_one, interner_one, _) = parser_one.into_result();

    let (ast_two, errs_two, interner_two, _) = parser_two.into_result();

    let pretty_one = ast_one.pretty_print(&interner_one, 0);
    let pretty_two = ast_two.pretty_print(&interner_two, 0);

    assert_eq!(pretty_one, pretty_two);
    assert_eq!(errs_one, errs_two)
}

#[test]
fn commented_function() {
    check(
        vec![r#"{- this is a comment -} function addTwoNums(a in 'A, b in 'B) returns 'B + a b    "#],
        expect![[r#"
            AST
            ____
            module test =
            {- this is a comment -}
            Func addTwoNums(
              a ∈ 'A,
              b ∈ 'B
            ) -> 'B +(var(a) var(b))
        "#]],
    )
}

#[test]
fn multi_commented_function() {
    check(
        vec![r#" {- comment one -} {- comment two -} function addTwoNums(a in 'A, b in 'B) returns 'B + a b    "#],
        expect![[r#"
            AST
            ____
            module test =
            {- comment one -}
            {- comment two -}
            Func addTwoNums(
              a ∈ 'A,
              b ∈ 'B
            ) -> 'B +(var(a) var(b))
        "#]],
    )
}

#[test]
fn list_expr() {
    check(
        vec!["function list_to_three() returns 'intlist [1, 2, 3]"],
        expect![[r#"
            AST
            ____
            module test =
            Func list_to_three() -> 'intlist [1, 2, 3]
        "#]],
    )
}

#[test]
fn list_with_nested_exprs() {
    check(
        vec!["function list_to_three() returns 'intlist [+ 1 2 , 2, + 1 + 2 3]"],
        expect![[r#"
            AST
            ____
            module test =
            Func list_to_three() -> 'intlist [+(1 2), 2, +(1 +(2 3))]
        "#]],
    )
}

#[test]
fn nested_list() {
    check(
        vec!["function list_to_three() returns 'intlist [[1, 2], [3, 4, + 1 2]]"],
        expect![[r#"
            AST
            ____
            module test =
            Func list_to_three() -> 'intlist [[1, 2], [3, 4, +(1 2)]]
        "#]],
    )
}

#[test]
fn fn_call() {
    check(
        vec!["function makes_function_call() returns 'unit ~fn_call a, b, c"],
        expect![[r#"
            AST
            ____
            module test =
            Func makes_function_call() -> 'unit call fn_call(var(a), var(b), var(c))
        "#]],
    )
}

#[test]
fn let_bindings() {
    check(
        vec![
            "function makes_function_call(c in 'int) returns 'int
                        let a = 1,
                            b = 20
                        ~fn_call a, b, c

            function fn_call(a in 'int, b in 'int, c in 'int) returns 'int + a + b c
                     ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            Func makes_function_call(
              c ∈ 'int
            ) -> 'int 
              let a = 1,
                  b = 20
              call fn_call(var(a), var(b), var(c))

            Func fn_call(
              a ∈ 'int,
              b ∈ 'int,
              c ∈ 'int
            ) -> 'int +(var(a) +(var(b) var(c)))
        "#]],
    )
}

#[test]
fn let_bindings_trailing_comma() {
    check(
        vec![
            "function makes_function_call(c in 'int) returns 'int
                        let a = 1,
                            b = 20,
                        ~fn_call a, b, c

            function fn_call(a in 'int, b in 'int, c in 'int) returns 'int + a + b c
                     ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            Func makes_function_call(
              c ∈ 'int
            ) -> 'int 
              let a = 1,
                  b = 20
              call fn_call(var(a), var(b), var(c))

            Func fn_call(
              a ∈ 'int,
              b ∈ 'int,
              c ∈ 'int
            ) -> 'int +(var(a) +(var(b) var(c)))
        "#]],
    )
}
