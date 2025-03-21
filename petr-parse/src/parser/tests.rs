use expect_test::expect;
use petr_utils::PrettyPrint;

use super::Parser;

fn check<T: Into<String>>(
    sources: Vec<T>,
    expected: expect_test::Expect,
) {
    let parser = Parser::new(sources.into_iter().map(|s| ("test", s)));
    let (ast, errs, interner, _) = parser.into_result();

    let pretty_printed_ast = ast.pretty_print(&interner, 0);
    let errs = errs.into_iter().map(|err| format!("{err:?}")).collect::<Vec<_>>().join("\n");
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
        vec!["fn addToFive() returns 'Integer + 4 1"],
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
        vec!["fn addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b"],
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
    let parser_one = Parser::new(vec![("test", "fn addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b")]);
    let parser_two = Parser::new(vec![("test", "fn addTwoNums(a in 'Integer, b in 'Integer) returns 'Integer + a b")]);
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
        vec![r#"{- this is a comment -} fn addTwoNums(a in 'A, b in 'B) returns 'B + a b    "#],
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
        vec![r#" {- comment one -} {- comment two -} fn addTwoNums(a in 'A, b in 'B) returns 'B + a b    "#],
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
        vec!["fn list_to_three() returns 'intlist [1, 2, 3]"],
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
        vec!["fn list_to_three() returns 'intlist [+ 1 2 , 2, + 1 + 2 3]"],
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
        vec!["fn list_to_three() returns 'intlist [[1, 2], [3, 4, + 1 2]]"],
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
        vec!["fn makes_function_call() returns 'unit ~fn_call a, b, c"],
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
            "fn makes_function_call(c in 'int) returns 'int
                        let a = 1;
                            b = 20
                        ~fn_call a, b, c

            fn fn_call(a in 'int, b in 'int, c in 'int) returns 'int + a + b c
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
            "fn makes_function_call(c in 'int) returns 'int
                        let a = 1;
                            b = 20;
                        ~fn_call a, b, c

            fn fn_call(a in 'int, b in 'int, c in 'int) returns 'int + a + b c
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
fn imports_and_exports() {
    check(
        vec![
            "import moduleA.itemA

             fn someFunction(a in 'int) returns 'int + a 1

             import something_else.foo as bar
            ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            import moduleA.itemA
            Func someFunction(
              a ∈ 'int
            ) -> 'int +(var(a) 1)
            import something_else.foo as bar

        "#]],
    )
}

#[test]
fn if_exp_basic() {
    check(
        vec![
            "fn if_exp() returns 'int
                if true then 1 else 0
            ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            Func if_exp() -> 'int if true then 1 else 0

        "#]],
    )
}

#[test]
fn nested_if_exp() {
    check(
        vec![
            "fn if_exp() returns 'int
                if true then if false then 1 else 0 else 0
            ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            Func if_exp() -> 'int if true then if false then 1 else 0 else 0

        "#]],
    )
}

#[test]
fn if_without_else() {
    check(
        vec![
            "
            fn hi(x in 'int) returns 'int
                if x then 1
                ",
        ],
        expect![[r#"
            AST
            ____
            module test =
            Func hi(
              x ∈ 'int
            ) -> 'int if var(x) then 1

        "#]],
    )
}

#[test]
fn constant_literal_types() {
    check(
        vec![
            r#"
            type MyType = 1 | 2 | 3 | "hi" | Constructed number 'int boolean 'bool
                "#,
        ],
        expect![[r#"
            AST
            ____
            module test =
            type MyType =
            1 |
            2 |
            3 |
            "hi" |
              Constructed(number: 'int boolean: 'bool)
        "#]],
    )
}

#[test]
fn unit_type() {
    check(
        vec![
            r#"
            type bar = a f1 'unit f2 ε 

            fn foo(x in ε, y in 'unit) returns 'unit 
              ~std.io.print "Hello, World!"
            "#,
        ],
        expect![[r#"
            AST
            ____
            module test =
            type bar =
              a(f1: 'unit f2: 'unit)Func foo(
              x ∈ 'unit,
              y ∈ 'unit
            ) -> 'unit call std.io.print("Hello, World!")

        "#]],
    )
}

#[test]
fn sum_types() {
    check(
        vec![r#"fn myFunc(x in 'sum 1 | 2 | 3) returns 'int 5"#],
        expect![[r#"
            AST
            ____
            module test =
            Func myFunc(
              x ∈ ''lit ty 1 | 'lit ty 2 | 'lit ty 3
            ) -> 'int 5

        "#]],
    )
}
