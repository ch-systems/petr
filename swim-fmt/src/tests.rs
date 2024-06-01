use expect_test::{expect, Expect};

use crate::{
    config::{FormatterConfig, FormatterConfigBuilder as FCB},
    Formattable, FormatterContext,
};

fn check(config: FormatterConfig, input: impl Into<String>, expect: Expect) {
    let input = input.into();
    let parser = swim_parse::Parser::new(vec![("test", input)]);
    let (ast, errs, interner, _source_map) = parser.into_result();
    if !errs.is_empty() {
        panic!("parse errors: {:#?}", errs);
    }
    let mut ctx = FormatterContext::from_interner(interner).with_config(config);
    let result = ast.format(&mut ctx).render();
    expect.assert_eq(&result);
}

#[test]
fn basic_func_decl() {
    check(
        Default::default(),
        "function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn func_decl_params_same_line() {
    check(
        FCB::default().put_fn_params_on_new_lines(false).build(),
        "function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            function foo(a ∈ 'int, b ∈ 'int ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn commented_fn_decl() {
    check(
        Default::default(),
        "{- this function does stuff -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            {- this function does stuff -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_comments_before_fn() {
    check(
        Default::default(),
        "{- comment one -} {- comment two -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            {- comment one
               comment two -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}
#[test]
fn multiple_comments_before_fn_no_join() {
    check(
        FCB::default().join_comments(false).build(),
        "{- comment one -} {- comment two -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            {- comment one -}
            {- comment two -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn extract_comments_from_within_decl() {
    check(
        FormatterConfig::default(),
        "function {- this comment should get moved to a more normal location -} foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
            {- this comment should get moved to a more normal location -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_functions() {
    check(
        Default::default(),
        r#"function foo(a in 'int, b in 'int) returns 'int + 2 3
        function bar(a in 'int, b in 'int) returns 'int + 2 3"#,
        expect![[r#"
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3

            function bar(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_functions_with_comments() {
    check(
        Default::default(),
        r#"

        function {- this function is called foo -} foo(a in 'int, b in 'int) returns 'int + 2 3

        function{- this function is called bar -} bar(a in 'int, b in 'int) returns 'int + 2 3"#,
        expect![[r#"
            {- this function is called foo -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3

            {- this function is called bar -}
            function bar(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_functions_more_newlines_between_functions() {
    check(
        FCB::default().newlines_between_items(2).build(),
        r#"

        function {- this function is called foo -} foo(a in 'int, b in 'int) returns 'int + 2 3

        function{- this function is called bar -} bar(a in 'int, b in 'int) returns 'int + 2 3"#,
        expect![[r#"
            {- this function is called foo -}
            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3


            {- this function is called bar -}
            function bar(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_functions_newlines_between_comment_and_item() {
    check(
        FCB::default().newlines_between_comment_and_item(1).build(),
        r#"

        function {- this function is called foo -} foo(a in 'int, b in 'int) returns 'int + 2 3

        {- bar should look like this -}
        function{- this function is called bar -} bar(a in 'int, b in 'int) returns 'int + 2 3"#,
        expect![[r#"
            {- this function is called foo -}

            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3

            {- bar should look like this
               this function is called bar -}

            function bar(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn multiple_functions_newlines_between_comment_and_item_unjoined() {
    check(
        FCB::default()
            .newlines_between_comment_and_item(1)
            .join_comments(false)
            .build(),
        r#"

        function {- this function is called foo -} foo(a in 'int, b in 'int) returns 'int + 2 3

        {- bar should look like this -}
        function{- this function is called bar -} bar(a in 'int, b in 'int) returns 'int + 2 3"#,
        expect![[r#"
            {- this function is called foo -}

            function foo(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3

            {- bar should look like this -}
            {- this function is called bar -}

            function bar(
              a ∈ 'int,
              b ∈ 'int,
            ) returns 'int
              + 2 3
        "#]],
    );
}

#[test]
fn ty_decl_basic() {
    check(
        Default::default(),
        "type foo  = a | b",
        expect![[r#"
        type foo = a
                 | b
    "#]],
    );
}

#[test]
fn ty_decl_basic_more_variants() {
    check(
        Default::default(),
        "type foo  = a | b | c | d | e",
        expect![[r#"
            type foo = a
                     | b
                     | c
                     | d
                     | e
        "#]],
    );
}
#[test]
fn ty_decl_basic_same_line() {
    check(
        FCB::default().put_variants_on_new_lines(false).build(),
        "type foo  = a | b | c | d | e",
        expect![[r#"
            type foo = a | b | c | d | e
        "#]],
    );
}

#[test]
fn ty_decl_no_variants() {
    check(
        Default::default(),
        "type foo",
        expect![[r#"
        type foo;
    "#]],
    );
}

#[test]
fn ty_decl_one_variant() {
    check(
        Default::default(),
        "type foo = a",
        expect![[r#"
            type foo = a

        "#]],
    );
}

#[test]
fn ty_decl_one_variant_fields() {
    check(
        Default::default(),
        "type foo = a 'int 'string",
        expect![[r#"
            type foo = a 'int 'string

        "#]],
    );
}

#[test]
fn ty_decl_multi_variant_fields() {
    check(
        Default::default(),
        "type foo = a 'int 'string | b 'bool 'bool",
        expect![[r#"
            type foo = a 'int 'string
                     | b 'bool 'bool
        "#]],
    );
}

#[test]
fn format_list() {
    check(
        Default::default(),
        "function returns_list() returns 'list [1, 2, 3]",
        expect![[r#""#]],
    );
}
