use expect_test::{expect, Expect};
use swim_parse::{comments::Commented, parser::ast::FunctionDeclaration};

use crate::{
    config::{FormatterConfig, FormatterConfigBuilder as FCB},
    Formattable, FormatterContext,
};

fn check_fn_decl(config: FormatterConfig, input: impl Into<String>, expect: Expect) {
    let input = input.into();
    let mut parser = swim_parse::parser::Parser::new(vec![input.as_ref()]);
    let decl: Commented<FunctionDeclaration> = match parser.parse() {
        Some(x) if parser.errors().is_empty() => x,
        _ => panic!("failed to parse: {:?}", parser.errors()),
    };
    let mut ctx = FormatterContext::from_interner(parser.interner().clone()).with_config(config);
    let result = decl.format(&mut ctx).render();
    expect.assert_eq(&result);
}

#[test]
fn basic_func_decl() {
    check_fn_decl(
        Default::default(),
        "function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                function foo(
                  a ∈ 'int,
                  b ∈ 'int,
                ) returns 'int
                  +  2 3
            "#]],
    );
}

#[test]
fn func_decl_params_same_line() {
    check_fn_decl(
        FCB::default().put_fn_params_on_new_lines(false).build(),
        "function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                function foo(a ∈ 'int, b ∈ 'int) returns 'int
                  +  2 3
            "#]],
    );
}

#[test]
fn commented_fn_decl() {
    check_fn_decl(
        Default::default(),
        "{- this function does stuff -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                {- this function does stuff -}
                function foo(
                  a ∈ 'int,
                  b ∈ 'int,
                ) returns 'int
                  +  2 3
            "#]],
    );
}

#[test]
fn multiple_comments_before_fn() {
    check_fn_decl(
        Default::default(),
        "{- comment one -} {- comment two -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                {- comment one
                   comment two -}
                function foo(
                  a ∈ 'int,
                  b ∈ 'int,
                ) returns 'int
                  +  2 3
            "#]],
    );
}
#[test]
fn multiple_comments_before_fn_no_join() {
    check_fn_decl(
        FCB::default().join_comments(false).build(),
        "{- comment one -} {- comment two -} function foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                {- comment one -}
                {- comment two -}
                function foo(
                  a ∈ 'int,
                  b ∈ 'int,
                ) returns 'int
                  +  2 3
            "#]],
    );
}

#[test]
fn extract_comments_from_within_decl() {
    check_fn_decl(
        FormatterConfig::default(),
        "function {- this comment should get moved to a more normal location -} foo(a in 'int, b in 'int) returns 'int + 2 3",
        expect![[r#"
                {- this comment should get moved to a more normal location -}
                function foo(
                  a ∈ 'int,
                  b ∈ 'int,
                ) returns 'int
                  +  2 3
            "#]],
    );
}
