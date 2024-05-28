use crate::parser::ast::pretty_print::*;
use expect_test::expect;

use super::Parser;

fn check<T: AsRef<str>>(sources: Vec<T>, expected: expect_test::Expect) {
    let parser = Parser::new(sources.iter().map(|s| s.as_ref()));
    let (ast, errs, interner) = parser.into_result();

    let pretty_printed_ast = ast.pretty_print(&interner, 0);

    expected.assert_eq(&format!("{pretty_printed_ast}\n\n{errs:#?}"));
}

#[test]
fn prefix_operator_expression() {
    check(
        vec!["function addToFive() returns 'Integer + 4 1"],
        expect![[r#"
            AST
              Func addToFive() -> 'Integer +(4 1)

            []"#]],
    )
}

#[test]
fn parse_parameters() {
    check(
        vec!["function addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b"],
        expect![[r#"
            AST
              Func addTwoNums(
                  a ∈ 'Integer,
                  b ∈ 'Integer
              ) -> 'Integer +(var(a) var(b))

            []"#]],
    )
}

#[test]
fn parse_parameters_in_keyword_identical_to_symbol() {
    let parser_one = Parser::new(vec![
        "function addTwoNums(a ∈ 'Integer, b ∈ 'Integer) returns 'Integer + a b",
    ]);
    let parser_two = Parser::new(vec![
        "function addTwoNums(a in 'Integer, b in 'Integer) returns 'Integer + a b",
    ]);
    let (ast_one, errs_one, interner_one) = parser_one.into_result();

    let (ast_two, errs_two, interner_two) = parser_two.into_result();

    let pretty_one = ast_one.pretty_print(&interner_one, 0);
    let pretty_two = ast_two.pretty_print(&interner_two, 0);

    assert_eq!(pretty_one, pretty_two);
    assert_eq!(errs_one, errs_two)
}
