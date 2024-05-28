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
