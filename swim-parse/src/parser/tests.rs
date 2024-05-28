use expect_test::expect;

use super::Parser;

fn check<T: AsRef<str>>(sources: Vec<T>, expected: expect_test::Expect) {
    let parser = Parser::new(sources.iter().map(|s| s.as_ref()));
    let (ast, errs) = parser.into_result();

    expected.assert_eq(&format!("{ast:#?}\n\n{errs:#?}"));
}
#[test]
fn prefix_operator_expression() {
    check(
        vec!["function addToFive() returns ~Integer + 4 1"],
        expect![[r#"
            AST {
                nodes: [
                    SpannedItem FunctionDeclaration(FunctionDeclaration { name: Identifier { id: SymbolKey(0), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(9), length: 9 } } }, parameters: [], return_type: Ty { ty_name: Identifier { id: SymbolKey(1), span: Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(30), length: 7 } } } }, body: SpannedItem Operator(OperatorExpression { lhs: SpannedItem Literal(Integer(4)) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(39), length: 2 } }], rhs: SpannedItem Literal(Integer(1)) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(41), length: 2 } }], op: SpannedItem Plus [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(39), length: 0 } }] }) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(37), length: 6 } }] }) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(0), length: 43 } }],
                ],
            }

            []"#]],
    )
}
