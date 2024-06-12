use expect_test::expect;

use super::*;
fn check<T: Into<String>>(sources: Vec<T>,
                          expected: expect_test::Expect) {
    let mut lexer = Lexer::new(sources.into_iter().map(|s| -> &'static str { Box::leak(s.into().into_boxed_str()) }));
    let mut toks = vec![];
    loop {
        let next_tok = lexer.advance();
        if next_tok.item() == &Token::Eof {
            break;
        }

        toks.push(next_tok);
    }

    expected.assert_eq(&format!("{toks:#?}"));
}
#[test]
fn test_lexer_advance() {
    check(vec!["I am some source code"],
          expect![[r#"
            [
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(0), length: 1 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(2), length: 2 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(5), length: 4 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(10), length: 6 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(17), length: 4 } }],
            ]"#]])
}

#[test]
fn test_lexer_advance_multiple_sources() {
    check(vec!["I am some", "Source code"],
          expect![[r#"
            [
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(0), length: 1 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(2), length: 2 } }],
                SpannedItem Identifier [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(5), length: 4 } }],
                SpannedItem Identifier [Span { source: SourceId(1), span: SourceSpan { offset: SourceOffset(0), length: 6 } }],
                SpannedItem Identifier [Span { source: SourceId(1), span: SourceSpan { offset: SourceOffset(7), length: 4 } }],
            ]"#]])
}

#[test]
fn test_symbols() {
    check(vec!["((5 +-/* 2)[]"],
          expect![[r#"
            [
                SpannedItem OpenParen [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(0), length: 1 } }],
                SpannedItem OpenParen [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(1), length: 1 } }],
                SpannedItem Integer [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(2), length: 1 } }],
                SpannedItem Plus [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(4), length: 1 } }],
                SpannedItem Minus [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(5), length: 1 } }],
                SpannedItem Slash [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(6), length: 1 } }],
                SpannedItem Star [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(7), length: 1 } }],
                SpannedItem Integer [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(9), length: 1 } }],
                SpannedItem CloseParen [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(10), length: 1 } }],
                SpannedItem OpenBracket [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(11), length: 1 } }],
                SpannedItem CloseBracket [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(12), length: 1 } }],
            ]"#]])
}
