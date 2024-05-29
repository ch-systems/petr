use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

use constants::{CLOSE_COMMENT_STR, OPEN_COMMENT_STR};
use pretty_print::PrettyPrint;
use swim_parse::{comments::Commented, parser::ast::*, SymbolInterner};

pub mod constants {
    pub const OPEN_COMMENT_STR: &str = "{- ";
    pub const CLOSE_COMMENT_STR: &str = " -}";
}

pub struct FormatterContext {
    interner: SymbolInterner,
    config: FormatterConfig,
    indentation: usize,
}

impl FormatterContext {
    pub fn from_interner(interner: SymbolInterner) -> Self {
        Self {
            interner,
            config: Default::default(),
            indentation: 0,
        }
    }
    pub fn new_line(&self, content: impl AsRef<str>) -> Line {
        Line {
            content: Rc::from(content.as_ref()),
            indentation: self.indentation,
        }
    }

    pub fn tab_in(&mut self) {
        self.indentation += 1;
    }

    pub fn tab_out(&mut self) {
        self.indentation -= 1;
    }

    pub fn with_config(self, config: FormatterConfig) -> FormatterContext {
        FormatterContext { config, ..self }
    }
}

pub struct FormatterConfig {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
    // false:
    // ```
    // {- a -}
    // {- b -}
    // function ....
    // ```
    //
    // true:
    // ```
    // {- a
    //    b -}
    // function ...
    // ```
    join_comments: bool,
}
impl FormatterConfig {
    pub fn put_fn_params_on_new_lines(self, arg: bool) -> FormatterConfig {
        FormatterConfig {
            put_fn_params_on_new_lines: arg,
            ..self
        }
    }
    pub fn join_comments(self, arg: bool) -> FormatterConfig {
        FormatterConfig {
            join_comments: arg,
            ..self
        }
    }

    pub fn use_set_notation_for_types(self, arg: bool) -> FormatterConfig {
        FormatterConfig {
            use_set_notation_for_types: arg,
            ..self
        }
    }
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            put_fn_params_on_new_lines: true,
            use_set_notation_for_types: true,
            join_comments: true,
        }
    }
}

impl<T> Formattable for Commented<T>
where
    T: Formattable,
{
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let comments = self.comments();
        let mut lines = Vec::new();
        // if we are joining comments, join their contents
        if ctx.config.join_comments && !comments.is_empty() {
            let mut buf = String::from(OPEN_COMMENT_STR);
            for c in comments.iter().take(comments.len() - 1) {
                // if this is not the first line, add enough space to offset the open comment syntax
                if !lines.is_empty() {
                    buf.push_str(&" ".repeat(OPEN_COMMENT_STR.len()));
                }
                buf.push_str(&*c.content());
                lines.push(ctx.new_line(buf));
                buf = Default::default();
            }
            if !lines.is_empty() {
                buf.push_str(&" ".repeat(OPEN_COMMENT_STR.len()));
            }
            buf.push_str(
                &*comments
                    .last()
                    .expect("invariant: is_empty() checked above")
                    .content(),
            );
            buf.push_str(CLOSE_COMMENT_STR);
            lines.push(ctx.new_line(buf));
        } else {
            for comment in comments {
                lines.push(ctx.new_line(comment.pretty_print(&ctx.interner, ctx.indentation)));
            }
        }
        let mut formatted_inner = self.item().format(ctx).lines;
        lines.append(&mut formatted_inner);
        FormattedLines::new(lines)
    }
}

impl Formattable for FunctionDeclaration {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let mut lines: Vec<Line> = Vec::new();
        let mut buf: String = "function ".into();

        buf.push_str(&*ctx.interner.get(self.name().id()));

        buf.push_str("(");
        if ctx.config.put_fn_params_on_new_lines {
            lines.push(ctx.new_line(buf));
            buf = Default::default();
        }
        let ty_in = if ctx.config.use_set_notation_for_types {
            "∈"
        } else {
            "in"
        };

        // parameter contents are indented by one
        ctx.tab_in();

        for (ix, param) in self.parameters().iter().enumerate() {
            let is_last = ix == self.parameters().len() - 1;
            buf.push_str(&*ctx.interner.get(param.name().id()));
            buf.push_str(&format!(" {ty_in} '"));
            buf.push_str(&*ctx.interner.get(param.ty().name().id()));
            if !is_last || ctx.config.put_fn_params_on_new_lines {
                buf.push_str(",");
            }

            if ctx.config.put_fn_params_on_new_lines {
                lines.push(ctx.new_line(buf));
                buf = Default::default();
            } else {
                // if we wanted to have "max length of params", we could
                // check that here
                if !is_last {
                    buf.push_str(" ");
                }
            }
        }

        ctx.tab_out();
        buf.push_str(") returns ");

        buf.push_str(&format!(
            "'{}",
            ctx.interner.get(self.return_type().name().id())
        ));

        lines.push(ctx.new_line(buf));
        ctx.tab_in();
        lines.append(&mut self.body().format(ctx).lines);
        ctx.tab_out();
        FormattedLines::new(lines)
    }
}

impl Formattable for Expression {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        match self {
            Expression::Operator(op) => {
                let mut buf = op.op().item().as_str().to_string();
                ctx.tab_in();
                let mut lhs = op.lhs().item().format(ctx).lines;
                let mut rhs = op.rhs().item().format(ctx).lines;
                ctx.tab_out();
                if lhs.len() == 1 && rhs.len() == 1 {
                    buf.push_str(" ");
                    buf.push_str(&lhs[0].content);
                    buf.push_str(&rhs[0].content);
                    FormattedLines::new(vec![ctx.new_line(buf)])
                } else {
                    let mut lines = Vec::new();
                    buf.push_str(" (");
                    lines.push(ctx.new_line(buf));
                    lines.append(&mut lhs);
                    lines.append(&mut rhs);
                    FormattedLines::new(lines)
                }
            }
            Expression::Literal(lit) => {
                FormattedLines::new(vec![ctx.new_line(format!(" {}", lit.to_string()))])
            }
            Expression::Variable(var) => {
                let ident_as_string = ctx.interner.get(var.name().id());
                FormattedLines::new(vec![ctx.new_line(ident_as_string)])
            }
            Expression::Block(..) => todo!(),
        }
    }
}

pub struct FormattedLines {
    lines: Vec<Line>,
}

impl FormattedLines {
    pub fn new(lines: Vec<Line>) -> Self {
        Self { lines }
    }

    pub fn render(&self) -> String {
        let mut buf = String::new();
        for Line {
            indentation,
            content,
        } in &self.lines
        {
            buf.push_str(&format!("{}{}\n", "  ".repeat(*indentation), content));
        }
        buf
    }
}

pub struct Line {
    indentation: usize,
    content: Rc<str>,
}

trait Formattable {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines;
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use swim_parse::{comments::Commented, parser::ast::FunctionDeclaration};

    use crate::{Formattable, FormatterConfig, FormatterContext};

    fn check_fn_decl(config: FormatterConfig, input: impl Into<String>, expect: Expect) {
        let input = input.into();
        let mut parser = swim_parse::parser::Parser::new(vec![input.as_ref()]);
        let decl: Commented<FunctionDeclaration> = match parser.parse() {
            Some(x) if parser.errors().is_empty() => x,
            _ => panic!("failed to parse: {:?}", parser.errors()),
        };
        let mut ctx =
            FormatterContext::from_interner(parser.interner().clone()).with_config(config);
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
            FormatterConfig::default().put_fn_params_on_new_lines(false),
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
            FormatterConfig::default(),
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
            FormatterConfig::default(),
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
            FormatterConfig::default().join_comments(false),
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
}
