use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

use swim_parse::{parser::ast::*, SymbolInterner};

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

struct FormatterConfig {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
}
impl FormatterConfig {
    pub fn put_fn_params_on_new_lines(self, arg: bool) -> FormatterConfig {
        FormatterConfig {
            put_fn_params_on_new_lines: arg,
            ..self
        }
    }
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            put_fn_params_on_new_lines: true,
            use_set_notation_for_types: true,
        }
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
        FormattedLines::new(lines)
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
    use swim_parse::parser::{ast::FunctionDeclaration, Parse};

    use crate::{Formattable, FormatterConfig, FormatterContext};

    fn check_fn_decl(config: FormatterConfig, input: impl Into<String>, expect: Expect) {
        let input = input.into();
        let mut parser = swim_parse::parser::Parser::new(vec![input.as_ref()]);
        let decl: FunctionDeclaration = match parser.parse() {
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
            "#]],
        );
    }
}
