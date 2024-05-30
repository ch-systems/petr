pub mod config;
pub mod constants;
pub mod ctx;
#[cfg(test)]
mod tests;

use std::rc::Rc;

fn main() {
    println!("Hello, world!");
}

use constants::{CLOSE_COMMENT_STR, OPEN_COMMENT_STR};
use ctx::FormatterContext;
use pretty_print::PrettyPrint;
use swim_parse::{comments::Commented, parser::ast::*};
use swim_utils::SpannedItem;

impl<T> Formattable for Commented<T>
where
    T: Formattable,
{
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let comments = self.comments();
        let mut lines = Vec::new();
        // if we are joining comments, join their contents
        if ctx.config.join_comments() && !comments.is_empty() {
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
                lines.push(ctx.new_line(comment.pretty_print(&ctx.interner, ctx.indentation())));
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
        if ctx.config.put_fn_params_on_new_lines() {
            lines.push(ctx.new_line(buf));
            buf = Default::default();
        }
        let ty_in = if ctx.config.use_set_notation_for_types() {
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
            if !is_last || ctx.config.put_fn_params_on_new_lines() {
                buf.push_str(",");
            }

            if ctx.config.put_fn_params_on_new_lines() {
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

impl Formattable for AST {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let mut lines = Vec::new();
        for (ix, item) in self.nodes().into_iter().enumerate() {
            lines.append(&mut item.format(ctx).lines);
            if ix != self.nodes().len() - 1 {
                for _ in 0..ctx.config.newlines_between_items() {
                    lines.push(ctx.new_line(""));
                }
            }
        }
        FormattedLines::new(lines)
    }
}

impl Formattable for AstNode {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        match self {
            AstNode::FunctionDeclaration(fd) => fd.format(ctx),
        }
    }
}

impl<T: Formattable> Formattable for SpannedItem<T> {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        self.item().format(ctx)
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
