pub mod config;
pub mod constants;
pub mod ctx;
#[cfg(test)]
mod tests;

use std::rc::Rc;

// potential improvements to the formatter:
// don't use Strings in the Lines struct, use a Vec<String> and join, separated by single spaces

fn main() {
    println!("Hello, world!");
}

use constants::{CLOSE_COMMENT_STR, INDENTATION_CHARACTER, OPEN_COMMENT_STR};
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
        for _ in 0..ctx.config.newlines_between_comment_and_item() {
            lines.push(ctx.new_line(""));
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
        // parameter contents are indented by one
        ctx.indented(|ctx| {
            for (ix, param) in self.parameters().iter().enumerate() {
                let mut param = (param).format(ctx).into_single_line().content.to_string();
                let is_last = ix == self.parameters().len() - 1;

                // if this is not the last parameter OR we are putting parameters on new lines, add a comma
                if !is_last || ctx.config.put_fn_params_on_new_lines() {
                    param.push_str(",");
                }
                // if we are putting params on a new line, push a new line
                if ctx.config.put_fn_params_on_new_lines() {
                    lines.push(ctx.new_line(param));
                } else {
                    buf.push_str(&format!("{param} "));
                }
            }
        });
        buf.push_str(") returns ");

        buf.push_str(&format!(
            "'{}",
            ctx.interner.get(self.return_type().name().id())
        ));

        lines.push(ctx.new_line(buf));
        ctx.indented(|ctx| {
            lines.append(&mut self.body().format(ctx).lines);
        });
        FormattedLines::new(lines)
    }
}

impl Formattable for FunctionParameter {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let mut buf = String::new();
        buf.push_str(&*ctx.interner.get(self.name().id()));

        let ty_in = if ctx.config.use_set_notation_for_types() {
            "∈"
        } else {
            "in"
        };

        buf.push_str(&format!(" {ty_in} '"));
        buf.push_str(&*ctx.interner.get(self.ty().name().id()));

        FormattedLines::new(vec![ctx.new_line(buf)])
    }
}

impl Formattable for Expression {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        match self {
            Expression::Operator(op) => {
                let mut buf = op.op().item().as_str().to_string();
                let (mut lhs, mut rhs) = ctx.indented(|ctx| {
                    let lhs = op.lhs().item().format(ctx).lines;
                    let rhs = op.rhs().item().format(ctx).lines;
                    (lhs, rhs)
                });
                if lhs.len() == 1 && rhs.len() == 1 {
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
            AstNode::TypeDeclaration(ty) => ty.format(ctx),
        }
    }
}

impl Formattable for TypeDeclaration {
    // TODO calculate offset to equals sign, align pipes there
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let mut lines = Vec::new();
        let mut buf = "type ".to_string();
        buf.push_str(&*ctx.interner.get(self.name().id()));
        let mut variants = self.variants().into_iter();
        if let Some(first_variant) = variants.next() {
            buf.push_str(" = ");
            let first_variant = first_variant.format(ctx).into_single_line().content;
            buf.push_str(&first_variant);
        } else {
            // this is a variantless struct
            buf.push(';');
            return FormattedLines::new(vec![ctx.new_line(buf)]);
        }
        let len_to_eq = buf.find('=').expect("invariant");
        let mut first_line = ctx.new_line(buf);
        lines.push(first_line);
        // TODO figure this one out
        buf = Default::default();
        ctx.indent_by(len_to_eq, |ctx| {
            // format variants 2..n
            for (ix, variant) in variants.into_iter().enumerate() {
                let is_first = ix == 0;
                if ctx.config.put_variants_on_new_lines() {
                    lines.push(ctx.new_line(buf));
                    buf = Default::default();
                }
                buf.push_str("| ");
                let variant = variant.format(ctx).into_single_line().content;
                buf.push_str(&variant);
            }
            lines.push(ctx.new_line(buf));
        });
        FormattedLines::new(lines)
    }
}

impl Formattable for TypeVariant {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        let name = ctx.interner.get(self.name().id());
        let fields = self.fields().into_iter();
        let mut buf = name.to_string();
        buf.push(' ');
        for field in fields {
            // TODO use impl Format for 'Ty here
            buf.push_str(&format!("'{}", ctx.interner.get(field.name().id())));
            buf.push(' ');
        }
        FormattedLines::new(vec![ctx.new_line(buf)])
    }
}

impl<T: Formattable> Formattable for SpannedItem<T> {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines {
        self.item().format(ctx)
    }
}

// TODO: methods like "continue on current line" are going to be useful
// this can also hold line length context
// Instead of pushing/appending, we should be using custom joins with "continues from previous line" logic
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
            buf.push_str(&format!(
                "{}{}\n",
                INDENTATION_CHARACTER.repeat(*indentation),
                content
            ));
        }
        buf
    }

    /// Forces a multi-line `FormattedLines` into a single line.
    fn into_single_line(&self) -> Line {
        let Some(indentation) = self.lines.get(0).map(|x| x.indentation) else {
            return Line {
                indentation: 0,
                content: Rc::from(""),
            };
        };
        let content = self
            .lines
            .iter()
            .map(|line| line.content.as_ref())
            .collect::<Vec<&str>>()
            .join(" ");
        Line {
            indentation,
            content: Rc::from(content),
        }
    }
}

#[derive(Debug)]
pub struct Line {
    indentation: usize,
    content: Rc<str>,
}

trait Formattable {
    fn format(&self, ctx: &mut FormatterContext) -> FormattedLines;
}
