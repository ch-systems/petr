pub mod config;
pub mod constants;
pub mod ctx;
#[cfg(test)]
mod tests;

// potential improvements to the formatter:
// don't use Strings in the Lines struct, use a Vec<String> and join, separated by single spaces
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    rc::Rc,
};

pub use config::{FormatterConfig, FormatterConfigBuilder};
use constants::{CLOSE_COMMENT_STR, INDENTATION_CHARACTER, OPEN_COMMENT_STR};
pub use ctx::FormatterContext;
use petr_ast::*;
use petr_parse::Parser;
use petr_utils::{render_error, PrettyPrint, SpannedItem};

pub fn format_sources(
    sources: Vec<(PathBuf, String)>,
    config: FormatterConfig,
) -> io::Result<()> {
    let longest_source_name = sources.iter().map(|(path, _)| path.display().to_string().len()).max().unwrap_or(0);

    let distance_to_check = longest_source_name + 5;

    for (source_name, source) in sources {
        let num_dots_to_display = distance_to_check - source_name.display().to_string().len();
        print!("formatting {}...", source_name.display());
        let string_source_name = source_name.to_string_lossy();
        let parser = Parser::new(vec![(string_source_name.clone(), source.clone())]);
        let (ast, errs, interner, source_map) = parser.into_result();
        print!("{}", ".".repeat(num_dots_to_display));
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let mut ctx = FormatterContext::from_interner(interner).with_config(Default::default());
        let formatted_content = ast.line_length_aware_format(&mut ctx).render();
        // Create a new formatter context
        print!("...");

        if config.backup() {
            let backup_path = format!("{string_source_name}.bak");
            fs::write(backup_path, &source)?;
        }

        // Write the formatted content back to the file
        let mut file = fs::File::create(source_name)?;
        file.write_all(formatted_content.as_bytes())?;
        println!("✅");
    }
    Ok(())
}

impl<T> Formattable for Commented<T>
where
    T: Formattable,
{
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
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
                buf.push_str(&c.content);
                lines.push(ctx.new_line(buf));
                buf = Default::default();
            }
            if !lines.is_empty() {
                buf.push_str(&" ".repeat(OPEN_COMMENT_STR.len()));
            }
            buf.push_str(&comments.last().expect("invariant: is_empty() checked above").content);
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
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut lines: Vec<Line> = Vec::new();
        let mut buf: String = if self.visibility == Visibility::Exported { "export fn " } else { "fn " }.to_string();

        buf.push_str(&ctx.interner.get(self.name.id));

        buf.push('(');
        if ctx.config.put_fn_params_on_new_lines() && !self.parameters.is_empty() {
            lines.push(ctx.new_line(buf));
            buf = Default::default();
        }

        // parameter contents are indented by one
        ctx.indented(|ctx| {
            for (ix, param) in self.parameters.iter().enumerate() {
                let mut param = (param).format(ctx).into_single_line().content.to_string();
                let is_last = ix == self.parameters.len() - 1;

                // if this is not the last parameter OR we are putting parameters on new lines, add a comma
                if !is_last || ctx.config.put_fn_params_on_new_lines() {
                    param.push(',');
                }
                // if we are putting params on a new line, push a new line
                if ctx.config.put_fn_params_on_new_lines() {
                    lines.push(ctx.new_line(param));
                } else {
                    buf.push_str(&format!("{param} "));
                }
            }
        });

        buf.push_str(") ");
        if ctx.config.use_symbol_for_fn_return_type() {
            buf.push_str("→ ");
        } else {
            buf.push_str("returns ");
        }

        buf.push_str(&self.return_type.format(ctx).into_single_line().content);

        lines.push(ctx.new_line(buf));

        let mut body = ctx.indented(|ctx| self.body.format(ctx));

        if ctx.config.put_fn_body_on_new_line() {
            lines.append(&mut body.lines);
        } else {
            let first_line_of_body = body.lines.remove(0);
            lines.last_mut().expect("invariant").join_with_line(first_line_of_body, " ");
            lines.append(&mut body.lines);
        }

        FormattedLines::new(lines)
    }
}

impl Formattable for FunctionParameter {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut buf = String::new();
        buf.push_str(&ctx.interner.get(self.name.id));

        let ty_in = if ctx.config.use_set_notation_for_types() { "∈" } else { "in" };

        buf.push_str(&format!(" {ty_in} "));
        buf.push_str(&self.ty.format(ctx).into_single_line().content);

        FormattedLines::new(vec![ctx.new_line(buf)])
    }
}

impl Formattable for Expression {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        match self {
            Expression::Operator(op) => {
                let mut buf = op.op.item().as_str().to_string();
                buf.push(' ');
                let (mut lhs, mut rhs) = ctx.indented(|ctx| {
                    let lhs = op.lhs.item().format(ctx).lines;
                    let rhs = op.rhs.item().format(ctx).lines;
                    (lhs, rhs)
                });
                if lhs.len() == 1 && rhs.len() == 1 {
                    buf.push_str(&lhs[0].content);
                    buf.push(' ');
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
            },
            Expression::Literal(lit) => FormattedLines::new(vec![ctx.new_line(lit.to_string())]),
            Expression::Variable(var) => {
                let ident_as_string = ctx.interner.get(var.id);
                FormattedLines::new(vec![ctx.new_line(ident_as_string)])
            },
            Expression::List(list) => list.format(ctx),
            Expression::TypeConstructor(..) => unreachable!("this is only constructed after binding, which the formatter doesn't do"),
            Expression::FunctionCall(f) => f.format(ctx),
            Expression::IntrinsicCall(i) => i.format(ctx),
            Expression::Binding(binding) => binding.format(ctx),
            Expression::If(r#if) => r#if.format(ctx),
        }
    }
}

impl Formattable for ExpressionWithBindings {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut lines = Vec::new();
        // First, format the bindings, if any.
        if !self.bindings.is_empty() {
            for (ix, binding) in self.bindings.iter().enumerate() {
                let is_first = ix == 0;
                let is_last = ix == self.bindings.len() - 1;
                let mut buf = if is_first { "let ".to_string() } else { "    ".to_string() };

                let name = ctx.interner.get(binding.name.id);
                buf.push_str(&name);
                buf.push_str(" = ");
                let expr_lines = ctx.indented(|ctx| binding.val.format(ctx).lines);

                if expr_lines.len() == 1 {
                    buf.push_str(&expr_lines[0].content);
                    lines.push(ctx.new_line(buf));
                } else {
                    // extend buf with first line
                    // then add the rest of the lines
                    buf.push_str(&expr_lines[0].content);
                    lines.push(ctx.new_line(buf));
                    lines.append(&mut expr_lines[1..].to_vec());
                }
                // add comma to the end of the last line
                if !is_last || ctx.config.put_trailing_semis_on_let_bindings() {
                    let last_line = lines.last().expect("invariant");
                    let last_line_indentation = last_line.indentation;
                    let mut last_line_content = last_line.content.to_string();
                    last_line_content.push(';');
                    *(lines.last_mut().expect("invariant")) = Line {
                        content:     Rc::from(last_line_content),
                        indentation: last_line_indentation,
                    };
                }
            }
        }

        // Then, format the expression itself.
        let expr_lines = self.expression.format(ctx).lines;
        lines.append(&mut expr_lines.to_vec());

        FormattedLines::new(lines)
    }
}

impl Formattable for IntrinsicCall {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        {
            let mut buf = String::new();
            let mut lines = vec![];

            buf.push_str(&format!("@{}", self.intrinsic));
            buf.push('(');

            if ctx.config.put_fn_args_on_new_lines() {
                lines.push(ctx.new_line(buf));
                buf = Default::default();
            }

            ctx.indented(|ctx| {
                for (ix, arg) in self.args.iter().enumerate() {
                    let mut arg = (arg).format(ctx).into_single_line().content.to_string();
                    let is_last = ix == self.args.len() - 1;

                    if !is_last || ctx.config.put_fn_args_on_new_lines() {
                        arg.push(',');
                    }

                    if !ctx.config.put_fn_args_on_new_lines() && !is_last {
                        arg.push(' ');
                    }
                    if ctx.config.put_fn_args_on_new_lines() {
                        lines.push(ctx.new_line(arg));
                        buf = Default::default();
                    } else {
                        buf.push_str(&arg);
                    }
                }
            });

            buf.push(')');

            lines.push(ctx.new_line(buf));
            FormattedLines::new(lines)
        }
    }
}

impl Formattable for FunctionCall {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        // function calls look like this: ~foo bar, baz
        // format as such
        let mut buf = String::new();
        let mut lines = vec![];

        buf.push('~');
        buf.push_str(&ctx.interner.get_path(&self.func_name).join("."));
        if self.args_were_parenthesized {
            buf.push('(');
        } else if !ctx.config.put_fn_args_on_new_lines() && !self.args.is_empty() {
            buf.push(' ');
        }

        if ctx.config.put_fn_args_on_new_lines() {
            lines.push(ctx.new_line(buf));
            buf = Default::default();
        }

        ctx.indented(|ctx| {
            for (ix, arg) in self.args.iter().enumerate() {
                let mut arg = (arg).format(ctx).into_single_line().content.to_string();
                let is_last = ix == self.args.len() - 1;

                if !is_last || ctx.config.put_fn_args_on_new_lines() {
                    arg.push(',');
                }

                if !ctx.config.put_fn_args_on_new_lines() && !is_last {
                    arg.push(' ');
                }
                if ctx.config.put_fn_args_on_new_lines() {
                    lines.push(ctx.new_line(arg));
                    buf = Default::default();
                } else {
                    buf.push_str(&arg);
                }
            }
        });
        if self.args_were_parenthesized {
            buf.push(')');
        }

        lines.push(ctx.new_line(buf));
        FormattedLines::new(lines)
    }
}

impl Formattable for If {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let condition_str = self.condition.format(ctx).into_single_line().content.to_string();

        FormattedLines::new(
            vec![ctx.new_line(format!("if {} then", condition_str))] // `if <condition> then`
                .into_iter()
                .chain(ctx.indented(|ctx| self.then_branch.format(ctx).lines)) // `then` branch body
                .chain(self.else_branch.as_ref().map_or_else(Vec::new, |else_branch| {
                    vec![ctx.new_line("else")]
                        .into_iter()
                        .chain(ctx.indented(|ctx| else_branch.format(ctx).lines))
                        .collect()
                })) // `else` branch body, if available
                .collect(),
        )
    }
}

impl Formattable for Ast {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        // realistically this will only get called on individual files, so there's only ever one module,
        // since there's no inline module syntax.
        let mut lines = Vec::new();
        for (ix, item) in self.modules.iter().enumerate() {
            lines.append(&mut item.format(ctx).lines);
            if ix != self.modules.len() - 1 {
                for _ in 0..ctx.config.newlines_between_items() {
                    lines.push(ctx.new_line(""));
                }
            }
        }
        FormattedLines::new(lines)
    }
}

impl Formattable for Module {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut lines = Vec::new();
        for (ix, item) in self.nodes.iter().enumerate() {
            lines.append(&mut item.format(ctx).lines);
            if ix != self.nodes.len() - 1 {
                for _ in 0..ctx.config.newlines_between_items() {
                    lines.push(ctx.new_line(""));
                }
            }
        }
        FormattedLines::new(lines)
    }
}

impl Formattable for AstNode {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        match self {
            AstNode::FunctionDeclaration(fd) => fd.format(ctx),
            AstNode::TypeDeclaration(ty) => ty.format(ctx),
            AstNode::ImportStatement(_) => todo!(),
        }
    }
}

impl Formattable for TypeVariantOrLiteral {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        match self {
            TypeVariantOrLiteral::Variant(variant) => variant.format(ctx),
            TypeVariantOrLiteral::Literal(lit) => FormattedLines::new(vec![ctx.new_line(lit.to_string())]),
        }
    }
}

impl Formattable for TypeDeclaration {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut lines = Vec::new();
        let mut buf: String = if self.visibility == Visibility::Exported { "Type " } else { "type " }.to_string();
        buf.push_str(&ctx.interner.get(self.name.id));
        let mut variants = self.variants.iter();
        if let Some(first_variant) = variants.next() {
            buf.push_str(" = ");
            let first_variant = first_variant.format(ctx).into_single_line().content;
            buf.push_str(&first_variant);
        } else {
            // this is a variantless struct
            buf.push(';');
            return FormattedLines::new(vec![ctx.new_line(buf)]);
        }
        let len_to_eq = if ctx.config.put_variants_on_new_lines() {
            buf.find('=').expect("invariant")
        } else {
            0
        };
        if ctx.config.put_variants_on_new_lines() {
            lines.push(ctx.new_line(buf));
            buf = Default::default();
        }
        ctx.indent_by(len_to_eq, |ctx| {
            // format variants 2..n
            for variant in variants {
                if ctx.config.put_variants_on_new_lines() && !buf.is_empty() {
                    lines.push(ctx.new_line(buf));
                    buf = Default::default();
                }
                if !ctx.config.put_variants_on_new_lines() {
                    buf.push(' ');
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
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let name = ctx.interner.get(self.name.id);
        let mut buf = name.to_string();
        if !self.fields.is_empty() {
            buf.push(' ');
        }
        let mut fields_buf = Vec::with_capacity(self.fields.len());
        for field in &*self.fields {
            fields_buf.push(field.format(ctx).into_single_line().content.to_string());
        }
        buf.push_str(&fields_buf.join(" "));
        FormattedLines::new(vec![ctx.new_line(buf)])
    }
}

// TODO: would be nice to format types and type fields as such
// type Ptr =
//      Unsized
//          address 'int
//    | Sized
//          address 'int
//          size 'int

impl Formattable for TypeField {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let name = ctx.interner.get(self.name.id);
        let mut buf = name.to_string();
        buf.push(' ');
        buf.push_str(&self.ty.format(ctx).into_single_line().content);
        FormattedLines::new(vec![ctx.new_line(buf)])
    }
}

impl Formattable for Ty {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let name = match self {
            Ty::Bool => "'bool".to_string(),
            Ty::Int => "'int".to_string(),
            Ty::String => "'string".to_string(),
            Ty::Unit => {
                if ctx.config.use_symbol_for_unit_type() {
                    "ε".to_string()
                } else {
                    "'unit".to_string()
                }
            },
            Ty::Named(name) => format!("'{}", ctx.interner.get(name.id)),
            Ty::Literal(lit) => lit.to_string(),
            Ty::Sum(tys) => format!(
                "'Σ {}",
                tys.iter()
                    .map(|ty| ty.format(ctx).into_single_line().content)
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
        };
        FormattedLines::new(vec![ctx.new_line(name)])
    }
}

impl Formattable for List {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let mut lines = Vec::new();
        let items = self.elements.iter();
        let mut item_buf = vec![];
        ctx.indented(|ctx| {
            for item in items {
                let element = item.format(ctx).into_single_line();
                item_buf.push(element);
            }
        });

        if ctx.config.put_list_elements_on_new_lines() {
            lines.push(ctx.new_line("["));
            for mut line in item_buf {
                line.content = Rc::from(format!("{},", line.content).as_str());
                lines.push(line);
            }
            lines.push(ctx.new_line("]"));
        } else {
            let text = item_buf.iter().map(|item| item.content.trim()).collect::<Vec<_>>().join(", ");
            lines.push(ctx.new_line(format!("[{}]", text)));
        }

        FormattedLines::new(lines)
    }
}

impl<T: Formattable> Formattable for SpannedItem<T> {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        self.item().format(ctx)
    }
}

// TODO: methods like "continue on current line" are going to be useful
// this can also hold line length context
// Instead of pushing/appending, we should be using custom joins with "continues from previous line" logic
#[derive(Debug)]
pub struct FormattedLines {
    lines: Vec<Line>,
}

impl FormattedLines {
    pub fn new(lines: Vec<Line>) -> Self {
        Self { lines }
    }

    pub fn max_length(&self) -> usize {
        self.lines
            .iter()
            .map(|line| line.content.len() + INDENTATION_CHARACTER.repeat(line.indentation).len())
            .max()
            .unwrap_or(0)
    }

    pub fn render(&self) -> String {
        let mut buf = String::new();
        for Line { indentation, content } in &self.lines {
            // don't print indentation if the line is empty
            // it would just be trailing whitespace
            if content.trim().is_empty() {
                buf.push('\n');
                continue;
            }

            // the line has non-whitespace content, so we indent and print it
            buf.push_str(&format!("{}{}\n", INDENTATION_CHARACTER.repeat(*indentation), content));
        }
        buf
    }

    /// Forces a multi-line `FormattedLines` into a single line.
    fn into_single_line(self) -> Line {
        let Some(indentation) = self.lines.first().map(|x| x.indentation) else {
            return Line {
                indentation: 0,
                content:     Rc::from(""),
            };
        };
        let content = self.lines.into_iter().map(|line| line.content).collect::<Vec<_>>().join(" ");
        Line {
            indentation,
            content: Rc::from(content),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Line {
    indentation: usize,
    content:     Rc<str>,
}

impl Line {
    pub fn join_with_line(
        &mut self,
        other: Line,
        join_str: &str,
    ) {
        self.content = Rc::from(format!("{}{join_str}{}", self.content, other.content).as_str());
    }
}

pub trait Formattable {
    fn format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines;
    /// the below is the actual entry point to the interface, which attempts to reformat the item
    /// with more things broken up across lines if the line length exceeds the limit
    fn line_length_aware_format(
        &self,
        ctx: &mut FormatterContext,
    ) -> FormattedLines {
        let base_result = self.format(ctx);
        // try the first configs, then the later ones
        // these are in order of preference
        let configs = vec![
            ctx.config.as_builder().put_fn_params_on_new_lines(true).build(),
            ctx.config
                .as_builder()
                .put_fn_params_on_new_lines(true)
                .put_fn_args_on_new_lines(true)
                .build(),
            ctx.config
                .as_builder()
                .put_fn_params_on_new_lines(true)
                .put_fn_body_on_new_line(true)
                .build(),
            ctx.config.as_builder().put_list_elements_on_new_lines(true).build(),
            ctx.config
                .as_builder()
                .put_fn_params_on_new_lines(true)
                .put_fn_body_on_new_line(true)
                .put_variants_on_new_lines(true)
                .build(),
            ctx.config
                .as_builder()
                .put_fn_params_on_new_lines(true)
                .put_list_elements_on_new_lines(true)
                .put_variants_on_new_lines(true)
                .put_list_elements_on_new_lines(true)
                .build(),
        ];

        for config in &configs {
            let result = self.try_config(ctx, *config);
            if result.max_length() < ctx.config.max_line_length() {
                return result;
            }
        }

        // if none of the above were good enough, pick the shortest one
        vec![base_result]
            .into_iter()
            .chain(configs.into_iter().map(|config| self.try_config(ctx, config)))
            .min_by_key(|fl| fl.max_length())
            .unwrap()
    }
    fn try_config(
        &self,
        ctx: &mut FormatterContext,
        config: FormatterConfig,
    ) -> FormattedLines {
        ctx.with_new_config(config, |ctx| self.format(ctx))
    }
}
