//! Basic WASM bindings for the pete compiler
//! Nothing fancy at all, could definitely be improved over time to support better error reporting,
//! etc

use petr_api::{render_error, resolve_symbols, type_check, Formattable, FormatterContext, Lowerer, Parser, Vm};
use wasm_bindgen::prelude::*;

#[cfg(test)]
mod tests;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = setOutputContent)]
    fn set_output_content(s: &str);

    #[wasm_bindgen(js_name = setCodeEditorContent)]
    fn set_code_editor_content(s: &str);
}

#[wasm_bindgen]
pub fn run_snippet(code: &str) {
    run_snippet_inner(code, set_output_content);
}

fn run_snippet_inner<F>(
    code: &str,
    set_output_content: F,
) where
    F: Fn(&str),
{
    let lowerer = match compile_snippet(code.to_string()) {
        Ok(o) => o,
        Err(e) => {
            let err_text = errors_to_html(&e);
            set_output_content(&err_text);
            return;
        },
    };

    let (data, instructions) = lowerer.finalize();

    let vm = Vm::new(instructions, data);
    let (result, _stack, logs) = match vm.run() {
        Ok(o) => o,
        Err(e) => {
            set_output_content(&format!("Runtime error: {:#?}", e));
            return;
        },
    };

    set_output_content(&format!("Logs:<br>\t{}<br>Result: <br>\t{:#?}", logs.join("\n\t"), result.inner()));
}

fn errors_to_html(e: &[String]) -> String {
    let mut buf = String::new();
    buf.push_str("<div class=\"errors\">");
    for err in e {
        buf.push_str(&format!("<div class=\"error\">{}</div>", err));
    }
    buf.push_str("</div>");
    buf
}

fn compile_snippet(input: String) -> Result<Lowerer, Vec<String>> {
    let mut sources = stdlib::stdlib();
    sources.push(("snippet", &input));
    let parser = petr_api::Parser::new(sources);
    let (ast, errs, interner, source_map) = parser.into_result();
    if !errs.is_empty() {
        return Err(errs.into_iter().map(|err| format!("{:?}", render_error(&source_map, err))).collect());
    }
    let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
    if !errs.is_empty() {
        return Err(errs.into_iter().map(|err| format!("{:?}", render_error(&source_map, err))).collect());
    }

    let (errs, type_checker) = type_check(resolved);

    if !errs.is_empty() {
        return Err(errs.into_iter().map(|err| format!("{:?}", render_error(&source_map, err))).collect());
    }

    let lowerer = match Lowerer::new(type_checker) {
        Ok(l) => l,
        Err(err) => panic!("lowering failed: {:?}", err),
    };

    Ok(lowerer)
}

#[wasm_bindgen]
pub fn format(code: String) {
    let parser = Parser::new(vec![("snippet".to_string(), code)]);
    let (ast, errs, interner, source_map) = parser.into_result();
    if !errs.is_empty() {
        let errs = errs
            .into_iter()
            .map(|e| format!("{:?}", render_error(&source_map, e)))
            .collect::<Vec<_>>();
        set_output_content(&errs.join("<br>"));
        return;
    }
    let mut ctx = FormatterContext::from_interner(interner).with_config(Default::default());
    let formatted_content = ast.line_length_aware_format(&mut ctx).render();
    set_code_editor_content(&formatted_content);
}
