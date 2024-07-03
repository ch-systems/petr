//! Basic WASM bindings for the pete compiler
//! Nothing fancy at all, could definitely be improved over time to support better error reporting,
//! etc

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = setOutputContent)]
    fn set_output_content(s: &str);
}

use petr_api::{render_error, resolve_symbols, type_check, FormatterConfig, Lowerer, Parser, Vm};

#[wasm_bindgen]
pub fn run_snippet(code: &str) {
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
    let result = match vm.run() {
        Ok(o) => o,
        Err(e) => {
            set_output_content(&format!("Runtime error: {:#?}", e));
            return;
        },
    };

    set_output_content(&format!("Result: {:#?}", result));
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

fn compile_snippet(code: String) -> Result<Lowerer, Vec<String>> {
    let buf = vec![("snippet".to_string(), code)];
    let mut errs = vec![];
    let parser = Parser::new(buf);
    // TODO include standard library in WASM compilation target and
    // bring it in to this repo
    let dependencies = vec![];

    let (ast, parse_errs, interner, source_map) = parser.into_result();
    // TODO after diagnostics are implemented for these errors, append them to the errors and
    // return them
    let (_resolution_errs, resolved) = resolve_symbols(ast, interner, dependencies);
    let (_type_errs, type_checker) = type_check(resolved);
    let lowerer = Lowerer::new(type_checker);

    errs.extend(parse_errs.into_iter().map(|e| format!("{:?}", render_error(&source_map, e))));

    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(lowerer)
    }
}

pub fn format(
    _code: String,
    _config: FormatterConfig,
) -> Result<String, String> {
    todo!()
}
