//! Basic WASM bindings for the pete compiler
//! Nothing fancy at all, could definitely be improved over time to support better error reporting,
//! etc

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = setOutputContent)]
    fn set_output_content(s: &str);

    #[wasm_bindgen(js_name = setCodeEditorContent)]
    fn set_code_editor_content(s: &str);
}

use petr_api::{render_error, resolve_symbols, type_check, Dependency, Formattable, FormatterContext, Lowerer, Parser, Vm};

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

#[test]
fn test_run_snippet() {
    fn retrieve_output_content(s: &str) {
        assert_eq!(s, "Logs:<br>\tHello, World!\nResult: <br>\tUnit");
    }

    run_snippet_inner(
        r#"
function main() returns 'unit
            let a = ~std.io.print("yo"),
            ~std.io.print "Hello, World!"
"#,
        retrieve_output_content,
    );
}

#[test]
fn repro_of_wasm_panic() {
    fn retrieve_output_content(s: &str) {
        assert_eq!(s, "Logs:<br>\tHello, World!\nResult: <br>\tUnit");
    }
    run_snippet_inner(
        r#"
        function main() returns 'int
  let boo = ~std.io.println "yo"
  5"#,
        retrieve_output_content,
    );
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

    let (ast, mut parse_errs, interner, source_map) = parser.into_result();
    // add the standard library to the sources
    let parser = Parser::new_with_existing_interner_and_source_map(stdlib::stdlib(), interner, source_map);
    let (dep_ast, mut new_parse_errs, interner, source_map) = parser.into_result();
    parse_errs.append(&mut new_parse_errs);

    let dependencies = vec![Dependency {
        key:          "stdlib".to_string(),
        name:         "std".into(),
        dependencies: vec![],
        ast:          dep_ast,
    }];

    let (resolution_errs, resolved) = resolve_symbols(ast, interner, dependencies);
    let (type_errs, type_checker) = type_check(resolved);
    let lowerer = Lowerer::new(type_checker);

    errs.extend(parse_errs.into_iter().map(|e| format!("{:?}", render_error(&source_map, e))));
    errs.extend(type_errs.into_iter().map(|e| format!("{:?}", render_error(&source_map, e))));
    errs.extend(resolution_errs.into_iter().map(|e| format!("{:?}", render_error(&source_map, e))));

    if !errs.is_empty() {
        Err(errs)
    } else {
        Ok(lowerer)
    }
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
    }
    let mut ctx = FormatterContext::from_interner(interner).with_config(Default::default());
    let formatted_content = ast.line_length_aware_format(&mut ctx).render();
    set_code_editor_content(&formatted_content);
}
