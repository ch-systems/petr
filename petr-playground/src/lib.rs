

//! Basic WASM bindings for the pete compiler
//! Nothing fancy at all, could definitely be improved over time to support better error reporting,
//! etc

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    alert(&format!("Hello, {}!", name));
}

use petr_api::{render_error, resolve_symbols, type_check, FormatterConfig, Lowerer, Parser, Vm};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_snippet(code: &str) -> Result<String, String> {
    let lowerer = compile_snippet(code.to_string()).map_err(|e| e.join("\n"))?;

    let (data, instructions) = lowerer.finalize();

    let vm = Vm::new(instructions, data);
    let result = match vm.run() {
        Ok(o) => o,
        Err(e) => return Err(format!("VM failed to run: {e:?}")),
    };

    Ok(format!("{:#?}", result))
}

fn compile_snippet(code: String) -> Result<Lowerer, Vec<String>> {
    let buf = vec![("snippet".to_string(), code)];
    let mut errs = vec![];
    let parser = Parser::new(buf);
    // TODO include standard library in WASM compilation target and
    // bring it in to this repo
    let dependencies = vec![];

    let (ast, mut parse_errs, mut interner, mut source_map) = parser.into_result();
    // TODO after diagnostics are implemented for these errors, append them to the errors and
    // return them
    let (resolution_errs, resolved) = resolve_symbols(ast, interner, dependencies);
    let (type_errs, type_checker) = type_check(resolved);
    let lowerer = Lowerer::new(type_checker);

    errs.extend(parse_errs.into_iter().map(|e| format!("{:?}", render_error(&source_map, e))));

    if !errs.is_empty() {
        return Err(errs);
    } else {
        Ok(lowerer)
    }
}

pub fn format(
    code: String,
    config: FormatterConfig,
) -> Result<String, String> {
    todo!()
}
