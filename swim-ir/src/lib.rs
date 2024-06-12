// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc

use std::{any::Any, collections::BTreeMap, rc::Rc};

use swim_typecheck::{Function as TypeCheckedFunction, SwimType, TypeChecker, TypeOrFunctionId, TypeVariable, TypedExpr, TypedFunctionId};
use swim_utils::{idx_map_key, IndexMap};

mod error;
mod opcodes;

use error::*;
use opcodes::*;
pub use opcodes::{DataLabel, IrOpcode, Reg};

// TODO: fully typed functions
pub struct Function {
    label: FunctionLabel,
    body: Vec<IrOpcode>,
}

pub type DataSection = IndexMap<DataLabel, DataSectionEntry>;
/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: DataSection,
    entry_point: Option<FunctionLabel>,
    function_definitions: BTreeMap<TypedFunctionId, Function>,
    reg_assigner: usize,
    function_label_assigner: usize,
    type_checker: TypeChecker,
}

#[derive(Debug, Clone)]
pub enum DataSectionEntry {
    Int64(i64),
    String(Rc<str>),
    Bool(bool),
}

impl Lowerer {
    pub fn new(type_checker: TypeChecker) -> Self {
        let mut lowerer = Self {
            data_section: IndexMap::default(),
            // TODO set entry point
            entry_point: None,
            function_definitions: BTreeMap::default(),
            reg_assigner: 0,
            function_label_assigner: 0,
            type_checker,
        };
        lowerer.lower_all_functions();
        lowerer
    }

    pub fn finalize(&self) -> (DataSection, Vec<IrOpcode>) {
        let mut program_section = vec![];

        if let Some(entry_point) = self.entry_point {
            // Add entry point function body to the program section
            if let Some(entry_func) = self.function_definitions.get(todo!()) {
                program_section.extend(entry_func.body.clone());
            }
        }

        // Add other function bodies to the program section
        for func in self.function_definitions.values() {
            if func.label != FunctionLabel::from(0) {
                program_section.extend(func.body.clone());
            }
        }

        (self.data_section.clone(), program_section)
    }

    fn new_function_label(&mut self) -> FunctionLabel {
        let label = self.function_label_assigner;
        self.function_label_assigner += 1;
        FunctionLabel::from(label)
    }

    fn lower_function(
        &mut self,
        id: TypedFunctionId,
        func: TypeCheckedFunction,
    ) -> Result<(), LoweringError> {
        let func_label = self.new_function_label();
        let mut buf = vec![];
        let mut param_to_reg_mapping = BTreeMap::new();
        // TODO: func should have type checked types...not just the AST type
        for (param_name, param_ty) in &func.params {
            // in order, assign parameters to registers
            if fits_in_reg(param_ty) {
                // load from stack into register
                let param_reg = self.fresh_reg();
                let ty_reg = TypedReg {
                    ty: self.to_ir_type(param_ty),
                    reg: param_reg,
                };
                buf.push(IrOpcode::StackPop(ty_reg));
                // insert param into mapping
                param_to_reg_mapping.insert(param_name, param_reg);
            } else {
                todo!("make reg a ptr to the value")
            }
        }

        // TODO we could support other return dests
        let return_dest = ReturnDestination::Stack;

        let mut expr_body = self.lower_expr(&func.body, &mut param_to_reg_mapping, return_dest)?;

        buf.append(&mut expr_body);
        self.function_definitions.insert(
            id,
            Function {
                body: buf,
                label: func_label,
            },
        );
        Ok(())
    }

    fn fresh_reg(&mut self) -> Reg {
        let val = self.reg_assigner;
        self.reg_assigner += 1;
        Reg::Virtual(val)
    }

    fn lower_expr(
        &mut self,
        body: &TypedExpr,
        param_to_reg_mapping: &mut BTreeMap<&swim_utils::Identifier, Reg>,
        return_destination: ReturnDestination,
    ) -> Result<Vec<IrOpcode>, LoweringError> {
        use TypedExpr::*;

        match body {
            Literal { value, ty } => {
                let data_label = self.insert_literal_data(value);
                let ty = self.to_ir_type(ty);
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::LoadData(reg, data_label)],
                    ReturnDestination::Stack => {
                        let reg = self.fresh_reg();
                        vec![IrOpcode::LoadData(reg, data_label), IrOpcode::StackPush(TypedReg { ty, reg })]
                    },
                })
            },
            FunctionCall { arg_types, ty } => todo!(),
            List { elements, ty } => todo!(),
            Unit => todo!(),
            Variable { ty } => todo!(),
            Intrinsic { ty, intrinsic } => todo!(),
            ErrorRecovery => todo!(),
        }
    }

    fn insert_literal_data(
        &mut self,
        lit: &swim_typecheck::Literal,
    ) -> DataLabel {
        use swim_typecheck::Literal::*;
        let label = self.data_section.insert(match lit {
            Integer(val) => DataSectionEntry::Int64(*val),
            Boolean(val) => DataSectionEntry::Bool(*val),
            String(val) => DataSectionEntry::String(val.clone()),
        });
        label
    }

    // convert a polytype type to an `IrTy`
    fn to_ir_type(
        &self,
        param_ty: &TypeVariable,
    ) -> IrTy {
        let realized_ty = self.type_checker.realize_type(param_ty);
        use SwimType::*;
        match realized_ty {
            Unit => IrTy::Unit,
            Integer => IrTy::Int64,
            Boolean => IrTy::Boolean,
        }
    }

    fn lower_all_functions(&mut self) -> Result<(), LoweringError> {
        for (id, func) in self.type_checker.functions() {
            self.lower_function(id, func)?;
        }
        Ok(())
    }
}

enum ReturnDestination {
    Reg(Reg),
    Stack,
}
fn fits_in_reg(param_ty: &TypeVariable) -> bool {
    // TODO
    true
}

fn literal_to_ir_ty(param_ty: swim_typecheck::Literal) -> IrTy {
    use swim_typecheck::Literal::*;
    match param_ty {
        Integer(_) => todo!(),
        Boolean(_) => todo!(),
        String(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};
    use swim_resolve::{resolve_symbols, QueryableResolvedItems};
    use swim_utils::{render_error, SourceId, SymbolInterner};

    use super::*;
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = swim_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let resolved = resolve_symbols(ast, interner);
        let type_checker = TypeChecker::new(resolved);
        let lowerer = Lowerer::new(type_checker);
        let res = pretty_print_lowerer(&lowerer);

        expect.assert_eq(&res);
    }

    fn pretty_print_lowerer(lowerer: &Lowerer) -> String {
        let mut result = String::new();
        result.push_str("; DATA_SECTION\n");
        for (label, entry) in lowerer.data_section.iter() {
            result.push_str(&format!("{}: {:?}\n", Into::<usize>::into(label), entry));
        }

        result.push_str("\n; PROGRAM_SECTION\n");
        for (id, func) in &lowerer.function_definitions {
            result.push_str(&format!("Function {:?}:\n", id));
            for opcode in &func.body {
                result.push_str(&format!("  {:?}\n", opcode));
            }
        }
        result
    }

    #[test]
    fn basic_main_func() {
        check(
            r#"
            function main() returns 'int 42
            "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(42)

                ; PROGRAM_SECTION
                Function TypedFunctionId(0):
                  LoadData(Virtual(0), DataLabel(0))
                  StackPush(TypedReg { ty: Int64, reg: Virtual(0) })
            "#]],
        );
    }
}
