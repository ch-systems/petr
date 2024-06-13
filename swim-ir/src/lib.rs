// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc
// - store position to jump back to after fn call
// - terminate instructions in correct places (end of entry point)
// - comments on IR ops

use std::{collections::BTreeMap, rc::Rc};

use swim_typecheck::{Function as TypeCheckedFunction, FunctionId, SwimType, TypeChecker, TypeVariable, TypedExpr};
use swim_utils::{Identifier, IndexMap, SymbolId};

mod error;
mod opcodes;

use error::*;
use opcodes::*;
pub use opcodes::{DataLabel, Intrinsic, IrOpcode, Reg};

pub fn lower(checker: TypeChecker) -> Result<(DataSection, Vec<IrOpcode>), LoweringError> {
    let lowerer = Lowerer::new(checker);
    Ok(lowerer.finalize())
}

// TODO: fully typed functions
pub struct Function {
    label: FunctionLabel,
    body:  Vec<IrOpcode>,
}

pub type DataSection = IndexMap<DataLabel, DataSectionEntry>;
/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: DataSection,
    entry_point: Option<FunctionLabel>,
    function_definitions: BTreeMap<FunctionId, Function>,
    reg_assigner: usize,
    function_label_assigner: usize,
    type_checker: TypeChecker,
    variables_in_scope: Vec<BTreeMap<SymbolId, Reg>>,
    /// What register functions expect their return jump destination to be stored in
    /// when they are called. TODO: use a context API for this so it is impossible to generate
    /// a function call without setting the return dest.
    function_return_destinations: BTreeMap<FunctionId, Reg>,
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
            variables_in_scope: Default::default(),
            function_return_destinations: Default::default(),
        };
        lowerer.lower_all_functions().expect("errors should get caught before lowering");
        lowerer
    }

    pub fn finalize(self) -> (DataSection, Vec<IrOpcode>) {
        let mut program_section = vec![];

        for (label, Function { label: _label, mut body }) in self.function_definitions {
            program_section.push(IrOpcode::FunctionLabel(label));
            program_section.append(&mut body);
        }

        // if let Some(entry_point) = self.entry_point {
        //     // Add entry point function body to the program section
        //     if let Some(entry_func) = self.function_definitions.get(todo!()) {
        //         program_section.extend(entry_func.body.clone());
        //     }
        // }

        // Add other function bodies to the program section
        // for func in self.function_definitions.values() {
        //     if func.label != FunctionLabel::from(0) {
        //         program_section.extend(func.body.clone());
        //     }
        // }

        (self.data_section.clone(), program_section)
    }

    fn new_function_label(&mut self) -> FunctionLabel {
        let label = self.function_label_assigner;
        self.function_label_assigner += 1;
        FunctionLabel::from(label)
    }

    fn lower_function(
        &mut self,
        id: FunctionId,
        func: TypeCheckedFunction,
    ) -> Result<(), LoweringError> {
        let func_label = self.new_function_label();
        let mut buf = vec![];
        self.with_variable_context(|ctx| -> Result<_, _> {
            // TODO: func should have type checked types...not just the AST type
            for (param_name, param_ty) in &func.params {
                // in order, assign parameters to registers
                if fits_in_reg(param_ty) {
                    // load from stack into register
                    let param_reg = ctx.fresh_reg();
                    let ty_reg = TypedReg {
                        ty:  ctx.to_ir_type(param_ty),
                        reg: param_reg,
                    };
                    buf.push(IrOpcode::StackPop(ty_reg));
                    // insert param into mapping
                    ctx.insert_var(param_name, param_reg);
                } else {
                    todo!("make reg a ptr to the value")
                }
            }

            // TODO we could support other return dests
            let return_dest = ReturnDestination::Stack;
            let mut expr_body = ctx.lower_expr(&func.body, return_dest)?;
            buf.append(&mut expr_body);

            let return_reg = ctx.fresh_reg();
            ctx.function_return_destinations.insert(id, return_reg);
            // jump back to caller
            let pop_reg = ctx.fresh_reg();
            todo!("function call stack");

            ctx.function_definitions.insert(
                id,
                Function {
                    body:  buf,
                    label: func_label,
                },
            );
            Ok(())
        })
    }

    fn fresh_reg(&mut self) -> Reg {
        let val = self.reg_assigner;
        self.reg_assigner += 1;
        Reg::Virtual(val)
    }

    fn lower_expr(
        &mut self,
        body: &TypedExpr,
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
            FunctionCall { func, args, ty } => {
                let mut buf = Vec::with_capacity(args.len());
                // push all args onto the stack in order
                for (_arg_name, arg_expr) in args {
                    let mut expr = self.lower_expr(arg_expr, ReturnDestination::Stack)?;
                    buf.append(&mut expr);
                }

                todo!();
                // buf.push(IrOpcode::(ctx.new_label()));
                buf.push(IrOpcode::JumpImmediate(*func));
                Ok(buf)
            },
            List { elements, ty } => todo!(),
            Unit => todo!(),
            Variable { name, ty } => {
                let var_reg = self
                    .variables_in_scope
                    .last()
                    .expect("should be at least one scope")
                    .get(&name.id)
                    .expect("var did not exist TODO err");
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::Copy(reg, *var_reg)],
                    ReturnDestination::Stack => vec![IrOpcode::StackPush(TypedReg {
                        ty:  self.to_ir_type(ty),
                        reg: *var_reg,
                    })],
                })
            },
            Intrinsic { ty, intrinsic } => self.lower_intrinsic(intrinsic, return_destination),
            ErrorRecovery => Err(LoweringError),
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
            String => IrTy::String,
        }
    }

    fn lower_all_functions(&mut self) -> Result<(), LoweringError> {
        for (id, func) in self.type_checker.functions() {
            self.lower_function(id, func)?;
        }
        Ok(())
    }

    fn lower_intrinsic(
        &mut self,
        intrinsic: &swim_typecheck::Intrinsic,
        return_destination: ReturnDestination,
    ) -> Result<Vec<IrOpcode>, LoweringError> {
        let instr = match intrinsic {
            swim_typecheck::Intrinsic::Puts(arg) => {
                // puts takes one arg and it is a string
                let arg_reg = self.fresh_reg();
                self.lower_expr(arg, ReturnDestination::Reg(arg_reg))?;
                IrOpcode::Intrinsic(Intrinsic::Puts(TypedReg {
                    ty:  IrTy::Ptr(Box::new(IrTy::String)),
                    reg: arg_reg,
                }))
            },
        };

        Ok(match return_destination {
            ReturnDestination::Reg(reg) => vec![instr, IrOpcode::LoadImmediate(reg, 0)],
            ReturnDestination::Stack => {
                let reg = self.fresh_reg();
                vec![instr, IrOpcode::StackPush(TypedReg { ty: IrTy::Unit, reg })]
            },
        })
    }

    /// Produces a new context for variables in a scope to be allocated
    fn with_variable_context<F, T>(
        &mut self,
        func: F,
    ) -> Result<T, LoweringError>
    where
        F: FnOnce(&mut Self) -> Result<T, LoweringError>,
    {
        self.variables_in_scope.push(Default::default());
        let res = func(self);
        self.variables_in_scope.pop();
        res
    }

    fn insert_var(
        &mut self,
        param_name: &Identifier,
        param_reg: Reg,
    ) {
        self.variables_in_scope.last_mut().map(|scope| scope.insert(param_name.id, param_reg));
    }

    pub fn pretty_print(&self) -> String {
        let mut result = String::new();
        result.push_str("; DATA_SECTION\n");
        for (label, entry) in self.data_section.iter() {
            result.push_str(&format!("{}: {:?}\n", Into::<usize>::into(label), entry));
        }

        result.push_str("\n; PROGRAM_SECTION\n");
        for (id, func) in &self.function_definitions {
            result.push_str(&format!("Function {:?}:\n", id));
            for opcode in &func.body {
                result.push_str(&format!("  {}\n", opcode));
            }
        }
        result
    }
}

type VariableScope = BTreeMap<SymbolId, Reg>;

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
        let (errs, resolved) = resolve_symbols(ast, interner);
        let type_checker = TypeChecker::new(resolved);
        let lowerer = Lowerer::new(type_checker);
        let res = lowerer.pretty_print();

        expect.assert_eq(&res);
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
                Function FunctionId(0):
                  ld v0 datalabel0
                  push v0
            "#]],
        );
    }

    #[test]
    fn func_calls_intrinsic() {
        check(
            r#"
                function main() returns 'unit @puts("hello")
                "#,
            expect![[r#"
                    ; DATA_SECTION
                    0: Int64(42)

                    ; PROGRAM_SECTION
                    Function TypedFunctionId(0):
                      ld v0 datalabel0
                      push v0
                "#]],
        );
    }
    #[test]
    fn func_calls_other_func() {
        check(
            r#"
                    function main() returns 'bool ~foo(123)
                    function foo(a in 'int) returns 'bool true
                    "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(123)
                1: Bool(true)

                ; PROGRAM_SECTION
                Function FunctionId(0):
                  ld v0 datalabel0
                  push v0
                  jfunc functionid1
                Function FunctionId(1):
                  pop v1
                  ld v2 datalabel1
                  push v2
            "#]],
        );
    }
    #[test]
    fn func_args_with_op() {
        check(
            r#"
                function add(x in 'int, y in 'int) returns 'int + x y
                function main() returns 'int ~add(1, 2)
                "#,
            expect![[r#""#]],
        );
    }
    #[test]
    fn func_args() {
        check(
            r#"
                function add(x in 'int, y in 'int) returns 'int x
                function main() returns 'int ~add(1, 2)
                "#,
            expect![[r#""#]],
        );
    }
}
