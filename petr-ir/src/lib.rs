// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc
// - store position to jump back to after fn call
// - terminate instructions in correct places (end of entry point)
// - comments on IR ops

use std::{collections::BTreeMap, rc::Rc};

use petr_typecheck::{Function as TypeCheckedFunction, FunctionId, TypeChecker, TypeVariable, TypedExpr};
use petr_utils::{Identifier, IndexMap, SymbolId};

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
    entry_point: Option<FunctionId>,
    function_definitions: BTreeMap<FunctionId, Function>,
    reg_assigner: usize,
    function_label_assigner: usize,
    type_checker: TypeChecker,
    variables_in_scope: Vec<BTreeMap<SymbolId, Reg>>,
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
            entry_point: {
                // set entry point to func named main
                type_checker
                    .functions()
                    .find(|(_func_id, func)| &*type_checker.get_symbol(func.name.id) == "main")
                    .map(|(id, _)| id)
            },
            function_definitions: BTreeMap::default(),
            reg_assigner: 0,
            function_label_assigner: 0,
            type_checker,
            variables_in_scope: Default::default(),
        };
        lowerer.lower_all_functions().expect("errors should get caught before lowering");
        lowerer
    }

    pub fn finalize(self) -> (DataSection, Vec<IrOpcode>) {
        let mut program_section = vec![];

        // insert jump to entry point as first instr
        if let Some(entry_point) = self.entry_point {
            program_section.push(IrOpcode::JumpImmediate(entry_point));
        } else {
            // TODO use diagnostics here
            eprintln!("Warning: Generating IR for program with no entry point");
            program_section.push(IrOpcode::Return());
        }

        for (label, Function { label: _label, mut body }) in self.function_definitions {
            program_section.push(IrOpcode::FunctionLabel(label));
            program_section.append(&mut body);
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

            // jump back to caller
            buf.push(IrOpcode::Return());

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
            FunctionCall { func, args, ty: _ty } => {
                let mut buf = Vec::with_capacity(args.len());
                // push all args onto the stack in order
                for (_arg_name, arg_expr) in args {
                    let mut expr = self.lower_expr(arg_expr, ReturnDestination::Stack)?;
                    buf.append(&mut expr);
                }
                // push current PC onto the stack
                buf.push(IrOpcode::PushPc());

                // jump to the function
                buf.push(IrOpcode::JumpImmediate(*func));
                Ok(buf)
            },
            List { .. } => todo!(),
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
            Intrinsic { ty: _ty, intrinsic } => self.lower_intrinsic(intrinsic, return_destination),
            ErrorRecovery => Err(LoweringError),
        }
    }

    fn insert_literal_data(
        &mut self,
        lit: &petr_typecheck::Literal,
    ) -> DataLabel {
        use petr_typecheck::Literal::*;

        self.data_section.insert(match lit {
            Integer(val) => DataSectionEntry::Int64(*val),
            Boolean(val) => DataSectionEntry::Bool(*val),
            String(val) => DataSectionEntry::String(val.clone()),
        })
    }

    // convert a polytype type to an `IrTy`
    fn to_ir_type(
        &self,
        param_ty: &TypeVariable,
    ) -> IrTy {
        let realized_ty = self.type_checker.realize_type(param_ty);
        use petr_typecheck::PetrType::*;
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
        intrinsic: &petr_typecheck::Intrinsic,
        return_destination: ReturnDestination,
    ) -> Result<Vec<IrOpcode>, LoweringError> {
        let mut buf = vec![];
        match intrinsic {
            petr_typecheck::Intrinsic::Puts(arg) => {
                // puts takes one arg and it is a string
                let arg_reg = self.fresh_reg();
                buf.append(&mut self.lower_expr(arg, ReturnDestination::Reg(arg_reg))?);
                buf.push(IrOpcode::Intrinsic(Intrinsic::Puts(TypedReg {
                    ty:  IrTy::Ptr(Box::new(IrTy::String)),
                    reg: arg_reg,
                })))
            },
        }

        match return_destination {
            ReturnDestination::Reg(reg) => {
                buf.push(IrOpcode::LoadImmediate(reg, 0));
            },
            ReturnDestination::Stack => {
                buf.push(IrOpcode::StackPushImmediate(0));
            },
        }
        Ok(buf)
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
        let mut pc = 0;

        result.push_str("\n; PROGRAM_SECTION\n");
        if let Some(entry_point) = self.entry_point {
            result.push_str(&format!("\tENTRY: {}\n", Into::<usize>::into(entry_point)));
        } else {
            result.push_str("\tNO ENTRY POINT\n");
        }
        for (id, func) in &self.function_definitions {
            result.push_str(&format!(
                "{}function {}:\n",
                if Some(*id) == self.entry_point { "ENTRY: " } else { "" },
                Into::<usize>::into(*id)
            ));
            for opcode in &func.body {
                result.push_str(&format!(" {pc}\t{}\n", opcode));
                pc += 1;
            }
        }
        result
    }
}

// type VariableScope = BTreeMap<SymbolId, Reg>;

enum ReturnDestination {
    Reg(Reg),
    Stack,
}
fn fits_in_reg(_: &TypeVariable) -> bool {
    // TODO
    true
}

#[allow(dead_code)]
fn literal_to_ir_ty(param_ty: petr_typecheck::Literal) -> IrTy {
    use petr_typecheck::Literal::*;
    match param_ty {
        Integer(_) => todo!(),
        Boolean(_) => todo!(),
        String(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};
    use petr_resolve::resolve_symbols;
    use petr_utils::render_error;

    use super::*;
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = petr_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
        if !errs.is_empty() {
            dbg!(&errs);
        }
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
                	ENTRY: 0
                ENTRY: function 0:
                 0	ld v0 datalabel0
                 1	push v0
                 2	ret
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
                0: String("hello")

                ; PROGRAM_SECTION
                	ENTRY: 0
                ENTRY: function 0:
                 0	ld v0 datalabel0
                 1	intrinsic @puts(v0)
                 2	pushi 0
                 3	ret
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
                	ENTRY: 0
                ENTRY: function 0:
                 0	ld v0 datalabel0
                 1	push v0
                 2	ppc
                 3	jumpi functionid1
                 4	ret
                function 1:
                 5	pop v1
                 6	ld v2 datalabel1
                 7	push v2
                 8	ret
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
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 1
                function 0:
                 0	pop v0
                 1	pop v1
                 2	push v0
                 3	ret
                ENTRY: function 1:
                 4	ld v2 datalabel0
                 5	push v2
                 6	ld v3 datalabel1
                 7	push v3
                 8	ppc
                 9	jumpi functionid0
                 10	ret
            "#]],
        );
    }

    #[test]
    fn let_bindings() {
        check(
            r#"
                function add(x in 'int, y in 'int) returns 'int
                    let a = 10,
                        b = 20
                    + a + b + x y
                function main() returns 'int ~add(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                function 0:
                 0	pop v0
                 1	pop v1
                 2	push v0
                 3	ret
                ENTRY: function 1:
                 4	ld v2 datalabel0
                 5	push v2
                 6	ld v3 datalabel1
                 7	push v3
                 8	ppc
                 9	jumpi functionid0
                 10	ret
            "#]],
        );
    }
}
