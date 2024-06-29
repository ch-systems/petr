// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc
// - store position to jump back to after fn call
// - terminate instructions in correct places (end of entry point)
// - comments on IR ops
// - dead code elimination

use std::{collections::BTreeMap, rc::Rc};

use petr_typecheck::{Function as TypeCheckedFunction, FunctionId, TypeChecker, TypeVariable, TypedExpr};
use petr_utils::{Identifier, IndexMap, SymbolId};

mod error;
mod opcodes;

use error::*;
use opcodes::*;
pub use opcodes::{DataLabel, Intrinsic, IrOpcode, Reg, ReservedRegister};

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
            program_section.push(IrOpcode::ReturnImmediate(0));
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

    /// this lowers a function declaration.
    fn lower_function(
        &mut self,
        id: FunctionId,
        func: TypeCheckedFunction,
    ) -> Result<(), LoweringError> {
        let func_label = self.new_function_label();
        let mut buf = vec![];
        self.with_variable_context(|ctx| -> Result<_, _> {
            // Pop parameters off the stack in reverse order -- the last parameter for the function
            // will be the first thing popped off the stack
            // When we lower a function call, we push them onto the stack from first to last. Since
            // the stack is FILO, we reverse that order here.

            for (param_name, param_ty) in func.params.iter().rev() {
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

            let return_reg = ctx.fresh_reg();
            let return_dest = ReturnDestination::Reg(return_reg);
            let mut expr_body = ctx.lower_expr(&func.body, return_dest)?;
            buf.append(&mut expr_body);
            // load return value into func return register

            buf.push(IrOpcode::Copy(Reg::Reserved(ReservedRegister::ReturnValueRegister), return_reg));

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
            Literal { value, ty: _ } => {
                let data_label = self.insert_literal_data(value);
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::LoadData(reg, data_label)],
                })
            },
            FunctionCall { func, args, ty: _ty } => {
                let mut buf = Vec::with_capacity(args.len());
                // push all args onto the stack in order
                for (_arg_name, arg_expr) in args {
                    let reg = self.fresh_reg();
                    let mut expr = self.lower_expr(arg_expr, ReturnDestination::Reg(reg))?;
                    expr.push(IrOpcode::StackPush(TypedReg {
                        ty: self.to_ir_type(&arg_expr.ty()),
                        reg,
                    }));

                    buf.append(&mut expr);
                }
                // push current PC onto the stack
                buf.push(IrOpcode::PushPc());

                // jump to the function
                buf.push(IrOpcode::JumpImmediate(*func));

                // after returning to this function, return the register
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        buf.push(IrOpcode::Copy(reg, Reg::Reserved(ReservedRegister::ReturnValueRegister)));
                    },
                }
                //
                Ok(buf)
            },
            List { .. } => todo!(),
            Unit => todo!(),
            Variable { name, ty: _ } => {
                let var_reg = self
                    .get_variable(name.id)
                    .unwrap_or_else(|| panic!("var {} did not exist TODO err", name.id));
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::Copy(reg, var_reg)],
                })
            },
            Intrinsic { ty: _ty, intrinsic } => self.lower_intrinsic(intrinsic, return_destination),
            ErrorRecovery => Err(LoweringError),
            ExprWithBindings { bindings, expression } => self.with_variable_context(|ctx| -> Result<_, _> {
                let mut buf = vec![];
                for (name, expr) in bindings {
                    let reg = ctx.fresh_reg();
                    let mut expr = ctx.lower_expr(expr, ReturnDestination::Reg(reg))?;
                    buf.append(&mut expr);
                    ctx.insert_var(name, reg);
                }
                let mut expr = ctx.lower_expr(expression, return_destination)?;
                buf.append(&mut expr);
                Ok(buf)
            }),
            TypeConstructor { .. } => todo!(),
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
            Variable(_) => todo!("untyped variable"),
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
                })));
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        buf.push(IrOpcode::LoadImmediate(reg, 0));
                    },
                }
                Ok(buf)
            },
            petr_typecheck::Intrinsic::Add(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Add),
            petr_typecheck::Intrinsic::Multiply(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Multiply),
            petr_typecheck::Intrinsic::Divide(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Divide),
            petr_typecheck::Intrinsic::Subtract(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Subtract),
        }
    }

    fn lower_arithmetic_op(
        &mut self,
        lhs: &TypedExpr,
        rhs: &TypedExpr,
        return_destination: ReturnDestination,
        op: fn(Reg, Reg, Reg) -> IrOpcode,
    ) -> Result<Vec<IrOpcode>, LoweringError> {
        let mut buf = vec![];
        let lhs_reg = self.fresh_reg();
        let rhs_reg = self.fresh_reg();
        buf.append(&mut self.lower_expr(lhs, ReturnDestination::Reg(lhs_reg))?);
        buf.append(&mut self.lower_expr(rhs, ReturnDestination::Reg(rhs_reg))?);
        let ReturnDestination::Reg(return_reg) = return_destination;
        buf.push(op(return_reg, lhs_reg, rhs_reg));
        Ok(buf)
    }

    fn get_variable(
        &self,
        id: SymbolId,
    ) -> Option<Reg> {
        for scope in self.variables_in_scope.iter().rev() {
            if let Some(reg) = scope.get(&id) {
                return Some(*reg);
            }
        }
        None
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

enum ReturnDestination {
    Reg(Reg),
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
        let parser = petr_parse::Parser::new(vec![
            ("std/ops.pt", "function add(lhs in 'int, rhs in 'int) returns 'int @add lhs, rhs"),
            ("test", &input),
        ]);
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
                	ENTRY: 1
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                ENTRY: function 1:
                 7	ld v5 datalabel0
                 8	cp rr(func return value) v5
                 9	ret
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
                	ENTRY: 1
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                ENTRY: function 1:
                 7	ld v6 datalabel0
                 8	intrinsic @puts(v6)
                 9	imm v5 0
                 10	cp rr(func return value) v5
                 11	ret
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
                	ENTRY: 1
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                ENTRY: function 1:
                 7	ld v6 datalabel0
                 8	push v6
                 9	ppc
                 10	jumpi functionid2
                 11	cp v5 rr(func return value)
                 12	cp rr(func return value) v5
                 13	ret
                function 2:
                 14	pop v7
                 15	ld v8 datalabel1
                 16	cp rr(func return value) v8
                 17	ret
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
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                function 1:
                 7	pop v5
                 8	pop v6
                 9	cp v8 v6
                 10	push v8
                 11	cp v9 v5
                 12	push v9
                 13	ppc
                 14	jumpi functionid0
                 15	cp v7 rr(func return value)
                 16	cp rr(func return value) v7
                 17	ret
                ENTRY: function 2:
                 18	ld v11 datalabel0
                 19	push v11
                 20	ld v12 datalabel1
                 21	push v12
                 22	ppc
                 23	jumpi functionid1
                 24	cp v10 rr(func return value)
                 25	cp rr(func return value) v10
                 26	ret
            "#]],
        );
    }

    #[test]
    fn func_args() {
        check(
            r#"
                function test(x in 'int, y in 'int) returns 'int x
                function main() returns 'int ~test(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                function 1:
                 7	pop v5
                 8	pop v6
                 9	cp v7 v6
                 10	cp rr(func return value) v7
                 11	ret
                ENTRY: function 2:
                 12	ld v9 datalabel0
                 13	push v9
                 14	ld v10 datalabel1
                 15	push v10
                 16	ppc
                 17	jumpi functionid1
                 18	cp v8 rr(func return value)
                 19	cp rr(func return value) v8
                 20	ret
            "#]],
        );
    }

    #[test]
    fn let_bindings_with_ops() {
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
                0: Int64(10)
                1: Int64(20)
                2: Int64(1)
                3: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                function 1:
                 7	pop v5
                 8	pop v6
                 9	ld v8 datalabel0
                 10	ld v9 datalabel1
                 11	cp v10 v8
                 12	push v10
                 13	cp v12 v9
                 14	push v12
                 15	cp v14 v6
                 16	push v14
                 17	cp v15 v5
                 18	push v15
                 19	ppc
                 20	jumpi functionid0
                 21	cp v13 rr(func return value)
                 22	push v13
                 23	ppc
                 24	jumpi functionid0
                 25	cp v11 rr(func return value)
                 26	push v11
                 27	ppc
                 28	jumpi functionid0
                 29	cp v7 rr(func return value)
                 30	cp rr(func return value) v7
                 31	ret
                ENTRY: function 2:
                 32	ld v17 datalabel2
                 33	push v17
                 34	ld v18 datalabel3
                 35	push v18
                 36	ppc
                 37	jumpi functionid1
                 38	cp v16 rr(func return value)
                 39	cp rr(func return value) v16
                 40	ret
            "#]],
        );
    }
    #[test]
    fn let_bindings() {
        check(
            r#"
                function hi(x in 'int, y in 'int) returns 'int
                    let a = x,
                        b = y,
                        c = 20,
                        d = 30,
                        e = 42,
                    + a + b + c + d e
                function main() returns 'int ~hi(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(20)
                1: Int64(30)
                2: Int64(42)
                3: Int64(1)
                4: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v0
                 1	pop v1
                 2	cp v3 v1
                 3	cp v4 v0
                 4	add v2 v3 v4
                 5	cp rr(func return value) v2
                 6	ret
                function 1:
                 7	pop v5
                 8	pop v6
                 9	cp v8 v6
                 10	cp v9 v5
                 11	ld v10 datalabel0
                 12	ld v11 datalabel1
                 13	ld v12 datalabel2
                 14	cp v13 v8
                 15	push v13
                 16	cp v15 v9
                 17	push v15
                 18	cp v17 v10
                 19	push v17
                 20	cp v19 v11
                 21	push v19
                 22	cp v20 v12
                 23	push v20
                 24	ppc
                 25	jumpi functionid0
                 26	cp v18 rr(func return value)
                 27	push v18
                 28	ppc
                 29	jumpi functionid0
                 30	cp v16 rr(func return value)
                 31	push v16
                 32	ppc
                 33	jumpi functionid0
                 34	cp v14 rr(func return value)
                 35	push v14
                 36	ppc
                 37	jumpi functionid0
                 38	cp v7 rr(func return value)
                 39	cp rr(func return value) v7
                 40	ret
                ENTRY: function 2:
                 41	ld v22 datalabel3
                 42	push v22
                 43	ld v23 datalabel4
                 44	push v23
                 45	ppc
                 46	jumpi functionid1
                 47	cp v21 rr(func return value)
                 48	cp rr(func return value) v21
                 49	ret
            "#]],
        );
    }
}
