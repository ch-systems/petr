// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc
// - store position to jump back to after fn call
// - terminate instructions in correct places (end of entry point)
// - comments on IR ops
// - dead code elimination
//

use std::{collections::BTreeMap, rc::Rc};

use petr_typecheck::{FunctionSignature, PetrType, TypeChecker, TypeVariable, TypedExpr, TypedExprKind};
use petr_utils::{idx_map_key, Identifier, IndexMap, SpannedItem, SymbolId};

mod error;
mod opcodes;

pub use error::LoweringError;
use opcodes::*;
pub use opcodes::{DataLabel, Intrinsic, IrOpcode, LabelId, Reg, ReservedRegister};

pub fn lower(checker: TypeChecker) -> Result<(DataSection, Vec<IrOpcode>)> {
    let lowerer = Lowerer::new(checker)?;
    Ok(lowerer.finalize())
}

// TODO: fully typed functions
pub struct Function {
    body: Vec<IrOpcode>,
}

idx_map_key!(MonomorphizedFunctionId);

pub type Result<T> = std::result::Result<T, SpannedItem<LoweringError>>;

pub type DataSection = IndexMap<DataLabel, DataSectionEntry>;
/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: DataSection,
    entry_point: Option<MonomorphizedFunctionId>,
    reg_assigner: usize,
    type_checker: TypeChecker,
    variables_in_scope: Vec<BTreeMap<SymbolId, Reg>>,
    monomorphized_functions: IndexMap<MonomorphizedFunctionId, (FunctionSignature, Function)>,
    errors: Vec<SpannedItem<LoweringError>>,
    label_assigner: usize,
}

#[derive(Debug, Clone)]
pub enum DataSectionEntry {
    Int64(i64),
    String(Rc<str>),
    Bool(bool),
}

impl Lowerer {
    pub fn new(type_checker: TypeChecker) -> Result<Self> {
        // if there is an entry point, set that
        // set entry point to func named main
        let entry_point = type_checker.get_main_function();

        let mut lowerer = Self {
            data_section: IndexMap::default(),
            entry_point: None,
            reg_assigner: 0,
            type_checker,
            variables_in_scope: Default::default(),
            monomorphized_functions: Default::default(),
            label_assigner: 0,
            errors: Default::default(),
        };

        let monomorphized_entry_point_id = match entry_point {
            None => None,
            Some((id, _func)) => {
                let monomorphized_entry_point_id = lowerer.monomorphize_function((id, vec![].into_boxed_slice()))?;
                Some(monomorphized_entry_point_id)
            },
        };

        lowerer.entry_point = monomorphized_entry_point_id;
        Ok(lowerer)
    }

    pub fn finalize(self) -> (DataSection, Vec<IrOpcode>) {
        let mut program_section = vec![];

        // insert jump to entry point as first instr
        if let Some(entry_point) = self.entry_point {
            program_section.push(IrOpcode::JumpImmediateFunction(entry_point));
        } else {
            // TODO use diagnostics here
            eprintln!("Warning: Generating IR for program with no entry point");
            program_section.push(IrOpcode::ReturnImmediate(0));
        }

        for (label, (_signature, mut function)) in self.monomorphized_functions.into_iter() {
            program_section.push(IrOpcode::FunctionLabel(label));
            program_section.append(&mut function.body);
        }

        (self.data_section.clone(), program_section)
    }

    /// this lowers a function declaration.
    fn monomorphize_function(
        &mut self,
        func: FunctionSignature,
    ) -> Result<MonomorphizedFunctionId> {
        if let Some(previously_monomorphized_definition) = self.monomorphized_functions.iter().find(|(_id, (sig, _))| *sig == func) {
            return Ok(previously_monomorphized_definition.0);
        }

        let func_def = self.type_checker.get_monomorphized_function(&func).clone();

        let mut buf = vec![];
        self.with_variable_context(|ctx| -> Result<_> {
            // Pop parameters off the stack in reverse order -- the last parameter for the function
            // will be the first thing popped off the stack
            // When we lower a function call, we push them onto the stack from first to last. Since
            // the stack is FILO, we reverse that order here.

            for (param_ty, (param_name, _)) in func.1.iter().zip(func_def.params).rev() {
                let param_ty = ctx.petr_type_to_ir_type(param_ty.clone());
                // in order, assign parameters to registers
                if param_ty.fits_in_reg() {
                    // load from stack into register
                    let param_reg = ctx.fresh_reg();
                    let ty_reg = TypedReg {
                        ty:  param_ty.clone(),
                        reg: param_reg,
                    };
                    buf.push(IrOpcode::StackPop(ty_reg));
                    // insert param into mapping

                    ctx.insert_var(&param_name, param_reg);
                } else {
                    todo!("make reg a ptr to the value")
                }
            }

            let return_reg = ctx.fresh_reg();
            let return_dest = ReturnDestination::Reg(return_reg);
            let mut expr_body = ctx.lower_expr(&func_def.body, return_dest)?;
            buf.append(&mut expr_body);
            // load return value into func return register

            buf.push(IrOpcode::Copy(Reg::Reserved(ReservedRegister::ReturnValueRegister), return_reg));

            // jump back to caller
            buf.push(IrOpcode::Return());

            Ok(ctx.monomorphized_functions.insert((func, Function { body: buf })))
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
    ) -> Result<Vec<IrOpcode>> {
        use TypedExprKind::*;

        match &body.kind {
            Literal { value, ty: _ } => {
                let data_label = self.insert_literal_data(value);
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::LoadData(reg, data_label)],
                })
            },
            FunctionCall { func, args, ty: _ty } => {
                let mut buf = Vec::new();
                // push all args onto the stack in order

                let mut arg_types = Vec::with_capacity(args.len());
                for (arg_name, arg_expr) in args {
                    let reg = self.fresh_reg();
                    let mut expr = self.lower_expr(arg_expr, ReturnDestination::Reg(reg))?;
                    let arg_ty = self.type_checker.expr_ty(arg_expr);
                    let petr_ty = self.type_checker.look_up_variable(arg_ty);
                    arg_types.push((*arg_name, petr_ty.clone()));
                    let ir_ty = self.petr_type_to_ir_type(petr_ty.clone());
                    expr.push(IrOpcode::StackPush(TypedReg { ty: ir_ty, reg }));

                    buf.append(&mut expr);
                }
                // push current PC onto the stack
                buf.push(IrOpcode::PushPc());

                let arg_petr_types = arg_types.iter().map(|(_name, ty)| ty.clone()).collect::<Vec<_>>();

                let monomorphized_func_id = self.monomorphize_function((*func, arg_petr_types.into_boxed_slice()))?;

                // jump to the function
                buf.push(IrOpcode::JumpImmediateFunction(monomorphized_func_id));

                // after returning to this function, return the register
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        buf.push(IrOpcode::Copy(reg, Reg::Reserved(ReservedRegister::ReturnValueRegister)));
                    },
                }

                // add this call's function singature (label + concrete types) to the list of
                // functions that need to be generated

                //
                Ok(buf)
            },
            List { elements, .. } => {
                let size_of_each_elements = elements
                    .iter()
                    .map(|el| self.to_ir_type(self.type_checker.expr_ty(el)).size().num_bytes() as u64)
                    .sum::<u64>();
                let size_of_list = size_of_each_elements * elements.len() as u64;
                let size_of_list_reg = self.fresh_reg();

                let mut buf = vec![];
                buf.push(IrOpcode::LoadImmediate(size_of_list_reg, size_of_list));
                let ReturnDestination::Reg(return_reg) = return_destination;
                buf.push(IrOpcode::Malloc(return_reg, size_of_list_reg));

                let mut current_offset = 0;
                let current_offset_reg = self.fresh_reg();
                for el in elements {
                    // currently this only works for types that fit in a single register,
                    // will need work for larger types
                    let reg = self.fresh_reg();
                    buf.append(&mut self.lower_expr(el, ReturnDestination::Reg(reg))?);
                    buf.push(IrOpcode::LoadImmediate(current_offset_reg, current_offset));
                    buf.push(IrOpcode::Add(current_offset_reg, current_offset_reg, return_reg));
                    buf.push(IrOpcode::WriteRegisterToMemory(reg, current_offset_reg));
                    current_offset += self.to_ir_type(self.type_checker.expr_ty(el)).size().num_bytes() as u64;
                }
                Ok(buf)
            },
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
            ErrorRecovery(span) => Err(span.with_item(LoweringError::Internal("Lowering should not be performed on an AST with errors".into()))),
            ExprWithBindings { bindings, expression } => self.with_variable_context(|ctx| -> Result<_> {
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
            TypeConstructor { ty, args } => {
                let mut buf = vec![];
                // the memory model for types is currently not finalized,
                // but for now, it is just sequential memory that is word-aligned
                let ir_ty = self.to_ir_type(*ty);
                let size_of_aggregate_type = ir_ty.size();
                let ReturnDestination::Reg(return_destination) = return_destination;
                buf.push(IrOpcode::MallocImmediate(return_destination, size_of_aggregate_type));
                // for each arg, lower it and store it in memory
                let mut current_size_offset = 0;
                let current_size_offset_reg = self.fresh_reg();
                for arg in args.iter() {
                    let reg = self.fresh_reg();
                    buf.append(&mut self.lower_expr(arg, ReturnDestination::Reg(reg))?);
                    buf.push(IrOpcode::LoadImmediate(current_size_offset_reg, current_size_offset));
                    buf.push(IrOpcode::Add(current_size_offset_reg, current_size_offset_reg, return_destination));
                    buf.push(IrOpcode::WriteRegisterToMemory(reg, current_size_offset_reg));

                    let arg_ty = self.type_checker.expr_ty(arg);

                    current_size_offset += self.to_ir_type(arg_ty).size().num_bytes() as u64;
                }
                Ok(buf)
            },
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut buf = vec![];
                let condition_reg = self.fresh_reg();
                buf.append(&mut self.lower_expr(condition, ReturnDestination::Reg(condition_reg))?);
                let else_label = self.new_label();
                let end_label = self.new_label();
                buf.push(IrOpcode::JumpIfFalseImmediate(condition_reg, else_label));
                buf.append(&mut self.lower_expr(then_branch, return_destination.clone())?);
                buf.push(IrOpcode::JumpImmediate(end_label));
                buf.push(IrOpcode::Label(else_label));
                buf.append(&mut self.lower_expr(else_branch, return_destination)?);
                buf.push(IrOpcode::Label(end_label));
                Ok(buf)
            },
        }
    }

    fn new_label(&mut self) -> LabelId {
        let label: LabelId = self.label_assigner.into();
        self.label_assigner += 1;
        label
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

    fn petr_type_to_ir_type(
        &mut self,
        ty: PetrType,
    ) -> IrTy {
        use petr_typecheck::PetrType::*;
        match ty {
            Unit => IrTy::Unit,
            Integer => IrTy::Int64,
            Boolean => IrTy::Boolean,
            String => IrTy::String,
            Ref(ty) => self.to_ir_type(ty),
            UserDefined { name: _, variants } => {
                // get the user type
                let mut variants_buf = Vec::with_capacity(variants.len());

                for variant in variants {
                    let mut fields_buf = Vec::with_capacity(variant.fields.len());
                    for field in &variant.fields {
                        fields_buf.push(self.to_ir_type(*field));
                    }
                    variants_buf.push(IrUserDefinedTypeVariant { fields: fields_buf });
                }
                IrTy::UserDefinedType { variants: variants_buf }
            },
            Arrow(_) => todo!(),
            ErrorRecovery => todo!(),
            List(ty) => IrTy::List(Box::new(self.to_ir_type(ty))),
            Infer(_, span) => {
                println!("Unable to infer ty: {ty:?}");
                self.errors.push(span.with_item(LoweringError::UnableToInferType));
                IrTy::Unit
            },
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_ir_type(
        &mut self,
        param_ty: TypeVariable,
    ) -> IrTy {
        let ty = self.type_checker.look_up_variable(param_ty).clone();
        self.petr_type_to_ir_type(ty)
    }

    fn lower_intrinsic(
        &mut self,
        intrinsic: &petr_typecheck::Intrinsic,
        return_destination: ReturnDestination,
    ) -> Result<Vec<IrOpcode>> {
        let mut buf = vec![];
        use petr_typecheck::Intrinsic::*;
        match intrinsic {
            Puts(arg) => {
                // puts takes one arg and it is a string
                let arg_reg = self.fresh_reg();
                buf.append(&mut self.lower_expr(arg, ReturnDestination::Reg(arg_reg))?);
                buf.push(IrOpcode::Intrinsic(Intrinsic::Puts(arg_reg)));
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        buf.push(IrOpcode::LoadImmediate(reg, 0));
                    },
                }
                Ok(buf)
            },
            Add(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Add),
            Multiply(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Multiply),
            Divide(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Divide),
            Subtract(lhs, rhs) => self.lower_arithmetic_op(lhs, rhs, return_destination, IrOpcode::Subtract),
            Malloc(size) => {
                let size_reg = self.fresh_reg();
                let ptr_dest = self.fresh_reg();
                buf.append(&mut self.lower_expr(size, ReturnDestination::Reg(size_reg))?);
                buf.push(IrOpcode::Malloc(ptr_dest, size_reg));
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        // `ptr_dest` will contain the ptr returned
                        // the user should check that the ptr is not null
                        // which would mean a failed allocation
                        buf.push(IrOpcode::Copy(reg, ptr_dest));
                    },
                }
                Ok(buf)
            },
            SizeOf(expr) => {
                let ty = self.type_checker.expr_ty(expr);
                let size = self.to_ir_type(ty).size();
                match return_destination {
                    ReturnDestination::Reg(reg) => {
                        buf.push(IrOpcode::LoadImmediate(reg, size.num_bytes() as u64));
                    },
                }
                Ok(buf)
            },
            Equals(lhs, rhs) => {
                let lhs_reg = self.fresh_reg();
                let rhs_reg = self.fresh_reg();
                buf.append(&mut self.lower_expr(lhs, ReturnDestination::Reg(lhs_reg))?);
                buf.append(&mut self.lower_expr(rhs, ReturnDestination::Reg(rhs_reg))?);
                let ReturnDestination::Reg(return_reg) = return_destination;
                buf.push(IrOpcode::Equal(return_reg, lhs_reg, rhs_reg));
                Ok(buf)
            },
        }
    }

    fn lower_arithmetic_op(
        &mut self,
        lhs: &TypedExpr,
        rhs: &TypedExpr,
        return_destination: ReturnDestination,
        op: fn(Reg, Reg, Reg) -> IrOpcode,
    ) -> Result<Vec<IrOpcode>> {
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
    ) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
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
        for (id, (_sig, func)) in self.monomorphized_functions.iter() {
            result.push_str(&format!(
                "{}function {}:\n",
                if Some(id) == self.entry_point { "ENTRY: " } else { "" },
                Into::<usize>::into(id)
            ));
            for opcode in &func.body {
                result.push_str(&format!(" {pc}\t{}\n", opcode));
                pc += 1;
            }
        }

        if !self.errors.is_empty() {
            result.push_str("\n____ERRORS____");
            for err in &self.errors {
                result.push_str(&format!("{:?}\n", err));
            }
        }
        result
    }

    pub fn errors(&self) -> &[SpannedItem<LoweringError>] {
        &self.errors
    }
}

#[derive(Clone)]
enum ReturnDestination {
    Reg(Reg),
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
            ("std/ops.pt", "fn add(lhs in 'int, rhs in 'int) returns 'int @add lhs, rhs"),
            ("test", &input),
        ]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("ir gen failed: code didn't parse");
        }
        let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
        if !errs.is_empty() {
            dbg!(&errs);
        }
        let type_checker = TypeChecker::new(resolved);

        let typecheck_errors = type_checker.errors();
        if !typecheck_errors.is_empty() {
            typecheck_errors.iter().for_each(|err| eprintln!("{:?}", err));
            panic!("ir gen failed: code didn't typecheck");
        }

        let lowerer = match Lowerer::new(type_checker) {
            Ok(lowerer) => lowerer,
            Err(err) => {
                eprintln!("{:?}", err);
                panic!("ir gen failed: code didn't lower");
            },
        };

        let res = lowerer.pretty_print();

        expect.assert_eq(&res);
    }

    #[test]
    fn basic_main_func() {
        check(
            r#"
            fn main() returns 'int 42
            "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(42)

                ; PROGRAM_SECTION
                	ENTRY: 0
                ENTRY: function 0:
                 0	ld v0 datalabel0
                 1	cp rr(func return value) v0
                 2	ret
            "#]],
        );
    }

    #[test]
    fn func_calls_intrinsic() {
        check(
            r#"
                fn main() returns 'unit @puts("hello")
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: String("hello")

                ; PROGRAM_SECTION
                	ENTRY: 0
                ENTRY: function 0:
                 0	ld v1 datalabel0
                 1	intrinsic @puts(v1)
                 2	imm v0 0
                 3	cp rr(func return value) v0
                 4	ret
            "#]],
        );
    }
    #[test]
    fn func_calls_other_func() {
        check(
            r#"
                    fn main() returns 'bool ~foo(123)
                    fn foo(a in 'int) returns 'bool true
                    "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(123)
                1: Bool(true)

                ; PROGRAM_SECTION
                	ENTRY: 1
                function 0:
                 0	pop v2
                 1	ld v3 datalabel1
                 2	cp rr(func return value) v3
                 3	ret
                ENTRY: function 1:
                 4	ld v1 datalabel0
                 5	push v1
                 6	ppc
                 7	jumpi monomorphizedfunctionid0
                 8	cp v0 rr(func return value)
                 9	cp rr(func return value) v0
                 10	ret
            "#]],
        );
    }
    #[test]
    fn func_args_with_op() {
        check(
            r#"
                fn add(x in 'int, y in 'int) returns 'int + x y
                fn main() returns 'int ~add(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v8
                 1	pop v9
                 2	cp v11 v9
                 3	cp v12 v8
                 4	add v10 v11 v12
                 5	cp rr(func return value) v10
                 6	ret
                function 1:
                 7	pop v3
                 8	pop v4
                 9	cp v6 v4
                 10	push v6
                 11	cp v7 v3
                 12	push v7
                 13	ppc
                 14	jumpi monomorphizedfunctionid0
                 15	cp v5 rr(func return value)
                 16	cp rr(func return value) v5
                 17	ret
                ENTRY: function 2:
                 18	ld v1 datalabel0
                 19	push v1
                 20	ld v2 datalabel1
                 21	push v2
                 22	ppc
                 23	jumpi monomorphizedfunctionid1
                 24	cp v0 rr(func return value)
                 25	cp rr(func return value) v0
                 26	ret
            "#]],
        );
    }

    #[test]
    fn func_args() {
        check(
            r#"
                fn test(x in 'int, y in 'int) returns 'int x
                fn main() returns 'int ~test(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)

                ; PROGRAM_SECTION
                	ENTRY: 1
                function 0:
                 0	pop v3
                 1	pop v4
                 2	cp v5 v4
                 3	cp rr(func return value) v5
                 4	ret
                ENTRY: function 1:
                 5	ld v1 datalabel0
                 6	push v1
                 7	ld v2 datalabel1
                 8	push v2
                 9	ppc
                 10	jumpi monomorphizedfunctionid0
                 11	cp v0 rr(func return value)
                 12	cp rr(func return value) v0
                 13	ret
            "#]],
        );
    }

    #[test]
    fn let_bindings_with_ops() {
        check(
            r#"
                fn add(x in 'int, y in 'int) returns 'int
                    let a = 10;
                        b = 20
                    + a + b + x y
                fn main() returns 'int ~add(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)
                2: Int64(10)
                3: Int64(20)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v14
                 1	pop v15
                 2	cp v17 v15
                 3	cp v18 v14
                 4	add v16 v17 v18
                 5	cp rr(func return value) v16
                 6	ret
                function 1:
                 7	pop v3
                 8	pop v4
                 9	ld v6 datalabel2
                 10	ld v7 datalabel3
                 11	cp v8 v6
                 12	push v8
                 13	cp v10 v7
                 14	push v10
                 15	cp v12 v4
                 16	push v12
                 17	cp v13 v3
                 18	push v13
                 19	ppc
                 20	jumpi monomorphizedfunctionid0
                 21	cp v11 rr(func return value)
                 22	push v11
                 23	ppc
                 24	jumpi monomorphizedfunctionid0
                 25	cp v9 rr(func return value)
                 26	push v9
                 27	ppc
                 28	jumpi monomorphizedfunctionid0
                 29	cp v5 rr(func return value)
                 30	cp rr(func return value) v5
                 31	ret
                ENTRY: function 2:
                 32	ld v1 datalabel0
                 33	push v1
                 34	ld v2 datalabel1
                 35	push v2
                 36	ppc
                 37	jumpi monomorphizedfunctionid1
                 38	cp v0 rr(func return value)
                 39	cp rr(func return value) v0
                 40	ret
            "#]],
        );
    }
    #[test]
    fn let_bindings() {
        check(
            r#"
                fn hi(x in 'int, y in 'int) returns 'int
                    let a = x;
                        b = y;
                        c = 20;
                        d = 30;
                        e = 42;
                    + a + b + c + d e
                fn main() returns 'int ~hi(1, 2)
                "#,
            expect![[r#"
                ; DATA_SECTION
                0: Int64(1)
                1: Int64(2)
                2: Int64(20)
                3: Int64(30)
                4: Int64(42)

                ; PROGRAM_SECTION
                	ENTRY: 2
                function 0:
                 0	pop v19
                 1	pop v20
                 2	cp v22 v20
                 3	cp v23 v19
                 4	add v21 v22 v23
                 5	cp rr(func return value) v21
                 6	ret
                function 1:
                 7	pop v3
                 8	pop v4
                 9	cp v6 v4
                 10	cp v7 v3
                 11	ld v8 datalabel2
                 12	ld v9 datalabel3
                 13	ld v10 datalabel4
                 14	cp v11 v6
                 15	push v11
                 16	cp v13 v7
                 17	push v13
                 18	cp v15 v8
                 19	push v15
                 20	cp v17 v9
                 21	push v17
                 22	cp v18 v10
                 23	push v18
                 24	ppc
                 25	jumpi monomorphizedfunctionid0
                 26	cp v16 rr(func return value)
                 27	push v16
                 28	ppc
                 29	jumpi monomorphizedfunctionid0
                 30	cp v14 rr(func return value)
                 31	push v14
                 32	ppc
                 33	jumpi monomorphizedfunctionid0
                 34	cp v12 rr(func return value)
                 35	push v12
                 36	ppc
                 37	jumpi monomorphizedfunctionid0
                 38	cp v5 rr(func return value)
                 39	cp rr(func return value) v5
                 40	ret
                ENTRY: function 2:
                 41	ld v1 datalabel0
                 42	push v1
                 43	ld v2 datalabel1
                 44	push v2
                 45	ppc
                 46	jumpi monomorphizedfunctionid1
                 47	cp v0 rr(func return value)
                 48	cp rr(func return value) v0
                 49	ret
            "#]],
        );
    }
}
