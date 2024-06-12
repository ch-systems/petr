// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc

use std::{any::Any, collections::BTreeMap, rc::Rc};

use swim_typecheck::{TypeChecker, TypeOrFunctionId, TypeVariable, TypedExpr, TypedFunctionId};
use swim_utils::{idx_map_key, IndexMap};

mod error;
mod opcodes;

use error::*;
use opcodes::*;

// TODO: fully typed functions
pub struct Function {
    body: Vec<IrOpcode>,
}

/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: IndexMap<DataLabel, DataSectionEntry>,
    entry_point:  FunctionLabel,
    functions:    BTreeMap<TypedFunctionId, Function>,
    reg_assigner: usize,
    type_checker: TypeChecker,
}

pub enum DataSectionEntry {
    Int64(i64),
    String(Rc<str>),
    Bool(bool),
}

impl Lowerer {
    pub fn new(
        //resolved_items: QueryableResolvedItems,
        type_checker: TypeChecker,
    ) -> Self {
        Self {
            data_section: IndexMap::default(),
            entry_point: todo!(),
            functions: BTreeMap::default(),
            reg_assigner: 0,
            type_checker,
        }
    }

    fn lower_function(
        &mut self,
        id: TypedFunctionId,
        ty: TypeVariable,
    ) -> Result<(), LoweringError> {
        let mut buf = vec![];
        let mut param_to_reg_mapping = BTreeMap::new();
        let func = self.type_checker.get_function(id).clone();
        // TODO: func should have type checked types...not just the AST type
        for (param_name, param_ty) in &func.params {
            // in order, assign parameters to registers
            if fits_in_reg(param_ty) {
                // load from stack into register
                let param_reg = self.fresh_reg();
                let ty_reg = TypedReg {
                    ty:  self.to_ir_type(param_ty),
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
        self.functions.insert(id, Function { body: buf });
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
        todo!()
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
