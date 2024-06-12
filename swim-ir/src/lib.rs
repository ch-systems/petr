// TODO:
// - reuse data labels
// - figure out actual interface around "return destination" etc

use std::{any::Any, collections::BTreeMap, rc::Rc};

use cranelift_object::object::write;
use swim_bind::FunctionId;
use swim_resolve::QueryableResolvedItems;
use swim_typecheck::{TypeChecker, TypeOrFunctionId, TypeVariable};
use swim_utils::{idx_map_key, IndexMap};

pub use crate::error::LoweringError;

mod error {
    pub struct LoweringError;
}

mod opcodes {
    use swim_utils::idx_map_key;

    idx_map_key!(FunctionLabel);

    idx_map_key!(DataLabel);
    macro_rules! ir_ops {
        ($($op_name:ident $op_code:literal $($args:ident),*);+) => {
            #[derive(Debug)]
            pub enum IrOpcode {
                $(
                    $op_name($($args),*),
                )+
            }
        };
    }

    ir_ops! {
        JumpToFunction "jfunc" FunctionLabel;
        Add "add" Reg, Reg, Reg;
        LoadData "ld" Reg, DataLabel;
        StackPop "pop" TypedReg;
        StackPush "push" TypedReg;
        Intrinsic "intrinsic" Intrinsic
    }
    #[derive(Debug)]
    pub enum Intrinsic {
        // given a pointer, print the thing it points to
        Puts(TypedReg),
    }

    #[derive(Debug)]
    pub struct TypedReg {
        pub ty:  IrTy,
        pub reg: Reg,
    }

    #[derive(Debug)]
    pub enum IrTy {
        Ptr(Box<IrTy>),
        Int64,
    }

    /// a virtual register
    #[derive(Debug, Clone, Copy)]
    pub enum Reg {
        Virtual(usize),
        Reserved(ReservedRegister),
    }

    #[derive(Debug, Clone, Copy)]
    pub enum ReservedRegister {}
}
use opcodes::*;

// TODO: fully typed functions
pub struct Function {
    body: Vec<IrOpcode>,
}

/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: IndexMap<DataLabel, DataSectionEntry>,
    entry_point:  FunctionLabel,
    functions:    BTreeMap<FunctionId, Function>,
    //    resolved_items: QueryableResolvedItems,
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
        todo!("Remove the resolved items, the type checker should be able to provide all necessary information");
        Self {
            data_section: IndexMap::default(),
            entry_point: todo!(),
            functions: BTreeMap::default(),
            // resolved_items,
            reg_assigner: 0,
            type_checker,
        }
    }

    pub fn lower(
        &mut self,
        nodes: BTreeMap<TypeOrFunctionId, TypeVariable>,
    ) -> Result<(), LoweringError> {
        for (id, ty) in nodes {
            match id {
                TypeOrFunctionId::FunctionId(id) => {
                    self.lower_function(id, ty)?;
                },
                TypeOrFunctionId::TypeId(id) => {
                    todo!()
                },
            }
        }
        todo!()
    }

    fn lower_function(
        &mut self,
        id: FunctionId,
        ty: TypeVariable,
    ) -> Result<(), LoweringError> {
        let mut buf = vec![];
        let mut param_to_reg_mapping = BTreeMap::new();
        let func = self.resolved_items.get_function(id).clone();
        let func_ty = self.type_checker.get_type(id);
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
        body: &swim_resolve::Expr,
        param_to_reg_mapping: &mut BTreeMap<&swim_utils::Identifier, Reg>,
        return_destination: ReturnDestination,
    ) -> Result<Vec<IrOpcode>, LoweringError> {
        use swim_resolve::ExprKind::*;
        match body.kind {
            Literal(ref lit) => {
                let ty = literal_to_ir_ty(&lit);
                let data_label = self.insert_literal_data(lit);
                Ok(match return_destination {
                    ReturnDestination::Reg(reg) => vec![IrOpcode::LoadData(reg, data_label)],
                    ReturnDestination::Stack => {
                        let reg = self.fresh_reg();
                        vec![IrOpcode::LoadData(reg, data_label), IrOpcode::StackPush(TypedReg { ty, reg })]
                    },
                })
            },
            List(_) => todo!(),
            FunctionCall(_) => todo!(),
            Variable(_) => todo!(),
            Intrinsic(_) => todo!(),
            Unit => todo!(),
            ErrorRecovery => todo!(),
        }
    }

    fn insert_literal_data(
        &mut self,
        lit: &swim_resolve::Literal,
    ) -> DataLabel {
        use swim_resolve::Literal::*;
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
        param_ty: TypeVariable,
    ) -> IrTy {
        let ty = self.type_checker.get_type(param_ty);
        todo!()
    }
}

enum ReturnDestination {
    Reg(Reg),
    Stack,
}
fn fits_in_reg(param_ty: &swim_resolve::Type) -> bool {
    // TODO
    true
}

fn literal_to_ir_ty(param_ty: swim_resolve::Literal) -> IrTy {
    match param_ty {
        swim_resolve::Type::Integer => IrTy::Int64,
        swim_resolve::Type::Bool => todo!(),
        swim_resolve::Type::Unit => todo!(),
        swim_resolve::Type::String => todo!(),
        swim_resolve::Type::ErrorRecovery => todo!(),
        swim_resolve::Type::Named(_) => todo!(),
    }
}
