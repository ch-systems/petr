use std::{collections::BTreeMap, rc::Rc};

use cranelift_object::object::write;
use swim_bind::FunctionId;
use swim_resolve::QueryableResolvedItems;
use swim_typecheck::{TypeOrFunctionId, TypeVariable};
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
            pub enum IrOpcode {
                $(
                    $op_name($($args),*),
                )+
            }

            // impl std::fmt::Display for IrOpcode {
            //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            //         match self {
            //             $(
            //                 IrOpcode::$op_name($($args),*) => {
            //                     write!(f, "{}", $op_code)?;
            //                     $(
            //                         write!(f, " {}", $args)?;
            //                     )*
            //                     Ok(())
            //                 }
            //             )+
            //         }
            //     }
            // }
        };
    }

    ir_ops! {
        JumpToFunction "jfunc" FunctionLabel;
        Add "add" Reg, Reg, Reg;
        LoadData "ld" Reg, DataLabel
    }
    /// a virtual register
    pub struct Reg(usize);
}
use opcodes::*;

// TODO: fully typed functions
pub struct Function;

/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: IndexMap<DataLabel, DataSectionEntry>,
    entry_point: FunctionLabel,
    functions: IndexMap<FunctionLabel, Function>,
    resolved_items: QueryableResolvedItems,
}

pub enum DataSectionEntry {
    Int64(i64),
    String(Rc<str>),
    Bool(bool),
}

impl Lowerer {
    pub fn new(resolved_items: QueryableResolvedItems) -> Self {
        Self {
            data_section: IndexMap::default(),
            entry_point: todo!(),
            functions: IndexMap::default(),
            resolved_items,
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
        &self,
        id: FunctionId,
        ty: TypeVariable,
    ) -> Result<(), LoweringError> {
        todo!()
    }
}
