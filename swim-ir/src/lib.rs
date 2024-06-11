use std::{collections::BTreeMap, rc::Rc};

use cranelift_object::object::write;
use swim_resolve::QueryableResolvedItems;
use swim_typecheck::{TypeOrFunctionId, TypeVariable};
use swim_utils::{idx_map_key, IndexMap};

pub use crate::error::LoweringError;

mod error {
    pub struct LoweringError;
}

idx_map_key!(FunctionLabel);

idx_map_key!(DataSectionLabel);

// TODO: fully typed functions
pub struct Function;

/// Lowers typed nodes into an IR suitable for code generation.
pub struct Lowerer {
    data_section: IndexMap<DataSectionLabel, DataSectionEntry>,
    entry_point:  FunctionLabel,
    functions:    IndexMap<FunctionLabel, Function>,
}

pub enum DataSectionEntry {
    Int64(i64),
    String(Rc<str>),
    Bool(bool),
}

impl Lowerer {
    pub fn new() -> Self {
        Self { data_section: IndexMap::default(),
               entry_point:  todo!(),
               functions:    IndexMap::default(), }
    }

    pub fn lower(&mut self,
                 items: QueryableResolvedItems,
                 nodes: BTreeMap<TypeOrFunctionId, TypeVariable>)
                 -> Result<(), LoweringError> {
        for (id, item) in nodes {
            match id {
                TypeOrFunctionId::FunctionId(id) => {
                    let func = items.get_function(id);
                    self.lower_function(func, item);
                },
                TypeOrFunctionId::TypeId(id) => {
                    todo!()
                },
            }
        }
        todo!()
    }

    fn lower_function(&self,
                      func: &swim_resolve::Function,
                      ty: TypeVariable)
                      -> Result<(), LoweringError> {
        todo!()
    }
}
