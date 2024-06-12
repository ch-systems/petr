//! Basic VM/interpreter for swim-ir. Primarily intended for testing the correctness of codegen and maybe some other features down the line,
//! like a debugger or repl.

use std::collections::BTreeMap;

use swim_ir::{DataLabel, DataSectionEntry, IrOpcode, Reg};
use swim_utils::{idx_map_key, IndexMap};

fn main() {
    todo!()
}

struct Vm {
    state: VmState,
    instructions: IndexMap<ProgramOffset, IrOpcode>,
}

idx_map_key!(Register);
idx_map_key!(ProgramOffset);

#[derive(Default)]
pub struct VmState {
    stack: Vec<Value>,
    static_data: IndexMap<DataLabel, DataSectionEntry>,
    registers: BTreeMap<Reg, Value>,
    program_counter: ProgramOffset,
}

impl Default for ProgramOffset {
    fn default() -> Self {
        0.into()
    }
}

#[derive(Clone, Copy)]
pub struct Value(usize);

pub struct VmError;

type Result<T> = std::result::Result<T, VmError>;

impl Vm {
    pub fn new(
        instructions: IndexMap<ProgramOffset, IrOpcode>,
        static_data: IndexMap<DataLabel, DataSectionEntry>,
    ) -> Self {
        Self {
            state: VmState {
                stack: Default::default(),
                static_data,
                registers: Default::default(),
                program_counter: 0.into(),
            },
            instructions,
        }
    }

    pub fn run(
        &mut self,
        ir: Vec<IrOpcode>,
    ) -> Result<()> {
        for opcode in ir {
            self.execute(opcode)?;
        }
        Ok(())
    }

    pub fn execute(
        &mut self,
        opcode: IrOpcode,
    ) -> Result<()> {
        match opcode {
            IrOpcode::JumpToFunction(label) => {
                let Some((place_to_jump_to, _)) = self.instructions.iter().find(|(position, op)| **op == IrOpcode::FunctionLabel(label)) else {
                    return Err(VmError);
                };
                self.state.program_counter = place_to_jump_to;
                Ok(())
            },
            IrOpcode::Add(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0 + rhs.0));
                Ok(())
            },
            IrOpcode::LoadData(_, _) => todo!(),
            IrOpcode::StackPop(_) => todo!(),
            IrOpcode::StackPush(_) => todo!(),
            IrOpcode::Intrinsic(_) => todo!(),
            IrOpcode::FunctionLabel(_) => Ok(()),
        }
    }

    fn get_register(
        &self,
        reg: swim_ir::Reg,
    ) -> Result<Value> {
        self.state.registers.get(&reg).copied().ok_or(VmError)
    }

    fn set_register(
        &mut self,
        dest: swim_ir::Reg,
        val: Value,
    ) {
        self.state.registers.insert(dest, val);
    }
}
