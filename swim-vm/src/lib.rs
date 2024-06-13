//! Basic VM/interpreter for swim-ir. Primarily intended for testing the correctness of codegen and maybe some other features down the line,
//! like a debugger or repl.

// TODO should use fallible index maps since invalid IR can result in labels pointing to things that don't exist. don't want to
// panic in those cases

use std::collections::BTreeMap;

use swim_ir::{DataLabel, DataSectionEntry, Intrinsic, IrOpcode, Reg};
use swim_utils::{idx_map_key, IndexMap};
use thiserror::Error;

pub struct Vm {
    state:        VmState,
    instructions: IndexMap<ProgramOffset, IrOpcode>,
}

idx_map_key!(Register);
idx_map_key!(ProgramOffset);

#[derive(Default)]
pub struct VmState {
    stack:           Vec<Value>,
    static_data:     IndexMap<DataLabel, DataSectionEntry>,
    registers:       BTreeMap<Reg, Value>,
    program_counter: ProgramOffset,
    memory:          Vec<usize>,
}

impl Default for ProgramOffset {
    fn default() -> Self {
        0.into()
    }
}

#[derive(Clone, Copy)]
pub struct Value(usize);

#[derive(Debug, Error)]
pub enum VmError {
    #[error("Function label not found when executing opcode {0}")]
    FunctionLabelNotFound(IrOpcode),
    #[error("Popped empty stack when executing opcode {0}")]
    PoppedEmptyStack(IrOpcode),
    #[error("Register {0} not found")]
    RegisterNotFound(Reg),
    #[error("PC value of {0} is out of bounds for program of length {1}")]
    ProgramCounterOutOfBounds(ProgramOffset, usize),
}

type Result<T> = std::result::Result<T, VmError>;

enum VmControlFlow {
    Continue,
    Terminate,
}

impl Vm {
    pub fn new(
        instructions: Vec<IrOpcode>,
        static_data: IndexMap<DataLabel, DataSectionEntry>,
    ) -> Self {
        let mut idx_map = IndexMap::default();
        for instr in instructions {
            idx_map.insert(instr);
        }
        Self {
            state:        VmState {
                stack: Default::default(),
                static_data,
                registers: Default::default(),
                program_counter: 0.into(),
                memory: Vec::with_capacity(100),
            },
            instructions: idx_map,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        use VmControlFlow::*;
        loop {
            match self.execute() {
                Ok(Continue) => continue,
                Ok(Terminate) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn execute(&mut self) -> Result<VmControlFlow> {
        use VmControlFlow::*;
        if self.state.program_counter.0 >= self.instructions.len() {
            return Err(VmError::ProgramCounterOutOfBounds(self.state.program_counter, self.instructions.len()));
        }
        let opcode = self.instructions.get(self.state.program_counter).clone();
        self.state.program_counter = (self.state.program_counter.0 + 1).into();
        match opcode {
            IrOpcode::JumpImmediate(label) => {
                let Some(offset) = self
                    .instructions
                    .iter()
                    .find_map(|(position, op)| if *op == IrOpcode::FunctionLabel(label) { Some(position) } else { None })
                else {
                    return Err(VmError::FunctionLabelNotFound(opcode));
                };
                self.state.program_counter = offset;
                Ok(Continue)
            },
            IrOpcode::Add(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0 + rhs.0));
                Ok(Continue)
            },
            IrOpcode::LoadData(dest, data_label) => {
                let data = data_section_to_val(self.state.static_data.get(data_label));
                self.set_register(dest, data);
                Ok(Continue)
            },
            IrOpcode::StackPop(ref dest) => {
                let Some(data) = self.state.stack.pop() else {
                    return Err(VmError::PoppedEmptyStack(opcode));
                };
                self.set_register(dest.reg, data);
                Ok(Continue)
            },
            IrOpcode::StackPush(val) => {
                let data = self.get_register(val.reg)?;
                self.state.stack.push(data);
                Ok(Continue)
            },
            IrOpcode::Intrinsic(intrinsic) => {
                match intrinsic {
                    Intrinsic::Puts(reg) => {
                        let ptr = self.get_register(reg.reg)?.0;
                        let len = self.state.memory[ptr];
                        let str = &self.state.memory[ptr + 1..ptr + 1 + len];
                        let str = str.iter().flat_map(|num| num.to_ne_bytes()).collect::<Vec<u8>>();
                        // convert vec of usizes to string
                        let string: String = str.iter().map(|&c| c as u8 as char).collect();
                        println!("{}", string);
                    },
                };
                Ok(Continue)
            },
            IrOpcode::FunctionLabel(_) => Ok(Continue),
            IrOpcode::LoadImmediate(dest, imm) => {
                self.set_register(dest, Value(imm as usize));
                Ok(Continue)
            },
            IrOpcode::Copy(dest, src) => {
                let val = self.get_register(src)?;
                self.set_register(dest, val);
                Ok(Continue)
            },
            IrOpcode::TerminateExecution() => return Ok(Terminate),
            IrOpcode::Jump(_) => todo!(),
            IrOpcode::Label(_) => todo!(),
        }
    }

    fn get_register(
        &self,
        reg: swim_ir::Reg,
    ) -> Result<Value> {
        self.state.registers.get(&reg).copied().ok_or(VmError::RegisterNotFound(reg))
    }

    fn set_register(
        &mut self,
        dest: swim_ir::Reg,
        val: Value,
    ) {
        self.state.registers.insert(dest, val);
    }
}

// TODO things larger than a register
fn data_section_to_val(data: &DataSectionEntry) -> Value {
    match data {
        DataSectionEntry::Int64(x) => Value(*x as usize),
        DataSectionEntry::String(_) => todo!(),
        DataSectionEntry::Bool(x) => Value(if *x { 1 } else { 0 }),
    }
}
