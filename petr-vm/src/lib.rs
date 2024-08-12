//! Basic VM/interpreter for petr-ir. Primarily intended for testing the correctness of codegen and maybe some other features down the line,
//! like a debugger or repl.

// TODO should use fallible index maps since invalid IR can result in labels pointing to things that don't exist. don't want to
// panic in those cases

use std::collections::BTreeMap;

use petr_ir::{DataLabel, DataSectionEntry, Intrinsic, IrOpcode, LabelId, Reg};
use petr_utils::{idx_map_key, IndexMap};
use thiserror::Error;

#[cfg(test)]
mod tests;

pub struct Vm {
    state:        VmState,
    instructions: IndexMap<ProgramOffset, IrOpcode>,
    /// any messages that were logged during execution
    stdout:       Vec<String>,
}

idx_map_key!(Register);
idx_map_key!(ProgramOffset);

#[derive(Default)]
pub struct VmState {
    stack:           Vec<Value>,
    static_data:     IndexMap<DataLabel, DataSectionEntry>,
    registers:       BTreeMap<Reg, Value>,
    program_counter: ProgramOffset,
    memory:          Vec<u64>,
    call_stack:      Vec<ProgramOffset>,
}

impl Default for ProgramOffset {
    fn default() -> Self {
        0.into()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Value(u64);

impl Value {
    pub fn inner(&self) -> u64 {
        self.0
    }

    pub fn is_false(&self) -> bool {
        self.0 == 0
    }

    pub fn is_true(&self) -> bool {
        !self.is_false()
    }
}

#[derive(Debug, Error)]
pub enum VmError {
    #[error("Function label not found when executing opcode {0}")]
    FunctionLabelNotFound(IrOpcode),
    #[error("Popped empty stack when executing opcode {0}")]
    PoppedEmptyStack(IrOpcode),
    #[error("Register {0} not found")]
    RegisterNotFound(Reg),
    #[error("PC value of {0} is out of bounds for program of length {1}")]
    ProgramCounterOutOfBounds(ProgramOffset, u64),
    #[error("Returned to an empty call stack when executing opcode {0}")]
    PoppedEmptyCallStack(IrOpcode),
    #[error("Attempted to write to memory at index {0} but memory only has length {1}")]
    OutOfBoundsMemoryWrite(usize, usize),
    #[error("Label not found when executing opcode {0}")]
    LabelNotFound(IrOpcode),
}

type Result<T> = std::result::Result<T, VmError>;

enum VmControlFlow {
    Continue,
    Terminate(Value),
}

pub type VmLogs = Vec<String>;

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
                call_stack: Default::default(),
            },
            instructions: idx_map,
            stdout:       vec![],
        }
    }

    pub fn run(mut self) -> Result<(Value, Vec<Value>, VmLogs)> {
        use VmControlFlow::*;
        let val = loop {
            match self.execute() {
                Ok(Continue) => continue,
                Ok(Terminate(val)) => break val,
                Err(e) => return Err(e),
            }
        };
        Ok((val, self.state.stack, self.stdout))
    }

    fn execute(&mut self) -> Result<VmControlFlow> {
        println!("PC is {}", self.state.program_counter.0);
        use VmControlFlow::*;
        if self.state.program_counter.0 >= self.instructions.len() {
            return Err(VmError::ProgramCounterOutOfBounds(
                self.state.program_counter,
                self.instructions.len() as u64,
            ));
        }
        let opcode = self.instructions.get(self.state.program_counter).clone();
        self.state.program_counter = (self.state.program_counter.0 + 1).into();
        for (ix, ins) in self.instructions.iter() {
            // println!("{ins}");
        }
        println!("Opcode is: {opcode}");
        match opcode {
            IrOpcode::JumpImmediateFunction(label) => {
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
                println!("Adding {} and {}", lhs, rhs);
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                let result = lhs.0.wrapping_add(rhs.0);
                println!("Result is {}", result);
                self.set_register(dest, Value(lhs.0.wrapping_add(rhs.0)));
                Ok(Continue)
            },
            IrOpcode::Multiply(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0.wrapping_mul(rhs.0)));
                Ok(Continue)
            },
            IrOpcode::Subtract(dest, lhs, rhs) => {
                println!("Subtracting {} and {}", lhs, rhs);
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0.wrapping_sub(rhs.0)));
                println!("Result is {}", lhs.0.wrapping_sub(rhs.0));
                Ok(Continue)
            },
            IrOpcode::Divide(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0 / rhs.0));
                Ok(Continue)
            },
            IrOpcode::LoadData(dest, data_label) => {
                let data = self.state.static_data.get(data_label).clone();
                let data = self.data_section_to_val(&data);
                self.set_register(dest, data);
                Ok(Continue)
            },
            IrOpcode::StackPop(ref dest) => {
                let Some(data) = self.state.stack.pop() else {
                    return Err(VmError::PoppedEmptyStack(opcode));
                };
                self.set_register(*dest, data);
                Ok(Continue)
            },
            IrOpcode::StackPush(val) => {
                let data = self.get_register(val)?;
                self.state.stack.push(data);
                Ok(Continue)
            },
            IrOpcode::Intrinsic(intrinsic) => {
                match intrinsic {
                    Intrinsic::Puts(reg) => {
                        let ptr = self.get_register(reg)?.0;
                        let ptr = ptr as usize;

                        let len = self.state.memory[ptr];
                        let len = len as usize;

                        // strings are padded to the nearest u64 boundary right now and that's working
                        // ...should be fine?
                        let str = &self.state.memory[ptr + 1..ptr + 1 + len];
                        let str = str.iter().flat_map(|num| num.to_ne_bytes()).collect::<Vec<u8>>();
                        // convert vec of usizes to string
                        let string: String = str.iter().map(|&c| c as char).collect();
                        self.stdout.push(string.clone());
                    },
                };
                Ok(Continue)
            },
            IrOpcode::FunctionLabel(_) => Ok(Continue),
            IrOpcode::LoadImmediate(dest, imm) => {
                self.set_register(dest, Value(imm));
                Ok(Continue)
            },
            IrOpcode::Copy(dest, src) => {
                let val = self.get_register(src)?;
                self.set_register(dest, val);
                Ok(Continue)
            },
            IrOpcode::Jump(_) => todo!(),
            IrOpcode::Label(_) => Ok(Continue),
            IrOpcode::Return(reg) => {
                let val = self.get_register(reg)?;
                // pop the fn stack, return there
                let Some(offset) = self.state.call_stack.pop() else {
                    return Ok(Terminate(val));
                };
                println!("Returning to PC: {}", offset.0);
                self.state.program_counter = offset;
                Ok(Continue)
            },
            IrOpcode::PushPc() => {
                println!("Pushing PC: {}", self.state.program_counter.0 + 1);
                self.state.call_stack.push((self.state.program_counter.0 + 1).into());
                Ok(Continue)
            },
            IrOpcode::StackPushImmediate(imm) => {
                self.state.stack.push(Value(imm));
                Ok(Continue)
            },
            IrOpcode::ReturnImmediate(imm) => {
                let Some(offset) = self.state.call_stack.pop() else {
                    return Ok(Terminate(Value(imm)));
                };
                self.state.program_counter = offset;
                Ok(Continue)
            },
            IrOpcode::Malloc(ptr_dest, size) => {
                let size = self.get_register(size)?;
                let ptr = self.state.memory.len();
                self.state.memory.resize(ptr + size.0 as usize, 0);
                self.set_register(ptr_dest, Value(ptr as u64));
                Ok(Continue)
            },
            IrOpcode::MallocImmediate(ptr_dest, size) => {
                let ptr = self.state.memory.len();
                self.state.memory.resize(ptr + size.num_bytes(), 0);
                self.set_register(ptr_dest, Value(ptr as u64));
                Ok(Continue)
            },
            IrOpcode::WriteRegisterToMemory(reg, dest_ptr) => {
                let dest_ptr = self.get_register(dest_ptr)?.0 as usize;
                let val = self.get_register(reg)?.0;

                if self.state.memory.len() <= dest_ptr {
                    return Err(VmError::OutOfBoundsMemoryWrite(dest_ptr, self.state.memory.len()));
                };
                self.state.memory[dest_ptr] = val;
                Ok(Continue)
            },
            IrOpcode::Comment(_) => Ok(Continue),
            IrOpcode::JumpIfFalseImmediate(condition, dest) => {
                let condition = self.get_register(condition)?;
                if condition.is_true() {
                    println!("Not jumping");
                }
                if condition.is_false() {
                    println!("Jumping to label {}", dest);
                    self.jump_to_label(dest)?
                }
                Ok(Continue)
            },
            IrOpcode::JumpImmediate(dest) => {
                self.jump_to_label(dest)?;
                Ok(Continue)
            },
            IrOpcode::Equal(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(if lhs.0 == rhs.0 { 1 } else { 0 }));
                Ok(Continue)
            },
        }
    }

    fn jump_to_label(
        &mut self,
        label: LabelId,
    ) -> Result<()> {
        let Some(offset) = self
            .instructions
            .iter()
            .find_map(|(position, op)| if *op == IrOpcode::Label(label) { Some(position) } else { None })
        else {
            return Err(VmError::LabelNotFound(IrOpcode::Label(label)));
        };
        self.state.program_counter = offset;
        Ok(())
    }

    fn get_register(
        &self,
        reg: petr_ir::Reg,
    ) -> Result<Value> {
        self.state.registers.get(&reg).copied().ok_or(VmError::RegisterNotFound(reg))
    }

    fn set_register(
        &mut self,
        dest: petr_ir::Reg,
        val: Value,
    ) {
        println!("Setting register {} to {}", dest, val.0);
        self.state.registers.insert(dest, val);
    }

    // TODO things larger than a register
    fn data_section_to_val(
        &mut self,
        data: &DataSectionEntry,
    ) -> Value {
        match data {
            DataSectionEntry::Int64(x) => Value(*x as u64),
            DataSectionEntry::String(val) => {
                let str_as_bytes = val.as_bytes();
                let bytes_compressed_as_u64s = str_as_bytes
                    .chunks(8)
                    .map(|chunk| {
                        let mut bytes = [0u8; 8];
                        // pad the chunk with 0s if it isn't a multiple of 8
                        let len = chunk.len();
                        let chunk = if len < 8 {
                            let mut padded = [0u8; 8];
                            padded[..len].copy_from_slice(chunk);
                            padded.to_vec()
                        } else {
                            chunk.to_vec()
                        };
                        bytes.copy_from_slice(&chunk[..]);
                        u64::from_ne_bytes(bytes)
                    })
                    .collect::<Vec<_>>();
                let ptr = self.state.memory.len();
                // first slot of a string is the len, then the content
                self.state.memory.push(bytes_compressed_as_u64s.len() as u64);
                self.state.memory.extend_from_slice(&bytes_compressed_as_u64s);
                Value(ptr as u64)
            },
            DataSectionEntry::Bool(x) => Value(if *x { 1 } else { 0 }),
        }
    }
}
