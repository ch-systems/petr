//! Basic VM/interpreter for swim-ir. Primarily intended for testing the correctness of codegen and maybe some other features down the line,
//! like a debugger or repl.

use std::collections::BTreeMap;

use swim_ir::{DataLabel, DataSectionEntry, Intrinsic, IrOpcode, Reg};
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
    memory: Vec<usize>,
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
                memory: Vec::with_capacity(100),
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
                let Some(offset) = self
                    .instructions
                    .iter()
                    .find_map(|(position, op)| if *op == IrOpcode::FunctionLabel(label) { Some(position) } else { None })
                else {
                    return Err(VmError);
                };
                self.state.program_counter = offset;
                Ok(())
            },
            IrOpcode::Add(dest, lhs, rhs) => {
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0 + rhs.0));
                Ok(())
            },
            IrOpcode::LoadData(_dest, _data_label) => todo!(),
            IrOpcode::StackPop(dest) => {
                let Some(data) = self.state.stack.pop() else {
                    return Err(VmError);
                };
                self.set_register(dest.reg, data);
                Ok(())
            },
            IrOpcode::StackPush(val) => {
                let data = self.get_register(val.reg)?;
                self.state.stack.push(data);
                Ok(())
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
                Ok(())
            },
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
