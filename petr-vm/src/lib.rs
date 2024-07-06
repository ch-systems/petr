//! Basic VM/interpreter for petr-ir. Primarily intended for testing the correctness of codegen and maybe some other features down the line,
//! like a debugger or repl.

// TODO should use fallible index maps since invalid IR can result in labels pointing to things that don't exist. don't want to
// panic in those cases

use std::collections::BTreeMap;

use petr_ir::{DataLabel, DataSectionEntry, Intrinsic, IrOpcode, Reg, ReservedRegister};
use petr_utils::{idx_map_key, IndexMap};
use thiserror::Error;

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};
    use petr_ir::Lowerer;
    use petr_resolve::resolve_symbols;
    use petr_typecheck::TypeChecker;
    use petr_utils::render_error;

    use super::*;
    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let mut sources = stdlib::stdlib();
        dbg!(&sources);
        sources.push(("test", &input));
        let parser = petr_parse::Parser::new(sources);
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
        let (data, ir) = lowerer.finalize();
        let vm = Vm::new(ir, data);
        let (res, _stack, logs) = match vm.run() {
            Ok(o) => o,
            Err(err) => panic!("vm returned error: {err:?}"),
        };

        let mut res = format!("{res:?}");

        if !logs.is_empty() {
            res.push_str("\n___LOGS___\n");

            res.push_str(&logs.join("\n"));
        }

        expect.assert_eq(&res);
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
        e = 12,
    a
function main() returns 'int ~hi(42, 3)
"#,
            expect!["Value(42)"],
        )
    }
    #[test]
    fn import_call() {
        check(
            r#"
import std.io.print

function main() returns 'unit 
  ~print("hello, world!")
  "#,
            expect!["Value(0)"],
        )
    }

    #[test]
    fn addition() {
        check(
            r#"
            function hi(x in 'int, y in 'int) returns 'int
    let a = x,
        b = y,
        c = 20,
        d = 30,
        e = 42,
    + a + b + c + d e

function main() returns 'int ~hi(1, 3)
"#,
            expect!("Value(96)"),
        )
    }

    #[test]
    fn addition_path_res() {
        check(
            r#"
            function hi(x in 'int, y in 'int) returns 'int
    let a = x,
        b = y,
        c = 20,
        d = 30,
        e = 42,
    ~std.ops.add(a,  + b + c + d e)

function main() returns 'int ~hi(1, 3)
"#,
            expect!("Value(96)"),
        )
    }

    #[test]
    fn subtraction() {
        check(
            r#"
            function hi(x in 'int) returns 'int
    let a = + x 1,
        b = - x 1,
        c = - 20 x,
        d = + 20 x
        d

function main() returns 'int ~hi(100)
"#,
            expect!("Value(120)"),
        )
    }

    #[test]
    fn overflowing_sub() {
        check(
            r#"
function main() returns 'int - 0 1
"#,
            expect!("Value(18446744073709551615)"),
        )
    }

    #[test]
    fn basic_malloc() {
        check(
            r#"
function main() returns 'int
    let a = @malloc 1
    let b = @malloc 1
    let c = @malloc 5
    let d = @malloc 1
    d
"#,
            expect!("Value(7)"),
        )
    }
}

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
        use VmControlFlow::*;
        if self.state.program_counter.0 >= self.instructions.len() {
            return Err(VmError::ProgramCounterOutOfBounds(
                self.state.program_counter,
                self.instructions.len() as u64,
            ));
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
                let lhs = self.get_register(lhs)?;
                let rhs = self.get_register(rhs)?;
                self.set_register(dest, Value(lhs.0.wrapping_sub(rhs.0)));
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
            IrOpcode::Label(_) => todo!(),
            IrOpcode::Return() => {
                let val = self.get_register(Reg::Reserved(ReservedRegister::ReturnValueRegister))?;
                // pop the fn stack, return there
                let Some(offset) = self.state.call_stack.pop() else {
                    return Ok(Terminate(val));
                };
                self.state.program_counter = offset;
                Ok(Continue)
            },
            IrOpcode::PushPc() => {
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
        }
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
