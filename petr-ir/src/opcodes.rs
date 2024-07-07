use petr_typecheck::FunctionId;
use petr_utils::idx_map_key;

idx_map_key!(FunctionLabel);

idx_map_key!(DataLabel);

macro_rules! ir_ops {
    ($($(#[$attr:meta])*
        $op_name:ident $op_code:literal $($args:ty: $arg_name:ident),*
     );+) => {

        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum IrOpcode {
            $(
                $(#[$attr])*
                $op_name($($args),*),
            )+
        }


        impl std::fmt::Display for IrOpcode {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        IrOpcode::$op_name($($arg_name),*) => {
                            write!(f, "{}", $op_code)?;
                            $(
                                write!(f, " {}", $arg_name)?;
                            )*
                            Ok(())
                        }
                    )+
                }
            }
        }

        // TODO serialize/deserialize using above display
    };
}

ir_ops! {
    JumpImmediate "jumpi" FunctionId: imm;
    Jump "jump" Reg:  dest;
    Add "add" Reg: dest, Reg: lhs, Reg: rhs;
    Multiply "mult" Reg: dest, Reg: lhs, Reg: rhs;
    Subtract "sub" Reg: dest, Reg: lhs, Reg: rhs;
    Divide "div" Reg: dest, Reg: lhs, Reg: rhs;
    LoadData "ld" Reg: dest, DataLabel: data;
    StackPop "pop" TypedReg: dest;
    StackPush "push" TypedReg: src;
    Intrinsic "intrinsic" Intrinsic: intr;
    FunctionLabel "func" FunctionId: label;
    LoadImmediate "imm" Reg: dest, u64: imm;
    Copy "cp" Reg: dest, Reg: src;
    Label "label" LabelId: label;
    Return "ret";
    ReturnImmediate "reti" u64: imm;
    PushPc "ppc";
    StackPushImmediate "pushi" u64: imm;
    Malloc "malloc" Reg: ptr_dest, Reg: size;
    MallocImmediate "malloci" Reg: ptr_dest, Size<Bytes>: imm;
    /// Register `src` will itself have its value written to the memory pointed to by `dest_ptr`
    WriteRegisterToMemory "sri" Reg: src, Reg: dest_ptr
}

idx_map_key!(LabelId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Intrinsic {
    // given a pointer, print the thing it points to
    Puts(Reg),
}

impl std::fmt::Display for Intrinsic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "@{}",
            match self {
                Intrinsic::Puts(x) => format!("puts({x})"),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedReg {
    pub ty:  IrTy,
    pub reg: Reg,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IrTy {
    Ptr(Box<IrTy>),
    Int64,
    Unit,
    String(Size<Bytes>),
    Boolean,
}

impl IrTy {
    pub fn size(&self) -> Size<Bytes> {
        match self {
            IrTy::Int64 => 8,
            IrTy::Ptr(_) => 8,
            IrTy::Unit => 0,
            IrTy::String(size) => return *size,
            IrTy::Boolean => 1,
        }
        .into()
    }

    pub(crate) fn fits_in_reg(&self) -> bool {
        self.size().num_bytes() <= 8
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Size<T>(T)
where
    T: SizeUnit;

impl<T: SizeUnit> std::fmt::Debug for Size<T> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{:?} bytes", self.0.num_bytes())
    }
}

impl std::fmt::Display for Size<Bytes> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{} bytes", self.0.num_bytes())
    }
}

impl<T: SizeUnit> Size<T> {
    pub fn num_bytes(&self) -> usize {
        self.0.num_bytes()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bytes(usize);

trait SizeUnit: Clone + Copy + PartialEq + Eq + PartialOrd + Ord {
    fn num_bytes(&self) -> usize;
}

impl SizeUnit for Bytes {
    fn num_bytes(&self) -> usize {
        self.0
    }
}

impl From<usize> for Size<Bytes> {
    fn from(x: usize) -> Self {
        Size(Bytes(x))
    }
}

/// a virtual register
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg {
    Virtual(usize),
    Reserved(ReservedRegister),
}

impl std::fmt::Display for Reg {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Reg::Virtual(a) => write!(f, "v{a}"),
            Reg::Reserved(reg) => write!(
                f,
                "rr({})",
                match reg {
                    ReservedRegister::ReturnValueRegister => "func return value",
                }
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReservedRegister {
    /// where functions put their return values
    ReturnValueRegister,
}

impl std::fmt::Display for TypedReg {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        // TODO do we care about typed registers in IR?
        match self.reg {
            Reg::Virtual(a) => write!(f, "v{a}"),
            Reg::Reserved(_) => todo!(),
        }
    }
}
