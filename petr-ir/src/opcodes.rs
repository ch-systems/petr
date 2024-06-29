use petr_typecheck::FunctionId;
use petr_utils::idx_map_key;

idx_map_key!(FunctionLabel);

idx_map_key!(DataLabel);

macro_rules! ir_ops {
    ($($op_name:ident $op_code:literal $($args:ident $arg_name:ident),*);+) => {
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub enum IrOpcode {
            $(
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
    JumpImmediate "jumpi" FunctionId imm;
    Jump "jump" Reg dest;
    Add "add" Reg dest, Reg lhs, Reg rhs;
    Multiply "mult" Reg dest, Reg lhs, Reg rhs;
    Subtract "sub" Reg dest, Reg lhs, Reg rhs;
    Divide "div" Reg dest, Reg lhs, Reg rhs;
    LoadData "ld" Reg dest, DataLabel data;
    StackPop "pop" TypedReg dest;
    StackPush "push" TypedReg src;
    Intrinsic "intrinsic" Intrinsic intr;
    FunctionLabel "func" FunctionId label;
    LoadImmediate "imm" Reg dest, u64 imm;
    Copy "cp" Reg dest, Reg src;
    Label "label" LabelId label;
    Return "ret";
    ReturnImmediate "reti" u64 imm;
    PushPc "ppc";
    StackPushImmediate "pushi" u64 imm;
    Malloc "malloc" Reg ptr_dest, Reg size
}

idx_map_key!(LabelId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Intrinsic {
    // given a pointer, print the thing it points to
    Puts(TypedReg),
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
    String,
    Boolean,
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
