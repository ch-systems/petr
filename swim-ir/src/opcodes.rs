use swim_utils::idx_map_key;

idx_map_key!(FunctionLabel);

idx_map_key!(DataLabel);

macro_rules! ir_ops {
    ($($op_name:ident $op_code:literal $($args:ident),*);+) => {
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    Intrinsic "intrinsic" Intrinsic;
    FunctionLabel "func_label" FunctionLabel
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Intrinsic {
    // given a pointer, print the thing it points to
    Puts(TypedReg),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedReg {
    pub ty: IrTy,
    pub reg: Reg,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IrTy {
    Ptr(Box<IrTy>),
    Int64,
    Unit,
    Boolean,
}

/// a virtual register
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg {
    Virtual(usize),
    Reserved(ReservedRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReservedRegister {}
