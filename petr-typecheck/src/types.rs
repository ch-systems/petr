use std::collections::BTreeSet;

use petr_resolve::Literal;
use petr_utils::{Identifier, IndexMap, Span};

use crate::TypeVariable;

#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
/// A type which is general, and has no constraints applied to it.
/// This is a generalization of [`SpecificType`].
/// This is more useful for IR generation, since functions are monomorphized
/// based on general types.
pub enum GeneralType {
    Unit,
    Integer,
    Boolean,
    String,
    UserDefined {
        name: Identifier,
        // TODO these should be boxed slices, as their size is not changed
        variants: Box<[GeneralizedTypeVariant]>,
        constant_literal_types: Vec<Literal>,
    },
    Arrow(Vec<TypeVariable>),
    ErrorRecovery,
    List(Box<GeneralType>),
    Infer(usize, Span),
    Sum(BTreeSet<GeneralType>),
}

impl GeneralType {
    /// Because [`GeneralType`]'s type info is less detailed (specific) than [`SpecificType`],
    /// we can losslessly cast any [`GeneralType`] into an instance of [`SpecificType`].
    pub fn safely_upcast(&self) -> SpecificType {
        match self {
            GeneralType::Unit => SpecificType::Unit,
            GeneralType::Integer => SpecificType::Integer,
            GeneralType::Boolean => SpecificType::Boolean,
            GeneralType::String => SpecificType::String,
            GeneralType::ErrorRecovery => SpecificType::ErrorRecovery,
            GeneralType::Infer(u, s) => SpecificType::Infer(*u, *s),
            GeneralType::UserDefined {
                name,
                variants,
                constant_literal_types,
            } => SpecificType::UserDefined {
                name: *name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let fields = variant.fields.iter().map(|field| field.safely_upcast()).collect::<Vec<_>>();

                        TypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                    .collect(),
                constant_literal_types: constant_literal_types.clone(),
            },
            GeneralType::Arrow(tys) => SpecificType::Arrow(tys.clone()),
            GeneralType::List(ty) => SpecificType::List(Box::new(ty.safely_upcast())),
            GeneralType::Sum(tys) => SpecificType::Sum(tys.iter().map(|ty| ty.safely_upcast()).collect()),
        }
    }
}
/// This is an information-rich type -- it tracks effects and data types. It is used for
/// the type-checking stage to provide rich information to the user.
/// Types are generalized into instances of [`GeneralType`] for monomorphization and
/// code generation.
#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
pub enum SpecificType {
    Unit,
    Integer,
    Boolean,
    /// a static length string known at compile time
    String,
    /// A reference to another type
    Ref(TypeVariable),
    /// A user-defined type
    UserDefined {
        name: Identifier,
        // TODO these should be boxed slices, as their size is not changed
        variants: Vec<TypeVariant>,
        constant_literal_types: Vec<Literal>,
    },
    Arrow(Vec<TypeVariable>),
    ErrorRecovery,
    // TODO make this petr type instead of typevariable
    List(Box<SpecificType>),
    /// the usize is just an identifier for use in rendering the type
    /// the span is the location of the inference, for error reporting if the inference is never
    /// resolved
    Infer(usize, Span),
    Sum(BTreeSet<SpecificType>),
    Literal(Literal),
}

#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
pub struct GeneralizedTypeVariant {
    pub fields: Box<[GeneralType]>,
}

impl SpecificType {
    fn generalize_inner(
        &self,
        types: &IndexMap<TypeVariable, SpecificType>,
    ) -> GeneralType {
        match self {
            SpecificType::Unit => GeneralType::Unit,
            SpecificType::Integer => GeneralType::Integer,
            SpecificType::Boolean => GeneralType::Boolean,
            SpecificType::String => GeneralType::String,
            SpecificType::Ref(ty) => types.get(*ty).generalize(types),
            SpecificType::UserDefined {
                name,
                variants,
                constant_literal_types,
            } => GeneralType::UserDefined {
                name: *name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let generalized_fields = variant.fields.iter().map(|field| field.generalize(types)).collect::<Vec<_>>();

                        GeneralizedTypeVariant {
                            fields: generalized_fields.into_boxed_slice(),
                        }
                    })
                    .collect(),
                constant_literal_types: constant_literal_types.clone(),
            },
            SpecificType::Arrow(tys) => GeneralType::Arrow(tys.clone()),
            SpecificType::ErrorRecovery => GeneralType::ErrorRecovery,
            SpecificType::List(ty) => {
                let ty = ty.generalize(types);
                GeneralType::List(Box::new(ty))
            },
            SpecificType::Infer(u, s) => GeneralType::Infer(*u, *s),
            SpecificType::Literal(l) => match l {
                Literal::Integer(_) => GeneralType::Integer,
                Literal::Boolean(_) => GeneralType::Boolean,
                Literal::String(_) => GeneralType::String,
            },
            SpecificType::Sum(tys) => {
                // generalize all types, fold if possible
                let all_generalized: BTreeSet<_> = tys.iter().map(|ty| ty.generalize(types)).collect();
                if all_generalized.len() == 1 {
                    // in this case, all specific types generalized to the same type
                    all_generalized.into_iter().next().expect("invariant")
                } else {
                    GeneralType::Sum(all_generalized.into_iter().collect())
                }
            },
        }
    }

    /// Use this to construct `[SpecificType::Sum]` types --
    /// it will attempt to collapse the sum into a single type if possible
    pub(crate) fn sum(tys: BTreeSet<SpecificType>) -> SpecificType {
        if tys.len() == 1 {
            tys.into_iter().next().expect("invariant")
        } else {
            SpecificType::Sum(tys)
        }
    }
}

#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
pub struct TypeVariant {
    pub fields: Box<[SpecificType]>,
}

pub trait Type {
    fn as_specific_ty(&self) -> SpecificType;

    fn generalize(
        &self,
        types: &IndexMap<TypeVariable, SpecificType>,
    ) -> GeneralType;
}

impl Type for SpecificType {
    fn as_specific_ty(&self) -> SpecificType {
        self.clone()
    }

    fn generalize(
        &self,
        types: &IndexMap<TypeVariable, SpecificType>,
    ) -> GeneralType {
        self.generalize_inner(types)
    }
}

impl Type for GeneralType {
    fn generalize(
        &self,
        _: &IndexMap<TypeVariable, SpecificType>,
    ) -> Self {
        self.clone()
    }

    fn as_specific_ty(&self) -> SpecificType {
        match self {
            GeneralType::Unit => SpecificType::Unit,
            GeneralType::Integer => SpecificType::Integer,
            GeneralType::Boolean => SpecificType::Boolean,
            GeneralType::String => SpecificType::String,
            GeneralType::UserDefined {
                name,
                variants,
                constant_literal_types,
            } => SpecificType::UserDefined {
                name: *name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let fields = variant.fields.iter().map(|field| field.as_specific_ty()).collect::<Vec<_>>();

                        TypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                    .collect(),
                constant_literal_types: constant_literal_types.clone(),
            },
            GeneralType::Arrow(tys) => SpecificType::Arrow(tys.clone()),
            GeneralType::ErrorRecovery => SpecificType::ErrorRecovery,
            GeneralType::List(ty) => SpecificType::List(Box::new(ty.as_specific_ty())),
            GeneralType::Infer(u, s) => SpecificType::Infer(*u, *s),
            GeneralType::Sum(tys) => {
                let tys = tys.iter().map(|ty| ty.as_specific_ty()).collect();
                SpecificType::Sum(tys)
            },
        }
    }
}

