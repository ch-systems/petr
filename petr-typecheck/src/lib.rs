//! TODO:
//! - Effectual Types
//! - Formalize constraints:
//!     - Unify
//!     - Satisfies
//!     - UnifyEffects
//!     - SatisfiesEffects

mod error;

use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use error::TypeConstraintError;
pub use petr_bind::FunctionId;
use petr_resolve::{Expr, ExprKind, QueryableResolvedItems};
pub use petr_resolve::{Intrinsic as ResolvedIntrinsic, IntrinsicName, Literal};
use petr_utils::{idx_map_key, Identifier, IndexMap, Span, SpannedItem, SymbolId, TypeId};

pub type TypeError = SpannedItem<TypeConstraintError>;
pub type TResult<T> = Result<T, TypeError>;

// TODO return QueryableTypeChecked instead of type checker
// Clean up API so this is the only function exposed
pub fn type_check(resolved: QueryableResolvedItems) -> (Vec<TypeError>, TypeChecker) {
    let mut type_checker = TypeChecker::new(resolved);
    type_checker.fully_type_check();
    (type_checker.errors.clone(), type_checker)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOrFunctionId {
    TypeId(TypeId),
    FunctionId(FunctionId),
}

impl From<TypeId> for TypeOrFunctionId {
    fn from(type_id: TypeId) -> Self {
        TypeOrFunctionId::TypeId(type_id)
    }
}

impl From<FunctionId> for TypeOrFunctionId {
    fn from(function_id: FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(function_id)
    }
}

impl From<&TypeId> for TypeOrFunctionId {
    fn from(type_id: &TypeId) -> Self {
        TypeOrFunctionId::TypeId(*type_id)
    }
}

impl From<&FunctionId> for TypeOrFunctionId {
    fn from(function_id: &FunctionId) -> Self {
        TypeOrFunctionId::FunctionId(*function_id)
    }
}

idx_map_key!(TypeVariable);

#[derive(Clone, Copy, Debug)]
pub struct TypeConstraint {
    kind: TypeConstraintKind,
    /// The span from which this type constraint originated
    span: Span,
}
impl TypeConstraint {
    fn unify(
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) -> Self {
        Self {
            kind: TypeConstraintKind::Unify(t1, t2),
            span,
        }
    }

    fn satisfies(
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) -> Self {
        Self {
            kind: TypeConstraintKind::Satisfies(t1, t2),
            span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TypeConstraintKind {
    Unify(TypeVariable, TypeVariable),
    // constraint that lhs is a "subtype" or satisfies the typeclass constraints of "rhs"
    Satisfies(TypeVariable, TypeVariable),
}

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
enum TypeConstraintKindValue {
    Unify,
    Satisfies,
}

pub struct TypeContext {
    types:          IndexMap<TypeVariable, SpecificType>,
    constraints:    Vec<TypeConstraint>,
    // known primitive type IDs
    unit_ty:        TypeVariable,
    string_ty:      TypeVariable,
    int_ty:         TypeVariable,
    bool_ty:        TypeVariable,
    error_recovery: TypeVariable,
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut types = IndexMap::default();
        // instantiate basic primitive types
        let unit_ty = types.insert(SpecificType::Unit);
        let string_ty = types.insert(SpecificType::String);
        let bool_ty = types.insert(SpecificType::Boolean);
        let int_ty = types.insert(SpecificType::Integer);
        let error_recovery = types.insert(SpecificType::ErrorRecovery);
        // insert primitive types
        TypeContext {
            types,
            constraints: Default::default(),
            bool_ty,
            unit_ty,
            string_ty,
            int_ty,
            error_recovery,
        }
    }
}

impl TypeContext {
    fn unify(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.constraints.push(TypeConstraint::unify(ty1, ty2, span));
    }

    fn satisfies(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.constraints.push(TypeConstraint::satisfies(ty1, ty2, span));
    }

    fn new_variable(
        &mut self,
        span: Span,
    ) -> TypeVariable {
        // infer is special -- it knows its own id, mostly for printing
        // and disambiguating
        let infer_id = self.types.len();
        self.types.insert(SpecificType::Infer(infer_id, span))
    }

    /// Update a type variable with a new PetrType
    fn update_type(
        &mut self,
        t1: TypeVariable,
        known: SpecificType,
    ) {
        *self.types.get_mut(t1) = known;
    }
}

pub type FunctionSignature = (FunctionId, Box<[GeneralType]>);

pub struct TypeChecker {
    ctx: TypeContext,
    type_map: BTreeMap<TypeOrFunctionId, TypeVariable>,
    monomorphized_functions: BTreeMap<FunctionSignature, Function>,
    typed_functions: BTreeMap<FunctionId, Function>,
    errors: Vec<TypeError>,
    resolved: QueryableResolvedItems,
    variable_scope: Vec<BTreeMap<Identifier, TypeVariable>>,
}

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
            _ => todo!(),
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
    fn generalize(
        &self,
        ctx: &TypeContext,
    ) -> GeneralType {
        match self {
            SpecificType::Unit => GeneralType::Unit,
            SpecificType::Integer => GeneralType::Integer,
            SpecificType::Boolean => GeneralType::Boolean,
            SpecificType::String => GeneralType::String,
            SpecificType::Ref(ty) => ctx.types.get(*ty).generalize(ctx),
            SpecificType::UserDefined {
                name,
                variants,
                constant_literal_types,
            } => GeneralType::UserDefined {
                name: *name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let generalized_fields = variant.fields.iter().map(|field| field.generalize(ctx)).collect::<Vec<_>>();

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
                let ty = ty.generalize(ctx);
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
                let all_generalized: BTreeSet<_> = tys.iter().map(|ty| ty.generalize(ctx)).collect();
                if all_generalized.len() == 1 {
                    // in this case, all specific types generalized to the same type
                    all_generalized.into_iter().next().expect("invariant")
                } else {
                    GeneralType::Sum(all_generalized.into_iter().collect())
                }
            },
        }
    }

    /// If `self` is a generalized form of `b`, return true
    /// A generalized form is a type that is a superset of the sum types.
    /// For example, `String` is a generalized form of `Sum(Literal("a") | Literal("B"))`
    fn is_superset_of(
        &self,
        b: &SpecificType,
        ctx: &TypeContext,
    ) -> bool {
        use SpecificType::*;
        let generalized_b = b.generalize(ctx).safely_upcast();
        match (self, b) {
            // If `a` is the generalized form of `b`, then `b` satisfies the constraint.
            (a, b) if a == b || *a == generalized_b => true,
            // If `a` is a sum type which contains `b` OR the generalized form of `b`, then `b`
            // satisfies the constraint.
            (Sum(a_tys), b) if a_tys.contains(b) || a_tys.contains(&generalized_b) => true,
            // if both `a` and `b` are sum types, then `a` must be a superset of `b`:
            // - every element in `b` must either:
            //      - be a member of `a`
            //      - generalize to a member of `a`
            (Sum(a_tys), Sum(b_tys)) => {
                // if a_tys is a superset of b_tys,
                // every element OR its generalized version is contained in a_tys
                for b_ty in b_tys {
                    let b_ty_generalized = b_ty.generalize(ctx).safely_upcast();
                    if !(a_tys.contains(b_ty) || a_tys.contains(&b_ty_generalized)) {
                        return false;
                    }
                }

                true
            },
            _otherwise => false,
        }
    }

    /// Use this to construct `[SpecificType::Sum]` types --
    /// it will attempt to collapse the sum into a single type if possible
    fn sum(tys: BTreeSet<SpecificType>) -> SpecificType {
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
    fn as_specific_ty(
        &self,
        ctx: &TypeContext,
    ) -> SpecificType;

    fn generalize(
        &self,
        ctx: &TypeContext,
    ) -> GeneralType {
        self.as_specific_ty(ctx).generalize(ctx)
    }
}

impl Type for SpecificType {
    fn as_specific_ty(
        &self,
        _ctx: &TypeContext,
    ) -> SpecificType {
        self.clone()
    }
}

impl Type for GeneralType {
    fn as_specific_ty(
        &self,
        _ctx: &TypeContext,
    ) -> SpecificType {
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
                        let fields = variant.fields.iter().map(|field| field.as_specific_ty(_ctx)).collect::<Vec<_>>();

                        TypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                    .collect(),
                constant_literal_types: constant_literal_types.clone(),
            },
            GeneralType::Arrow(tys) => SpecificType::Arrow(tys.clone()),
            GeneralType::ErrorRecovery => SpecificType::ErrorRecovery,
            GeneralType::List(ty) => SpecificType::List(Box::new(ty.as_specific_ty(_ctx))),
            GeneralType::Infer(u, s) => SpecificType::Infer(*u, *s),
            GeneralType::Sum(tys) => {
                let tys = tys.iter().map(|ty| ty.as_specific_ty(_ctx)).collect();
                SpecificType::Sum(tys)
            },
        }
    }
}

impl TypeChecker {
    pub fn insert_type<T: Type>(
        &mut self,
        ty: &T,
    ) -> TypeVariable {
        let ty = ty.as_specific_ty(&self.ctx);
        // TODO: check if type already exists and return that ID instead
        self.ctx.types.insert(ty)
    }

    pub fn look_up_variable(
        &self,
        ty: TypeVariable,
    ) -> &SpecificType {
        self.ctx.types.get(ty)
    }

    pub fn get_symbol(
        &self,
        id: SymbolId,
    ) -> Rc<str> {
        self.resolved.interner.get(id).clone()
    }

    fn with_type_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.variable_scope.push(Default::default());
        let res = f(self);
        self.variable_scope.pop();
        res
    }

    fn generic_type(
        &mut self,
        id: &Identifier,
    ) -> TypeVariable {
        for scope in self.variable_scope.iter().rev() {
            if let Some(ty) = scope.get(id) {
                return *ty;
            }
        }
        let fresh_ty = self.fresh_ty_var(id.span);
        match self.variable_scope.last_mut() {
            Some(entry) => {
                entry.insert(*id, fresh_ty);
            },
            None => {
                self.errors.push(id.span.with_item(TypeConstraintError::Internal(
                    "attempted to insert generic type into variable scope when no variable scope existed".into(),
                )));
                self.ctx.update_type(fresh_ty, SpecificType::ErrorRecovery);
            },
        };
        fresh_ty
    }

    fn find_variable(
        &self,
        id: Identifier,
    ) -> Option<TypeVariable> {
        for scope in self.variable_scope.iter().rev() {
            if let Some(ty) = scope.get(&id) {
                return Some(*ty);
            }
        }
        None
    }

    fn fully_type_check(&mut self) {
        for (id, decl) in self.resolved.types() {
            let ty = self.fresh_ty_var(decl.name.span);
            let variants = decl
                .variants
                .iter()
                .map(|variant| {
                    self.with_type_scope(|ctx| {
                        let fields = variant.fields.iter().map(|field| ctx.to_petr_type(&field.ty)).collect::<Vec<_>>();
                        TypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                })
                .collect::<Vec<_>>();
            self.ctx.update_type(
                ty,
                SpecificType::UserDefined {
                    name: decl.name,
                    variants,
                    constant_literal_types: decl.constant_literal_types,
                },
            );
            self.type_map.insert(id.into(), ty);
        }

        for (id, func) in self.resolved.functions() {
            let typed_function = func.type_check(self);

            let ty = self.arrow_type([typed_function.params.iter().map(|(_, b)| *b).collect(), vec![typed_function.return_ty]].concat());
            self.type_map.insert(id.into(), ty);
            self.typed_functions.insert(id, typed_function);
        }
        // type check the main func with no params
        let main_func = self.get_main_function();
        // construct a function call for the main function, if one exists
        if let Some((id, func)) = main_func {
            let call = petr_resolve::FunctionCall {
                function: id,
                args:     vec![],
                span:     func.name.span,
            };
            call.type_check(self);
        }

        // before applying existing constraints, it is likely that many duplicate constraints
        // exist. We can safely remove any duplicate constraints to avoid excessive error
        // reporting.
        self.deduplicate_constraints();

        // we have now collected our constraints and can solve for them
        self.apply_constraints();
    }

    pub fn get_main_function(&self) -> Option<(FunctionId, Function)> {
        self.functions().find(|(_, func)| &*self.get_symbol(func.name.id) == "main")
    }

    /// iterate through each constraint and transform the underlying types to satisfy them
    /// - unification tries to collapse two types into one
    /// - satisfaction tries to make one type satisfy the constraints of another, although type
    ///   constraints don't exist in the language yet
    fn apply_constraints(&mut self) {
        let constraints = self.ctx.constraints.clone();
        for constraint in constraints {
            match &constraint.kind {
                TypeConstraintKind::Unify(t1, t2) => {
                    self.apply_unify_constraint(*t1, *t2, constraint.span);
                },
                TypeConstraintKind::Satisfies(t1, t2) => {
                    self.apply_satisfies_constraint(*t1, *t2, constraint.span);
                },
            }
        }
    }

    /// Attempt to unify two types, returning an error if they cannot be unified
    /// The more specific of the two types will instantiate the more general of the two types.
    ///
    /// TODO: The unify constraint should attempt to upcast `t2` as `t1`  if possible, but will never
    /// downcast `t1` as `t2`. This is not currently how it works and needs investigation.
    fn apply_unify_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) {
        let ty1 = self.ctx.types.get(t1).clone();
        let ty2 = self.ctx.types.get(t2).clone();
        use SpecificType::*;
        match (ty1, ty2) {
            (a, b) if a == b => (),
            (ErrorRecovery, _) | (_, ErrorRecovery) => (),
            (Ref(a), _) => self.apply_unify_constraint(a, t2, span),
            (_, Ref(b)) => self.apply_unify_constraint(t1, b, span),
            (Infer(id, _), Infer(id2, _)) if id != id2 => {
                // if two different inferred types are unified, replace the second with a reference
                // to the first
                self.ctx.update_type(t2, Ref(t1));
            },
            (a @ Sum(_), b @ Sum(_)) => {
                // the unification of two sum types is the union of the two types if and only if
                // `t2` is a total subset of `t1`
                // `t1` remains unchanged, as we are trying to coerce `t2` into something that
                // represents `t1`
                // TODO remove clone
                if a.is_superset_of(&b, &self.ctx) {
                } else {
                    self.push_error(span.with_item(self.unify_err(a, b)));
                }
            },
            // If `t2` is a non-sum type, and `t1` is a sum type, then `t1` must contain either
            // exactly the same specific type OR the generalization of that type
            // If the latter, then the specific type must be updated to its generalization
            (Sum(sum_tys), other) => {
                if sum_tys.contains(&other) {
                    self.ctx.update_type(t2, Ref(t1));
                } else {
                    let generalization = other.generalize(&self.ctx).safely_upcast();
                    if sum_tys.contains(&generalization) {
                        self.ctx.update_type(t2, generalization);
                    } else {
                        self.push_error(span.with_item(self.unify_err(Sum(sum_tys.clone()), generalization)));
                    }
                }
            },
            // literals can unify to each other if they're equal
            (Literal(l1), Literal(l2)) if l1 == l2 => (),
            (Literal(l1), Literal(l2)) if l1 != l2 => {
                // update t1 to a sum type of both,
                // and update t2 to reference t1
                let sum = Sum([Literal(l1), Literal(l2)].into());
                self.ctx.update_type(t1, sum);
                self.ctx.update_type(t2, Ref(t1));
            },
            (Literal(l1), Sum(tys)) => {
                // update t1 to a sum type of both,
                // and update t2 to reference t1
                let sum = Sum([Literal(l1)].iter().chain(tys.iter()).cloned().collect());
                self.ctx.update_type(t1, sum);
                self.ctx.update_type(t2, Ref(t1));
            },
            // literals can unify broader parent types
            // but the broader parent type gets instantiated with the literal type
            (ty, Literal(lit)) => match (&lit, ty) {
                (petr_resolve::Literal::Integer(_), Integer)
                | (petr_resolve::Literal::Boolean(_), Boolean)
                | (petr_resolve::Literal::String(_), String) => self.ctx.update_type(t1, SpecificType::Literal(lit)),
                (lit, ty) => self.push_error(span.with_item(self.unify_err(ty.clone(), SpecificType::Literal(lit.clone())))),
            },
            // literals can unify broader parent types
            // but the broader parent type gets instantiated with the literal type
            (Literal(lit), ty) => match (&lit, ty) {
                (petr_resolve::Literal::Integer(_), Integer)
                | (petr_resolve::Literal::Boolean(_), Boolean)
                | (petr_resolve::Literal::String(_), String) => self.ctx.update_type(t2, SpecificType::Literal(lit)),
                (lit, ty) => {
                    self.push_error(span.with_item(self.unify_err(ty.clone(), SpecificType::Literal(lit.clone()))));
                },
            },
            (other, Sum(sum_tys)) => {
                // `other` must be a member of the Sum type
                if !sum_tys.contains(&other) {
                    self.push_error(span.with_item(self.unify_err(other.clone(), SpecificType::sum(sum_tys.clone()))));
                }
                // unify both types to the other type
                self.ctx.update_type(t2, other);
            },
            // instantiate the infer type with the known type
            (Infer(_, _), known) => {
                self.ctx.update_type(t1, known);
            },
            (known, Infer(_, _)) => {
                self.ctx.update_type(t2, known);
            },
            // lastly, if no unification rule exists for these two types, it is a mismatch
            (a, b) => {
                self.push_error(span.with_item(self.unify_err(a, b)));
            },
        }
    }

    // This function will need to be rewritten when type constraints and bounded polymorphism are
    // implemented.
    fn apply_satisfies_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) {
        let ty1 = self.ctx.types.get(t1);
        let ty2 = self.ctx.types.get(t2);
        use SpecificType::*;
        match (ty1, ty2) {
            (a, b) if a == b => (),
            (ErrorRecovery, _) | (_, ErrorRecovery) => (),
            (Ref(a), _) => self.apply_satisfies_constraint(*a, t2, span),
            (_, Ref(b)) => self.apply_satisfies_constraint(t1, *b, span),
            // if t1 is a fully instantiated type, then t2 can be updated to be a reference to t1
            (Unit | Integer | Boolean | UserDefined { .. } | String | Arrow(..) | List(..) | Literal(_) | Sum(_), Infer(_, _)) => {
                self.ctx.update_type(t2, Ref(t1));
            },
            // the "parent" infer type will not instantiate to the "child" type
            (Infer(_, _), Unit | Integer | Boolean | UserDefined { .. } | String | Arrow(..) | List(..) | Literal(_) | Sum(_)) => (),
            (Sum(a_tys), Sum(b_tys)) => {
                // calculate the intersection of these types, update t2 to the intersection
                let intersection = a_tys.iter().filter(|a_ty| b_tys.contains(a_ty)).cloned().collect();
                self.ctx.update_type(t2, SpecificType::sum(intersection));
            },
            // if `ty1` is a generalized version of the sum type,
            // then it satisfies the sum type
            (ty1, other) if ty1.is_superset_of(other, &self.ctx) => (),
            (Literal(l1), Literal(l2)) if l1 == l2 => (),
            // Literals can satisfy broader parent types
            (ty, Literal(lit)) => match (lit, ty) {
                (petr_resolve::Literal::Integer(_), Integer) => (),
                (petr_resolve::Literal::Boolean(_), Boolean) => (),
                (petr_resolve::Literal::String(_), String) => (),
                (lit, ty) => {
                    self.push_error(span.with_item(self.satisfy_err(ty.clone(), SpecificType::Literal(lit.clone()))));
                },
            },
            // if we are trying to satisfy an inferred type with no bounds, this is ok
            (Infer(..), _) => (),
            (a, b) => {
                self.push_error(span.with_item(self.satisfy_err(a.clone(), b.clone())));
            },
        }
    }

    pub fn new(resolved: QueryableResolvedItems) -> Self {
        let ctx = TypeContext::default();
        let mut type_checker = TypeChecker {
            ctx,
            type_map: Default::default(),
            errors: Default::default(),
            typed_functions: Default::default(),
            resolved,
            variable_scope: Default::default(),
            monomorphized_functions: Default::default(),
        };

        type_checker.fully_type_check();
        type_checker
    }

    pub fn insert_variable(
        &mut self,
        id: Identifier,
        ty: TypeVariable,
    ) {
        self.variable_scope
            .last_mut()
            .expect("inserted variable when no scope existed")
            .insert(id, ty);
    }

    pub fn fresh_ty_var(
        &mut self,
        span: Span,
    ) -> TypeVariable {
        self.ctx.new_variable(span)
    }

    fn arrow_type(
        &mut self,
        tys: Vec<TypeVariable>,
    ) -> TypeVariable {
        assert!(!tys.is_empty(), "arrow_type: tys is empty");

        if tys.len() == 1 {
            return tys[0];
        }

        let ty = SpecificType::Arrow(tys);
        self.ctx.types.insert(ty)
    }

    pub fn to_petr_type(
        &mut self,
        ty: &petr_resolve::Type,
    ) -> SpecificType {
        match ty {
            petr_resolve::Type::Integer => SpecificType::Integer,
            petr_resolve::Type::Bool => SpecificType::Boolean,
            petr_resolve::Type::Unit => SpecificType::Unit,
            petr_resolve::Type::String => SpecificType::String,
            petr_resolve::Type::ErrorRecovery(_) => {
                // unifies to anything, fresh var
                SpecificType::ErrorRecovery
            },
            petr_resolve::Type::Named(ty_id) => SpecificType::Ref(*self.type_map.get(&ty_id.into()).expect("type did not exist in type map")),
            petr_resolve::Type::Generic(generic_name) => {
                // TODO don't create an ID and then reference it -- this is messy
                let id = self.generic_type(generic_name);
                SpecificType::Ref(id)
            },
            petr_resolve::Type::Sum(tys) => SpecificType::Sum(tys.iter().map(|ty| self.to_petr_type(ty)).collect()),
            petr_resolve::Type::Literal(l) => SpecificType::Literal(l.clone()),
        }
    }

    pub fn to_type_var(
        &mut self,
        ty: &petr_resolve::Type,
    ) -> TypeVariable {
        let petr_ty = self.to_petr_type(ty);
        self.ctx.types.insert(petr_ty)
    }

    pub fn get_type(
        &self,
        key: impl Into<TypeOrFunctionId>,
    ) -> &TypeVariable {
        self.type_map.get(&key.into()).expect("type did not exist in type map")
    }

    fn convert_literal_to_type(
        &mut self,
        literal: &petr_resolve::Literal,
    ) -> TypeVariable {
        let ty = SpecificType::Literal(literal.clone());
        self.ctx.types.insert(ty)
    }

    fn push_error(
        &mut self,
        e: TypeError,
    ) {
        self.errors.push(e);
    }

    pub fn unify(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.ctx.unify(ty1, ty2, span);
    }

    pub fn satisfies(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.ctx.satisfies(ty1, ty2, span);
    }

    fn get_untyped_function(
        &self,
        function: FunctionId,
    ) -> &petr_resolve::Function {
        self.resolved.get_function(function)
    }

    pub fn get_function(
        &mut self,
        id: &FunctionId,
    ) -> Function {
        if let Some(func) = self.typed_functions.get(id) {
            return func.clone();
        }

        // if the function hasn't been type checked yet, type check it
        let func = self.get_untyped_function(*id).clone();
        let type_checked = func.type_check(self);
        self.typed_functions.insert(*id, type_checked.clone());
        type_checked
    }

    pub fn get_monomorphized_function(
        &self,
        id: &FunctionSignature,
    ) -> &Function {
        self.monomorphized_functions.get(id).expect("invariant: should exist")
    }

    // TODO unideal clone
    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, Function)> {
        self.typed_functions.iter().map(|(a, b)| (*a, b.clone())).collect::<Vec<_>>().into_iter()
    }

    pub fn expr_ty(
        &self,
        expr: &TypedExpr,
    ) -> TypeVariable {
        use TypedExprKind::*;
        match &expr.kind {
            FunctionCall { ty, .. } => *ty,
            Literal { ty, .. } => *ty,
            List { ty, .. } => *ty,
            Unit => self.unit(),
            Variable { ty, .. } => *ty,
            Intrinsic { ty, .. } => *ty,
            ErrorRecovery(..) => self.ctx.error_recovery,
            ExprWithBindings { expression, .. } => self.expr_ty(expression),
            TypeConstructor { ty, .. } => *ty,
            If { then_branch, .. } => self.expr_ty(then_branch),
        }
    }

    /// Given a concrete [`PetrType`], unify it with the return type of the given expression.
    pub fn unify_expr_return(
        &mut self,
        ty: TypeVariable,
        expr: &TypedExpr,
    ) {
        let expr_ty = self.expr_ty(expr);
        self.unify(ty, expr_ty, expr.span());
    }

    pub fn string(&self) -> TypeVariable {
        self.ctx.string_ty
    }

    pub fn unit(&self) -> TypeVariable {
        self.ctx.unit_ty
    }

    pub fn int(&self) -> TypeVariable {
        self.ctx.int_ty
    }

    pub fn bool(&self) -> TypeVariable {
        self.ctx.bool_ty
    }

    /// To reference an error recovery type, you must provide an error.
    /// This holds the invariant that error recovery types are only generated when
    /// an error occurs.
    pub fn error_recovery(
        &mut self,
        err: TypeError,
    ) -> TypeVariable {
        self.push_error(err);
        self.ctx.error_recovery
    }

    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    fn unify_err(
        &self,
        clone_1: SpecificType,
        clone_2: SpecificType,
    ) -> TypeConstraintError {
        let pretty_printed_b = pretty_printing::pretty_print_petr_type(&clone_2, self);
        match clone_1 {
            SpecificType::Sum(tys) => {
                let tys = tys.iter().map(|ty| pretty_printing::pretty_print_petr_type(ty, self)).collect::<Vec<_>>();
                TypeConstraintError::NotSubtype(tys, pretty_printed_b)
            },
            _ => {
                let pretty_printed_a = pretty_printing::pretty_print_petr_type(&clone_1, self);
                TypeConstraintError::UnificationFailure(pretty_printed_a, pretty_printed_b)
            },
        }
    }

    fn satisfy_err(
        &self,
        clone_1: SpecificType,
        clone_2: SpecificType,
    ) -> TypeConstraintError {
        let pretty_printed_b = pretty_printing::pretty_print_petr_type(&clone_2, self);
        match clone_1 {
            SpecificType::Sum(tys) => {
                let tys = tys.iter().map(|ty| pretty_printing::pretty_print_petr_type(ty, self)).collect::<Vec<_>>();
                TypeConstraintError::NotSubtype(tys, pretty_printed_b)
            },
            _ => {
                let pretty_printed_a = pretty_printing::pretty_print_petr_type(&clone_1, self);
                TypeConstraintError::FailedToSatisfy(pretty_printed_a, pretty_printed_b)
            },
        }
    }

    fn satisfy_expr_return(
        &mut self,
        ty: TypeVariable,
        expr: &TypedExpr,
    ) {
        let expr_ty = self.expr_ty(expr);
        self.satisfies(ty, expr_ty, expr.span());
    }

    pub fn ctx(&self) -> &TypeContext {
        &self.ctx
    }

    /// terms:
    /// ### resolved type variable
    ///
    /// a type variable that is not a `Ref`. To get the resolved type of
    /// a type variable, you must follow the chain of `Ref`s until you reach a non-Ref type.
    ///
    /// ### constraint kind strength:
    /// The following is the hierarchy of constraints in terms of strength, from strongest (1) to
    /// weakest:
    /// 1. Unify(t1, t2) (t2 _must_ be coerceable to exactly equal t1)
    /// 2. Satisfies (t2 must be a subset of t1. For all cases where t2 can unify to t1, t2
    ///    satisfies t1 as a constraint)
    ///
    /// ### constraint strength
    /// A constraint `a` is _stronger than_ a constraint `b` iff:
    /// - `a` is higher than `b` in terms of constraint kind strength `a` is a more specific constraint than `b`
    ///    - e.g. Unify(Literal(5), x) is stronger than Unify(Int, x) because the former is more specific
    ///    - e.g. Unify(a, b) is stronger than Satisfies(a, b)
    ///
    ///
    /// ### duplicated constraint:
    /// A constraint `a` is _duplicated by_ constraint `b` iff:
    /// - `a` and `b` are the same constraint kind, and the resolved type variables are the same
    /// - `a` is a stronger constraint than `b`
    ///
    fn deduplicate_constraints(&mut self) {
        use TypeConstraintKindValue as Kind;
        let mut constraints = ConstraintDeduplicator::default();
        let mut errs = vec![];
        for constraint in &self.ctx.constraints {
            println!("on constraint: {:?}", constraint);
            let (mut tys, kind) = match &constraint.kind {
                TypeConstraintKind::Unify(t1, t2) => (vec![*t1, *t2], Kind::Unify),
                TypeConstraintKind::Satisfies(t1, t2) => (vec![*t1, *t2], Kind::Satisfies),
            };

            // resolve all `Ref` types to get a resolved type variable
            'outer: for ty_var in tys.iter_mut() {
                // track what we have seen, in case a circular reference is present
                let mut seen_vars = BTreeSet::new();
                seen_vars.insert(*ty_var);
                let ty = self.ctx.types.get(*ty_var);
                while let SpecificType::Ref(t) = ty {
                    if seen_vars.insert(*t) {
                        *ty_var = *t;
                    } else {
                        // circular reference
                        errs.push(constraint.span.with_item(TypeConstraintError::CircularType));
                        continue 'outer;
                    }
                    *ty_var = *t;
                }
            }

            constraints.insert((kind, tys), *constraint);
        }

        for err in errs {
            self.push_error(err);
        }

        self.ctx.constraints = constraints.into_values();
    }
}

/// the `key` type is what we use to deduplicate constraints
#[derive(Default)]
struct ConstraintDeduplicator {
    constraints: BTreeMap<(TypeConstraintKindValue, Vec<TypeVariable>), TypeConstraint>,
}

impl ConstraintDeduplicator {
    fn insert(
        &mut self,
        key: (TypeConstraintKindValue, Vec<TypeVariable>),
        constraint: TypeConstraint,
    ) {
        self.constraints.insert(key, constraint);
    }

    fn into_values(self) -> Vec<TypeConstraint> {
        self.constraints.into_values().collect()
    }
}

#[derive(Clone)]
pub enum Intrinsic {
    Puts(Box<TypedExpr>),
    Add(Box<TypedExpr>, Box<TypedExpr>),
    Multiply(Box<TypedExpr>, Box<TypedExpr>),
    Divide(Box<TypedExpr>, Box<TypedExpr>),
    Subtract(Box<TypedExpr>, Box<TypedExpr>),
    Malloc(Box<TypedExpr>),
    SizeOf(Box<TypedExpr>),
    Equals(Box<TypedExpr>, Box<TypedExpr>),
}

impl std::fmt::Debug for Intrinsic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Intrinsic::Puts(expr) => write!(f, "@puts({:?})", expr),
            Intrinsic::Add(lhs, rhs) => write!(f, "@add({:?}, {:?})", lhs, rhs),
            Intrinsic::Multiply(lhs, rhs) => write!(f, "@multiply({:?}, {:?})", lhs, rhs),
            Intrinsic::Divide(lhs, rhs) => write!(f, "@divide({:?}, {:?})", lhs, rhs),
            Intrinsic::Subtract(lhs, rhs) => write!(f, "@subtract({:?}, {:?})", lhs, rhs),
            Intrinsic::Malloc(size) => write!(f, "@malloc({:?})", size),
            Intrinsic::SizeOf(expr) => write!(f, "@sizeof({:?})", expr),
            Intrinsic::Equals(lhs, rhs) => write!(f, "@equal({:?}, {:?})", lhs, rhs),
        }
    }
}

#[derive(Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    span:     Span,
}

impl TypedExpr {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub enum TypedExprKind {
    FunctionCall {
        func: FunctionId,
        args: Vec<(Identifier, TypedExpr)>,
        ty:   TypeVariable,
    },
    Literal {
        value: Literal,
        ty:    TypeVariable,
    },
    List {
        elements: Vec<TypedExpr>,
        ty:       TypeVariable,
    },
    Unit,
    Variable {
        ty:   TypeVariable,
        name: Identifier,
    },
    Intrinsic {
        ty:        TypeVariable,
        intrinsic: Intrinsic,
    },
    ErrorRecovery(Span),
    ExprWithBindings {
        bindings:   Vec<(Identifier, TypedExpr)>,
        expression: Box<TypedExpr>,
    },
    TypeConstructor {
        ty:   TypeVariable,
        args: Box<[TypedExpr]>,
    },
    If {
        condition:   Box<TypedExpr>,
        then_branch: Box<TypedExpr>,
        else_branch: Box<TypedExpr>,
    },
}

impl std::fmt::Debug for TypedExpr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        use TypedExprKind::*;
        match &self.kind {
            FunctionCall { func, args, .. } => {
                write!(f, "function call to {} with args: ", func)?;
                for (name, arg) in args {
                    write!(f, "{}: {:?}, ", name.id, arg)?;
                }
                Ok(())
            },
            Literal { value, .. } => write!(f, "literal: {}", value),
            List { elements, .. } => {
                write!(f, "list: [")?;
                for elem in elements {
                    write!(f, "{:?}, ", elem)?;
                }
                write!(f, "]")
            },
            Unit => write!(f, "unit"),
            Variable { name, .. } => write!(f, "variable: {}", name.id),
            Intrinsic { intrinsic, .. } => write!(f, "intrinsic: {:?}", intrinsic),
            ErrorRecovery(span) => {
                write!(f, "error recovery {span:?}")
            },
            ExprWithBindings { bindings, expression } => {
                write!(f, "bindings: ")?;
                for (name, expr) in bindings {
                    write!(f, "{}: {:?}, ", name.id, expr)?;
                }
                write!(f, "expression: {:?}", expression)
            },
            TypeConstructor { ty, .. } => write!(f, "type constructor: {:?}", ty),
            If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {:?} then {:?} else {:?}", condition, then_branch, else_branch)
            },
        }
    }
}

impl TypeCheck for Expr {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        let kind = match &self.kind {
            ExprKind::Literal(lit) => {
                let ty = ctx.convert_literal_to_type(lit);
                TypedExprKind::Literal { value: lit.clone(), ty }
            },
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    let ty = ctx.unit();
                    TypedExprKind::List { elements: vec![], ty }
                } else {
                    let type_checked_exprs = exprs.iter().map(|expr| expr.type_check(ctx)).collect::<Vec<_>>();
                    // unify the type of the first expr against everything else in the list
                    let first_ty = ctx.expr_ty(&type_checked_exprs[0]);
                    for expr in type_checked_exprs.iter().skip(1) {
                        let second_ty = ctx.expr_ty(expr);
                        ctx.unify(first_ty, second_ty, expr.span());
                    }
                    let first_ty = ctx.ctx.types.get(first_ty).clone();
                    TypedExprKind::List {
                        elements: type_checked_exprs,
                        ty:       ctx.insert_type::<SpecificType>(&SpecificType::List(Box::new(first_ty))),
                    }
                }
            },
            ExprKind::FunctionCall(call) => (*call).type_check(ctx),
            ExprKind::Unit => TypedExprKind::Unit,
            ExprKind::ErrorRecovery => TypedExprKind::ErrorRecovery(self.span),
            ExprKind::Variable { name, ty } => {
                // look up variable in scope
                // find its expr return type
                let var_ty = ctx.find_variable(*name).expect("variable not found in scope");
                let ty = ctx.to_type_var(ty);

                ctx.unify(var_ty, ty, name.span());

                TypedExprKind::Variable { ty, name: *name }
            },
            ExprKind::Intrinsic(intrinsic) => return self.span.with_item(intrinsic.clone()).type_check(ctx),
            ExprKind::TypeConstructor(parent_type_id, args) => {
                // This ExprKind only shows up in the body of type constructor functions, and
                // is basically a noop. The surrounding function decl will handle type checking for
                // the type constructor.
                let args = args.iter().map(|arg| arg.type_check(ctx)).collect::<Vec<_>>();
                let ty = ctx.get_type(*parent_type_id);
                TypedExprKind::TypeConstructor {
                    ty:   *ty,
                    args: args.into_boxed_slice(),
                }
            },
            ExprKind::ExpressionWithBindings { bindings, expression } => {
                // for each binding, type check the rhs
                ctx.with_type_scope(|ctx| {
                    let mut type_checked_bindings = Vec::with_capacity(bindings.len());
                    for binding in bindings {
                        let binding_ty = binding.expression.type_check(ctx);
                        let binding_expr_return_ty = ctx.expr_ty(&binding_ty);
                        ctx.insert_variable(binding.name, binding_expr_return_ty);
                        type_checked_bindings.push((binding.name, binding_ty));
                    }

                    TypedExprKind::ExprWithBindings {
                        bindings:   type_checked_bindings,
                        expression: Box::new(expression.type_check(ctx)),
                    }
                })
            },
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = condition.type_check(ctx);
                let condition_ty = ctx.expr_ty(&condition);
                ctx.unify(condition_ty, ctx.bool(), condition.span());

                let then_branch = then_branch.type_check(ctx);
                let then_ty = ctx.expr_ty(&then_branch);

                let else_branch = else_branch.type_check(ctx);
                let else_ty = ctx.expr_ty(&else_branch);

                ctx.unify(then_ty, else_ty, else_branch.span());

                TypedExprKind::If {
                    condition:   Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                }
            },
        };

        TypedExpr { kind, span: self.span }
    }
}

fn unify_basic_math_op(
    lhs: &Expr,
    rhs: &Expr,
    ctx: &mut TypeChecker,
) -> (TypedExpr, TypedExpr) {
    let lhs = lhs.type_check(ctx);
    let rhs = rhs.type_check(ctx);
    let lhs_ty = ctx.expr_ty(&lhs);
    let rhs_ty = ctx.expr_ty(&rhs);
    let int_ty = ctx.int();
    ctx.unify(int_ty, lhs_ty, lhs.span());
    ctx.unify(int_ty, rhs_ty, rhs.span());
    (lhs, rhs)
}

impl TypeCheck for SpannedItem<ResolvedIntrinsic> {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        use petr_resolve::IntrinsicName::*;
        let kind = match self.item().intrinsic {
            Puts => {
                if self.item().args.len() != 1 {
                    todo!("puts arg len check");
                }
                // puts takes a single string and returns unit
                let arg = self.item().args[0].type_check(ctx);
                ctx.unify_expr_return(ctx.string(), &arg);
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Puts(Box::new(arg)),
                    ty:        ctx.unit(),
                }
            },
            Add => {
                if self.item().args.len() != 2 {
                    todo!("add arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Add(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Subtract => {
                if self.item().args.len() != 2 {
                    todo!("sub arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);

                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Subtract(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Multiply => {
                if self.item().args.len() != 2 {
                    todo!("mult arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Multiply(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },

            Divide => {
                if self.item().args.len() != 2 {
                    todo!("Divide arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Divide(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Malloc => {
                // malloc takes one integer (the number of bytes to allocate)
                // and returns a pointer to the allocated memory
                // will return `0` if the allocation fails
                // in the future, this might change to _words_ of allocation,
                // depending on the compilation target
                if self.item().args.len() != 1 {
                    todo!("malloc arg len check");
                }
                let arg = self.item().args[0].type_check(ctx);
                let arg_ty = ctx.expr_ty(&arg);
                let int_ty = ctx.int();
                ctx.unify(int_ty, arg_ty, arg.span());
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Malloc(Box::new(arg)),
                    ty:        int_ty,
                }
            },
            SizeOf => {
                if self.item().args.len() != 1 {
                    todo!("size_of arg len check");
                }

                let arg = self.item().args[0].type_check(ctx);

                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::SizeOf(Box::new(arg)),
                    ty:        ctx.int(),
                }
            },
            Equals => {
                if self.item().args.len() != 2 {
                    todo!("equal arg len check");
                }

                let lhs = self.item().args[0].type_check(ctx);
                let rhs = self.item().args[1].type_check(ctx);
                ctx.unify(ctx.expr_ty(&lhs), ctx.expr_ty(&rhs), self.span());
                TypedExprKind::Intrinsic {
                    intrinsic: Intrinsic::Equals(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.bool(),
                }
            },
        };

        TypedExpr { kind, span: self.span() }
    }
}

trait TypeCheck {
    type Output;
    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output;
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name:      Identifier,
    pub params:    Vec<(Identifier, TypeVariable)>,
    pub body:      TypedExpr,
    pub return_ty: TypeVariable,
}

impl TypeCheck for petr_resolve::Function {
    type Output = Function;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        ctx.with_type_scope(|ctx| {
            let params = self.params.iter().map(|(name, ty)| (*name, ctx.to_type_var(ty))).collect::<Vec<_>>();

            for (name, ty) in &params {
                ctx.insert_variable(*name, *ty);
            }

            // unify types within the body with the parameter
            let body = self.body.type_check(ctx);

            let declared_return_type = ctx.to_type_var(&self.return_type);

            Function {
                name: self.name,
                params,
                return_ty: declared_return_type,
                body,
            }
        })
    }
}

impl TypeCheck for petr_resolve::FunctionCall {
    type Output = TypedExprKind;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        let func_decl = ctx.get_function(&self.function).clone();

        if self.args.len() != func_decl.params.len() {
            // TODO: support partial application
            ctx.push_error(self.span().with_item(TypeConstraintError::ArgumentCountMismatch {
                expected: func_decl.params.len(),
                got:      self.args.len(),
                function: ctx.get_symbol(func_decl.name.id).to_string(),
            }));
            return TypedExprKind::ErrorRecovery(self.span());
        }

        let mut args: Vec<(Identifier, TypedExpr, TypeVariable)> = Vec::with_capacity(self.args.len());

        // unify all of the arg types with the param types
        for (arg, (name, param_ty)) in self.args.iter().zip(func_decl.params.iter()) {
            let arg = arg.type_check(ctx);
            let arg_ty = ctx.expr_ty(&arg);
            ctx.satisfies(*param_ty, arg_ty, arg.span());
            args.push((*name, arg, arg_ty));
        }

        let concrete_arg_types: Vec<_> = args
            .iter()
            .map(|(_, _, ty)| ctx.look_up_variable(*ty).generalize(&ctx.ctx).clone())
            .collect();

        let signature: FunctionSignature = (self.function, concrete_arg_types.clone().into_boxed_slice());
        // now that we know the argument types, check if this signature has been monomorphized
        // already
        if ctx.monomorphized_functions.contains_key(&signature) {
            return TypedExprKind::FunctionCall {
                func: self.function,
                args: args.into_iter().map(|(name, expr, _)| (name, expr)).collect(),
                ty:   func_decl.return_ty,
            };
        }

        // unify declared return type with body return type
        let declared_return_type = func_decl.return_ty;

        ctx.satisfy_expr_return(declared_return_type, &func_decl.body);

        // to create a monomorphized func decl, we don't actually have to update all of the types
        // throughout the entire definition. We only need to update the parameter types.
        let mut monomorphized_func_decl = Function {
            name:      func_decl.name,
            params:    func_decl.params.clone(),
            return_ty: declared_return_type,
            body:      func_decl.body.clone(),
        };

        // update the parameter types to be the concrete types
        for (param, concrete_ty) in monomorphized_func_decl.params.iter_mut().zip(concrete_arg_types.iter()) {
            let param_ty = ctx.insert_type(concrete_ty);
            param.1 = param_ty;
        }

        // if there are any variable exprs in the body, update those ref types
        let mut num_replacements = 0;
        replace_var_reference_types(
            &mut monomorphized_func_decl.body.kind,
            &monomorphized_func_decl.params,
            &mut num_replacements,
        );

        ctx.monomorphized_functions.insert(signature, monomorphized_func_decl);

        TypedExprKind::FunctionCall {
            func: self.function,
            args: args.into_iter().map(|(name, expr, _)| (name, expr)).collect(),
            ty:   declared_return_type,
        }
    }
}

fn replace_var_reference_types(
    expr: &mut TypedExprKind,
    params: &Vec<(Identifier, TypeVariable)>,
    num_replacements: &mut usize,
) {
    match expr {
        TypedExprKind::Variable { ref mut ty, name } => {
            if let Some((_param_name, ty_var)) = params.iter().find(|(param_name, _)| param_name.id == name.id) {
                *num_replacements += 1;
                *ty = *ty_var;
            }
        },
        TypedExprKind::FunctionCall { args, .. } => {
            for (_, arg) in args {
                replace_var_reference_types(&mut arg.kind, params, num_replacements);
            }
        },
        TypedExprKind::Intrinsic { intrinsic, .. } => {
            use Intrinsic::*;
            match intrinsic {
                // intrinsics which take one arg, grouped for convenience
                Puts(a) | Malloc(a) | SizeOf(a) => {
                    replace_var_reference_types(&mut a.kind, params, num_replacements);
                },
                // intrinsics which take two args, grouped for convenience
                Add(a, b) | Subtract(a, b) | Multiply(a, b) | Divide(a, b) | Equals(a, b) => {
                    replace_var_reference_types(&mut a.kind, params, num_replacements);
                    replace_var_reference_types(&mut b.kind, params, num_replacements);
                },
            }
        },
        // TODO other expr kinds like bindings
        _ => (),
    }
}

mod pretty_printing {
    use crate::*;

    #[cfg(test)]
    pub fn pretty_print_type_checker(type_checker: TypeChecker) -> String {
        let mut s = String::new();
        for (id, ty) in &type_checker.type_map {
            let text = match id {
                TypeOrFunctionId::TypeId(id) => {
                    let ty = type_checker.resolved.get_type(*id);

                    let name = type_checker.resolved.interner.get(ty.name.id);
                    format!("type {}", name)
                },
                TypeOrFunctionId::FunctionId(id) => {
                    let func = type_checker.resolved.get_function(*id);

                    let name = type_checker.resolved.interner.get(func.name.id);

                    format!("fn {}", name)
                },
            };
            s.push_str(&text);
            s.push_str(": ");
            s.push_str(&pretty_print_ty(ty, &type_checker));

            s.push('\n');
            match id {
                TypeOrFunctionId::TypeId(_) => (),
                TypeOrFunctionId::FunctionId(func) => {
                    let func = type_checker.typed_functions.get(func).unwrap();
                    let body = &func.body;
                    s.push_str(&pretty_print_typed_expr(body, &type_checker));
                    s.push('\n');
                },
            }
            s.push('\n');
        }

        if !type_checker.monomorphized_functions.is_empty() {
            s.push_str("__MONOMORPHIZED FUNCTIONS__");
        }

        for func in type_checker.monomorphized_functions.values() {
            let func_name = type_checker.resolved.interner.get(func.name.id);
            let arg_types = func.params.iter().map(|(_, ty)| pretty_print_ty(ty, &type_checker)).collect::<Vec<_>>();
            s.push_str(&format!(
                "\nfn {}({:?}) -> {}",
                func_name,
                arg_types,
                pretty_print_ty(&func.return_ty, &type_checker)
            ));
        }

        if !type_checker.monomorphized_functions.is_empty() {
            s.push('\n');
        }

        if !type_checker.errors.is_empty() {
            s.push_str("\n__ERRORS__\n");
            for error in type_checker.errors {
                s.push_str(&format!("{:?}\n", error));
            }
        }
        s
    }

    pub fn pretty_print_ty(
        ty: &TypeVariable,
        type_checker: &TypeChecker,
    ) -> String {
        let mut ty = type_checker.look_up_variable(*ty);
        while let SpecificType::Ref(t) = ty {
            println!("looping");
            ty = type_checker.look_up_variable(*t);
        }
        pretty_print_petr_type(ty, type_checker)
    }

    pub fn pretty_print_petr_type(
        ty: &SpecificType,
        type_checker: &TypeChecker,
    ) -> String {
        match ty {
            SpecificType::Unit => "unit".to_string(),
            SpecificType::Integer => "int".to_string(),
            SpecificType::Boolean => "bool".to_string(),
            SpecificType::String => "string".to_string(),
            SpecificType::Ref(ty) => pretty_print_ty(ty, type_checker),
            SpecificType::UserDefined { name, .. } => {
                let name = type_checker.resolved.interner.get(name.id);
                name.to_string()
            },
            SpecificType::Arrow(tys) => {
                let mut s = String::new();
                s.push('(');
                for (ix, ty) in tys.iter().enumerate() {
                    let is_last = ix == tys.len() - 1;

                    s.push_str(&pretty_print_ty(ty, type_checker));
                    if !is_last {
                        s.push_str(" → ");
                    }
                }
                s.push(')');
                s
            },
            SpecificType::ErrorRecovery => "error recovery".to_string(),
            SpecificType::List(ty) => format!("[{}]", pretty_print_petr_type(ty, type_checker)),
            SpecificType::Infer(id, _) => format!("infer t{id}"),
            SpecificType::Sum(tys) => {
                let mut s = String::new();
                s.push('(');
                for (ix, ty) in tys.iter().enumerate() {
                    let is_last = ix == tys.len() - 1;
                    // print the petr ty
                    s.push_str(&pretty_print_petr_type(ty, type_checker));
                    if !is_last {
                        s.push_str(" | ");
                    }
                }
                s.push(')');
                s
            },
            SpecificType::Literal(l) => format!("{}", l),
        }
    }

    #[cfg(test)]
    pub fn pretty_print_typed_expr(
        typed_expr: &TypedExpr,
        type_checker: &TypeChecker,
    ) -> String {
        let interner = &type_checker.resolved.interner;
        match &typed_expr.kind {
            TypedExprKind::ExprWithBindings { bindings, expression } => {
                let mut s = String::new();
                for (name, expr) in bindings {
                    let ident = interner.get(name.id);
                    let ty = type_checker.expr_ty(expr);
                    let ty = pretty_print_ty(&ty, type_checker);
                    s.push_str(&format!("{ident}: {:?} ({}),\n", expr, ty));
                }
                let expr_ty = type_checker.expr_ty(expression);
                let expr_ty = pretty_print_ty(&expr_ty, type_checker);
                s.push_str(&format!("{:?} ({})", pretty_print_typed_expr(expression, type_checker), expr_ty));
                s
            },
            TypedExprKind::Variable { name, ty } => {
                let name = interner.get(name.id);
                let ty = pretty_print_ty(ty, type_checker);
                format!("variable {name}: {ty}")
            },

            TypedExprKind::FunctionCall { func, args, ty } => {
                let mut s = String::new();
                s.push_str(&format!("function call to {} with args: ", func));
                for (name, arg) in args {
                    let name = interner.get(name.id);
                    let arg_ty = type_checker.expr_ty(arg);
                    let arg_ty = pretty_print_ty(&arg_ty, type_checker);
                    s.push_str(&format!("{name}: {}, ", arg_ty));
                }
                let ty = pretty_print_ty(ty, type_checker);
                s.push_str(&format!("returns {ty}"));
                s
            },
            TypedExprKind::TypeConstructor { ty, .. } => format!("type constructor: {}", pretty_print_ty(ty, type_checker)),
            _otherwise => format!("{:?}", typed_expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use petr_resolve::resolve_symbols;
    use petr_utils::render_error;

    use super::*;
    use crate::pretty_printing::*;

    fn check(
        input: impl Into<String>,
        expect: Expect,
    ) {
        let input = input.into();
        let parser = petr_parse::Parser::new(vec![("test", input)]);
        let (ast, errs, interner, source_map) = parser.into_result();
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("test failed: code didn't parse");
        }
        let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("unresolved symbols in test");
        }
        let type_checker = TypeChecker::new(resolved);
        let res = pretty_print_type_checker(type_checker);

        expect.assert_eq(&res);
    }

    #[test]
    fn identity_resolution_concrete_type() {
        check(
            r#"
            fn foo(x in 'int) returns 'int x
            "#,
            expect![[r#"
                fn foo: (int → int)
                variable x: int

            "#]],
        );
    }

    #[test]
    fn identity_resolution_generic() {
        check(
            r#"
            fn foo(x in 'A) returns 'A x
            "#,
            expect![[r#"
                fn foo: (infer t5 → infer t5)
                variable x: infer t5

            "#]],
        );
    }

    #[test]
    fn identity_resolution_custom_type() {
        check(
            r#"
            type MyType = A | B
            fn foo(x in 'MyType) returns 'MyType x
            "#,
            expect![[r#"
                type MyType: MyType

                fn A: MyType
                type constructor: MyType

                fn B: MyType
                type constructor: MyType

                fn foo: (MyType → MyType)
                variable x: MyType

            "#]],
        );
    }

    #[test]
    fn identity_resolution_two_custom_types() {
        check(
            r#"
            type MyType = A | B
            type MyComposedType = firstVariant someField 'MyType | secondVariant someField 'int someField2 'MyType someField3 'GenericType
            fn foo(x in 'MyType) returns 'MyComposedType ~firstVariant(x)
            "#,
            expect![[r#"
                type MyType: MyType

                type MyComposedType: MyComposedType

                fn A: MyType
                type constructor: MyType

                fn B: MyType
                type constructor: MyType

                fn firstVariant: (MyType → MyComposedType)
                type constructor: MyComposedType

                fn secondVariant: (int → MyType → infer t16 → MyComposedType)
                type constructor: MyComposedType

                fn foo: (MyType → MyComposedType)
                function call to functionid2 with args: someField: MyType, returns MyComposedType

                __MONOMORPHIZED FUNCTIONS__
                fn firstVariant(["MyType"]) -> MyComposedType
            "#]],
        );
    }

    #[test]
    fn literal_unification_fail() {
        check(
            r#"
            fn foo() returns 'int 5
            fn bar() returns 'bool 5
            "#,
            expect![[r#"
                fn foo: int
                literal: 5

                fn bar: bool
                literal: 5

            "#]],
        );
    }

    #[test]
    fn literal_unification_success() {
        check(
            r#"
            fn foo() returns 'int 5
            fn bar() returns 'bool true
            "#,
            expect![[r#"
                fn foo: int
                literal: 5

                fn bar: bool
                literal: true

            "#]],
        );
    }

    #[test]
    fn pass_zero_arity_func_to_intrinsic() {
        check(
            r#"
        fn string_literal() returns 'string
          "This is a string literal."

        fn my_func() returns 'unit
          @puts(~string_literal)"#,
            expect![[r#"
                fn string_literal: string
                literal: "This is a string literal."

                fn my_func: unit
                intrinsic: @puts(function call to functionid0 with args: )

                __MONOMORPHIZED FUNCTIONS__
                fn string_literal([]) -> string
            "#]],
        );
    }

    #[test]
    fn pass_literal_string_to_intrinsic() {
        check(
            r#"
        fn my_func() returns 'unit
          @puts("test")"#,
            expect![[r#"
                fn my_func: unit
                intrinsic: @puts(literal: "test")

            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_literal_to_intrinsic() {
        check(
            r#"
        fn my_func() returns 'unit
          @puts(true)"#,
            expect![[r#"
                fn my_func: unit
                intrinsic: @puts(literal: true)


                __ERRORS__
                SpannedItem UnificationFailure("string", "true") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(52), length: 4 } }]
            "#]],
        );
    }

    #[test]
    fn intrinsic_and_return_ty_dont_match() {
        check(
            r#"
        fn my_func() returns 'bool
          @puts("test")"#,
            expect![[r#"
                fn my_func: bool
                intrinsic: @puts(literal: "test")

            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_fn_call_to_intrinsic() {
        check(
            r#"
        fn bool_literal() returns 'bool
            true

        fn my_func() returns 'unit
          @puts(~bool_literal)"#,
            expect![[r#"
                fn bool_literal: bool
                literal: true

                fn my_func: unit
                intrinsic: @puts(function call to functionid0 with args: )

                __MONOMORPHIZED FUNCTIONS__
                fn bool_literal([]) -> bool

                __ERRORS__
                SpannedItem UnificationFailure("string", "bool") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(110), length: 14 } }]
            "#]],
        );
    }

    #[test]
    fn multiple_calls_to_fn_dont_unify_params_themselves() {
        check(
            r#"
        fn bool_literal(a in 'A, b in 'B) returns 'bool
            true

        fn my_func() returns 'bool
            ~bool_literal(1, 2)

        {- should not unify the parameter types of bool_literal -}
        fn my_second_func() returns 'bool
            ~bool_literal(true, false)
        "#,
            expect![[r#"
                fn bool_literal: (infer t5 → infer t7 → bool)
                literal: true

                fn my_func: bool
                function call to functionid0 with args: a: 1, b: 2, returns bool

                fn my_second_func: bool
                function call to functionid0 with args: a: true, b: false, returns bool

                __MONOMORPHIZED FUNCTIONS__
                fn bool_literal(["int", "int"]) -> bool
                fn bool_literal(["bool", "bool"]) -> bool
            "#]],
        );
    }
    #[test]
    fn list_different_types_type_err() {
        check(
            r#"
                fn my_list() returns 'list [ 1, true ]
            "#,
            expect![[r#"
                fn my_list: infer t8
                list: [literal: 1, literal: true, ]

            "#]],
        );
    }

    #[test]
    fn incorrect_number_of_args() {
        check(
            r#"
                fn add(a in 'int, b in 'int) returns 'int a

                fn add_five(a in 'int) returns 'int ~add(5)
            "#,
            expect![[r#"
                fn add: (int → int → int)
                variable a: int

                fn add_five: (int → int)
                error recovery Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(113), length: 8 } }


                __ERRORS__
                SpannedItem ArgumentCountMismatch { function: "add", expected: 2, got: 1 } [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(113), length: 8 } }]
            "#]],
        );
    }

    #[test]
    fn infer_let_bindings() {
        check(
            r#"
            fn hi(x in 'int, y in 'int) returns 'int
    let a = x;
        b = y;
        c = 20;
        d = 30;
        e = 42;
    a
fn main() returns 'int ~hi(1, 2)"#,
            expect![[r#"
                fn hi: (int → int → int)
                a: variable: symbolid2 (int),
                b: variable: symbolid4 (int),
                c: literal: 20 (20),
                d: literal: 30 (30),
                e: literal: 42 (42),
                "variable a: int" (int)

                fn main: int
                function call to functionid0 with args: x: 1, y: 2, returns int

                __MONOMORPHIZED FUNCTIONS__
                fn hi(["int", "int"]) -> int
                fn main([]) -> int
            "#]],
        )
    }

    #[test]
    fn if_rejects_non_bool_condition() {
        check(
            r#"
            fn hi(x in 'int) returns 'int
                if x then 1 else 2
            fn main() returns 'int ~hi(1)"#,
            expect![[r#"
                fn hi: (int → int)
                if variable: symbolid2 then literal: 1 else literal: 2

                fn main: int
                function call to functionid0 with args: x: 1, returns int

                __MONOMORPHIZED FUNCTIONS__
                fn hi(["int"]) -> int
                fn main([]) -> int

                __ERRORS__
                SpannedItem UnificationFailure("int", "bool") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(61), length: 2 } }]
            "#]],
        )
    }

    #[test]
    fn if_rejects_non_unit_missing_else() {
        check(
            r#"
            fn hi() returns 'int
                if true then 1
            fn main() returns 'int ~hi()"#,
            expect![[r#"
                fn hi: int
                if literal: true then literal: 1 else unit

                fn main: int
                function call to functionid0 with args: returns int

                __MONOMORPHIZED FUNCTIONS__
                fn hi([]) -> int
                fn main([]) -> int

                __ERRORS__
                SpannedItem UnificationFailure("unit", "1") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(33), length: 46 } }]
            "#]],
        )
    }

    #[test]
    fn if_allows_unit_missing_else() {
        check(
            r#"
            fn hi() returns 'unit
                if true then @puts "hi"

            fn main() returns 'unit ~hi()"#,
            expect![[r#"
                fn hi: unit
                if literal: true then intrinsic: @puts(literal: "hi") else unit

                fn main: unit
                function call to functionid0 with args: returns unit

                __MONOMORPHIZED FUNCTIONS__
                fn hi([]) -> unit
                fn main([]) -> unit
            "#]],
        )
    }

    #[test]
    fn disallow_incorrect_constant_int() {
        check(
            r#"
            type OneOrTwo = 1 | 2

            fn main() returns 'OneOrTwo
                ~OneOrTwo 10
                "#,
            expect![[r#"
                type OneOrTwo: OneOrTwo

                fn OneOrTwo: ((1 | 2) → OneOrTwo)
                type constructor: OneOrTwo

                fn main: OneOrTwo
                function call to functionid0 with args: OneOrTwo: 10, returns OneOrTwo

                __MONOMORPHIZED FUNCTIONS__
                fn OneOrTwo(["int"]) -> OneOrTwo
                fn main([]) -> OneOrTwo

                __ERRORS__
                SpannedItem NotSubtype(["1", "2"], "10") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(104), length: 0 } }]
            "#]],
        )
    }

    #[test]
    fn disallow_incorrect_constant_string() {
        check(
            r#"
            type AOrB = "A" | "B"

            fn main() returns 'AOrB
                ~AOrB "c"
                "#,
            expect![[r#"
                type AOrB: AOrB

                fn AOrB: (("A" | "B") → AOrB)
                type constructor: AOrB

                fn main: AOrB
                function call to functionid0 with args: AOrB: "c", returns AOrB

                __MONOMORPHIZED FUNCTIONS__
                fn AOrB(["string"]) -> AOrB
                fn main([]) -> AOrB

                __ERRORS__
                SpannedItem NotSubtype(["\"A\"", "\"B\""], "\"c\"") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(97), length: 0 } }]
            "#]],
        )
    }

    // TODO remove ignore before merging
    #[ignore]
    #[test]
    fn disallow_incorrect_constant_bool() {
        check(
            r#"
        type AlwaysTrue = true

        fn main() returns 'AlwaysTrue
            ~AlwaysTrue false 
            "#,
            expect![[r#"
                type AlwaysTrue: AlwaysTrue

                fn AlwaysTrue: ((true) → AlwaysTrue)
                type constructor: AlwaysTrue

                fn main: AlwaysTrue
                function call to functionid0 with args: AlwaysTrue: false, returns AlwaysTrue

                __MONOMORPHIZED FUNCTIONS__
                fn AlwaysTrue(["bool"]) -> AlwaysTrue
                fn main([]) -> AlwaysTrue
                __ERRORS__
                SpannedItem FailedToSatisfy("false", "(true)") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(100), length: 0 } }]
            "#]],
        )
    }

    // TODO remove ignore before merging
    #[ignore]
    #[test]
    fn disallow_wrong_sum_type_in_add() {
        check(
            r#"
            type IntBelowFive = 1 | 2 | 3 | 4 | 5
            {- reject an `add` which may return an int above five -}
            fn add(a in 'IntBelowFive, b in 'IntBelowFive) returns 'IntBelowFive @add(a, b)
"#,
            expect![[r#""#]],
        )
    }

    #[ignore]
    #[test]
    fn allow_wrong_sum_type_in_add() {
        check(
            r#"
            type IntBelowFive = 1 | 2 | 3 | 4 | 5
            {- reject an `add` which may return an int above five -}
            fn add(a in 'IntBelowFive, b in 'IntBelowFive) returns 'int @add(a, b)
"#,
            expect![[r#""#]],
        )
    }

    #[test]
    fn sum_type_unifies_to_superset() {
        check(
            r"fn test(a in 'sum 1 | 2 | 3) returns 'sum 1 | 2 | 3 a
              fn test_(a in 'sum 1 | 2) returns 'sum 1 | 2 a
              fn main() returns 'int
                {- should be of specific type lit 2 -}
                let x = 2;
                    {- should be of specific type 'sum 1 | 2 -}
                    y = ~test_(x);
                    {- should be of specific type 'sum 1 | 2 | 3 -}
                    z = ~test(y);
                    {- should also be of specific type 'sum 1 | 2 | 3 -}
                    zz = ~test(x)

                {- and should generalize to 'int with no problems -}
                zz
            ",
            expect![[r#"
                fn test: ((1 | 2 | 3) → (1 | 2 | 3))
                variable a: (1 | 2 | 3)

                fn test_: ((1 | 2) → (1 | 2))
                variable a: (1 | 2)

                fn main: int
                x: literal: 2 (2),
                y: function call to functionid1 with args: symbolid1: variable: symbolid5,  ((1 | 2)),
                z: function call to functionid0 with args: symbolid1: variable: symbolid6,  ((1 | 2 | 3)),
                zz: function call to functionid0 with args: symbolid1: variable: symbolid5,  ((1 | 2 | 3)),
                "variable zz: (1 | 2 | 3)" ((1 | 2 | 3))

                __MONOMORPHIZED FUNCTIONS__
                fn test(["int"]) -> (1 | 2 | 3)
                fn test_(["int"]) -> (1 | 2)
                fn main([]) -> int
            "#]],
        )
    }

    #[test]
    fn specific_type_generalizes() {
        check(
            r#"fn test(a in 'sum 'int | 'string) returns 'sum 'int | 'string a
               fn test_(a in 'int) returns 'sum 'int | 'string a
               fn main() returns 'int
                 let x = ~test_(5);
                     y = ~test("a string");
                 42
            "#,
            expect![[r#"
                fn test: ((int | string) → (int | string))
                variable a: (int | string)

                fn test_: (int → (int | string))
                variable a: int

                fn main: int
                x: function call to functionid1 with args: symbolid1: literal: 5,  ((int | string)),
                y: function call to functionid0 with args: symbolid1: literal: "a string",  ((int | string)),
                "literal: 42" (42)

                __MONOMORPHIZED FUNCTIONS__
                fn test(["string"]) -> (int | string)
                fn test_(["int"]) -> (int | string)
                fn main([]) -> int
            "#]],
        )
    }

    #[test]
    fn disallow_bad_generalization() {
        check(
            r#"fn test(a in 'sum 'int | 'string) returns 'sum 'int | 'string a
               fn test_(a in 'bool) returns 'sum 'int | 'string a
               fn main() returns 'int
                 {- we are passing 'bool into 'int | 'string so this should fail to satisfy constraints -} 
                 let y = ~test(~test_(true));
                 42
            "#,
            expect![[r#"
                fn test: ((int | string) → (int | string))
                variable a: (int | string)

                fn test_: (bool → (int | string))
                variable a: bool

                fn main: int
                y: function call to functionid0 with args: symbolid1: function call to functionid1 with args: symbolid1: literal: true, ,  ((int | string)),
                "literal: 42" (42)

                __MONOMORPHIZED FUNCTIONS__
                fn test(["(int | string)"]) -> (int | string)
                fn test_(["bool"]) -> (int | string)
                fn main([]) -> int

                __ERRORS__
                SpannedItem NotSubtype(["int", "string"], "bool") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(129), length: 0 } }]
            "#]],
        )
    }

    #[test]
    fn order_of_sum_type_doesnt_matter() {
        check(
            r#"fn test(a in 'sum 'int | 'string) returns 'sum 'string | 'int a
            "#,
            expect![[r#"
                fn test: ((int | string) → (int | string))
                variable a: (int | string)

            "#]],
        )
    }

    #[test]
    fn can_return_superset() {
        check(
            r#"fn test(a in 'sum 'int | 'string) returns 'sum 'string | 'int | 'bool a
            "#,
            expect![[r#"
                fn test: ((int | string) → (int | bool | string))
                variable a: (int | string)

            "#]],
        )
    }
}
