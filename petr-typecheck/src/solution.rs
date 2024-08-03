use std::collections::{BTreeMap, BTreeSet};

use petr_bind::FunctionId;
use petr_resolve::Literal;
use petr_utils::{IndexMap, Span, SpannedItem, SymbolInterner};

use crate::{
    constraint_generation::FunctionSignature,
    error::TypeConstraintError,
    pretty_printing,
    typed_ast::{TypedExpr, TypedExprKind},
    types::{GeneralType, GeneralizedTypeVariant, SpecificType},
    Function, TypeError, TypeVariable,
};

/// Represents the result of the type-checking stage for an individual type variable.
pub struct TypeSolutionEntry {
    source: TypeSolutionSource,
    ty:     SpecificType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeSolutionSource {
    Inherent,
    Axiomatic,
    Inferred,
}

impl TypeSolutionEntry {
    pub fn new_axiomatic(ty: SpecificType) -> Self {
        Self {
            source: TypeSolutionSource::Axiomatic,
            ty,
        }
    }

    pub fn new_inherent(ty: SpecificType) -> Self {
        Self {
            source: TypeSolutionSource::Inherent,
            ty,
        }
    }

    pub fn new_inferred(ty: SpecificType) -> Self {
        Self {
            source: TypeSolutionSource::Inferred,
            ty,
        }
    }

    pub fn is_axiomatic(&self) -> bool {
        self.source == TypeSolutionSource::Axiomatic
    }
}

pub struct TypeSolution {
    solution: BTreeMap<TypeVariable, TypeSolutionEntry>,
    unsolved_types: IndexMap<TypeVariable, SpecificType>,
    errors: Vec<TypeError>,
    interner: SymbolInterner,
    error_recovery: TypeVariable,
    unit: TypeVariable,
    functions: BTreeMap<FunctionId, Function>,
    monomorphized_functions: BTreeMap<FunctionSignature, Function>,
}

impl TypeSolution {
    pub fn new(
        unsolved_types: IndexMap<TypeVariable, SpecificType>,
        error_recovery: TypeVariable,
        unit: TypeVariable,
        functions: BTreeMap<FunctionId, Function>,
        monomorphized_functions: BTreeMap<FunctionSignature, Function>,
        interner: SymbolInterner,
        preexisting_errors: Vec<TypeError>,
    ) -> Self {
        let solution = vec![
            (unit, TypeSolutionEntry::new_inherent(SpecificType::Unit)),
            (error_recovery, TypeSolutionEntry::new_inherent(SpecificType::ErrorRecovery)),
        ]
        .into_iter()
        .collect();
        Self {
            solution,
            unsolved_types,
            errors: preexisting_errors,
            interner,
            functions,
            monomorphized_functions,
            unit,
            error_recovery,
        }
    }

    fn push_error(
        &mut self,
        e: TypeError,
    ) {
        self.errors.push(e);
    }

    pub fn insert_solution(
        &mut self,
        ty: TypeVariable,
        entry: TypeSolutionEntry,
        span: Span,
    ) {
        if self.solution.contains_key(&ty) {
            self.update_type(ty, entry, span);
            return;
        }
        self.solution.insert(ty, entry);
    }

    fn pretty_print_type(
        &self,
        ty: &SpecificType,
    ) -> String {
        pretty_printing::pretty_print_petr_type(ty, &self.unsolved_types, &self.interner)
    }

    fn unify_err(
        &self,
        clone_1: SpecificType,
        clone_2: SpecificType,
    ) -> TypeConstraintError {
        let pretty_printed_b = self.pretty_print_type(&clone_2);
        match clone_1 {
            SpecificType::Sum(tys) => {
                let tys = tys.iter().map(|ty| self.pretty_print_type(ty)).collect::<Vec<_>>();
                TypeConstraintError::NotSubtype(tys, pretty_printed_b)
            },
            _ => {
                let pretty_printed_a = self.pretty_print_type(&clone_1);
                TypeConstraintError::UnificationFailure(pretty_printed_a, pretty_printed_b)
            },
        }
    }

    fn satisfy_err(
        &self,
        clone_1: SpecificType,
        clone_2: SpecificType,
    ) -> TypeConstraintError {
        let pretty_printed_b = self.pretty_print_type(&clone_2);
        match clone_1 {
            SpecificType::Sum(tys) => {
                let tys = tys.iter().map(|ty| self.pretty_print_type(ty)).collect::<Vec<_>>();
                TypeConstraintError::NotSubtype(tys, pretty_printed_b)
            },
            _ => {
                let pretty_printed_a = self.pretty_print_type(&clone_1);
                TypeConstraintError::FailedToSatisfy(pretty_printed_a, pretty_printed_b)
            },
        }
    }

    pub fn update_type(
        &mut self,
        ty: TypeVariable,
        entry: TypeSolutionEntry,
        span: Span,
    ) {
        match self.solution.get_mut(&ty) {
            Some(e) => {
                if e.is_axiomatic() {
                    let pretty_printed_preexisting = pretty_printing::pretty_print_petr_type(&entry.ty, &self.unsolved_types, &self.interner);
                    let pretty_printed_ty = pretty_printing::pretty_print_petr_type(&entry.ty, &self.unsolved_types, &self.interner);
                    self.errors
                        .push(span.with_item(TypeConstraintError::InvalidTypeUpdate(pretty_printed_preexisting, pretty_printed_ty)));
                    return;
                }
                *e = entry;
            },
            None => {
                self.solution.insert(ty, entry);
            },
        }
    }

    pub(crate) fn into_result(self) -> Result<TypeSolution, Vec<SpannedItem<TypeConstraintError>>> {
        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(self.errors)
        }
    }

    /// Attempt to unify two types, returning an error if they cannot be unified
    pub(crate) fn apply_unify_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) {
        let ty1 = self.get_latest_type(t1).clone();
        let ty2 = self.get_latest_type(t2).clone();
        use SpecificType::*;
        match (ty1, ty2) {
            (a, b) if a == b => (),
            (ErrorRecovery, _) | (_, ErrorRecovery) => (),
            (Ref(a), _) => self.apply_unify_constraint(a, t2, span),
            (_, Ref(b)) => self.apply_unify_constraint(t1, b, span),
            (Infer(id, _), Infer(id2, _)) if id != id2 => {
                // if two different inferred types are unified, replace the second with a reference
                // to the first
                let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t2, entry, span);
            },
            (a @ Sum(_), b @ Sum(_)) => {
                // the unification of two sum types is the union of the two types if and only if
                // `t2` is a total subset of `t1`
                // `t1` remains unchanged, as we are trying to coerce `t2` into something that
                // represents `t1`
                // TODO remove clone
                if self.a_superset_of_b(&a, &b) {
                    let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                    self.update_type(t2, entry, span);
                } else {
                    // the union of the two sets is the new type
                    let a_tys = match a {
                        Sum(tys) => tys,
                        _ => unreachable!(),
                    };
                    let b_tys = match b {
                        Sum(tys) => tys,
                        _ => unreachable!(),
                    };
                    let union = a_tys.iter().chain(b_tys.iter()).cloned().collect();
                    let entry = TypeSolutionEntry::new_inferred(Sum(union));
                    self.update_type(t1, entry, span);
                    let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                    self.update_type(t2, entry, span);
                }
            },
            // If `t2` is a non-sum type, and `t1` is a sum type, then `t1` must contain either
            // exactly the same specific type OR the generalization of that type
            // If the latter, then the specific type must be updated to its generalization
            (ref t1_ty @ Sum(_), other) => {
                if self.a_superset_of_b(t1_ty, &other) {
                    // t2 unifies to the more general form provided by t1
                    let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                    self.update_type(t2, entry, span);
                } else {
                    // add `other` to  `t1`
                    let mut tys = match t1_ty {
                        Sum(tys) => tys.clone(),
                        _ => unreachable!(),
                    };
                    tys.insert(other);
                    let entry = TypeSolutionEntry::new_inferred(Sum(tys));
                    self.update_type(t1, entry, span);
                }
            },
            // literals can unify to each other if they're equal
            (Literal(l1), Literal(l2)) if l1 == l2 => (),
            // if they're not equal, their unification is the sum of both
            (Literal(l1), Literal(l2)) if l1 != l2 => {
                // update t1 to a sum type of both,
                // and update t2 to reference t1
                let sum = Sum([Literal(l1), Literal(l2)].into());
                let t1_entry = TypeSolutionEntry::new_inferred(sum);
                let t2_entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t1, t1_entry, span);
                self.update_type(t2, t2_entry, span);
            },
            (Literal(l1), Sum(tys)) => {
                // update t1 to a sum type of both,
                // and update t2 to reference t1
                let sum = Sum([Literal(l1)].iter().chain(tys.iter()).cloned().collect());
                let t1_entry = TypeSolutionEntry::new_inferred(sum);
                let t2_entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t1, t1_entry, span);
                self.update_type(t2, t2_entry, span);
            },
            (a, b) if self.a_superset_of_b(&a, &b) => {
                // if `a` is a superset of `b`, then `b` unifies to `a` as it is more general
                let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t2, entry, span);
            },
            /* TODO rewrite below rules
            // literals can unify broader parent types
            // but the broader parent type gets instantiated with the literal type
            // TODO(alex) this rule feels incorrect. A literal being  unified to the parent type
            // should upcast the lit, not downcast t1. Check after refactoring.
            (ty, Literal(lit)) => match (&lit, ty) {
                (petr_resolve::Literal::Integer(_), Integer)
                | (petr_resolve::Literal::Boolean(_), Boolean)
                | (petr_resolve::Literal::String(_), String) => {
                    let entry = TypeSolutionEntry::new_inferred(SpecificType::Literal(lit));
                    self.update_type(t1, entry, span);
                },
                (lit, ty) => self.push_error(span.with_item(self.unify_err(ty.clone(), SpecificType::Literal(lit.clone())))),
            },
            // literals can unify broader parent types
            // but the broader parent type gets instantiated with the literal type
            (Literal(lit), ty) => match (&lit, ty) {
                (petr_resolve::Literal::Integer(_), Integer)
                | (petr_resolve::Literal::Boolean(_), Boolean)
                | (petr_resolve::Literal::String(_), String) => self.update_type(t2, SpecificType::Literal(lit)),
                (lit, ty) => {
                    self.push_error(span.with_item(self.unify_err(ty.clone(), SpecificType::Literal(lit.clone()))));
                },
            },
            */
            (other, ref t2_ty @ Sum(_)) => {
                // if `other` is a superset of `t2`, then `t2` unifies to `other` as it is more
                // general
                if self.a_superset_of_b(&other, t2_ty) {
                    let entry = TypeSolutionEntry::new_inferred(other);
                    self.update_type(t2, entry, span);
                }
            },
            // instantiate the infer type with the known type
            (Infer(_, _), _known) => {
                let entry = TypeSolutionEntry::new_inferred(Ref(t2));
                self.update_type(t1, entry, span);
            },
            (_known, Infer(_, _)) => {
                let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t2, entry, span);
            },
            // lastly, if no unification rule exists for these two types, it is a mismatch
            (a, b) => {
                self.push_error(span.with_item(self.unify_err(a, b)));
            },
        }
    }

    // This function will need to be rewritten when type constraints and bounded polymorphism are
    // implemented.
    pub(crate) fn apply_satisfies_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) {
        let ty1 = self.get_latest_type(t1);
        let ty2 = self.get_latest_type(t2);
        use SpecificType::*;
        match (ty1, ty2) {
            (a, b) if a == b => (),
            (ErrorRecovery, _) | (_, ErrorRecovery) => (),
            (Ref(a), _) => self.apply_satisfies_constraint(a, t2, span),
            (_, Ref(b)) => self.apply_satisfies_constraint(t1, b, span),
            // if t1 is a fully instantiated type, then t2 can be updated to be a reference to t1
            (Unit | Integer | Boolean | UserDefined { .. } | String | Arrow(..) | List(..) | Literal(_) | Sum(_), Infer(_, _)) => {
                let entry = TypeSolutionEntry::new_inferred(Ref(t1));
                self.update_type(t2, entry, span);
            },
            // the "parent" infer type will not instantiate to the "child" type
            (Infer(_, _), Unit | Integer | Boolean | UserDefined { .. } | String | Arrow(..) | List(..) | Literal(_) | Sum(_)) => (),
            (Sum(a_tys), Sum(b_tys)) => {
                // calculate the intersection of these types, update t2 to the intersection
                let intersection = a_tys.iter().filter(|a_ty| b_tys.contains(a_ty)).cloned().collect();
                let entry = TypeSolutionEntry::new_inferred(SpecificType::sum(intersection));
                self.update_type(t2, entry, span);
            },
            // if `ty1` is a generalized version of the sum type,
            // then it satisfies the sum type
            (ty1, other) if self.a_superset_of_b(&ty1, &other) => (),
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

    /// Gets the latest version of a type available. First checks solved types,
    /// and if it doesn't exist, gets it from the unsolved types.
    pub fn get_latest_type(
        &self,
        t1: TypeVariable,
    ) -> SpecificType {
        self.solution
            .get(&t1)
            .map(|entry| entry.ty.clone())
            .unwrap_or_else(|| self.unsolved_types.get(t1).clone())
    }

    /// To reference an error recovery type, you must provide an error.
    /// This holds the invariant that error recovery types are only generated when
    /// an error occurs.
    pub fn error_recovery(
        &mut self,
        err: TypeError,
    ) -> TypeVariable {
        self.push_error(err);
        self.error_recovery
    }

    /// If `a` is a generalized form of `b`, return true
    /// A generalized form is a type that is a superset of the sum types.
    /// For example, `String` is a generalized form of `Sum(Literal("a") | Literal("B"))`
    fn a_superset_of_b(
        &self,
        a: &SpecificType,
        b: &SpecificType,
    ) -> bool {
        use SpecificType::*;
        let generalized_b = self.generalize(b).safely_upcast();
        match (a, b) {
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
                    let b_ty_generalized = self.generalize(b_ty).safely_upcast();
                    if !(a_tys.contains(b_ty) || a_tys.contains(&b_ty_generalized)) {
                        return false;
                    }
                }

                true
            },
            _otherwise => false,
        }
    }

    pub fn generalize(
        &self,
        b: &SpecificType,
    ) -> GeneralType {
        match b {
            SpecificType::Unit => GeneralType::Unit,
            SpecificType::Integer => GeneralType::Integer,
            SpecificType::Boolean => GeneralType::Boolean,
            SpecificType::String => GeneralType::String,
            SpecificType::Ref(ty) => self.generalize(&self.get_latest_type(*ty)),
            SpecificType::UserDefined {
                name,
                variants,
                constant_literal_types,
            } => GeneralType::UserDefined {
                name: *name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let generalized_fields = variant.fields.iter().map(|field| self.generalize(field)).collect::<Vec<_>>();

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
                let ty = self.generalize(ty);
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
                let all_generalized: BTreeSet<_> = tys.iter().map(|ty| self.generalize(ty)).collect();
                if all_generalized.len() == 1 {
                    // in this case, all specific types generalized to the same type
                    all_generalized.into_iter().next().expect("invariant")
                } else {
                    GeneralType::Sum(all_generalized.into_iter().collect())
                }
            },
        }
    }

    #[cfg(test)]
    pub fn pretty_print(&self) -> String {
        let mut pretty = "__SOLVED TYPES__\n".to_string();

        let mut num_entries = 0;
        for (ty, entry) in self.solution.iter().filter(|(id, _)| ![self.unit, self.error_recovery].contains(id)) {
            pretty.push_str(&format!("{}: {}\n", Into::<usize>::into(*ty), self.pretty_print_type(&entry.ty)));
            num_entries += 1;
        }
        if num_entries == 0 {
            Default::default()
        } else {
            pretty
        }
    }

    pub fn get_main_function(&self) -> Option<(&FunctionId, &Function)> {
        self.functions.iter().find(|(_, func)| &*self.interner.get(func.name.id) == "main")
    }

    pub fn get_monomorphized_function(
        &self,
        id: &FunctionSignature,
    ) -> &Function {
        self.monomorphized_functions.get(id).expect("invariant: should exist")
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
            Unit => self.unit,
            Variable { ty, .. } => *ty,
            Intrinsic { ty, .. } => *ty,
            ErrorRecovery(..) => self.error_recovery,
            ExprWithBindings { expression, .. } => self.expr_ty(expression),
            TypeConstructor { ty, .. } => *ty,
            If { then_branch, .. } => self.expr_ty(then_branch),
        }
    }
}
