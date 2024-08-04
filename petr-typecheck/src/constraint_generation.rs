use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use petr_bind::FunctionId;
use petr_resolve::{Expr, FunctionCall, QueryableResolvedItems};
use petr_utils::{Identifier, IndexMap, Span, SpannedItem, SymbolId};

use crate::{
    error::TypeConstraintError,
    solution::{TypeSolution, TypeSolutionEntry},
    typed_ast::{TypedExpr, TypedExprKind},
    types::{GeneralType, SpecificType, Type, TypeVariant},
    Function, TypeError, TypeOrFunctionId, TypeVariable,
};

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

    fn axiom(
        t1: TypeVariable,
        span: Span,
    ) -> Self {
        Self {
            kind: TypeConstraintKind::Axiom(t1),
            span,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TypeConstraintKind {
    Unify(TypeVariable, TypeVariable),
    // constraint that lhs is a "subtype" or satisfies the typeclass constraints of "rhs"
    Satisfies(TypeVariable, TypeVariable),
    // If a type variable is constrained to be an axiom, it means that the type variable
    // cannot be updated by the inference engine. It effectively fixes the type, or pins the type.
    Axiom(TypeVariable),
}

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
enum TypeConstraintKindValue {
    Unify,
    Satisfies,
    Axiom,
}

pub type FunctionSignature = (FunctionId, Box<[GeneralType]>);

pub struct TypeConstraintContext {
    type_map: BTreeMap<TypeOrFunctionId, TypeVariable>,
    monomorphized_functions: BTreeMap<FunctionSignature, Function>,
    typed_functions: BTreeMap<FunctionId, Function>,
    errors: Vec<TypeError>,
    resolved: QueryableResolvedItems,
    variable_scope: Vec<BTreeMap<Identifier, TypeVariable>>,
    types: IndexMap<TypeVariable, SpecificType>,
    constraints: Vec<TypeConstraint>,
    // known primitive type IDs
    unit_ty: TypeVariable,
    string_ty: TypeVariable,
    int_ty: TypeVariable,
    bool_ty: TypeVariable,
    error_recovery: TypeVariable,
}

/// This trait defines the interface for AST nodes to output the type constraints that they
/// produce by modifying the constraint context.
pub trait GenerateTypeConstraints {
    type Output;
    fn type_check(
        &self,
        ctx: &mut TypeConstraintContext,
    ) -> Self::Output;
}

impl TypeConstraintContext {
    pub fn unify(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.constraints.push(TypeConstraint::unify(ty1, ty2, span));
    }

    pub fn satisfies(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
        span: Span,
    ) {
        self.constraints.push(TypeConstraint::satisfies(ty1, ty2, span));
    }

    pub fn axiom(
        &mut self,
        ty1: TypeVariable,
        span: Span,
    ) {
        self.constraints.push(TypeConstraint::axiom(ty1, span));
    }

    pub fn new_variable(
        &mut self,
        span: Span,
    ) -> TypeVariable {
        // infer is special -- it knows its own id, mostly for printing
        // and disambiguating
        let infer_id = self.types.len();
        self.types.insert(SpecificType::Infer(infer_id, span))
    }

    /// Update a type variable with a new SpecificType
    pub fn update_type(
        &mut self,
        t1: TypeVariable,
        known: SpecificType,
    ) {
        *self.types.get_mut(t1) = known;
    }

    pub fn types(&self) -> &IndexMap<TypeVariable, SpecificType> {
        &self.types
    }

    pub fn insert_type<T: Type>(
        &mut self,
        ty: &T,
    ) -> TypeVariable {
        let ty = ty.as_specific_ty();
        // TODO: check if type already exists and return that ID instead
        self.types.insert(ty)
    }

    pub fn look_up_variable(
        &self,
        ty: TypeVariable,
    ) -> &SpecificType {
        self.types.get(ty)
    }

    pub fn get_symbol(
        &self,
        id: SymbolId,
    ) -> Rc<str> {
        self.resolved.interner.get(id).clone()
    }

    pub(crate) fn with_type_scope<T>(
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
                self.update_type(fresh_ty, SpecificType::ErrorRecovery);
            },
        };
        fresh_ty
    }

    pub(crate) fn find_variable(
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

    pub fn fully_type_check(&mut self) {
        let mut ty_map: Vec<(TypeVariable, _, _)> = Vec::with_capacity(self.resolved.types().count());
        for (id, decl) in self.resolved.types() {
            let ty = self.fresh_ty_var(decl.name.span);
            ty_map.push((ty, id, decl));
            self.type_map.insert(id.into(), ty);
        }

        for (ty, _id, decl) in ty_map {
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

            self.update_type(
                ty,
                SpecificType::UserDefined {
                    name: decl.name,
                    variants,
                    constant_literal_types: decl.constant_literal_types,
                },
            );
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
    }

    pub fn get_main_function(&self) -> Option<(FunctionId, Function)> {
        self.functions().find(|(_, func)| &*self.get_symbol(func.name.id) == "main")
    }

    /// iterate through each constraint and transform the underlying types to satisfy them
    /// - unification tries to collapse two types into one
    /// - satisfaction tries to make one type satisfy the constraints of another, although type
    ///   constraints don't exist in the language yet
    pub fn into_solution(self) -> Result<TypeSolution, Vec<TypeError>> {
        let constraints = self.constraints.clone();
        let mut solution = TypeSolution::new(
            self.types.clone(),
            self.error_recovery,
            self.unit_ty,
            self.typed_functions,
            self.monomorphized_functions,
            self.resolved.interner,
            self.errors,
        );
        for TypeConstraint { kind, span } in constraints.iter().filter(|c| matches!(c.kind, TypeConstraintKind::Axiom(_))) {
            let TypeConstraintKind::Axiom(axiomatic_variable) = kind else {
                unreachable!("above filter ensures that all constraints are axioms here")
            };
            // first, pin all axiomatic type variables in the solution
            let ty = self.types.get(*axiomatic_variable).clone();
            solution.insert_solution(*axiomatic_variable, TypeSolutionEntry::new_axiomatic(ty), *span);
        }

        // now apply the constraints
        for constraint in constraints.iter().filter(|c| !matches!(c.kind, TypeConstraintKind::Axiom(_))) {
            match &constraint.kind {
                TypeConstraintKind::Unify(t1, t2) => {
                    solution.apply_unify_constraint(*t1, *t2, constraint.span);
                },
                TypeConstraintKind::Satisfies(t1, t2) => {
                    solution.apply_satisfies_constraint(*t1, *t2, constraint.span);
                },
                TypeConstraintKind::Axiom(_) => unreachable!(),
            }
        }

        solution.into_result()
    }

    pub fn new(resolved: QueryableResolvedItems) -> Self {
        let mut types = IndexMap::default();
        // instantiate basic primitive types
        let unit_ty = types.insert(SpecificType::Unit);
        let string_ty = types.insert(SpecificType::String);
        let bool_ty = types.insert(SpecificType::Boolean);
        let int_ty = types.insert(SpecificType::Integer);
        let error_recovery = types.insert(SpecificType::ErrorRecovery);

        TypeConstraintContext {
            type_map: Default::default(),
            errors: Default::default(),
            typed_functions: Default::default(),
            resolved,
            variable_scope: Default::default(),
            monomorphized_functions: Default::default(),
            types,
            constraints: Default::default(),
            unit_ty,
            string_ty,
            int_ty,
            bool_ty,
            error_recovery,
        }
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
        self.new_variable(span)
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
        self.types.insert(ty)
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
            petr_resolve::Type::Named(ty_id) => SpecificType::Ref(
                *self
                    .type_map
                    .get(&ty_id.into())
                    .unwrap_or_else(|| panic!("ty {ty_id} type did not exist in type map")),
            ),
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
        self.types.insert(petr_ty)
    }

    pub fn get_type(
        &self,
        key: impl Into<TypeOrFunctionId>,
    ) -> &TypeVariable {
        self.type_map.get(&key.into()).expect("type did not exist in type map")
    }

    pub(crate) fn convert_literal_to_type(
        &mut self,
        literal: &petr_resolve::Literal,
    ) -> TypeVariable {
        let ty = SpecificType::Literal(literal.clone());
        self.types.insert(ty)
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
            ErrorRecovery(..) => self.error_recovery,
            ExprWithBindings { expression, .. } => self.expr_ty(expression),
            TypeConstructor { ty, .. } => *ty,
            If { then_branch, .. } => self.expr_ty(then_branch),
        }
    }

    /// Given a concrete [`SpecificType`], unify it with the return type of the given expression.
    pub fn unify_expr_return(
        &mut self,
        ty: TypeVariable,
        expr: &TypedExpr,
    ) {
        let expr_ty = self.expr_ty(expr);
        self.unify(ty, expr_ty, expr.span());
    }

    pub fn string(&self) -> TypeVariable {
        self.string_ty
    }

    pub fn unit(&self) -> TypeVariable {
        self.unit_ty
    }

    pub fn int(&self) -> TypeVariable {
        self.int_ty
    }

    pub fn bool(&self) -> TypeVariable {
        self.bool_ty
    }

    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    pub fn satisfy_expr_return(
        &mut self,
        ty: TypeVariable,
        expr: &TypedExpr,
    ) {
        let expr_ty = self.expr_ty(expr);
        self.satisfies(ty, expr_ty, expr.span());
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
        for constraint in &self.constraints {
            let (mut tys, kind) = match &constraint.kind {
                TypeConstraintKind::Unify(t1, t2) => (vec![*t1, *t2], Kind::Unify),
                TypeConstraintKind::Satisfies(t1, t2) => (vec![*t1, *t2], Kind::Satisfies),
                TypeConstraintKind::Axiom(t1) => (vec![*t1], Kind::Axiom),
            };

            // resolve all `Ref` types to get a resolved type variable
            'outer: for ty_var in tys.iter_mut() {
                // track what we have seen, in case a circular reference is present
                let mut seen_vars = BTreeSet::new();
                seen_vars.insert(*ty_var);
                let mut ty = self.types.get(*ty_var);
                while let SpecificType::Ref(t) = ty {
                    if seen_vars.contains(t) {
                        // circular reference
                        errs.push(constraint.span.with_item(TypeConstraintError::CircularType));
                        continue 'outer;
                    }
                    *ty_var = *t;
                    ty = self.types.get(*t);
                }
            }

            constraints.insert((kind, tys), *constraint);
        }

        self.constraints = constraints.into_values();
    }

    pub fn push_error(
        &mut self,
        e: TypeError,
    ) {
        self.errors.push(e);
    }

    pub fn monomorphized_functions(&self) -> &BTreeMap<FunctionSignature, Function> {
        &self.monomorphized_functions
    }

    pub(crate) fn insert_monomorphized_function(
        &mut self,
        signature: (FunctionId, Box<[GeneralType]>),
        monomorphized_func_decl: Function,
    ) {
        self.monomorphized_functions.insert(signature, monomorphized_func_decl);
    }

    pub fn type_map(&self) -> &BTreeMap<TypeOrFunctionId, TypeVariable> {
        &self.type_map
    }

    pub fn resolved(&self) -> &QueryableResolvedItems {
        &self.resolved
    }

    pub fn typed_functions(&self) -> &BTreeMap<FunctionId, Function> {
        &self.typed_functions
    }

    pub fn constraints(&self) -> &[TypeConstraint] {
        &self.constraints
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

pub fn unify_basic_math_op(
    lhs: &Expr,
    rhs: &Expr,
    ctx: &mut TypeConstraintContext,
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

impl GenerateTypeConstraints for petr_resolve::Function {
    type Output = Function;

    fn type_check(
        &self,
        ctx: &mut TypeConstraintContext,
    ) -> Self::Output {
        ctx.with_type_scope(|ctx| {
            let params = self.params.iter().map(|(name, ty)| (*name, ctx.to_type_var(ty))).collect::<Vec<_>>();
            // declared parameters are axiomatic, they won't be updated by any inference

            for (name, ty) in &params {
                ctx.insert_variable(*name, *ty);
                // TODO get span for type annotation instead of just the name of the parameter
                ctx.axiom(*ty, name.span);
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

impl GenerateTypeConstraints for FunctionCall {
    type Output = TypedExprKind;

    fn type_check(
        &self,
        ctx: &mut TypeConstraintContext,
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
            .map(|(_, _, ty)| ctx.look_up_variable(*ty).generalize(&ctx.types).clone())
            .collect();

        let signature: FunctionSignature = (self.function, concrete_arg_types.clone().into_boxed_slice());
        // now that we know the argument types, check if this signature has been monomorphized
        // already
        if ctx.monomorphized_functions().contains_key(&signature) {
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
        // todo!("figure out how to re-do type check OR use replace_var_types to generate more constraints, maybe via unification?");

        // update the parameter types to be the concrete types
        for (param, concrete_ty) in monomorphized_func_decl.params.iter_mut().zip(concrete_arg_types.iter()) {
            let param_ty = ctx.insert_type(concrete_ty);
            param.1 = param_ty;
        }

        // if there are any variable exprs in the body, update those ref types
        let mut num_replacements = 0;
        let new_constraints = replace_var_reference_types(
            &mut monomorphized_func_decl.body.kind,
            &monomorphized_func_decl.params,
            &mut num_replacements,
        );
        for constraint in new_constraints {
            ctx.constraints.push(constraint);
        }

        // re-do constraint generation on replaced func body
        //        monomorphized_func_decl.body.type_check(ctx);

        ctx.insert_monomorphized_function(signature, monomorphized_func_decl);
        // if there are any variable exprs in the body, update those ref types

        TypedExprKind::FunctionCall {
            func: self.function,
            args: args.into_iter().map(|(name, expr, _)| (name, expr)).collect(),
            ty:   declared_return_type,
        }
    }
}

impl GenerateTypeConstraints for SpannedItem<petr_resolve::Intrinsic> {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeConstraintContext,
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
                    intrinsic: crate::Intrinsic::Puts(Box::new(arg)),
                    ty:        ctx.unit(),
                }
            },
            Add => {
                if self.item().args.len() != 2 {
                    todo!("add arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: crate::Intrinsic::Add(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Subtract => {
                if self.item().args.len() != 2 {
                    todo!("sub arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);

                TypedExprKind::Intrinsic {
                    intrinsic: crate::Intrinsic::Subtract(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Multiply => {
                if self.item().args.len() != 2 {
                    todo!("mult arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: crate::Intrinsic::Multiply(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },

            Divide => {
                if self.item().args.len() != 2 {
                    todo!("Divide arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.item().args[0], &self.item().args[1], ctx);
                TypedExprKind::Intrinsic {
                    intrinsic: crate::Intrinsic::Divide(Box::new(lhs), Box::new(rhs)),
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
                    intrinsic: crate::Intrinsic::Malloc(Box::new(arg)),
                    ty:        int_ty,
                }
            },
            SizeOf => {
                if self.item().args.len() != 1 {
                    todo!("size_of arg len check");
                }

                let arg = self.item().args[0].type_check(ctx);

                TypedExprKind::Intrinsic {
                    intrinsic: crate::Intrinsic::SizeOf(Box::new(arg)),
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
                    intrinsic: crate::Intrinsic::Equals(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.bool(),
                }
            },
        };

        TypedExpr { kind, span: self.span() }
    }
}

fn replace_var_reference_types(
    expr: &mut TypedExprKind,
    params: &Vec<(Identifier, TypeVariable)>,
    num_replacements: &mut usize,
) -> Vec<TypeConstraint> {
    match expr {
        TypedExprKind::Variable { ref mut ty, name } => {
            if let Some((_param_name, ty_var)) = params.iter().find(|(param_name, _)| param_name.id == name.id) {
                *num_replacements += 1;
                println!("replacing var named {} ", name.id);
                //                *ty = *ty_var;
                return vec![TypeConstraint::unify(*ty, *ty_var, name.span)];
            }
            vec![]
        },
        TypedExprKind::FunctionCall { args, .. } => {
            let mut buf = Vec::new();
            for (_, arg) in args {
                buf.append(&mut replace_var_reference_types(&mut arg.kind, params, num_replacements));
            }
            buf
        },
        TypedExprKind::Intrinsic { intrinsic, .. } => {
            use crate::Intrinsic::*;
            match intrinsic {
                // intrinsics which take one arg, grouped for convenience
                Puts(a) | Malloc(a) | SizeOf(a) => replace_var_reference_types(&mut a.kind, params, num_replacements),
                // intrinsics which take two args, grouped for convenience
                Add(a, b) | Subtract(a, b) | Multiply(a, b) | Divide(a, b) | Equals(a, b) => {
                    let mut constraints = replace_var_reference_types(&mut a.kind, params, num_replacements);
                    constraints.append(&mut replace_var_reference_types(&mut b.kind, params, num_replacements));
                    constraints
                },
            }
        },
        // TODO other expr kinds like bindings
        _ => vec![],
    }
}
