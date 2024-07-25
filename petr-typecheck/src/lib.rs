mod error;

use std::{collections::BTreeMap, rc::Rc};

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

pub struct TypeContext {
    types:          IndexMap<TypeVariable, PetrType>,
    constraints:    Vec<TypeConstraint>,
    // known primitive type IDs
    unit_ty:        TypeVariable,
    string_ty:      TypeVariable,
    int_ty:         TypeVariable,
    error_recovery: TypeVariable,
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut types = IndexMap::default();
        // instantiate basic primitive types
        let unit_ty = types.insert(PetrType::Unit);
        let string_ty = types.insert(PetrType::String);
        let int_ty = types.insert(PetrType::Integer);
        let error_recovery = types.insert(PetrType::ErrorRecovery);
        // insert primitive types
        TypeContext {
            types,
            constraints: Default::default(),
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
        let infer_id = self.types.len();
        self.types.insert(PetrType::Infer(infer_id, span))
    }

    /// Update a type variable with a new PetrType
    fn update_type(
        &mut self,
        t1: TypeVariable,
        known: PetrType,
    ) {
        *self.types.get_mut(t1) = known;
    }
}

pub type FunctionSignature = (FunctionId, Box<[PetrType]>);

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
pub enum PetrType {
    Unit,
    Integer,
    Boolean,
    /// a static length string known at compile time
    String,
    /// A reference to another type
    Ref(TypeVariable),
    /// A user-defined type
    UserDefined {
        name:     Identifier,
        variants: Vec<TypeVariant>,
    },
    Arrow(Vec<TypeVariable>),
    ErrorRecovery,
    List(TypeVariable),
    /// the usize is just an identifier for use in rendering the type
    /// the span is the location of the inference, for error reporting if the inference is never
    /// resolved
    Infer(usize, Span),
}

#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord)]
pub struct TypeVariant {
    pub fields: Box<[TypeVariable]>,
}

impl TypeChecker {
    pub fn insert_type(
        &mut self,
        ty: PetrType,
    ) -> TypeVariable {
        // TODO: check if type already exists and return that ID instead
        self.ctx.types.insert(ty)
    }

    pub fn look_up_variable(
        &self,
        ty: TypeVariable,
    ) -> &PetrType {
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
                self.ctx.update_type(fresh_ty, PetrType::ErrorRecovery);
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
                        let fields = variant.fields.iter().map(|field| ctx.to_type_var(&field.ty)).collect::<Vec<_>>();
                        TypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                })
                .collect::<Vec<_>>();
            self.ctx.update_type(ty, PetrType::UserDefined { name: decl.name, variants });
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
    fn apply_unify_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
        span: Span,
    ) {
        let ty1 = self.ctx.types.get(t1).clone();
        let ty2 = self.ctx.types.get(t2).clone();
        use PetrType::*;
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
            // instantiate the infer type with the known type
            (Infer(_, _), known) => {
                self.ctx.update_type(t1, known);
            },
            (known, Infer(_, _)) => {
                self.ctx.update_type(t2, known);
            },
            // lastly, if no unification rule exists for these two types, it is a mismatch
            (a, b) => {
                self.push_error(span.with_item(TypeConstraintError::UnificationFailure(a, b)));
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
        use PetrType::*;
        match (ty1, ty2) {
            (a, b) if a == b => (),
            (ErrorRecovery, _) | (_, ErrorRecovery) => (),
            (Ref(a), _) => self.apply_satisfies_constraint(*a, t2, span),
            (_, Ref(b)) => self.apply_satisfies_constraint(t1, *b, span),
            // if t1 is a fully instantiated type, then t2 can be updated to be a reference to t1
            (Unit | Integer | Boolean | UserDefined { .. } | String | Arrow(..) | List(..), Infer(_, _)) => {
                self.ctx.update_type(t2, Ref(t1));
            },
            // if we are trying to satisfy an inferred type with no bounds, this is ok
            (Infer(..), _) => (),
            (a, b) => {
                self.push_error(span.with_item(TypeConstraintError::FailedToSatisfy(a.clone(), b.clone())));
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

        let ty = PetrType::Arrow(tys);
        self.ctx.types.insert(ty)
    }

    pub fn to_type_var(
        &mut self,
        ty: &petr_resolve::Type,
    ) -> TypeVariable {
        let ty = match ty {
            petr_resolve::Type::Integer => PetrType::Integer,
            petr_resolve::Type::Bool => PetrType::Boolean,
            petr_resolve::Type::Unit => PetrType::Unit,
            petr_resolve::Type::String => PetrType::String,
            petr_resolve::Type::ErrorRecovery(span) => {
                // unifies to anything, fresh var
                return self.fresh_ty_var(*span);
            },
            petr_resolve::Type::Named(ty_id) => PetrType::Ref(*self.type_map.get(&ty_id.into()).expect("type did not exist in type map")),
            petr_resolve::Type::Generic(generic_name) => {
                return self.generic_type(generic_name);
            },
        };
        self.ctx.types.insert(ty)
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
        use petr_resolve::Literal::*;
        let ty = match literal {
            Integer(_) => PetrType::Integer,
            Boolean(_) => PetrType::Boolean,
            String(_) => PetrType::String,
        };
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
        println!("looking for function {:?}", id);
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
        id: &(FunctionId, Box<[PetrType]>),
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
            ErrorRecovery(..) => write!(f, "error recovery"),
            ExprWithBindings { bindings, expression } => {
                write!(f, "bindings: ")?;
                for (name, expr) in bindings {
                    write!(f, "{}: {:?}, ", name.id, expr)?;
                }
                write!(f, "expression: {:?}", expression)
            },
            TypeConstructor { ty, .. } => write!(f, "type constructor: {:?}", ty),
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
                    TypedExprKind::List {
                        elements: type_checked_exprs,
                        ty:       ctx.insert_type(PetrType::List(first_ty)),
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
    ctx.unify(lhs_ty, int_ty, lhs.span());
    ctx.unify(rhs_ty, int_ty, rhs.span());
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
                ctx.unify(arg_ty, int_ty, arg.span());
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

        // unify declared return type with body return type
        let declared_return_type = func_decl.return_ty;

        ctx.unify_expr_return(declared_return_type, &func_decl.body);

        let concrete_arg_types: Vec<PetrType> = args.iter().map(|(_, _, ty)| ctx.look_up_variable(*ty).clone()).collect();

        ctx.monomorphized_functions
            .insert((self.function, concrete_arg_types.into_boxed_slice()), func_decl);

        TypedExprKind::FunctionCall {
            func: self.function,
            args: args.into_iter().map(|(name, expr, _)| (name, expr)).collect(),
            ty:   declared_return_type,
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use petr_resolve::resolve_symbols;
    use petr_utils::render_error;

    use super::*;
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
        assert!(errs.is_empty(), "can't typecheck: unresolved symbols");
        let type_checker = TypeChecker::new(resolved);
        let res = pretty_print_type_checker(type_checker);

        expect.assert_eq(&res);
    }

    fn pretty_print_type_checker(type_checker: TypeChecker) -> String {
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

        if !type_checker.errors.is_empty() {
            s.push_str("\nErrors:\n");
            for error in type_checker.errors {
                s.push_str(&format!("{:?}\n", error));
            }
        }
        s
    }

    fn pretty_print_ty(
        ty: &TypeVariable,
        type_checker: &TypeChecker,
    ) -> String {
        let mut ty = type_checker.look_up_variable(*ty);
        while let PetrType::Ref(t) = ty {
            ty = type_checker.look_up_variable(*t);
        }
        match ty {
            PetrType::Unit => "unit".to_string(),
            PetrType::Integer => "int".to_string(),
            PetrType::Boolean => "bool".to_string(),
            PetrType::String => "string".to_string(),
            PetrType::Ref(ty) => pretty_print_ty(ty, type_checker),
            PetrType::UserDefined { name, variants: _ } => {
                let name = type_checker.resolved.interner.get(name.id);
                name.to_string()
            },
            PetrType::Arrow(tys) => {
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
            PetrType::ErrorRecovery => "error recovery".to_string(),
            PetrType::List(ty) => format!("[{}]", pretty_print_ty(ty, type_checker)),
            PetrType::Infer(id, _) => format!("t{id}"),
        }
    }

    fn pretty_print_typed_expr(
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
                fn foo: (t4 → t4)
                variable x: t4

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

                fn secondVariant: (int → MyType → t18 → MyComposedType)
                type constructor: MyComposedType

                fn foo: (MyType → MyComposedType)
                function call to functionid2 with args: someField: MyType, returns MyComposedType

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


                Errors:
                SpannedItem UnificationFailure(String, Boolean) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(52), length: 4 } }]
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


                Errors:
                SpannedItem UnificationFailure(String, Boolean) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(110), length: 14 } }]
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
                fn bool_literal: (t4 → t5 → bool)
                literal: true

                fn my_func: bool
                function call to functionid0 with args: a: int, b: int, returns bool

                fn my_second_func: bool
                function call to functionid0 with args: a: bool, b: bool, returns bool

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
                fn my_list: t7
                list: [literal: 1, literal: true, ]


                Errors:
                SpannedItem UnificationFailure(Integer, Boolean) [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(48), length: 5 } }]
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
                error recovery


                Errors:
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
                c: literal: 20 (int),
                d: literal: 30 (int),
                e: literal: 42 (int),
                "variable a: int" (int)

                fn main: int
                function call to functionid0 with args: x: int, y: int, returns int

            "#]],
        )
    }
}
