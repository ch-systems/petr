mod error;

use std::{collections::BTreeMap, rc::Rc};

use error::{TypeCheckError, TypeCheckErrorKind, TypeConstraintError};
pub use petr_bind::FunctionId;
use petr_bind::TypeId;
use petr_resolve::{Expr, ExprKind, QueryableResolvedItems};
pub use petr_resolve::{Intrinsic as ResolvedIntrinsic, IntrinsicName, Literal};
use petr_utils::{idx_map_key, Identifier, IndexMap, SymbolId};

// TODO return QueryableTypeChecked instead of type checker
// Clean up API so this is the only function exposed
pub fn type_check(resolved: QueryableResolvedItems) -> (Vec<TypeCheckError>, TypeChecker) {
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

#[derive(Clone, Copy)]
pub struct TypeConstraint {
    kind: TypeConstraintKind,
    // TODO(pipe spans through for spanned type errors)
    //     span
}
impl TypeConstraint {
    fn unify(
        t1: TypeVariable,
        t2: TypeVariable,
    ) -> Self {
        Self {
            kind: TypeConstraintKind::Unify(t1, t2),
        }
    }

    fn satisfies(
        t1: TypeVariable,
        t2: TypeVariable,
    ) -> Self {
        Self {
            kind: TypeConstraintKind::Satisfies(t1, t2),
        }
    }
}

#[derive(Clone, Copy)]
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
    ) {
        self.constraints.push(TypeConstraint::unify(ty1, ty2));
    }

    fn satisfies(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
    ) {
        self.constraints.push(TypeConstraint::satisfies(ty1, ty2));
    }

    fn new_variable(&self) -> TypeVariable {
        todo!()
    }
}

pub struct TypeChecker {
    ctx: TypeContext,
    type_map: BTreeMap<TypeOrFunctionId, TypeVariable>,
    typed_functions: BTreeMap<FunctionId, Function>,
    errors: Vec<TypeCheckError>,
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
    UserDefined(TypeId),
    Arrow(Vec<TypeVariable>),
    ErrorRecovery,
    List(TypeVariable),
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
        let fresh_ty = self.fresh_ty_var();
        self.variable_scope
            .last_mut()
            .expect("looked for generic when no scope existed")
            .insert(*id, fresh_ty);
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
        for (id, _) in self.resolved.types() {
            let ty = self.fresh_ty_var();
            self.type_map.insert(id.into(), ty);
        }
        for (id, func) in self.resolved.functions() {
            let typed_function = func.type_check(self);

            let ty = self.arrow_type([typed_function.params.iter().map(|(_, b)| *b).collect(), vec![typed_function.return_ty]].concat());
            self.type_map.insert(id.into(), ty);
            self.typed_functions.insert(id, typed_function);
        }

        self.apply_constraints();

        // we have now collected our constraints and can solve for them
    }

    /// iterate through each constraint and transform the underlying types to satisfy them
    /// - unification tries to collapse two types into one
    /// - satisfaction tries to make one type satisfy the constraints of another, although type
    ///   constraints don't exist in the language yet
    fn apply_constraints(&mut self) {
        let mut errs = Vec::new();
        let constraints = self.ctx.constraints.clone();
        for constraint in constraints {
            match &constraint.kind {
                TypeConstraintKind::Unify(t1, t2) => {
                    if let Err(err) = self.apply_unify_constraint(*t1, *t2) {
                        errs.push(err);
                    }
                },
                TypeConstraintKind::Satisfies(t1, t2) => {
                    if let Err(err) = self.apply_satisfies_constraint(*t1, *t2) {
                        errs.push(err);
                    }
                },
            }
        }

        errs.into_iter().for_each(|err| self.push_error(err));
    }

    /// Attempt to unify two types, returning an error if they cannot be unified
    /// The more specific of the two types will instantiate the more general of the two types.
    fn apply_unify_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
    ) -> Result<(), TypeConstraintError> {
        let ty1 = self.ctx.types.get(t1);
        let ty2 = self.ctx.types.get(t2);
        use PetrType::*;
        match (ty1, ty2) {
            (a, b) if a == b => Ok(()),
            (ErrorRecovery, _) | (_, ErrorRecovery) => Ok(()),
            (Ref(a), _) => self.apply_unify_constraint(*a, t2),
            (_, Ref(b)) => self.apply_unify_constraint(t1, *b),
            (a, b) => todo!("Need to write unification rule for {:?} and {:?}", a, b),
        }
    }

    // This function will need to be rewritten when type constraints and bounded polymorphism are
    // implemented.
    fn apply_satisfies_constraint(
        &mut self,
        t1: TypeVariable,
        t2: TypeVariable,
    ) -> Result<(), TypeConstraintError> {
        let ty1 = self.ctx.types.get(t1);
        let ty2 = self.ctx.types.get(t2);
        use PetrType::*;
        match (ty1, ty2) {
            (a, b) if a == b => Ok(()),
            (ErrorRecovery, _) | (_, ErrorRecovery) => Ok(()),
            (Ref(a), _) => self.apply_satisfies_constraint(*a, t2),
            (_, Ref(b)) => self.apply_satisfies_constraint(t1, *b),
            (a, b) => todo!("Need to write satisfies rule for {:?} and {:?}", a, b),
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

    pub fn fresh_ty_var(&mut self) -> TypeVariable {
        self.ctx.new_variable()
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
            petr_resolve::Type::ErrorRecovery => {
                // unifies to anything, fresh var
                return self.fresh_ty_var();
            },
            petr_resolve::Type::Named(ty_id) => PetrType::Ref(*self.type_map.get(&ty_id.into()).expect("type did not exist in type map")),
            petr_resolve::Type::Generic(generic_name) => {
                // TODO I think this needs to be a qualifier
                // polytype has support for qualifying polymorphic types
                // but instead I'm going to do the lazy thing and instantiate the generic
                // with a fresh type variable
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
        e: impl Into<TypeCheckErrorKind>,
    ) {
        let kind = e.into();
        self.errors.push(kind.into());
    }

    pub fn unify(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
    ) {
        self.ctx.unify(ty1, ty2);
    }

    pub fn satisfies(
        &mut self,
        ty1: TypeVariable,
        ty2: TypeVariable,
    ) {
        self.ctx.satisfies(ty1, ty2);
    }

    fn get_untyped_function(
        &self,
        function: FunctionId,
    ) -> &petr_resolve::Function {
        self.resolved.get_function(function)
    }

    /// Given a symbol ID, look it up in the interner and realize it as a
    /// string.
    fn realize_symbol(
        &self,
        id: petr_utils::SymbolId,
    ) -> Rc<str> {
        self.resolved.interner.get(id)
    }

    pub fn get_function(
        &self,
        id: &FunctionId,
    ) -> &Function {
        self.typed_functions.get(id).expect("invariant: should exist")
    }

    // TODO unideal clone
    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, Function)> {
        self.typed_functions.iter().map(|(a, b)| (*a, b.clone())).collect::<Vec<_>>().into_iter()
    }

    pub fn expr_ty(
        &self,
        expr: &TypedExpr,
    ) -> TypeVariable {
        use TypedExpr::*;
        match expr {
            FunctionCall { ty, .. } => *ty,
            Literal { ty, .. } => *ty,
            List { ty, .. } => *ty,
            Unit => self.unit(),
            Variable { ty, .. } => *ty,
            Intrinsic { ty, .. } => *ty,
            ErrorRecovery => self.error_recovery(),
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
        self.unify(ty, expr_ty);
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

    pub fn error_recovery(&self) -> TypeVariable {
        self.ctx.error_recovery
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
        }
    }
}

#[derive(Clone)]
pub enum TypedExpr {
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
    // TODO put a span here?
    ErrorRecovery,
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
        use TypedExpr::*;
        match self {
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
            ErrorRecovery => write!(f, "error recovery"),
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

impl TypedExpr {}

impl TypeCheck for Expr {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        match &self.kind {
            ExprKind::Literal(lit) => {
                let ty = ctx.convert_literal_to_type(lit);
                TypedExpr::Literal { value: lit.clone(), ty }
            },
            ExprKind::List(exprs) => {
                if exprs.is_empty() {
                    let ty = ctx.unit();
                    TypedExpr::List { elements: vec![], ty }
                } else {
                    let type_checked_exprs = exprs.iter().map(|expr| expr.type_check(ctx)).collect::<Vec<_>>();
                    // unify the type of the first expr against everything else in the list
                    let first_ty = ctx.expr_ty(&type_checked_exprs[0]);
                    for expr in type_checked_exprs.iter().skip(1) {
                        let second_ty = ctx.expr_ty(expr);
                        ctx.unify(first_ty, second_ty);
                    }
                    TypedExpr::List {
                        elements: type_checked_exprs,
                        ty:       ctx.insert_type(PetrType::List(first_ty)),
                    }
                }
            },
            ExprKind::FunctionCall(call) => {
                // unify args with params
                // return the func return type
                let func_decl = ctx.get_untyped_function(call.function).clone();
                if call.args.len() != func_decl.params.len() {
                    ctx.push_error(TypeCheckErrorKind::ArgumentCountMismatch {
                        expected: func_decl.params.len(),
                        got:      call.args.len(),
                        function: ctx.realize_symbol(func_decl.name.id).to_string(),
                    });
                    return TypedExpr::ErrorRecovery;
                }
                let mut args = Vec::with_capacity(call.args.len());

                for (arg, (param_name, param)) in call.args.iter().zip(func_decl.params.iter()) {
                    let arg_expr = arg.type_check(ctx);
                    let param_ty = ctx.to_type_var(param);
                    let arg_ty = ctx.expr_ty(&arg_expr);
                    ctx.satisfies(arg_ty, param_ty);
                    args.push((*param_name, arg_expr));
                }
                TypedExpr::FunctionCall {
                    func: call.function,
                    args,
                    ty: ctx.to_type_var(&func_decl.return_type),
                }
            },
            ExprKind::Unit => TypedExpr::Unit,
            ExprKind::ErrorRecovery => TypedExpr::ErrorRecovery,
            ExprKind::Variable { name, ty } => {
                // look up variable in scope
                // find its expr return type
                let var_ty = ctx.find_variable(*name).expect("variable not found in scope");
                let ty = ctx.to_type_var(ty);

                ctx.unify(var_ty, ty);

                TypedExpr::Variable { ty, name: *name }
            },
            ExprKind::Intrinsic(intrinsic) => intrinsic.type_check(ctx),
            ExprKind::TypeConstructor(args) => {
                // This ExprKind only shows up in the body of type constructor functions, and
                // is basically a noop. The surrounding function decl will handle type checking for
                // the type constructor.
                let args = args.iter().map(|arg| arg.type_check(ctx)).collect::<Vec<_>>();
                TypedExpr::TypeConstructor {
                    // Right now we'll just give this a fresh variable and it'll get unified to the
                    // return type of the function. This should work....
                    ty:   ctx.fresh_ty_var(),
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

                    TypedExpr::ExprWithBindings {
                        bindings:   type_checked_bindings,
                        expression: Box::new(expression.type_check(ctx)),
                    }
                })
            },
        }
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
    ctx.unify(lhs_ty, int_ty);
    ctx.unify(rhs_ty, int_ty);
    (lhs, rhs)
}

impl TypeCheck for ResolvedIntrinsic {
    type Output = TypedExpr;

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        use petr_resolve::IntrinsicName::*;
        let string_ty = ctx.string();
        match self.intrinsic {
            Puts => {
                if self.args.len() != 1 {
                    todo!("puts arg len check");
                }
                // puts takes a single string and returns unit
                let arg = self.args[0].type_check(ctx);
                ctx.unify_expr_return(string_ty, &arg);
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Puts(Box::new(arg)),
                    ty:        ctx.unit(),
                }
            },
            Add => {
                if self.args.len() != 2 {
                    todo!("add arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.args[0], &self.args[1], ctx);
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Add(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Subtract => {
                if self.args.len() != 2 {
                    todo!("sub arg len check");
                }
                let (lhs, rhs) = unify_basic_math_op(&self.args[0], &self.args[1], ctx);
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Subtract(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },
            Multiply => {
                if self.args.len() != 2 {
                    todo!("mult arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.args[0], &self.args[1], ctx);
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Multiply(Box::new(lhs), Box::new(rhs)),
                    ty:        ctx.int(),
                }
            },

            Divide => {
                if self.args.len() != 2 {
                    todo!("Divide arg len check");
                }

                let (lhs, rhs) = unify_basic_math_op(&self.args[0], &self.args[1], ctx);
                TypedExpr::Intrinsic {
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
                if self.args.len() != 1 {
                    todo!("malloc arg len check");
                }
                let arg = self.args[0].type_check(ctx);
                let arg_ty = ctx.expr_ty(&arg);
                let int_ty = ctx.int();
                ctx.unify(arg_ty, int_ty);
                TypedExpr::Intrinsic {
                    intrinsic: Intrinsic::Malloc(Box::new(arg)),
                    ty:        int_ty,
                }
            },
        }
    }
}

trait TypeCheck {
    type Output;
    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output;
}

#[derive(Clone)]
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

            let body_ty = ctx.expr_ty(&body);

            ctx.unify(declared_return_type, body_ty);

            Function {
                name: self.name,
                params,
                return_ty: declared_return_type,
                body,
            }
        })
        // in a scope that contains the above names to type variables, check the body
        // TODO: introduce scopes here, like in the binder, except with type variables
    }
}

impl TypeCheck for petr_resolve::FunctionCall {
    type Output = ();

    fn type_check(
        &self,
        ctx: &mut TypeChecker,
    ) -> Self::Output {
        // use polytype::Type::substitute to sub in the types of the arg exprs
        // and then unify with the function's type
        // get the function type
        let func_type = *ctx.get_type(self.function);
        let args = self.args.iter().map(|arg| arg.type_check(ctx)).collect::<Vec<_>>();

        let mut arg_types = Vec::with_capacity(args.len());

        for arg in args.iter() {
            arg_types.push(ctx.expr_ty(arg));
        }

        let arg_type = ctx.arrow_type(arg_types);

        ctx.unify(func_type, arg_type);
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
            panic!("fmt failed: code didn't parse");
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

                    format!("function {}", name)
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
                s.push_str(&format!("{}\n", miette::Report::new(error)));
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
            PetrType::UserDefined(id) => {
                let ty = type_checker.resolved.get_type(*id);
                let name = type_checker.resolved.interner.get(ty.name.id);
                name.to_string()
            },
            PetrType::Arrow(tys) => {
                let mut s = String::new();
                s.push_str("(");
                for (ix, ty) in tys.into_iter().enumerate() {
                    let is_last = ix == tys.len() - 1;

                    s.push_str(&pretty_print_ty(ty, type_checker));
                    if !is_last {
                        s.push_str(" → ");
                    }
                }
                s.push_str(")");
                s
            },
            PetrType::ErrorRecovery => "error recovery".to_string(),
            PetrType::List(ty) => format!("[{}]", pretty_print_ty(ty, type_checker)),
        }
    }

    fn pretty_print_typed_expr(
        typed_expr: &TypedExpr,
        type_checker: &TypeChecker,
    ) -> String {
        let interner = &type_checker.resolved.interner;
        match typed_expr {
            TypedExpr::ExprWithBindings { bindings, expression } => {
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
            TypedExpr::Variable { name, ty } => {
                let name = interner.get(name.id);
                let ty = pretty_print_ty(ty, type_checker);
                format!("variable {name}: {ty}")
            },

            TypedExpr::FunctionCall { func, args, ty } => {
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
            TypedExpr::TypeConstructor { ty, .. } => format!("type constructor: {}", ty),
            otherwise => format!("{:?}", otherwise),
        }
    }

    #[test]
    fn identity_resolution_concrete_type() {
        check(
            r#"
            function foo(x in 'int) returns 'int x
            "#,
            expect![[r#"
                function foo: (int → int)
                variable x: int

            "#]],
        );
    }

    #[test]
    fn identity_resolution_generic() {
        check(
            r#"
            function foo(x in 'A) returns 'A x
            "#,
            expect![[r#"
                function foo → t0 → t0
                variable: x (t0)

            "#]],
        );
    }

    #[test]
    fn identity_resolution_custom_type() {
        check(
            r#"
            type MyType = A | B
            function foo(x in 'MyType) returns 'MyType x
            "#,
            expect![[r#"
                type MyType → t0

                function A → t0
                type constructor: t1

                function B → t0
                type constructor: t2

                function foo → t0 → t0
                variable: x (t0)

            "#]],
        );
    }

    #[test]
    fn literal_unification_fail() {
        check(
            r#"
            function foo() returns 'int 5
            function bar() returns 'bool 5
            "#,
            expect![[r#"
                function foo → int
                literal: 5

                function bar → bool
                literal: 5


                Errors:
                Failed to unify types: Failure(bool, int)
            "#]],
        );
    }

    #[test]
    fn literal_unification_success() {
        check(
            r#"
            function foo() returns 'int 5
            function bar() returns 'bool true
            "#,
            expect![[r#"
                function foo: int
                literal: 5

                function bar: bool
                literal: true

            "#]],
        );
    }

    #[test]
    fn pass_zero_arity_func_to_intrinsic() {
        check(
            r#"
        function string_literal() returns 'string
          "This is a string literal."

        function my_func() returns 'unit
          @puts(~string_literal)"#,
            expect![[r#"
                function string_literal → string
                literal: "This is a string literal."

                function my_func → unit
                intrinsic: @puts(function call to functionid0 with args: )

            "#]],
        );
    }

    #[test]
    fn pass_literal_string_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts("test")"#,
            expect![[r#"
                function my_func → unit
                intrinsic: @puts(literal: "test")

            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_literal_to_intrinsic() {
        check(
            r#"
        function my_func() returns 'unit
          @puts(true)"#,
            expect![[r#"
                function my_func → unit
                intrinsic: @puts(literal: true)


                Errors:
                Failed to unify types: Failure(string, bool)
            "#]],
        );
    }

    #[test]
    fn intrinsic_and_return_ty_dont_match() {
        check(
            r#"
        function my_func() returns 'bool
          @puts("test")"#,
            expect![[r#"
                function my_func → bool
                intrinsic: @puts(literal: "test")


                Errors:
                Failed to unify types: Failure(bool, unit)
            "#]],
        );
    }

    #[test]
    fn pass_wrong_type_fn_call_to_intrinsic() {
        check(
            r#"
        function bool_literal() returns 'bool
            true

        function my_func() returns 'unit
          @puts(~bool_literal)"#,
            expect![[r#"
                function bool_literal → bool
                literal: true

                function my_func → unit
                intrinsic: @puts(function call to functionid0 with args: )


                Errors:
                Failed to unify types: Failure(string, bool)
            "#]],
        );
    }

    #[test]
    fn multiple_calls_to_fn_dont_unify_params_themselves() {
        check(
            r#"
        function bool_literal(a in 'A, b in 'B) returns 'bool
            true

        function my_func() returns 'bool
            ~bool_literal(1, 2)

        {- should not unify the parameter types of bool_literal -}
        function my_second_func() returns 'bool
            ~bool_literal(true, false)
        "#,
            expect![[r#"
                function bool_literal → (t0 → t1) → bool
                literal: true

                function my_func → bool
                function call to functionid0 with args: a: int, b: int, returns bool

                function my_second_func → bool
                function call to functionid0 with args: a: bool, b: bool, returns bool

            "#]],
        );
    }
    #[test]
    fn list_different_types_type_err() {
        check(
            r#"
                function my_list() returns 'list [ 1, true ]
            "#,
            expect![[r#"
                function my_list → t0
                list: [literal: 1, literal: true, ]


                Errors:
                Failed to unify types: Failure(int, bool)
            "#]],
        );
    }

    #[test]
    fn incorrect_number_of_args() {
        check(
            r#"
                function add(a in 'int, b in 'int) returns 'int a

                function add_five(a in 'int) returns 'int ~add(5)
            "#,
            expect![[r#"
                function add → (int → int) → int
                variable: a (int)

                function add_five → int → int
                error recovery


                Errors:
                Function add takes 2 arguments, but got 1 arguments.
                Failed to unify types: Failure(int, error)
            "#]],
        );
    }

    #[test]
    fn infer_let_bindings() {
        check(
            r#"
            function hi(x in 'int, y in 'int) returns 'int
    let a = x,
        b = y,
        c = 20,
        d = 30,
        e = 42,
    a
function main() returns 'int ~hi(1, 2)"#,
            expect![[r#"
                function hi: (int → int → int)
                a: variable: symbolid2 (int),
                b: variable: symbolid4 (int),
                c: literal: 20 (int),
                d: literal: 30 (int),
                e: literal: 42 (int),
                "variable a: int" (int)

                function main: int
                function call to functionid0 with args: x: int, y: int, returns int

            "#]],
        )
    }
}
