use petr_utils::SymbolInterner;
use types::SpecificType;

use crate::*;

#[cfg(test)]
pub fn pretty_print_type_checker(type_checker: &TypeConstraintContext) -> String {
    let mut s = String::new();
    for (id, ty) in type_checker.type_map() {
        let text = match id {
            TypeOrFunctionId::TypeId(id) => {
                let ty = type_checker.resolved().get_type(*id);

                let name = type_checker.resolved().interner.get(ty.name.id);
                format!("type {}", name)
            },
            TypeOrFunctionId::FunctionId(id) => {
                let func = type_checker.resolved().get_function(*id);

                let name = type_checker.resolved().interner.get(func.name.id);

                format!("fn {}", name)
            },
        };
        s.push_str(&text);
        s.push_str(": ");
        s.push_str(&pretty_print_ty(ty, type_checker.types(), &type_checker.resolved().interner));

        s.push('\n');
        match id {
            TypeOrFunctionId::TypeId(_) => (),
            TypeOrFunctionId::FunctionId(func) => {
                let func = type_checker.typed_functions().get(func).unwrap();
                let body = &func.body;
                s.push_str(&pretty_print_typed_expr(body, type_checker));
                s.push('\n');
            },
        }
        s.push('\n');
    }

    if !type_checker.constraints().is_empty() {
        s.push_str("__CONSTRAINTS__\n");
        for constraint in type_checker.constraints() {
            s.push_str(&format!("{:?}\n", constraint));
        }
    }

    if !type_checker.monomorphized_functions().is_empty() {
        s.push_str("\n__MONOMORPHIZED FUNCTIONS__");
    }

    for func in type_checker.monomorphized_functions().values() {
        let func_name = type_checker.resolved().interner.get(func.name.id);
        let arg_types = func
            .params
            .iter()
            .map(|(_, ty)| pretty_print_ty(ty, type_checker.types(), &type_checker.resolved().interner))
            .collect::<Vec<_>>();
        s.push_str(&format!(
            "\nfn {}({:?}) -> {}",
            func_name,
            arg_types,
            pretty_print_ty(&func.return_ty, type_checker.types(), &type_checker.resolved().interner)
        ));
    }

    if !type_checker.monomorphized_functions().is_empty() {
        s.push('\n');
    }

    s
}

pub fn pretty_print_ty(
    ty: &TypeVariable,
    types: &IndexMap<TypeVariable, SpecificType>,
    interner: &SymbolInterner,
) -> String {
    let mut ty = types.get(*ty);
    while let SpecificType::Ref(t) = ty {
        ty = types.get(*t);
    }
    pretty_print_petr_type(ty, types, interner)
}

pub fn pretty_print_petr_type(
    ty: &SpecificType,
    types: &IndexMap<TypeVariable, SpecificType>,
    interner: &SymbolInterner,
) -> String {
    match ty {
        SpecificType::Unit => "unit".to_string(),
        SpecificType::Integer => "int".to_string(),
        SpecificType::Boolean => "bool".to_string(),
        SpecificType::String => "string".to_string(),
        SpecificType::Ref(ty) => pretty_print_ty(ty, types, interner),
        SpecificType::UserDefined { name, .. } => {
            let name = interner.get(name.id);
            name.to_string()
        },
        SpecificType::Arrow(tys) => {
            let mut s = String::new();
            s.push('(');
            for (ix, ty) in tys.iter().enumerate() {
                let is_last = ix == tys.len() - 1;

                s.push_str(&pretty_print_ty(ty, types, interner));
                if !is_last {
                    s.push_str(" → ");
                }
            }
            s.push(')');
            s
        },
        SpecificType::ErrorRecovery => "error recovery".to_string(),
        SpecificType::List(ty) => format!("[{}]", pretty_print_petr_type(ty, types, interner)),
        SpecificType::Infer(id, _) => format!("infer t{id}"),
        SpecificType::Sum(tys) => {
            let mut s = String::new();
            s.push('(');
            for (ix, ty) in tys.iter().enumerate() {
                let is_last = ix == tys.len() - 1;
                // print the petr ty
                s.push_str(&pretty_print_petr_type(ty, types, interner));
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
    type_checker: &TypeConstraintContext,
) -> String {
    let interner = &type_checker.resolved().interner;
    let types = &type_checker.types();
    match &typed_expr.kind {
        TypedExprKind::ExprWithBindings { bindings, expression } => {
            let mut s = String::new();
            for (name, expr) in bindings {
                let ident = interner.get(name.id);
                let ty = type_checker.expr_ty(expr);
                let ty = pretty_print_ty(&ty, types, interner);
                s.push_str(&format!("{ident}: {:?} ({}),\n", expr, ty));
            }
            let expr_ty = type_checker.expr_ty(expression);
            let expr_ty = pretty_print_ty(&expr_ty, types, interner);
            s.push_str(&format!("{:?} ({})", pretty_print_typed_expr(expression, type_checker), expr_ty));
            s
        },
        TypedExprKind::Variable { name, ty } => {
            let name = interner.get(name.id);
            let ty = pretty_print_ty(ty, types, interner);
            format!("variable {name}: {ty}")
        },

        TypedExprKind::FunctionCall { func, args, ty } => {
            let mut s = String::new();
            s.push_str(&format!("function call to {} with args: ", func));
            for (name, arg) in args {
                let name = interner.get(name.id);
                let arg_ty = type_checker.expr_ty(arg);
                let arg_ty = pretty_print_ty(&arg_ty, types, interner);
                s.push_str(&format!("{name}: {}, ", arg_ty));
            }
            let ty = pretty_print_ty(ty, types, interner);
            s.push_str(&format!("returns {ty}"));
            s
        },
        TypedExprKind::TypeConstructor { ty, .. } => format!("type constructor: {}", pretty_print_ty(ty, types, interner)),
        _otherwise => format!("{:?}", typed_expr),
    }
}
