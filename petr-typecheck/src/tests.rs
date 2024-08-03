use expect_test::{expect, Expect};
use petr_resolve::resolve_symbols;
use petr_utils::render_error;

use crate::{constraint_generation::TypeConstraintContext, pretty_printing::*};

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
    let mut type_checker = TypeConstraintContext::new(resolved);
    type_checker.fully_type_check();
    let mut res = pretty_print_type_checker(&type_checker);

    let solved_constraints = match type_checker.into_solution() {
        Ok(solution) => solution.pretty_print(),
        Err(errs) => {
            res.push_str("__ERRORS__\n");
            errs.into_iter().map(|err| format!("{:?}", err)).collect::<Vec<_>>().join("\n")
        },
    };

    res.push('\n');
    res.push_str(&solved_constraints);

    expect.assert_eq(res.trim());
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


                __SOLVED TYPES__
                5: int"#]],
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


                __SOLVED TYPES__
                6: infer t5"#]],
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


                __SOLVED TYPES__
                8: MyType"#]],
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

                __SOLVED TYPES__
                14: int
                17: infer t16
                23: MyType"#]],
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
                literal: 5"#]],
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
                literal: true"#]],
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
                fn string_literal([]) -> string"#]],
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


                __SOLVED TYPES__
                5: string"#]],
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

                SpannedItem UnificationFailure("string", "true") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(52), length: 4 } }]"#]],
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


                __SOLVED TYPES__
                5: string"#]],
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

                SpannedItem UnificationFailure("string", "bool") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(110), length: 14 } }]"#]],
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

                __SOLVED TYPES__
                6: infer t5
                8: infer t7"#]],
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


                __SOLVED TYPES__
                5: (1 | true)
                6: 1"#]],
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

                SpannedItem ArgumentCountMismatch { function: "add", expected: 2, got: 1 } [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(113), length: 8 } }]"#]],
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

                __SOLVED TYPES__
                5: int
                6: int"#]],
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

                SpannedItem UnificationFailure("bool", "int") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(61), length: 2 } }]"#]],
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

                SpannedItem UnificationFailure("1", "unit") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(33), length: 46 } }]"#]],
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

                __SOLVED TYPES__
                5: bool
                6: string"#]],
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

                SpannedItem NotSubtype(["1", "2"], "10") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(104), length: 0 } }]"#]],
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

                SpannedItem NotSubtype(["\"A\"", "\"B\""], "\"c\"") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(97), length: 0 } }]"#]],
    )
}

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

                SpannedItem NotSubtype(["true"], "false") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(100), length: 0 } }]"#]],
    )
}

#[test]
fn disallow_wrong_sum_type_in_add() {
    check(
        r#"
            type IntBelowFive = 1 | 2 | 3 | 4 | 5
            {- reject an `add` which may return an int above five -}
            fn add(a in 'IntBelowFive, b in 'IntBelowFive) returns 'IntBelowFive @add(a, b)
"#,
        expect![[r#"
                type IntBelowFive: IntBelowFive

                fn IntBelowFive: ((1 | 2 | 3 | 4 | 5) → IntBelowFive)
                type constructor: IntBelowFive

                fn add: (IntBelowFive → IntBelowFive → IntBelowFive)
                intrinsic: @add(variable: symbolid3, variable: symbolid4)

                __ERRORS__

                SpannedItem UnificationFailure("int", "IntBelowFive") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(208), length: 2 } }]"#]],
    )
}

// TODO: decide if user-defined types should be able to safely upcast, or if only
// anonymous types should be able to do so
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

                __SOLVED TYPES__
                5: (1 | 2 | 3)
                9: (1 | 2)
                11: (1 | 2)"#]],
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

                __SOLVED TYPES__
                5: (int | string)
                9: int"#]],
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

                SpannedItem NotSubtype(["int", "string"], "bool") [Span { source: SourceId(0), span: SourceSpan { offset: SourceOffset(129), length: 0 } }]"#]],
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


                __SOLVED TYPES__
                5: (int | string)"#]],
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


                __SOLVED TYPES__
                5: (int | string)"#]],
    )
}

#[test]
fn if_exp_basic() {
    check(
        "fn main() returns 'int if true then 1 else 0",
        expect![[r#"
            fn main: int
            if literal: true then literal: 1 else literal: 0

            __MONOMORPHIZED FUNCTIONS__
            fn main([]) -> int

            __SOLVED TYPES__
            5: bool
            6: (0 | 1)
            7: 1"#]],
    );
}
