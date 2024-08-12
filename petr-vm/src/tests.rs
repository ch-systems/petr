use expect_test::{expect, Expect};
use petr_ir::Lowerer;
use petr_resolve::resolve_symbols;
use petr_typecheck::type_check;
use petr_utils::render_error;

use super::*;

fn check_no_stdlib(
    input: impl Into<String>,
    expect: Expect,
) {
    let input = input.into();
    let mut sources = vec![];
    sources.push(("test", input.clone()));
    check_inner(sources, expect);
}
fn check_inner(
    sources: Vec<(&str, String)>,
    expect: Expect,
) {
    let parser = petr_parse::Parser::new(sources);
    let (ast, errs, interner, source_map) = parser.into_result();
    if !errs.is_empty() {
        errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
        panic!("build failed: code didn't parse");
    }
    let (errs, resolved) = resolve_symbols(ast, interner, Default::default());
    if !errs.is_empty() {
        dbg!(&errs);
        panic!("build failed: resolution");
    }
    let solution = match type_check(resolved) {
        Ok(s) => s,
        Err(type_errs) => {
            type_errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("build failed: code didn't type check");
        },
    };

    let lowerer = match Lowerer::new(solution) {
        Ok(l) => l,
        Err(err) => panic!("lowering failed: {err:?}"),
    };

    let mut res = String::new();

    if !lowerer.errors().is_empty() {
        lowerer
            .errors()
            .iter()
            .for_each(|err| eprintln!("{:?}", render_error(&source_map, err.clone())));
        panic!("build failed: code didn't lower");
    }

    println!("pretty printed lowerer is: {}", lowerer.pretty_print());
    let (data, ir) = lowerer.finalize();
    let vm = Vm::new(ir, data);
    let (vm_res, _stack, logs) = match vm.run() {
        Ok(o) => o,
        Err(err) => panic!("vm returned error: {err:?}"),
    };

    res.push_str(&format!("{vm_res:?}"));

    if !logs.is_empty() {
        res.push_str("\n___LOGS___\n");

        res.push_str(&logs.join("\n"));
    }

    expect.assert_eq(&res);
}

fn check(
    input: impl Into<String>,
    expect: Expect,
) {
    let input = input.into();
    let mut sources = petr_stdlib::stdlib().into_iter().map(|(k, v)| (k, v.to_string())).collect::<Vec<_>>();
    sources.push(("test", input));
    check_inner(sources, expect);
}

#[test]
fn size_of_int() {
    check(
        r#"
fn main() returns 'int
    ~std.mem.size_of(42)
    "#,
        expect!["Value(8)"],
    )
}

/* uncomment when aggregate type memory layout works
   #[test]
    fn size_of_aggregate_type() {
        check(
            r#"
type Foo = FooBar a 'int b 'int c 'int
fn main() returns 'int
    let a = ~FooBar 12, 12, 12
    ~std.mem.size_of(a)
    "#,
            expect!["Value(32)"],
        )
    }
    */

#[test]
#[should_panic]
fn passthrough_type_should_fail() {
    check(
        r#"
fn main() returns 'int
    ~f(true)

fn f(expr in 'A) returns 'int 
  expr
    "#,
        expect![""],
    )
}

#[test]
fn let_bindings() {
    check(
        r#"
fn hi(x in 'int, y in 'int) returns 'int
    let a = x;
        b = y;
        c = 20;
        d = 30;
        e = 12;
    a
fn main() returns 'int ~hi(42, 3)
"#,
        expect!["Value(42)"],
    )
}
#[test]
fn import_call() {
    check(
        r#"
import std.io.print

fn main() returns 'unit 
  ~print("hello, world!")
  "#,
        expect![[r#"
                Value(0)
                ___LOGS___
                hello, world!   "#]],
    )
}

#[test]
fn addition() {
    check(
        r#"
            fn hi(x in 'int, y in 'int) returns 'int
    let a = x;
        b = y;
        c = 20;
        d = 30;
        e = 42;
    + a + b + c + d e

fn main() returns 'int ~hi(1, 3)
"#,
        expect!("Value(96)"),
    )
}

#[test]
fn addition_path_res() {
    check(
        r#"
            fn hi(x in 'int, y in 'int) returns 'int
    let a = x;
        b = y;
        c = 20;
        d = 30;
        e = 42;
    ~std.ops.add(a,  + b + c + d e)

fn main() returns 'int ~hi(1, 3)
"#,
        expect!("Value(96)"),
    )
}

#[test]
fn subtraction() {
    check(
        r#"
            fn hi(x in 'int) returns 'int
    let a = + x 1;
        b = - x 1;
        c = - 20 x;
        d = + 20 x
        d

fn main() returns 'int ~hi(100)
"#,
        expect!("Value(120)"),
    )
}

#[test]
fn overflowing_sub() {
    check(
        r#"
fn main() returns 'int - 0 1
"#,
        expect!("Value(18446744073709551615)"),
    )
}

#[test]
fn basic_malloc() {
    check(
        r#"
fn main() returns 'int
    let a = @malloc 1
    let b = @malloc 1
    let c = @malloc 5
    let d = @malloc 1
    d
"#,
        expect!("Value(7)"),
    )
}

#[test]
fn ptr_mem() {
    check(
        r#"
fn main() returns 'Ptr
    let pointer = ~std.mem.malloc(20);
    let pointer2 = ~std.mem.malloc(20);
    side_effect = ~std.io.print "Hello, World!"

    pointer2
      "#,
        expect!([r#"
                Value(56)
                ___LOGS___
                Hello, World!   "#]),
    )
}

#[test]
fn if_exp_basic() {
    check(
        r#"
fn main() returns 'int if true then 1 else 0
"#,
        expect!("Value(1)"),
    )
}

#[test]
fn if_exp_nested() {
    check(
        r#"
fn main() returns 'int if true then if false then 1 else 0 else 0
"#,
        expect!("Value(0)"),
    )
}

#[test]
fn int_to_char() {
    check(
        r#"
        fn int_to_char(i in 'int) returns 'string
            if = i 0 then "a"
            else if = i 1 then "b"
            else if = i 2 then "c"
            else if = i 3 then "d"
            else "unknown"

            {- just use puts intrinsic for now because it is the only way
             to generate a unit type -}
        {- TODO: add unit type expr to the language -} 
        fn fold(i in 'list) returns 'unit @puts ""

        fn main() returns 'unit
            ~fold [
                ~std.io.print(~int_to_char(0)),
                ~std.io.print(~int_to_char(1)),
                ~std.io.print(~int_to_char(2)),
                ~std.io.print(~int_to_char(3)),
            ]
            "#,
        expect!([r#"
            Value(0)
            ___LOGS___
            a       
            b       
            c       
            d       
        "#]),
    )
}

mod constant_type_tests {
    //! These tests cover constant literal types
    use super::*;

    #[test]
    fn basic_int() {
        check(
            r#"
            type OneOrTwo = 1 | 2

            fn main() returns 'OneOrTwo
                ~OneOrTwo 1
                "#,
            expect![[r#"Value(1)"#]],
        )
    }

    // TODO: this should work. 'List is recursive, which is causing a type system failure.
    #[test]
    fn repro() {
        check(
            r#"fn main() returns 'int 
  + 2 40


type List = Initialized head 'A tail 'List | Uninitialized"#,
            expect!["Value(42)"],
        )
    }

    #[test]
    fn repro_2() {
        check(
            r#"
            fn main() returns 'int 
  + 40 2 


type List = Initialized head 'A tail 'Tail | Uninitialized

type Tail = ListTail list 'List
            "#,
            expect!["Value(42)"],
        )
    }

    // TODO: Support accessing fields of records/product types
    #[test]
    #[ignore]
    fn nth_elem_in_list() {
        check(
            r#"
        type List = Node head 'A tail 'List 
                  | Leaf

        fn nth_elem(n in 'int, list in 'List) returns 'A
            if = n 0 then list.head else ~nth_elem(- n 1, list.tail)
                  "#,
            expect![[r#""#]],
        )
    }

    #[test]
    fn fibonacci() {
        check(
            r#"
{- Try running "format" on the right side of the screen to format this code! -}

{- Calculates the nth number in the fibonacci sequence -}
fn fibonacci_sequence(n in 'int) returns 'int
  if = n 0 then 0
  else if = n 1 then 1
  else 
        + (~fibonacci_sequence ( - n 1 ) ) (~fibonacci_sequence (- n 2 )) 

fn main() returns 'int ~fibonacci_sequence(10)
            "#,
            expect![[r#""#]],
        )
    }

    #[test]
    fn basic_recursion() {
        check(
            r#"
fn repro(n in 'int) returns 'int
  if = n 42 then n
  else 
    ~repro(+ n 1)
fn main() returns 'int ~repro(5)
            "#,
            expect!["Value(42)"],
        )
    }

    #[test]
    fn basic_recursion_with_let_binding() {
        check(
            r#"
fn repro(n in 'int) returns 'int
  if = n 42 then n
  else 
    let res = ~repro(+ n 1)
    res
fn main() returns 'int ~repro(5)
            "#,
            expect!["Value(42)"],
        )
    }

    #[test]
    fn basic_recursion_with_let_binding_and_add() {
        check(
            r#"
fn repro(n in 'int) returns 'int
  if = n 42 then n
  else 
     ~repro(+ n 1)
fn main() returns 'int 
 + ~repro(5) ~repro(5)
            "#,
            expect!["Value(84)"],
        )
    }

    #[test]
    fn basic_recursion_with_else_if() {
        check(
            r#"
fn repro(n in 'int) returns 'int
  if = n 0 then 4242
  else if = n 1 then 4243
  else 
    ~repro(- n 1)
fn main() returns 'int ~repro(5)
            "#,
            expect!["Value(4243)"],
        )
    }

    #[test]
    fn basic_recursion_with_add_result() {
        check(
            r#"
fn repro(n in 'int) returns 'int
  if = n 0 then 42
  else 
    ~repro(- n 1)
fn main() returns 'int + (~repro(5)) (~repro(5))
            "#,
            expect!["Value(84)"],
        )
    }

    #[test]
    fn basic_recursion_with_else_if_and_add_result() {
        check_no_stdlib(
            r#"
fn repro(n in 'int) returns 'int
  if @equals(n, 0) then 0
  else if @equals( n,0) then 0
  else 
    let a = ~repro(@subtract(n ,1));
        b = ~repro(2);
    5
fn main() returns 'int ~repro(5)
            "#,
            expect![[r#""#]],
        )
    }
}
