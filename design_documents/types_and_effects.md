# Types and Effects

petr uses a type system that is based on the Hindley-Milner type system, with some modifications. The type system is designed to be expressive, but not overly verbose. The type system is also designed to be easy to read and understand, although the reader should know that sufficiently generic and complex types will be difficult regardless of the language they are being expressed in.

petr also is an effects-based language. This means that the type system is augmented with the ability to track effects. Effects are things like IO, state, and exceptions. Effects are tracked in association with types, but are not part of the type itself. The effects system has yet to be designed.


## Effects

Effects are evaluated in a pass separate from typechecking. Every expression in a petr program has an expression ID, which can be associated with some effects. Expressions which are composed of other expressions are aware of their effects, and therefore bubble the correct effects up.

For example:

```
function foo(a in 'A) returns 'string
    {- read a string from the user -}
    reads() {- this has the effect "IO" -}
    {- so any call to this function has the effect "IO" -}
```

Some effects are inherently internal. For example, mutation.
```
function foo(a in 'string) returns 'unit
    {- mutate a -}
    a = "hello"
    {- this has the effect "mutation on a" -}
    {- because a is passed in, this effect escapes the function. -}

function bar(a in 'string) returns 'string
  let new_variable = "hello"
  new_variable = "bye" {- this has the effect "mutation on new_variable" -}
  a {- because none of the inputs were mutated, the mutation effect does not escape the function -}
}
```
