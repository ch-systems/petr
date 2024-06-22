# Syntax Design

This document describes some of the motivations and design principles behind the syntax of petr. The syntax is not yet finalized, and there will likely be iteration upon the syntax in order to better achieve these goals.

## Goals

1. **Readability**
2. **Expressivity**
3. **Ergonomics**
4. **Consistency**

### Readability

Readability as a principle often leads to excessive sigil usage to denote different language constructs. For example, Rust is arguably very readable, but it is _so_ verbose that it is, at times, unreadable. Types very quickly become syntactic messes. For this reason, expressivity is closely related to readability.

In the other direction, languages like Haskell are very minimal in their syntax. Haskell is very expressive, but loses some readability due to the lack of distinction between different language constructs.


petr approaches readability with a few principles:
- **Construct Discriminants**: This is probably the biggest difference between petr and other languages. As a programmer, or reader of code, your brain should not have to perform its own name resolution and/or typecheck pass to know what is going on. For example, in Rust, is `a::b::c` a module, a type, a function, or a variable? In Haskell, is  `a b c` a function call, a type constructor, or a typeclass instance? In most languages, is `A` a type, a function, a variable, or module, or any other symbol?

In petr, this is always clear, and your brain doesn't need contextual knowledge of the codebase to understand what is going on. Anchors accomplish this. Right now, `~` denotes a function application, `'` denotes a type, and `@` denotes an intrinsic.

- **Anchors**: petr uses a minimal amount of sigils to denote different language constructs (we call them _construct discriminants_).

These construct discriminants also accomplish another goal: visual demarcation of the source code. Syntax highlighting is great, but it is not always available or particularly useful. Anchors provide anchor points for your eye to follow, and make it easier to visually parse the code.

A lot of petr's syntax is designed with the concept of anchors in mind. It should always be easy to tell when a function is being called, when a type is being defined, when a type is being instantiated, when declarations are being made, etc.

- **Minimalism**: petr prioritizes simplicity of syntax over explicitness/verbosity. petr is not designed to look like other languages (I've thrown away the concept of a [novelty budget](https://shimweasel.com/2018/08/25/novelty-budgets)), and so there's a lot of freedom to disregard preexisting concepts to free up syntax for other things. For example, single quotes do not produce a `char` -- this frees up `'` as a sigil to denote a type.
