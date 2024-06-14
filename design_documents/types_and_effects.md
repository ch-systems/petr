# Types and Effects

Swim uses a type system that is based on the Hindley-Milner type system, with some modifications. The type system is designed to be expressive, but not overly verbose. The type system is also designed to be easy to read and understand, although the reader should know that sufficiently generic and complex types will be difficult regardless of the language they are being expressed in.

Swim also is an effects-based language. This means that the type system is augmented with the ability to track effects. Effects are things like IO, state, and exceptions. Effects are tracked in association with types, but are not part of the type itself. The effects system has yet to be designed.
