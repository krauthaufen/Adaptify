namespace Adaptify

open System

/// Instructs the PreCompiler to emit a simple property for the given field instead of some adaptive representation.
[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type NonAdaptiveAttribute() = inherit Attribute() 

/// Instructs the PreCompler to emit an aval for the given field.
[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type TreatAsValueAttribute() = inherit Attribute()

/// Instructs the PreCompiler to generate an adaptive-type for this type.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct)>]
type ModelTypeAttribute() = inherit Attribute()

