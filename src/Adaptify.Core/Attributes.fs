namespace Adaptify

open System
open FSharp.Data.Adaptive

[<Struct>]
type Unpersist<'T, 'AdaptiveT> =
    {
        init        : 'T -> 'AdaptiveT
        update      : 'AdaptiveT -> 'T -> unit
    }

module Unpersist =
    
    let create (init : 'T -> 'AdaptiveT) (update : 'AdaptiveT -> 'T -> unit) =
        { 
            init = init
            update = update
        }

    [<GeneralizableValue>]
    let aval<'T> =
        {
            init = fun v -> cval v :> aval<'T>
            update = fun c v -> (unbox<cval<'T>> c).Value <- v
        }
        
    [<GeneralizableValue>]
    let aset<'T> =
        {
            init = fun v -> cset v :> aset<'T>
            update = fun c v -> (unbox<cset<'T>> c).Value <- v
        }
        
    [<GeneralizableValue>]
    let alist<'T> =
        {
            init = fun v -> clist v :> alist<'T>
            update = fun c v -> (unbox<clist<'T>> c).Value <- v
        }

    [<GeneralizableValue>]
    let amap<'K, 'V> =
        {
            init = fun v -> cmap v :> amap<'K, 'V>
            update = fun c v -> (unbox<cmap<'K, 'V>> c).Value <- v
        }

    let inline instance< ^T, ^AdaptiveT when (^T or ^AdaptiveT) : (static member Unpersist : Unpersist< ^T, ^AdaptiveT >) > =
        ((^T or ^AdaptiveT) : (static member Unpersist : Unpersist< ^T, ^AdaptiveT >) ())



/// Instructs the PreCompiler to emit a simple property for the given field instead of some adaptive representation.
[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type NonAdaptiveAttribute() = inherit Attribute() 

/// Instructs the PreCompler to emit an aval for the given field.
[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property)>]
type TreatAsValueAttribute() = inherit Attribute()

/// Instructs the PreCompiler to generate an adaptive-type for this type.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct)>]
type ModelTypeAttribute() = inherit Attribute()

