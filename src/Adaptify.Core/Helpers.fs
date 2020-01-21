namespace Adaptify

#nowarn "1337"
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

[<AbstractClass; CompilerMessage("AdaptiveValue should not be used directly", 1337, IsHidden = true)>]
type AdaptiveValue<'T>() =
    inherit AdaptiveObject()
    let mutable lastValue = Unchecked.defaultof<'T>

    abstract member Compute : AdaptiveToken -> 'T

    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                lastValue <- x.Compute token
            lastValue
        )

    interface IAdaptiveValue with
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif
            
    interface aval<'T> with
        member x.GetValue t = x.GetValue t

