namespace Adaptify

#nowarn "1337"
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type ElementDelta<'v, 'd> =
    | Insert of 'v
    | Delete
    | Update of 'd

module HashMap =
    let getElementDeltas (a : HashMap<'k, 'v>) (ops : HashMap<'k, ElementOperation<'v>>) (computeDelta : 'v -> 'v -> option< 'd>) =
        HashMap.ApplyDelta(a, ops, fun k v op ->
            match v with
            | ValueNone -> 
                match op with
                | Set v -> 
                    let op = ElementDelta.Insert v |> ValueSome
                    struct(ValueNone, op)
                | _ -> 
                    struct(ValueNone, ValueNone)
            | ValueSome v ->    
                match op with
                | Remove ->
                    struct(ValueNone, ValueSome ElementDelta.Delete)
                | Set nv ->
                    match computeDelta v nv with
                    | Some delta ->
                        struct(ValueNone, ValueSome (ElementDelta.Update delta))
                    | None ->
                        struct(ValueNone, ValueNone)
        ) |> snd
        

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
        member x.Accept(v) = v.Visit(x)
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif
            
    interface aval<'T> with
        member x.GetValue t = x.GetValue t

