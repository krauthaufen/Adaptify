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
        member x.Accept(v) = v.Visit(x)
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif
            
    interface aval<'T> with
        member x.GetValue t = x.GetValue t

type ChangeableListList<'T>(initial : list<'T>) =
    let inner = clist<'T>(IndexList.ofList initial)
    
    member x.Update(value : list<'T>) =
        let ops = IndexList.computeDeltaToList DefaultEqualityComparer.Instance inner.Value value
        inner.Perform ops
    
    interface alist<'T> with
        member x.IsConstant = false
        member x.Content = (inner :> alist<_>).Content
        member x.GetReader() = (inner :> alist<_>).GetReader()
        member x.History = (inner :> alist<_>).History
     
type ChangeableListArray<'T>(initial : array<'T>) =
    let inner = clist<'T>(IndexList.ofArray initial)
    
    member x.Update(value : array<'T>) =
        let ops = IndexList.computeDeltaToArray DefaultEqualityComparer.Instance inner.Value value
        inner.Perform ops
    
    interface alist<'T> with
        member x.IsConstant = false
        member x.Content = (inner :> alist<_>).Content
        member x.GetReader() = (inner :> alist<_>).GetReader()
        member x.History = (inner :> alist<_>).History
      
       
type ChangeableValueCustomEquality<'T>(value : 'T, equality : 'T -> 'T -> bool) =
    inherit AdaptiveObject()
    let mutable value = value
    
    member x.Value
        with get() = value
        and set(v) =
            if not (equality value v) then
                value <- v
                x.MarkOutdated()

    member x.GetValue(t : AdaptiveToken) =
        x.EvaluateAlways t (fun t -> value)
    
    interface IAdaptiveValue with
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.Accept(v) = v.Visit(x)
        member x.ContentType = typeof<'T>
    
    interface aval<'T> with
        member x.GetValue t = x.GetValue t
        