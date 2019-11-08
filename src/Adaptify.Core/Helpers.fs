namespace Adaptify

#nowarn "1337"
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type private ChangeableModelMapReader<'K, 'C, 'A>(inner : IHashMapReader<'K, 'C>, view : 'C -> 'A) =
    inherit AbstractReader<HashMap<'K, 'A>, HashMapDelta<'K, 'A>>(HashMap.trace)

    override x.Compute(token : AdaptiveToken) =
        inner.GetChanges(token) 
        |> HashMapDelta.toHashMap 
        |> HashMap.map (fun _k op ->
            match op with
            | Set v -> Set (view v)
            | Remove -> Remove
        ) |> HashMapDelta.ofHashMap

type private ChangeableModelListReader<'C, 'A>(inner : IIndexListReader<'C>, view : 'C -> 'A) =
    inherit AbstractReader<IndexList<'A>, IndexListDelta<'A>>(IndexList.trace)

    override x.Compute(token : AdaptiveToken) =
        inner.GetChanges(token) 
        |> IndexListDelta.map (fun _i op ->
            match op with
            | Set v -> Set (view v)
            | Remove -> Remove
        )
   
[<CompilerMessage("ChangeableModelMap should not be used directly", 1337, IsHidden = true)>]
type ChangeableModelMap<'K, 'V, 'C, 'A>(initial : HashMap<'K, 'V>, init : 'V -> 'C, update : 'C -> 'V -> 'C, view : 'C -> 'A) =
    let _current = cval initial
    let store = cmap (initial |> HashMap.map (fun _ v -> init v))
    let content = (store :> amap<_,_>).Content |> AVal.map (HashMap.map (fun _ -> view))

    member x.current = _current

    member x.GetReader() =
        ChangeableModelMapReader(store.GetReader(), view) :> IHashMapReader<_,_>

    member x.update(value : HashMap<'K, 'V>) = 
        if not (_current.Value.ConservativeEquals value) then
            _current.Value <- value
            store.Value <- 
                store.Value.UpdateTo(value, fun _k o v ->
                    match o with
                    | Some o -> update o v
                    | None -> init v
                )

    interface amap<'K, 'A> with
        member x.IsConstant = false
        member x.Content = content
        member x.GetReader() = x.GetReader()

[<CompilerMessage("ChangeableModelList should not be used directly", 1337, IsHidden = true)>]
type ChangeableModelList<'T, 'C, 'A>(initial : IndexList<'T>, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    let _current = cval initial
    let store = clist (initial |> IndexList.map init)
    let content = (store :> alist<_>).Content |> AVal.map (IndexList.map view)

    member x.current = _current

    member x.update(value : IndexList<'T>) = 
        if not (_current.Value.ConservativeEquals value) then
            _current.Value <- value
            store.Value <- 
                store.Value.UpdateTo(value, fun _i o v ->
                    match o with
                    | Some o -> update o v
                    | None -> init v
                )
    member x.GetReader() =
        ChangeableModelListReader((store :> alist<_>).GetReader(), view) :> IIndexListReader<_>

    interface alist<'A> with
        member x.IsConstant = false
        member x.Content = content
        member x.GetReader() = x.GetReader()
 
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

    interface AdaptiveValue with
        member x.GetValueUntyped t = x.GetValue t :> obj
        member x.ContentType =
            #if FABLE_COMPILER
            typeof<obj>
            #else
            typeof<'T>
            #endif
            
    interface aval<'T> with
        member x.GetValue t = x.GetValue t

