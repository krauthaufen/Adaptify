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
type ChangeableModelMap<'K, 'V, 'C, 'A>(map : HashMap<'K, 'V>, init : 'V -> 'C, update : 'C -> 'V -> 'C, view : 'C -> 'A) =
    let mutable map = map 
    let _current = AVal.custom (fun _ -> map)
    let store = cmap (map |> HashMap.map (fun _ v -> init v))
    let content = (store :> amap<_,_>).Content |> AVal.map (HashMap.map (fun _ -> view))

    member x.Current = _current :> aval<_>

    member x.GetReader() =
        match (store :> amap<_,_>).History with
        | Some r ->
            r.NewReader(HashMap.trace, HashMapDelta.toHashMap >> HashMap.map (fun i op -> match op with | Set v -> Set (view v) | Remove -> Remove) >> HashMapDelta.ofHashMap)
        | None ->
            ChangeableModelMapReader(store.GetReader(), view) :> IHashMapReader<_,_>

    member x.Update(value : HashMap<'K, 'V>) = 
        if not (ShallowEqualityComparer<_>.ShallowEquals(map, value)) then
            map <- value
            _current.MarkOutdated()
            store.UpdateTo(value, init, update)

    interface amap<'K, 'A> with
        member x.IsConstant = false
        member x.Content = content
        member x.GetReader() = x.GetReader()
        member x.History = None

[<CompilerMessage("ChangeableModelList should not be used directly", 1337, IsHidden = true)>]
type ChangeableModelList<'T, 'C, 'A>(list : IndexList<'T>, init : 'T -> 'C, update : 'C -> 'T -> 'C, view : 'C -> 'A) =
    let mutable list = list 
    let _current = AVal.custom (fun _ -> list)
    let store = clist (list |> IndexList.map init)
    let content = (store :> alist<_>).Content |> AVal.map (IndexList.map view)

    member x.Current = _current :> aval<_>

    member x.Update(value : IndexList<'T>) = 
        if not (ShallowEqualityComparer<_>.ShallowEquals(list, value)) then
            list <- value
            _current.MarkOutdated()
            store.UpdateTo(value, init, update)

    member x.GetReader() =
        match (store :> alist<_>).History with
        | Some r ->
            r.NewReader(IndexList.trace, IndexListDelta.map (fun i op -> match op with | Set v -> Set (view v) | Remove -> Remove))
        | None ->
            ChangeableModelListReader((store :> alist<_>).GetReader(), view) :> IIndexListReader<_>

    interface alist<'A> with
        member x.IsConstant = false
        member x.Content = content
        member x.GetReader() = x.GetReader()
        member x.History = None
 
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

