namespace Adaptify

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

type private ChangeableModelListReader<'C, 'A>(inner : IIndexListReader<'C>, view : 'C -> 'A) =
    inherit AbstractReader<IndexList<'A>, IndexListDelta<'A>>(IndexList.trace)

    override x.Compute(token : AdaptiveToken) =
        inner.GetChanges(token) 
        |> IndexListDelta.map (fun _i op ->
            match op with
            | Set v -> Set (view v)
            | Remove -> Remove
        )
   
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
        
type ChangeableModelList =
    static member Create(list : IndexList<'a>, init : 'a -> cval<'a>, update : cval<'a> -> 'a -> cval<'a>, view : cval<'a> -> aval<'a>) =
        ChangeableModelList<'a, 'a, 'a>(list, id, (fun _ v -> v), id)
        
    static member Create(list : IndexList<'a>, init : 'a -> 'b, update : 'b-> 'a -> 'b, view : 'b -> 'c) =
        ChangeableModelList<'a, 'b, 'c>(list, init, update, view)


type ChangeableModelOption<'A, 'CA, 'AA>(initial : option<'A>, init : 'A -> 'CA, update : 'CA -> 'A -> 'CA, view : 'CA -> 'AA) =
    inherit AdaptiveObject()

    let mutable current = initial
    let mutable store = 
        initial |> Option.map (fun v -> 
            let c = init v
            let a = view c
            c, a
        )

    member x.update(v : option<'A>) =
        if not (System.Object.ReferenceEquals(current, v)) then
            current <- v
            match store with
            | Some (c, a) ->
                match v with
                | Some v -> 
                    let c' = update c v
                    if not (Unchecked.equals c' c) then
                        store <- Some(c', view c')
                        x.MarkOutdated()
                | None ->
                    store <- None
                    x.MarkOutdated()
            | None ->
                match v with
                | None -> ()
                | Some v ->
                    let c = init v
                    store <- Some (c, view c)
                    x.MarkOutdated()                                                

    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            match store with
            | Some(_, a) -> Some a
            | None -> None
        )

    interface AdaptiveValue<option<'AA>> with
        member x.GetValue t = x.GetValue t

type Instance<'a, 'ca, 'aa> =
    {
        ainit    : 'a -> 'ca
        aupdate  : 'ca -> 'a -> 'ca
        aview    : 'ca -> 'aa
    }

module Instance =
    let box (i : Instance<'a, 'ca, 'aa>) : Instance<'a, obj, 'aa> =
        {
            ainit = fun a -> i.ainit a :> obj
            aupdate = fun o a -> i.aupdate (unbox o) a :> obj
            aview = fun o -> i.aview (unbox o)
        }








