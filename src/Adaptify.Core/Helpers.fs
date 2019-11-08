namespace Adaptify

open FSharp.Data.Adaptive
open FSharp.Data.Traceable

type Adaptor<'a, 'ca, 'aa> =
    {
        init    : 'a -> 'ca
        update  : 'ca -> 'a -> 'ca
        view    : 'ca -> 'aa
    }

    member x.IsTrivial = typeof<'a> = typeof<'ca> && typeof<'aa> = typeof<'ca>
    member x.HasView = typeof<'aa> <> typeof<'ca>
    member x.HasTrivialView = typeof<'aa>.IsAssignableFrom typeof<'ca>

module Adaptor =

    let identity<'a> : Adaptor<'a, 'a, 'a> =
        {
            init = id
            update = fun _ v -> v
            view = id
        }
        
    let ref<'a> : Adaptor<'a, ref<'a>, 'a> =
        {
            init = ref
            update = fun r v -> r := v; r
            view = fun r -> !r
        }

    let create (init : 'a -> 'ca) (update : 'ca -> 'a -> 'ca) (view : 'ca -> 'aa) =
        {
            init = init
            update = update
            view = view
        }

    let box (i : Adaptor<'a, 'ca, 'aa>) : Adaptor<'a, obj, 'aa> =
        {
            init = fun a -> i.init a :> obj
            update = fun o a -> i.update (unbox o) a :> obj
            view = fun o -> i.view (unbox o)
        }

    let inline instance< ^a, ^c, ^v when (^a or ^c or ^v) : (static member Adaptor : Adaptor< ^a, ^c, ^v>) > =
        ((^a or ^c or ^v) : (static member Adaptor : Adaptor< ^a, ^c, ^v>) ())



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

    new(initial : HashMap<'K, 'V>, a : Adaptor<'V, 'C, 'A>) =
        ChangeableModelMap(initial, a.init, a.update, a.view)

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









