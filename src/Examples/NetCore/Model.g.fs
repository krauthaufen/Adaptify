namespace Model
open FSharp.Data.Adaptive
open Adaptify
[<AutoOpen>]
module rec ModelAdaptor =
    /// Adaptive representation for `Thing`
    type AdaptiveThing private(__initial : Model.Thing) =
        let __current = cval __initial
        let _name = cval(__initial.name)
        member __.current = __current :> aval<_>
        /// The current value of name as `aval<string>`.
        member __.name = _name :> aval<_>
        /// Updates all values in the `AdaptiveThing` to the given `Thing`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Model.Thing) : unit =
            if not (System.Object.ReferenceEquals(__current.Value, value)) then
                __current.Value <- value
                let __value = value
                _name.Value <- __value.name
        /// Creates a new `AdaptiveThing` using the given `Thing`.
        static member create(value : Model.Thing) : AdaptiveThing = 
            AdaptiveThing(value)
    /// Adaptive representation for `Foo`
    type AdaptiveFoo<'a, 'aAdaptive, 'aView> private(__initial : Model.Foo<'a>, inita : 'a -> 'aAdaptive, updatea : 'aAdaptive -> 'a -> 'aAdaptive, viewa : 'aAdaptive -> 'aView) =
        let __current = cval __initial
        let _list = ChangeableModelList(__initial.list, (fun v -> inita(v)), (fun (t : 'aAdaptive) v -> updatea (t) (v)), (fun v -> viewa (v)))
        member __.current = __current :> aval<_>
        /// The current value of list as `alist<_>`.
        member __.list = _list :> alist<_>
        /// Updates all values in the `AdaptiveFoo` to the given `Foo`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Model.Foo<'a>) : unit =
            if not (System.Object.ReferenceEquals(__current.Value, value)) then
                __current.Value <- value
                let __value = value
                _list.update(__value.list)
        /// Creates a new `AdaptiveFoo` using the given `Foo`.
        static member create(value : Model.Foo<'a>, inita : 'a -> 'aAdaptive, updatea : 'aAdaptive -> 'a -> 'aAdaptive, viewa : 'aAdaptive -> 'aView) : AdaptiveFoo<'a, 'aAdaptive, 'aView> = 
            AdaptiveFoo(value, inita, updatea, viewa)
    /// Adaptive representation for `Model`
    type AdaptiveModel private(__initial : Model.Model) =
        let __current = cval __initial
        let _set = cset(__initial.set)
        let _all = ChangeableModelMap(__initial.all, (fun v -> AdaptiveModel.create(v)), (fun (t : AdaptiveModel) v -> t.update(v); t), (fun v -> v))
        let _value = cval(__initial.value)
        let _test = ChangeableModelList(__initial.test, (fun v -> AdaptiveModel.create(v)), (fun (t : AdaptiveModel) v -> t.update(v); t), (fun v -> v))
        let _foo = cval(__initial.foo)
        let _bar = cmap __initial.bar
        let _nested =
            let inline inita v = v
            let inline updatea _t v = v
            let inline viewa v = v
            AdaptiveFoo.create(__initial.nested, inita, updatea, viewa)
        member __.current = __current :> aval<_>
        /// The current value of set as `aset<int>`.
        member __.set = _set :> aset<_>
        /// The current value of all as `amap<_,_>`.
        member __.all = _all :> amap<_,_>
        /// The current value of value as `aval<int>`.
        member __.value = _value :> aval<_>
        /// The current value of test as `alist<_>`.
        member __.test = _test :> alist<_>
        /// The current value of foo as `aval<string>`.
        member __.foo = _foo :> aval<_>
        /// The current value of bar as `amap<int, string>`.
        member __.bar = _bar :> amap<_,_>
        /// The current value of nested as `AdaptiveFoo`.
        member __.nested = _nested
        /// Updates all values in the `AdaptiveModel` to the given `Model`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Model.Model) : unit =
            if not (System.Object.ReferenceEquals(__current.Value, value)) then
                __current.Value <- value
                let __value = value
                _set.Value <- __value.set
                _all.update(__value.all)
                _value.Value <- __value.value
                _test.update(__value.test)
                _foo.Value <- __value.foo
                _bar.Value <- __value.bar
                _nested.update(__value.nested)
        /// Creates a new `AdaptiveModel` using the given `Model`.
        static member create(value : Model.Model) : AdaptiveModel = 
            AdaptiveModel(value)
    type AdaptiveMyUnion private(__current : Model.MyUnion, __value : AdaptiveMyUnionConstructor) = 
        inherit AdaptiveObject()
        let mutable __current = __current
        let mutable __value = __value
        member x.GetValue(token: AdaptiveToken) = 
            x.EvaluateAlways token (fun _ -> __value)
        interface aval<AdaptiveMyUnionConstructor> with
            member x.IsConstant = false
            member x.GetValue t = x.GetValue t
        static member create(value : Model.MyUnion) =
            match value with
            |  CaseA(_value, _dst) -> AdaptiveMyUnion(value, AdaptiveMyUnionCaseA(_value, _dst))
            |  CaseB(_Item) -> AdaptiveMyUnion(value, AdaptiveMyUnionCaseB(_Item))
        member x.update(value : Model.MyUnion) =
            if not (System.Object.ReferenceEquals(__current, value)) then
                __current <- value
                match __value, value with
                | (:? AdaptiveMyUnionCaseA as __dst), CaseA(_value, _dst) -> __dst.update(_value, _dst)
                | (:? AdaptiveMyUnionCaseB as __dst), CaseB(_Item) -> __dst.update(_Item)
                | _, CaseA(_value, _dst) -> __value <- AdaptiveMyUnionCaseA(_value, _dst); x.MarkOutdated()
                | _, CaseB(_Item) -> __value <- AdaptiveMyUnionCaseB(_Item); x.MarkOutdated()
    and AdaptiveMyUnionConstructor =
        abstract member Tag : int
        abstract member IsCaseA : bool
        abstract member IsCaseB : bool
    and private AdaptiveMyUnionCaseA(_value : int, _dst : float) =
        let _value = cval(_value)
        let _dst = cval(_dst)
        member x.value = _value :> aval<_>
        member x.dst = _dst :> aval<_>
        member x.update(_nvalue : int, _ndst : float) =
             _value.Value <- _nvalue
             _dst.Value <- _ndst
        interface AdaptiveMyUnionConstructor with
            member x.Tag = 0
            member x.IsCaseA = true
            member x.IsCaseB = false
    and private AdaptiveMyUnionCaseB(_Item : Model) =
        let _Item = AdaptiveModel.create(_Item)
        member x.Item = _Item
        member x.update(_nItem : Model) =
             _Item.update(_nItem)
        interface AdaptiveMyUnionConstructor with
            member x.Tag = 1
            member x.IsCaseA = false
            member x.IsCaseB = true
    
    let (|AdaptiveCaseA|AdaptiveCaseB|) (value : AdaptiveMyUnionConstructor) = 
        match value with
        | :? AdaptiveMyUnionCaseA as value -> AdaptiveCaseA(value.value, value.dst)
        | :? AdaptiveMyUnionCaseB as value -> AdaptiveCaseB(value.Item)
        | _ -> failwith "not a union case"
