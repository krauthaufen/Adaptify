namespace SomeNamespace
open FSharp.Data.Adaptive
[<AutoOpen>]
module rec TestModelAdaptor =
    /// Adaptive representation for `Value`
    type AdaptiveValue private(__initial : SomeNamespace.Value) =
        let _value = cval(__initial.value)
        /// The current value of value as `aval<int>`.
        member __.value = _value :> aval<_>
        /// Updates all values in the `AdaptiveValue` to the given `Value`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : SomeNamespace.Value) : unit =
            let __value = value
            _value.Value <- __value.value
        /// Creates a new `AdaptiveValue` using the given `Value`.
        static member create(value : SomeNamespace.Value) : AdaptiveValue = 
            AdaptiveValue(value)
    type AdaptiveSimpleUnion private(__current : SomeNamespace.SimpleUnion, __value : AdaptiveSimpleUnionConstructor) = 
        inherit AdaptiveObject()
        let mutable __current = __current
        let mutable __value = __value
        member x.GetValue(token: AdaptiveToken) = 
            x.EvaluateAlways token (fun _ -> __value)
        interface aval<AdaptiveSimpleUnionConstructor> with
            member x.IsConstant = false
            member x.GetValue t = x.GetValue t
        static member create(value : SomeNamespace.SimpleUnion) =
            match value with
            |  CaseA(_value) -> AdaptiveSimpleUnion(value, AdaptiveSimpleUnionCaseA(_value))
            |  CaseB(_float, _int) -> AdaptiveSimpleUnion(value, AdaptiveSimpleUnionCaseB(_float, _int))
            |  CaseC -> AdaptiveSimpleUnion(value, AdaptiveSimpleUnionCaseC())
        member x.update(value : SomeNamespace.SimpleUnion) =
            if not (System.Object.ReferenceEquals(__current, value)) then
                __current <- value
                match __value, value with
                | (:? AdaptiveSimpleUnionCaseA as __dst), CaseA(_value) -> __dst.update(_value)
                | (:? AdaptiveSimpleUnionCaseB as __dst), CaseB(_float, _int) -> __dst.update(_float, _int)
                | (:? AdaptiveSimpleUnionCaseC), CaseC -> ()
                | _, CaseA(_value) -> __value <- AdaptiveSimpleUnionCaseA(_value); x.MarkOutdated()
                | _, CaseB(_float, _int) -> __value <- AdaptiveSimpleUnionCaseB(_float, _int); x.MarkOutdated()
                | _, CaseC -> __value <- AdaptiveSimpleUnionCaseC(); x.MarkOutdated()
    and AdaptiveSimpleUnionConstructor =
        abstract member Tag : int
        abstract member IsCaseA : bool
        abstract member IsCaseB : bool
        abstract member IsCaseC : bool
    and private AdaptiveSimpleUnionCaseA(_value : int) =
        let _value = cval(_value)
        member x.value = _value :> aval<_>
        member x.update(_nvalue : int) =
             _value.Value <- _nvalue
        interface AdaptiveSimpleUnionConstructor with
            member x.Tag = 0
            member x.IsCaseA = true
            member x.IsCaseB = false
            member x.IsCaseC = false
    and private AdaptiveSimpleUnionCaseB(_float : float, _int : IndexList<Value>) =
        let _float = cval(_float)
        let _int = clist(_int |> IndexList.map (fun v -> AdaptiveValue.create(v)))
        member x.float = _float :> aval<_>
        member x.int = _int :> alist<_>
        member x.update(_nfloat : float, _nint : IndexList<Value>) =
             _float.Value <- _nfloat
             let updateValue t v =
                 let t = unbox<AdaptiveValue> t
                 t.update(v); t
             let merge k (o : option<_>) (n : option<_>) =
                 match o with
                 | Some t -> 
                     match n with
                     | Some v -> updateValue t v |> Some
                     | None -> None
                 | None ->
                     match n with
                     | Some v -> Some ((fun v -> AdaptiveValue.create(v)) v)
                     | None -> None
             _int.Value <- IndexList.choose2 merge _int.Value _nint
        interface AdaptiveSimpleUnionConstructor with
            member x.Tag = 1
            member x.IsCaseA = false
            member x.IsCaseB = true
            member x.IsCaseC = false
    and private AdaptiveSimpleUnionCaseC() =
        interface AdaptiveSimpleUnionConstructor with
            member x.Tag = 2
            member x.IsCaseA = false
            member x.IsCaseB = false
            member x.IsCaseC = true
    
    let (|AdaptiveCaseA|AdaptiveCaseB|AdaptiveCaseC|) (value : AdaptiveSimpleUnionConstructor) = 
        match value with
        | :? AdaptiveSimpleUnionCaseA as value -> AdaptiveCaseA(value.value)
        | :? AdaptiveSimpleUnionCaseB as value -> AdaptiveCaseB(value.float, value.int)
        | :? AdaptiveSimpleUnionCaseC -> AdaptiveCaseC
        | _ -> failwith "not a union case"
    /// Adaptive representation for `Record`
    type AdaptiveRecord private(__initial : SomeNamespace.Record) =
        let _opt =
            let cell = 
                match __initial.opt with
                | Some value ->
                    let res = cset(value)
                    cval(Some res)
                | None ->
                    cval None
            let inline view inner = inner :> aset<_>
            (cell, AVal.map (Option.map view) cell)
        let _foo = clist(__initial.foo)
        let _bar = clist(__initial.bar |> IndexList.map (fun v -> AdaptiveValue.create(v)))
        let _un = AdaptiveSimpleUnion.create(__initial.un)
        let _list = clist(__initial.list |> IndexList.map (fun v -> AdaptiveSimpleUnion.create(v)))
        /// The current value of opt as `aval<_>`.
        member __.opt = snd _opt
        /// The current value of foo as `alist<int>`.
        member __.foo = _foo :> alist<_>
        /// The current value of bar as `amap<_>`.
        member __.bar = _bar :> alist<_>
        /// The current value of un as `AdaptiveSimpleUnion`.
        member __.un = _un
        /// The current value of list as `amap<_>`.
        member __.list = _list :> alist<_>
        /// Updates all values in the `AdaptiveRecord` to the given `Record`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : SomeNamespace.Record) : unit =
            let __value = value
            let (dst,_) = _opt
            match dst.Value, __value.opt with
            | Some dst, Some value -> 
                dst.Value <- value
            | None, Some value ->
                let res = cset(value)
                dst.Value <- Some res
            | _, None ->
                dst.Value <- None
            _foo.Value <- __value.foo
            let updateValue t v =
                let t = unbox<AdaptiveValue> t
                t.update(v); t
            let merge k (o : option<_>) (n : option<_>) =
                match o with
                | Some t -> 
                    match n with
                    | Some v -> updateValue t v |> Some
                    | None -> None
                | None ->
                    match n with
                    | Some v -> Some ((fun v -> AdaptiveValue.create(v)) v)
                    | None -> None
            _bar.Value <- IndexList.choose2 merge _bar.Value __value.bar
            _un.update(__value.un)
            let updateValue t v =
                let t = unbox<AdaptiveSimpleUnion> t
                t.update(v); t
            let merge k (o : option<_>) (n : option<_>) =
                match o with
                | Some t -> 
                    match n with
                    | Some v -> updateValue t v |> Some
                    | None -> None
                | None ->
                    match n with
                    | Some v -> Some ((fun v -> AdaptiveSimpleUnion.create(v)) v)
                    | None -> None
            _list.Value <- IndexList.choose2 merge _list.Value __value.list
        /// Creates a new `AdaptiveRecord` using the given `Record`.
        static member create(value : SomeNamespace.Record) : AdaptiveRecord = 
            AdaptiveRecord(value)
