//2d7e92ed-6829-f51b-35b0-6be6fc02b564
//796e1e7a-b7b1-bbad-f4c9-b20b72c08313
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec LibraryModel

open System
open FSharp.Data.Adaptive
open Adaptify
open LibraryModel
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveThing(value : Thing) =
    let _name_ = FSharp.Data.Adaptive.cval(value.name)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Thing) = AdaptiveThing(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Thing) -> AdaptiveThing(value)) (fun (adaptive : AdaptiveThing) (value : Thing) -> adaptive.Update(value))
    member __.Update(value : Thing) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Thing>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _name_.Value <- value.name
    member __.Current = __adaptive
    member __.name = _name_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
[<AutoOpen; System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
module ThingLenses = 
    type Thing with
        static member name_ = ((fun (self : Thing) -> self.name), (fun (value : Microsoft.FSharp.Core.string) (self : Thing) -> { self with name = value }))
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveMyUnionCase =
    abstract member Update : MyUnion -> AdaptiveMyUnionCase
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type private AdaptiveMyUnionA(Item : Microsoft.FSharp.Core.int) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.Update(Item : Microsoft.FSharp.Core.int) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Microsoft.FSharp.Core.int>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    interface AdaptiveMyUnionCase with
        member x.Update(value : MyUnion) =
            match value with
            | MyUnion.A(Item) ->
                x.Update(Item)
                x :> AdaptiveMyUnionCase
            | MyUnion.B(Item) -> AdaptiveMyUnionB(Item) :> AdaptiveMyUnionCase
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type private AdaptiveMyUnionB(Item : Microsoft.FSharp.Core.float) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.Update(Item : Microsoft.FSharp.Core.float) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Microsoft.FSharp.Core.float>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    interface AdaptiveMyUnionCase with
        member x.Update(value : MyUnion) =
            match value with
            | MyUnion.A(Item) -> AdaptiveMyUnionA(Item) :> AdaptiveMyUnionCase
            | MyUnion.B(Item) ->
                x.Update(Item)
                x :> AdaptiveMyUnionCase
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveMyUnion(value : MyUnion) =
    inherit Adaptify.AdaptiveValue<AdaptiveMyUnionCase>()
    let mutable __value = value
    let mutable __current =
        match value with
        | MyUnion.A(Item) -> AdaptiveMyUnionA(Item) :> AdaptiveMyUnionCase
        | MyUnion.B(Item) -> AdaptiveMyUnionB(Item) :> AdaptiveMyUnionCase
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : MyUnion) =
        match value with
        | MyUnion.A(Item) -> AdaptiveMyUnionA(Item) :> AdaptiveMyUnionCase
        | MyUnion.B(Item) -> AdaptiveMyUnionB(Item) :> AdaptiveMyUnionCase
    static member Create(value : MyUnion) = AdaptiveMyUnion(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : MyUnion) -> AdaptiveMyUnion(value)) (fun (adaptive : AdaptiveMyUnion) (value : MyUnion) -> adaptive.Update(value))
    member __.Current = __adaptive
    member __.Update(value : MyUnion) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<MyUnion>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<AdaptiveMyUnionCase>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current
[<AutoOpen; System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
module AdaptiveMyUnion = 
    let (|AdaptiveA|AdaptiveB|) (value : AdaptiveMyUnionCase) =
        match value with
        | (:? AdaptiveMyUnionA as a) -> AdaptiveA(a.Item)
        | (:? AdaptiveMyUnionB as b) -> AdaptiveB(b.Item)
        | _ -> failwith "unreachable"
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveObject<'a, '_prima, '_aa>(value : Object<'a>, _primainit : 'a -> System.Object, _primaupdate : System.Object -> 'a -> System.Object, _primaview : System.Object -> '_prima, _ainit : 'a -> System.Object, _aupdate : System.Object -> 'a -> System.Object, _aview : System.Object -> '_aa) =
    let _value_ = _ainit value.value
    let _a_ =
        let inline __arg1 (v : Object<'a>) =
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (_aupdate (unbox<System.Object> o) v)
                o
            AdaptiveObject<'a, '_prima, '_aa>(v, (fun (v : 'a) -> _primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> _primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primaview (unbox<System.Object> o)), (fun (v : 'a) -> _ainit v :> System.Object), __arg5, (fun (o : System.Object) -> _aview (unbox<System.Object> o))) :> System.Object
        let inline __arg2 (o : System.Object) (v : Object<'a>) =
            (unbox<AdaptiveObject<'a, '_prima, '_aa>> o).Update(v)
            o
        let inline __arg4 (v : Object<'a>) =
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (_aupdate (unbox<System.Object> o) v)
                o
            AdaptiveObject<'a, '_prima, '_aa>(v, (fun (v : 'a) -> _primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> _primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primaview (unbox<System.Object> o)), (fun (v : 'a) -> _ainit v :> System.Object), __arg5, (fun (o : System.Object) -> _aview (unbox<System.Object> o))) :> System.Object
        let inline __arg5 (o : System.Object) (v : Object<'a>) =
            (unbox<AdaptiveObject<'a, '_prima, '_aa>> o).Update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.string) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o).Value <- v
            o
        Adaptify.FSharp.Core.AdaptiveResult<LibraryModel.Object<'a>, LibraryModel.AdaptiveObject<'a, '_prima, '_aa>, LibraryModel.AdaptiveObject<'a, '_prima, '_aa>, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>(value.a, __arg1, __arg2, (fun (o : System.Object) -> unbox<AdaptiveObject<'a, '_prima, '_aa>> o), __arg4, __arg5, (fun (o : System.Object) -> unbox<AdaptiveObject<'a, '_prima, '_aa>> o), (fun (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.string> o), (fun (v : Microsoft.FSharp.Core.string) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))
    let _b_ = FSharp.Data.Adaptive.cval(value.b)
    let _map_ =
        let inline __arg1 (v : Object<'a>) =
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (_aupdate (unbox<System.Object> o) v)
                o
            AdaptiveObject<'a, '_prima, '_aa>(v, (fun (v : 'a) -> _primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> _primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primaview (unbox<System.Object> o)), (fun (v : 'a) -> _ainit v :> System.Object), __arg5, (fun (o : System.Object) -> _aview (unbox<System.Object> o)))
        let inline __arg2 (m : AdaptiveObject<'a, '_prima, '_aa>) (v : Object<'a>) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelMap(value.map, __arg1, __arg2, (fun (m : AdaptiveObject<'a, '_prima, '_aa>) -> m))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Object<'a>, _primainit : 'a -> System.Object, _primaupdate : System.Object -> 'a -> System.Object, _primaview : System.Object -> '_prima, _ainit : 'a -> System.Object, _aupdate : System.Object -> 'a -> System.Object, _aview : System.Object -> '_aa) = AdaptiveObject<'a, '_prima, '_aa>(value, _primainit, _primaupdate, _primaview, _ainit, _aupdate, _aview)
    member __.Update(value : Object<'a>) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Object<'a>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            ignore (_aupdate _value_ value.value)
            _a_.Update(value.a)
            _b_.Value <- value.b
            _map_.Update(value.map)
    member __.Current = __adaptive
    member __.value = _aview _value_
    member __.a = _a_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveResultCase<Object<'a>, AdaptiveObject<'a, '_prima, '_aa>, AdaptiveObject<'a, '_prima, '_aa>, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>>
    member __.b = _b_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.map = _map_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, AdaptiveObject<'a, '_prima, '_aa>>
[<AutoOpen; System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
module ObjectLenses = 
    type Object<'a> with
        static member value_ = ((fun (self : Object<'a>) -> self.value), (fun (value : 'a) (self : Object<'a>) -> { self with value = value }))
        static member a_ = ((fun (self : Object<'a>) -> self.a), (fun (value : Microsoft.FSharp.Core.Result<Object<'a>, Microsoft.FSharp.Core.string>) (self : Object<'a>) -> { self with a = value }))
        static member b_ = ((fun (self : Object<'a>) -> self.b), (fun (value : Microsoft.FSharp.Core.float) (self : Object<'a>) -> { self with b = value }))
        static member map_ = ((fun (self : Object<'a>) -> self.map), (fun (value : FSharp.Data.Adaptive.HashMap<Microsoft.FSharp.Core.int, Object<'a>>) (self : Object<'a>) -> { self with map = value }))

