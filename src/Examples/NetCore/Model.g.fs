//fb7a4f46-72e2-87d7-0cf1-62726254f712
//b62f7cd5-09d1-2f15-0058-78cac906d62d
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
type AdaptiveThing(value : Thing) =
    let _name_ = FSharp.Data.Adaptive.cval(value.name)
    let mutable __value = value
    member __.update(value : Thing) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Thing>.ShallowEquals(value, __value))) then
            __value <- value
            _name_.Value <- value.name
    member __.name = _name_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
type AdaptiveFoo<'a, 'paa, 'aa>(value : Foo<'a>, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa) =
    let _value_ = ainit value.value
    let _list_ = Adaptify.ChangeableModelList(value.list, (fun (v : 'a) -> primainit v), (fun (m : System.Object) (v : 'a) -> primaupdate m v), (fun (m : System.Object) -> primaview m))
    let mutable __value = value
    member __.update(value : Foo<'a>) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Foo<'a>>.ShallowEquals(value, __value))) then
            __value <- value
            ignore (aupdate _value_ value.value)
            _list_.update(value.list)
    member __.value = aview _value_
    member __.list = _list_ :> FSharp.Data.Adaptive.alist<'paa>
type AdaptiveModel(value : Model) =
    let _set_ = FSharp.Data.Adaptive.cset(value.set)
    let _all_ =
        let inline __arg2 (m : AdaptiveModel) (v : Model) =
            m.update(v)
            m
        Adaptify.ChangeableModelMap(value.all, (fun (v : Model) -> AdaptiveModel(v)), __arg2, (fun (m : AdaptiveModel) -> m))
    let _value_ = FSharp.Data.Adaptive.cval(value.value)
    let _test_ =
        let inline __arg2 (m : AdaptiveModel) (v : Model) =
            m.update(v)
            m
        Adaptify.ChangeableModelList(value.test, (fun (v : Model) -> AdaptiveModel(v)), __arg2, (fun (m : AdaptiveModel) -> m))
    let _foo_ = FSharp.Data.Adaptive.cval(value.foo)
    let _bar_ = FSharp.Data.Adaptive.cmap(value.bar)
    let _nested_ =
        let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
            o
        AdaptiveFoo<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>(value.nested, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>))
    let mutable __value = value
    member __.update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            _set_.Value <- value.set
            _all_.update(value.all)
            _value_.Value <- value.value
            _test_.update(value.test)
            _foo_.Value <- value.foo
            _bar_.Value <- value.bar
            _nested_.update(value.nested)
    member __.set = _set_ :> FSharp.Data.Adaptive.aset<Microsoft.FSharp.Core.int>
    member __.all = _all_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, AdaptiveModel>
    member __.value = _value_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.test = _test_ :> FSharp.Data.Adaptive.alist<AdaptiveModel>
    member __.foo = _foo_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    member __.bar = _bar_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.string>
    member __.nested = _nested_
type AdaptiveMyUnionCase =
    abstract member update : MyUnion -> AdaptiveMyUnionCase
type private AdaptiveMyUnionCaseA(value : Microsoft.FSharp.Core.int, dst : Microsoft.FSharp.Core.float) =
    let _value_ = FSharp.Data.Adaptive.cval(value)
    let _dst_ = FSharp.Data.Adaptive.cval(dst)
    let mutable __dst = dst
    let mutable __value = value
    member __.update(value : Microsoft.FSharp.Core.int, dst : Microsoft.FSharp.Core.float) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Microsoft.FSharp.Core.int>.ShallowEquals(value, __value) && Adaptify.ShallowEqualityComparer<Microsoft.FSharp.Core.float>.ShallowEquals(dst, __dst))) then
            __value <- value
            __dst <- dst
            _value_.Value <- value
            _dst_.Value <- dst
    member __.value = _value_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.dst = _dst_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    interface AdaptiveMyUnionCase with
        member x.update(value : MyUnion) =
            match value with
            | MyUnion.CaseA(value, dst) ->
                x.update(value, dst)
                x :> AdaptiveMyUnionCase
            | MyUnion.CaseB(Item) -> AdaptiveMyUnionCaseB(Item) :> AdaptiveMyUnionCase
type private AdaptiveMyUnionCaseB(Item : Model) =
    let _Item_ = AdaptiveModel(Item)
    let mutable __Item = Item
    member __.update(Item : Model) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Model>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            _Item_.update(Item)
    member __.Item = _Item_
    interface AdaptiveMyUnionCase with
        member x.update(value : MyUnion) =
            match value with
            | MyUnion.CaseA(value, dst) -> AdaptiveMyUnionCaseA(value, dst) :> AdaptiveMyUnionCase
            | MyUnion.CaseB(Item) ->
                x.update(Item)
                x :> AdaptiveMyUnionCase
type AdaptiveMyUnion(value : MyUnion) =
    inherit Adaptify.AdaptiveValue<AdaptiveMyUnionCase>()
    let mutable __value =
        match value with
        | MyUnion.CaseA(value, dst) -> AdaptiveMyUnionCaseA(value, dst) :> AdaptiveMyUnionCase
        | MyUnion.CaseB(Item) -> AdaptiveMyUnionCaseB(Item) :> AdaptiveMyUnionCase
    static member CreateAdaptiveCase(value : MyUnion) =
        match value with
        | MyUnion.CaseA(value, dst) -> AdaptiveMyUnionCaseA(value, dst) :> AdaptiveMyUnionCase
        | MyUnion.CaseB(Item) -> AdaptiveMyUnionCaseB(Item) :> AdaptiveMyUnionCase
    member __.update(value : MyUnion) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveMyUnionCase>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value
[<AutoOpen>]
module AdaptiveMyUnion = 
    let (|AdaptiveCaseA|AdaptiveCaseB|) (value : AdaptiveMyUnionCase) =
        match value with
        | (:? AdaptiveMyUnionCaseA as CaseA) -> AdaptiveCaseA(CaseA.value, CaseA.dst)
        | (:? AdaptiveMyUnionCaseB as CaseB) -> AdaptiveCaseB(CaseB.Item)
        | _ -> failwith "unreachable"

