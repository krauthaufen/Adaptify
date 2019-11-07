//dc55f503-8332-08ea-3b06-c6811a815089
//f3d0ee79-6beb-d135-a56c-59002bdf4102
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
type AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab> =
    abstract member update : MyUnion<'a, 'b> -> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
type private AdaptiveMyUnionCaseA<'a, 'paa, 'aa, 'b, 'pab, 'ab>(value : Microsoft.FSharp.Core.int, dst : 'a, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
    let _value_ = FSharp.Data.Adaptive.cval(value)
    let _dst_ = ainit dst
    let mutable __dst = dst
    let mutable __value = value
    member __.update(value : Microsoft.FSharp.Core.int, dst : 'a) =
        if Microsoft.FSharp.Core.Operators.not((Microsoft.FSharp.Core.Operators.Unchecked.equals value __value && System.Object.ReferenceEquals(dst, __dst))) then
            __value <- value
            __dst <- dst
            _value_.Value <- value
            ignore (aupdate _dst_ dst)
    member __.value = _value_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.dst = aview _dst_
    interface AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab> with
        member x.update(value : MyUnion<'a, 'b>) =
            match value with
            | MyUnion.CaseA(value, dst) ->
                x.update(value, dst)
                x :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
            | MyUnion.CaseB(Item) ->
                let inline __arg5 (o : System.Object) (v : 'a) =
                    ignore (aupdate (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'b) =
                    ignore (bupdate (unbox<System.Object> o) v)
                    o
                AdaptiveMyUnionCaseB(Item, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg5, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg11, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
type private AdaptiveMyUnionCaseB<'a, 'paa, 'aa, 'b, 'pab, 'ab>(Item : 'b, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
    let _Item_ = binit Item
    let mutable __Item = Item
    member __.update(Item : 'b) =
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(Item, __Item))) then
            __Item <- Item
            ignore (bupdate _Item_ Item)
    member __.Item = bview _Item_
    interface AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab> with
        member x.update(value : MyUnion<'a, 'b>) =
            match value with
            | MyUnion.CaseA(value, dst) ->
                let inline __arg6 (o : System.Object) (v : 'a) =
                    ignore (aupdate (unbox<System.Object> o) v)
                    o
                let inline __arg12 (o : System.Object) (v : 'b) =
                    ignore (bupdate (unbox<System.Object> o) v)
                    o
                AdaptiveMyUnionCaseA(value, dst, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg6, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg12, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
            | MyUnion.CaseB(Item) ->
                x.update(Item)
                x :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
type AdaptiveMyUnion<'a, 'paa, 'aa, 'b, 'pab, 'ab>(value : MyUnion<'a, 'b>, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
    inherit FSharp.Data.Adaptive.AdaptiveObject()
    let mutable __value =
        match value with
        | MyUnion.CaseA(value, dst) ->
            let inline __arg6 (o : System.Object) (v : 'a) =
                ignore (aupdate (unbox<System.Object> o) v)
                o
            let inline __arg12 (o : System.Object) (v : 'b) =
                ignore (bupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyUnionCaseA(value, dst, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg6, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg12, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
        | MyUnion.CaseB(Item) ->
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (aupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'b) =
                ignore (bupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyUnionCaseB(Item, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg5, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg11, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
    static member CreateAdaptiveCase(value : MyUnion<'a, 'b>, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
        match value with
        | MyUnion.CaseA(value, dst) ->
            let inline __arg6 (o : System.Object) (v : 'a) =
                ignore (aupdate (unbox<System.Object> o) v)
                o
            let inline __arg12 (o : System.Object) (v : 'b) =
                ignore (bupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyUnionCaseA(value, dst, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg6, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg12, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
        | MyUnion.CaseB(Item) ->
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (aupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'b) =
                ignore (bupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyUnionCaseB(Item, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg5, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg11, (fun (o : System.Object) -> bview (unbox<System.Object> o))) :> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
    member __.update(value : MyUnion<'a, 'b>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    interface FSharp.Data.Adaptive.aval<AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>> with
        member x.GetValue(t : FSharp.Data.Adaptive.AdaptiveToken) = x.EvaluateAlways t (fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value)
[<AutoOpen>]
module AdaptiveMyUnion = 
    let (|AdaptiveCaseA|AdaptiveCaseB|) (value : AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>) =
        match value with
        | (:? AdaptiveMyUnionCaseA<'a, 'paa, 'aa, 'b, 'pab, 'ab> as CaseA) ->
            AdaptiveCaseA(CaseA.value, CaseA.dst)
        | (:? AdaptiveMyUnionCaseB<'a, 'paa, 'aa, 'b, 'pab, 'ab> as CaseB) ->
            AdaptiveCaseB(CaseB.Item)
        | _ ->
            failwith "unreachable"
type AdaptiveGeny<'a, 'paa, 'aa, 'b, 'pab, 'ab>(value : Geny<'a, 'b>, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
    let _a_ =
        let inline __arg5 (o : System.Object) (v : 'a) =
            ignore (aupdate (unbox<System.Object> o) v)
            o
        let inline __arg11 (o : System.Object) (v : 'b) =
            ignore (bupdate (unbox<System.Object> o) v)
            o
        AdaptiveMyUnion<'a, 'paa, 'aa, 'b, 'pab, 'ab>(value.a, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg5, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg11, (fun (o : System.Object) -> bview (unbox<System.Object> o)))
    let _b_ =
        let inline __arg1 (v : MyUnion<'a, 'b>) =
            let inline __arg5 (o : System.Object) (v : 'a) =
                ignore (aupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'b) =
                ignore (bupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyUnion<'a, 'paa, 'aa, 'b, 'pab, 'ab>.CreateAdaptiveCase(v, (fun (v : 'a) -> primainit v :> System.Object), (fun (o : System.Object) (v : 'a) -> primaupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primaview (unbox<System.Object> o)), (fun (v : 'a) -> ainit v :> System.Object), __arg5, (fun (o : System.Object) -> aview (unbox<System.Object> o)), (fun (v : 'b) -> primbinit v :> System.Object), (fun (o : System.Object) (v : 'b) -> primbupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primbview (unbox<System.Object> o)), (fun (v : 'b) -> binit v :> System.Object), __arg11, (fun (o : System.Object) -> bview (unbox<System.Object> o)))
        Adaptify.ChangeableModelList(value.b, __arg1, (fun (m : AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>) (v : MyUnion<'a, 'b>) -> m.update(v)), (fun (m : AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>) -> m))
    let mutable __value = value
    member __.update(value : Geny<'a, 'b>) =
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(value, __value))) then
            __value <- value
            _a_.update(value.a)
            _b_.update(value.b)
    member __.a = _a_ :> FSharp.Data.Adaptive.aval<AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>>
    member __.b = _b_ :> FSharp.Data.Adaptive.alist<AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>>
type AdaptiveSeppy<'a, 'paa, 'aa>(value : Seppy<'a>, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa) =
    let _x_ = ainit value.x
    let _y_ = Adaptify.ChangeableModelList(value.y, (fun (v : 'a) -> primainit v), (fun (m : System.Object) (v : 'a) -> primaupdate m v), (fun (m : System.Object) -> primaview m))
    let mutable __value = value
    member __.update(value : Seppy<'a>) =
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(value, __value))) then
            __value <- value
            ignore (aupdate _x_ value.x)
            _y_.update(value.y)
    member __.x = aview _x_
    member __.y = _y_ :> FSharp.Data.Adaptive.alist<'paa>
type AdaptiveRecy(value : Recy) =
    let _f_ =
        let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
            o
        let inline __arg11 (o : System.Object) (v : Recy) =
            (unbox<AdaptiveRecy> o).update(v)
            o
        AdaptiveGeny<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Recy, AdaptiveRecy, AdaptiveRecy>(value.f, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>), (fun (v : Recy) -> AdaptiveRecy(v) :> System.Object), (fun (o : System.Object) (v : Recy) -> (unbox<AdaptiveRecy> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveRecy> o), (fun (v : Recy) -> AdaptiveRecy(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<AdaptiveRecy> o))
    let _g_ =
        let inline __arg1 (v : MyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.float>) =
            let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
                (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
                o
            let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.float) =
                (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.float>> o).Value <- v
                o
            AdaptiveMyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>.CreateAdaptiveCase(v, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>), (fun (v : Microsoft.FSharp.Core.float) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.float) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.float> o), (fun (v : Microsoft.FSharp.Core.float) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.float>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>)) :> System.Object
        let inline __arg4 (v : MyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.float>) =
            let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
                (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
                o
            let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.float) =
                (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.float>> o).Value <- v
                o
            AdaptiveMyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>(v, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>), (fun (v : Microsoft.FSharp.Core.float) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.float) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.float> o), (fun (v : Microsoft.FSharp.Core.float) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.float>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>)) :> System.Object
        let inline __arg5 (o : System.Object) (v : MyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.float>) =
            (unbox<AdaptiveMyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>> o).update(v)
            o
        AdaptiveSeppy<MyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.float>, AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>, FSharp.Data.Adaptive.aval<AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>>>(value.g, __arg1, (fun (o : System.Object) (v : MyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.float>) -> (unbox<AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>> o), __arg4, __arg5, (fun (o : System.Object) -> unbox<AdaptiveMyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>> o :> FSharp.Data.Adaptive.aval<AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Microsoft.FSharp.Core.float, Microsoft.FSharp.Core.float, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>>>))
    let mutable __value = value
    member __.update(value : Recy) =
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(value, __value))) then
            __value <- value
            _f_.update(value.f)
            _g_.update(value.g)
    member __.f = _f_
    member __.g = _g_
type AdaptivefffCase =
    abstract member update : fff -> AdaptivefffCase
type private AdaptivefffGgg(Item : Microsoft.FSharp.Core.int) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.update(Item : Microsoft.FSharp.Core.int) =
        if Microsoft.FSharp.Core.Operators.not((Microsoft.FSharp.Core.Operators.Unchecked.equals Item __Item)) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) ->
                x.update(Item)
                x :> AdaptivefffCase
            | fff.Aaa(Item) ->
                AdaptivefffAaa(Item) :> AdaptivefffCase
            | fff.YYY ->
                AdaptivefffYYY() :> AdaptivefffCase
type private AdaptivefffAaa(Item : Microsoft.FSharp.Core.string) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.update(Item : Microsoft.FSharp.Core.string) =
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(Item, __Item))) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) ->
                AdaptivefffGgg(Item) :> AdaptivefffCase
            | fff.Aaa(Item) ->
                x.update(Item)
                x :> AdaptivefffCase
            | fff.YYY ->
                AdaptivefffYYY() :> AdaptivefffCase
type private AdaptivefffYYY() =
    member __.update() = ()
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) ->
                AdaptivefffGgg(Item) :> AdaptivefffCase
            | fff.Aaa(Item) ->
                AdaptivefffAaa(Item) :> AdaptivefffCase
            | fff.YYY ->
                x.update()
                x :> AdaptivefffCase
type Adaptivefff(value : fff) =
    inherit FSharp.Data.Adaptive.AdaptiveObject()
    let mutable __value =
        match value with
        | fff.Ggg(Item) ->
            AdaptivefffGgg(Item) :> AdaptivefffCase
        | fff.Aaa(Item) ->
            AdaptivefffAaa(Item) :> AdaptivefffCase
        | fff.YYY ->
            AdaptivefffYYY() :> AdaptivefffCase
    static member CreateAdaptiveCase(value : fff) =
        match value with
        | fff.Ggg(Item) ->
            AdaptivefffGgg(Item) :> AdaptivefffCase
        | fff.Aaa(Item) ->
            AdaptivefffAaa(Item) :> AdaptivefffCase
        | fff.YYY ->
            AdaptivefffYYY() :> AdaptivefffCase
    member __.update(value : fff) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((System.Object.ReferenceEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    interface FSharp.Data.Adaptive.aval<AdaptivefffCase> with
        member x.GetValue(t : FSharp.Data.Adaptive.AdaptiveToken) = x.EvaluateAlways t (fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value)
[<AutoOpen>]
module Adaptivefff = 
    let (|AdaptiveGgg|AdaptiveAaa|AdaptiveYYY|) (value : AdaptivefffCase) =
        match value with
        | (:? AdaptivefffGgg as Ggg) ->
            AdaptiveGgg(Ggg.Item)
        | (:? AdaptivefffAaa as Aaa) ->
            AdaptiveAaa(Aaa.Item)
        | (:? AdaptivefffYYY as YYY) ->
            AdaptiveYYY
        | _ ->
            failwith "unreachable"

