//3217fcfd-d03c-62c9-09a5-6521aa7799d4
//fb2edecf-53f6-e96e-e560-12037a1eae1e
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "3199" // rec in rec
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
type AdaptiveIFace(value : IFace) =
    let _Sepp_ = FSharp.Data.Adaptive.cval(value.Sepp)
    let mutable __value = value
    member __.update(value : IFace) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<IFace>.ShallowEquals(value, __value))) then
            __value <- value
            _Sepp_.Value <- value.Sepp
    member __.Sepp = _Sepp_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
type AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError> =
    abstract member update : MyResult<'T, 'TError> -> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
type private AdaptiveMyResultOk<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>(ResultValue : 'T, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT, primTErrorinit : 'TError -> System.Object, primTErrorupdate : System.Object -> 'TError -> System.Object, primTErrorview : System.Object -> 'paTError, TErrorinit : 'TError -> System.Object, TErrorupdate : System.Object -> 'TError -> System.Object, TErrorview : System.Object -> 'aTError) =
    let _ResultValue_ = Tinit ResultValue
    let mutable __ResultValue = ResultValue
    member __.update(ResultValue : 'T) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T>.ShallowEquals(ResultValue, __ResultValue))) then
            __ResultValue <- ResultValue
            ignore (Tupdate _ResultValue_ ResultValue)
    member __.ResultValue = Tview _ResultValue_
    interface AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError> with
        member x.update(value : MyResult<'T, 'TError>) =
            match value with
            | MyResult.Ok(ResultValue) ->
                x.update(ResultValue)
                x :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
            | MyResult.Error(ErrorValue) ->
                let inline __arg5 (o : System.Object) (v : 'T) =
                    ignore (Tupdate (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'TError) =
                    ignore (TErrorupdate (unbox<System.Object> o) v)
                    o
                AdaptiveMyResultError(ErrorValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
type private AdaptiveMyResultError<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>(ErrorValue : 'TError, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT, primTErrorinit : 'TError -> System.Object, primTErrorupdate : System.Object -> 'TError -> System.Object, primTErrorview : System.Object -> 'paTError, TErrorinit : 'TError -> System.Object, TErrorupdate : System.Object -> 'TError -> System.Object, TErrorview : System.Object -> 'aTError) =
    let _ErrorValue_ = TErrorinit ErrorValue
    let mutable __ErrorValue = ErrorValue
    member __.update(ErrorValue : 'TError) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'TError>.ShallowEquals(ErrorValue, __ErrorValue))) then
            __ErrorValue <- ErrorValue
            ignore (TErrorupdate _ErrorValue_ ErrorValue)
    member __.ErrorValue = TErrorview _ErrorValue_
    interface AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError> with
        member x.update(value : MyResult<'T, 'TError>) =
            match value with
            | MyResult.Ok(ResultValue) ->
                let inline __arg5 (o : System.Object) (v : 'T) =
                    ignore (Tupdate (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'TError) =
                    ignore (TErrorupdate (unbox<System.Object> o) v)
                    o
                AdaptiveMyResultOk(ResultValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
            | MyResult.Error(ErrorValue) ->
                x.update(ErrorValue)
                x :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
type AdaptiveMyResult<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>(value : MyResult<'T, 'TError>, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT, primTErrorinit : 'TError -> System.Object, primTErrorupdate : System.Object -> 'TError -> System.Object, primTErrorview : System.Object -> 'paTError, TErrorinit : 'TError -> System.Object, TErrorupdate : System.Object -> 'TError -> System.Object, TErrorview : System.Object -> 'aTError) =
    inherit Adaptify.AdaptiveValue<AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>>()
    let mutable __value =
        match value with
        | MyResult.Ok(ResultValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyResultOk(ResultValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
        | MyResult.Error(ErrorValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyResultError(ErrorValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
    static member CreateAdaptiveCase(value : MyResult<'T, 'TError>, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT, primTErrorinit : 'TError -> System.Object, primTErrorupdate : System.Object -> 'TError -> System.Object, primTErrorview : System.Object -> 'paTError, TErrorinit : 'TError -> System.Object, TErrorupdate : System.Object -> 'TError -> System.Object, TErrorview : System.Object -> 'aTError) =
        match value with
        | MyResult.Ok(ResultValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyResultOk(ResultValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
        | MyResult.Error(ErrorValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveMyResultError(ErrorValue, (fun (v : 'T) -> primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTview (unbox<System.Object> o)), (fun (v : 'T) -> Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> Tview (unbox<System.Object> o)), (fun (v : 'TError) -> primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> TErrorview (unbox<System.Object> o))) :> AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>
    member __.update(value : MyResult<'T, 'TError>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value
[<AutoOpen>]
module AdaptiveMyResult = 
    let (|AdaptiveOk|AdaptiveError|) (value : AdaptiveMyResultCase<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError>) =
        match value with
        | (:? AdaptiveMyResultOk<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError> as Ok) -> AdaptiveOk(Ok.ResultValue)
        | (:? AdaptiveMyResultError<'T, 'paT, 'aT, 'TError, 'paTError, 'aTError> as Error) -> AdaptiveError(Error.ErrorValue)
        | _ -> failwith "unreachable"
type AdaptiveRecord(value : Record) =
    let _foo_ = FSharp.Data.Adaptive.cval(value.foo)
    let _fa_ =
        let inline __arg5 (o : System.Object) (v : IFace) =
            (unbox<AdaptiveIFace> o).update(v)
            o
        let inline __arg11 (o : System.Object) (v : IFace) =
            (unbox<AdaptiveIFace> o).update(v)
            o
        Adaptify.FSharp.Core.AdaptiveChoice<Model.IFace, Model.AdaptiveIFace, Model.AdaptiveIFace, Model.IFace, Model.AdaptiveIFace, Model.AdaptiveIFace>(value.fa, (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), (fun (o : System.Object) (v : IFace) -> (unbox<AdaptiveIFace> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveIFace> o), (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveIFace> o), (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), (fun (o : System.Object) (v : IFace) -> (unbox<AdaptiveIFace> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveIFace> o), (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<AdaptiveIFace> o))
    let _fb_ =
        let inline __arg2 (m : AdaptiveRecord) (v : Record) =
            m.update(v)
            m
        Adaptify.ChangeableModelList(value.fb, (fun (v : Record) -> AdaptiveRecord(v)), __arg2, (fun (m : AdaptiveRecord) -> m))
    let _fc_ =
        let inline __arg2 (m : AdaptiveIFace) (v : IFace) =
            m.update(v)
            m
        Adaptify.ChangeableModelList(value.fc, (fun (v : IFace) -> AdaptiveIFace(v)), __arg2, (fun (m : AdaptiveIFace) -> m))
    let _x_ = FSharp.Data.Adaptive.cval(value.x)
    let _test_ =
        let inline __arg5 (o : System.Object) (v : Record) =
            (unbox<AdaptiveRecord> o).update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
            o
        AdaptiveMyResult<Record, AdaptiveRecord, AdaptiveRecord, Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>(value.test, (fun (v : Record) -> AdaptiveRecord(v) :> System.Object), (fun (o : System.Object) (v : Record) -> (unbox<AdaptiveRecord> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveRecord> o), (fun (v : Record) -> AdaptiveRecord(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveRecord> o), (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>))
    let mutable __value = value
    member __.update(value : Record) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Record>.ShallowEquals(value, __value))) then
            __value <- value
            _foo_.Value <- value.foo
            _fa_.update(value.fa)
            _fb_.update(value.fb)
            _fc_.update(value.fc)
            _x_.Value <- value.x
            _test_.update(value.test)
    member __.foo = _foo_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.fa = _fa_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveChoiceCase<IFace, AdaptiveIFace, AdaptiveIFace, IFace, AdaptiveIFace, AdaptiveIFace>>
    member __.fb = _fb_ :> FSharp.Data.Adaptive.alist<AdaptiveRecord>
    member __.fc = _fc_ :> FSharp.Data.Adaptive.alist<AdaptiveIFace>
    member __.x = _x_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.int>>
    member __.test = _test_ :> FSharp.Data.Adaptive.aval<AdaptiveMyResultCase<Record, AdaptiveRecord, AdaptiveRecord, Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>>
type AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab> =
    abstract member update : MyUnion<'a, 'b> -> AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>
type private AdaptiveMyUnionCaseA<'a, 'paa, 'aa, 'b, 'pab, 'ab>(value : Microsoft.FSharp.Core.int, dst : 'a, primainit : 'a -> System.Object, primaupdate : System.Object -> 'a -> System.Object, primaview : System.Object -> 'paa, ainit : 'a -> System.Object, aupdate : System.Object -> 'a -> System.Object, aview : System.Object -> 'aa, primbinit : 'b -> System.Object, primbupdate : System.Object -> 'b -> System.Object, primbview : System.Object -> 'pab, binit : 'b -> System.Object, bupdate : System.Object -> 'b -> System.Object, bview : System.Object -> 'ab) =
    let mutable __dst = dst
    let mutable __value = value
    member __.update(value : Microsoft.FSharp.Core.int, dst : 'a) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Microsoft.FSharp.Core.int>.ShallowEquals(value, __value) && Adaptify.ShallowEqualityComparer<'a>.ShallowEquals(dst, __dst))) then
            __value <- value
            __dst <- dst
            ()
    member __.value = __value
    member __.dst = __dst
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
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'b>.ShallowEquals(Item, __Item))) then
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
    inherit Adaptify.AdaptiveValue<AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>>()
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
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value
[<AutoOpen>]
module AdaptiveMyUnion = 
    let (|AdaptiveCaseA|AdaptiveCaseB|) (value : AdaptiveMyUnionCase<'a, 'paa, 'aa, 'b, 'pab, 'ab>) =
        match value with
        | (:? AdaptiveMyUnionCaseA<'a, 'paa, 'aa, 'b, 'pab, 'ab> as CaseA) -> AdaptiveCaseA(CaseA.value, CaseA.dst)
        | (:? AdaptiveMyUnionCaseB<'a, 'paa, 'aa, 'b, 'pab, 'ab> as CaseB) -> AdaptiveCaseB(CaseB.Item)
        | _ -> failwith "unreachable"
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
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Geny<'a, 'b>>.ShallowEquals(value, __value))) then
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
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Seppy<'a>>.ShallowEquals(value, __value))) then
            __value <- value
            ignore (aupdate _x_ value.x)
            _y_.update(value.y)
    member __.x = aview _x_
    member __.y = _y_ :> FSharp.Data.Adaptive.alist<'paa>
type AdaptiveRecy(value : Recy) =
    let _u_ =
        let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
            o
        let inline __arg11 (o : System.Object) (v : Recy) =
            (unbox<AdaptiveRecy> o).update(v)
            o
        AdaptiveMyUnion<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Recy, AdaptiveRecy, AdaptiveRecy>(value.u, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>), (fun (v : Recy) -> AdaptiveRecy(v) :> System.Object), (fun (o : System.Object) (v : Recy) -> (unbox<AdaptiveRecy> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveRecy> o), (fun (v : Recy) -> AdaptiveRecy(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<AdaptiveRecy> o))
    let mutable __value = value
    member __.update(value : Recy) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Recy>.ShallowEquals(value, __value))) then
            __value <- value
            _u_.update(value.u)
    member __.u = _u_ :> FSharp.Data.Adaptive.aval<AdaptiveMyUnionCase<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>, Recy, AdaptiveRecy, AdaptiveRecy>>
type AdaptivefffCase =
    abstract member update : fff -> AdaptivefffCase
type private AdaptivefffGgg(Item : Microsoft.FSharp.Core.int) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.update(Item : Microsoft.FSharp.Core.int) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Microsoft.FSharp.Core.int>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) ->
                x.update(Item)
                x :> AdaptivefffCase
            | fff.Aaa(Item) -> AdaptivefffAaa(Item) :> AdaptivefffCase
            | fff.YYY -> AdaptivefffYYY() :> AdaptivefffCase
type private AdaptivefffAaa(Item : Microsoft.FSharp.Core.string) =
    let _Item_ = FSharp.Data.Adaptive.cval(Item)
    let mutable __Item = Item
    member __.update(Item : Microsoft.FSharp.Core.string) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Microsoft.FSharp.Core.string>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            _Item_.Value <- Item
    member __.Item = _Item_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) -> AdaptivefffGgg(Item) :> AdaptivefffCase
            | fff.Aaa(Item) ->
                x.update(Item)
                x :> AdaptivefffCase
            | fff.YYY -> AdaptivefffYYY() :> AdaptivefffCase
type private AdaptivefffYYY() =
    member __.update() = ()
    interface AdaptivefffCase with
        member x.update(value : fff) =
            match value with
            | fff.Ggg(Item) -> AdaptivefffGgg(Item) :> AdaptivefffCase
            | fff.Aaa(Item) -> AdaptivefffAaa(Item) :> AdaptivefffCase
            | fff.YYY ->
                x.update()
                x :> AdaptivefffCase
type Adaptivefff(value : fff) =
    inherit Adaptify.AdaptiveValue<AdaptivefffCase>()
    let mutable __value =
        match value with
        | fff.Ggg(Item) -> AdaptivefffGgg(Item) :> AdaptivefffCase
        | fff.Aaa(Item) -> AdaptivefffAaa(Item) :> AdaptivefffCase
        | fff.YYY -> AdaptivefffYYY() :> AdaptivefffCase
    static member CreateAdaptiveCase(value : fff) =
        match value with
        | fff.Ggg(Item) -> AdaptivefffGgg(Item) :> AdaptivefffCase
        | fff.Aaa(Item) -> AdaptivefffAaa(Item) :> AdaptivefffCase
        | fff.YYY -> AdaptivefffYYY() :> AdaptivefffCase
    member __.update(value : fff) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptivefffCase>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value
[<AutoOpen>]
module Adaptivefff = 
    let (|AdaptiveGgg|AdaptiveAaa|AdaptiveYYY|) (value : AdaptivefffCase) =
        match value with
        | (:? AdaptivefffGgg as Ggg) -> AdaptiveGgg(Ggg.Item)
        | (:? AdaptivefffAaa as Aaa) -> AdaptiveAaa(Aaa.Item)
        | (:? AdaptivefffYYY as YYY) -> AdaptiveYYY
        | _ -> failwith "unreachable"


namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
[<AutoOpen>]
module rec Adaptify =
    [<AutoOpen>]
    module rec Test =
        type AdaptiveX(value : Model.Test.X) =
            let _A_ =
                let (__v0 : Microsoft.FSharp.Core.int, __v1 : Microsoft.FSharp.Core.string) = value.A
                ((FSharp.Data.Adaptive.cval(__v0)), (FSharp.Data.Adaptive.cval(__v1)))
            let mutable __value = value
            member __.update(value : Model.Test.X) =
                if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Model.Test.X>.ShallowEquals(value, __value))) then
                    __value <- value
                    let (__c0 : FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>, __c1 : FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>) = _A_
                    let (__v0 : Microsoft.FSharp.Core.int, __v1 : Microsoft.FSharp.Core.string) = value.A
                    __c0.Value <- __v0
                    __c1.Value <- __v1
            member __.A =
                let (__c0 : FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>, __c1 : FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>) = _A_
                ((__c0 :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>), (__c1 :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))

