//681d0e7d-358a-d203-c26c-890ea38dcd45
//3d6ec6c9-ffdb-b8a6-ac76-07d7d47bb6fd
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
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
type AdaptiveRecord(value : Record) =
    let _fa_ =
        let inline __arg5 (o : System.Object) (v : IFace) =
            (unbox<AdaptiveIFace> o).update(v)
            o
        let inline __arg11 (o : System.Object) (v : Record) =
            (unbox<AdaptiveRecord> o).update(v)
            o
        Adaptify.FSharp.Core.AdaptiveChoice<Model.IFace, Model.AdaptiveIFace, Model.AdaptiveIFace, Model.Record, Model.AdaptiveRecord, Model.AdaptiveRecord>(value.fa, (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), (fun (o : System.Object) (v : IFace) -> (unbox<AdaptiveIFace> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveIFace> o), (fun (v : IFace) -> AdaptiveIFace(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveIFace> o), (fun (v : Record) -> AdaptiveRecord(v) :> System.Object), (fun (o : System.Object) (v : Record) -> (unbox<AdaptiveRecord> o).update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveRecord> o), (fun (v : Record) -> AdaptiveRecord(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<AdaptiveRecord> o))
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
    let _test_ = FSharp.Data.Adaptive.cval(value.test)
    let mutable __value = value
    member __.update(value : Record) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<Record>.ShallowEquals(value, __value))) then
            __value <- value
            _fa_.update(value.fa)
            _fb_.update(value.fb)
            _fc_.update(value.fc)
            _x_.Value <- value.x
            _test_.Value <- value.test
    member __.fa = _fa_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveChoiceCase<IFace, AdaptiveIFace, AdaptiveIFace, Record, AdaptiveRecord, AdaptiveRecord>>
    member __.fb = _fb_ :> FSharp.Data.Adaptive.alist<AdaptiveRecord>
    member __.fc = _fc_ :> FSharp.Data.Adaptive.alist<AdaptiveIFace>
    member __.x = _x_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.int>>
    member __.test = _test_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Choice<Record, Microsoft.FSharp.Core.int>>

