//1f393f24-1629-f7a3-f49d-eaeb02c60d70
//fc724c27-fcd8-96fc-37e4-0a3ecccc3b9d
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveMyModel(value : MyModel) =
    let _a_ =
        let inline __arg5 (o : System.Object) (v : MyModel) =
            (unbox<AdaptiveMyModel> o).Update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.string) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o).Value <- v
            o
        Adaptify.FSharp.Core.AdaptiveResult<Model.MyModel, Model.AdaptiveMyModel, Model.AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>(value.a, (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), (fun (o : System.Object) (v : MyModel) -> (unbox<AdaptiveMyModel> o).Update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.string> o), (fun (v : Microsoft.FSharp.Core.string) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))
    let _b_ = FSharp.Data.Adaptive.cval(value.b)
    let _list_ = FSharp.Data.Adaptive.cval(value.list)
    let _map_ =
        let inline __arg2 (m : AdaptiveMyModel) (v : MyModel) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelMap(value.map, (fun (v : MyModel) -> AdaptiveMyModel(v)), __arg2, (fun (m : AdaptiveMyModel) -> m))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : MyModel) = AdaptiveMyModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : MyModel) -> AdaptiveMyModel(value)) (fun (adaptive : AdaptiveMyModel) (value : MyModel) -> adaptive.Update(value))
    member __.Update(value : MyModel) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<MyModel>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _a_.Update(value.a)
            _b_.Value <- value.b
            _list_.Value <- value.list
            _map_.Update(value.map)
    member __.Current = __adaptive
    member __.a = _a_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveResultCase<MyModel, AdaptiveMyModel, AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>>
    member __.b = _b_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.list = _list_ :> FSharp.Data.Adaptive.aval<FSharp.Data.Adaptive.HashSet<MyModel>>
    member __.map = _map_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, AdaptiveMyModel>

