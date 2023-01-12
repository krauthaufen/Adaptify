//7ff49558-c0cb-8b0a-10c9-fbf82b7296e3
//540cf499-4d3f-ad19-cc83-09873b3c72b8
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
open Model
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
type AdaptiveMyModel(value : MyModel) =
    let mutable _d_ =
        let inline __arg2 (o : System.Object) (v : MyModel) =
            (unbox<AdaptiveMyModel> o).Update(v)
            o
        let inline __arg5 (o : System.Object) (v : MyModel) =
            (unbox<AdaptiveMyModel> o).Update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.string) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o).Value <- v
            o
        Adaptify.FSharp.Core.AdaptiveResult<Model.MyModel, Model.AdaptiveMyModel, Model.AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>(value.d, (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), __arg2, (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.string> o), (fun (v : Microsoft.FSharp.Core.string) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))
    let _e_ = FSharp.Data.Adaptive.cval(value.e)
    let _g_ = FSharp.Data.Adaptive.cmap(value.g)
    let _a_ =
        let inline __arg2 (o : System.Object) (v : MyModel) =
            (unbox<AdaptiveMyModel> o).Update(v)
            o
        let inline __arg5 (o : System.Object) (v : MyModel) =
            (unbox<AdaptiveMyModel> o).Update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.string) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o).Value <- v
            o
        Adaptify.FSharp.Core.AdaptiveResult<Model.MyModel, Model.AdaptiveMyModel, Model.AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>(value.a, (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), __arg2, (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : MyModel) -> AdaptiveMyModel(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveMyModel> o), (fun (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.string> o), (fun (v : Microsoft.FSharp.Core.string) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))
    let _b_ = FSharp.Data.Adaptive.cval(value.b)
    let _x_ = FSharp.Data.Adaptive.cval(value.x)
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
            _d_.Update(value.d)
            _e_.Value <- value.e
            _g_.Value <- value.g
            _a_.Update(value.a)
            _b_.Value <- value.b
            _x_.Value <- value.x
            _map_.Update(value.map)
    member __.Current = __adaptive
    member __.d = _d_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveResultCase<MyModel, AdaptiveMyModel, AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>>
    member __.e = _e_ :> FSharp.Data.Adaptive.aval<LibraryCSharp.StaticClass.CSharpClass>
    member __.g = _g_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.string, LibraryCSharp.StaticClass.CSharpStruct>
    member __.a = _a_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveResultCase<MyModel, AdaptiveMyModel, AdaptiveMyModel, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>>
    member __.b = _b_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.x = _x_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.map = _map_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, AdaptiveMyModel>

