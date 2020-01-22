//b6c461f1-f3c2-ea84-45ca-158b19478f18
//ebbaec5b-c1a3-a364-3864-6c7183c6e947
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec LibraryModel

open System
open FSharp.Data.Adaptive
open Adaptify
open LibraryModel
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
        | (:? AdaptiveMyUnionA as A) -> AdaptiveA(A.Item)
        | (:? AdaptiveMyUnionB as B) -> AdaptiveB(B.Item)
        | _ -> failwith "unreachable"
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveObject(value : Object) =
    let _a_ =
        let inline __arg5 (o : System.Object) (v : Object) =
            (unbox<AdaptiveObject> o).Update(v)
            o
        let inline __arg11 (o : System.Object) (v : Microsoft.FSharp.Core.string) =
            (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o).Value <- v
            o
        Adaptify.FSharp.Core.AdaptiveResult<LibraryModel.Object, LibraryModel.AdaptiveObject, LibraryModel.AdaptiveObject, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>(value.a, (fun (v : Object) -> AdaptiveObject(v) :> System.Object), (fun (o : System.Object) (v : Object) -> (unbox<AdaptiveObject> o).Update(v) :> System.Object), (fun (o : System.Object) -> unbox<AdaptiveObject> o), (fun (v : Object) -> AdaptiveObject(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<AdaptiveObject> o), (fun (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.string) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.string> o), (fun (v : Microsoft.FSharp.Core.string) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg11, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.string>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>))
    let _b_ = FSharp.Data.Adaptive.cval(value.b)
    let _map_ =
        let inline __arg2 (m : AdaptiveObject) (v : Object) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelMap(value.map, (fun (v : Object) -> AdaptiveObject(v)), __arg2, (fun (m : AdaptiveObject) -> m))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Object) = AdaptiveObject(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Object) -> AdaptiveObject(value)) (fun (adaptive : AdaptiveObject) (value : Object) -> adaptive.Update(value))
    member __.Update(value : Object) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Object>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _a_.Update(value.a)
            _b_.Value <- value.b
            _map_.Update(value.map)
    member __.Current = __adaptive
    member __.a = _a_ :> FSharp.Data.Adaptive.aval<Adaptify.FSharp.Core.AdaptiveResultCase<Object, AdaptiveObject, AdaptiveObject, Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.string, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>>>
    member __.b = _b_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.map = _map_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, AdaptiveObject>

