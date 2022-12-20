//01768e15-d2af-b3fe-e082-1b73c4955b4b
//f3ff857d-791d-4e63-68e5-5fbc3a04ec33
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec LibraryModel2

open System
open FSharp.Data.Adaptive
open Adaptify
open LibraryModel2
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveSoup(value : Soup) =
    let _important_ = LibraryModel.AdaptiveThing(value.important)
    let _things_ =
        let inline __arg2 (m : LibraryModel.AdaptiveThing) (v : LibraryModel.Thing) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelMap(value.things, (fun (v : LibraryModel.Thing) -> LibraryModel.AdaptiveThing(v)), __arg2, (fun (m : LibraryModel.AdaptiveThing) -> m))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Soup) = AdaptiveSoup(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Soup) -> AdaptiveSoup(value)) (fun (adaptive : AdaptiveSoup) (value : Soup) -> adaptive.Update(value))
    member __.Update(value : Soup) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Soup>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _important_.Update(value.important)
            _things_.Update(value.things)
    member __.Current = __adaptive
    member __.important = _important_
    member __.things = _things_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, LibraryModel.AdaptiveThing>

