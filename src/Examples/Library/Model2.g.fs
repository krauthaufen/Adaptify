//05a89e3c-de71-db0d-8077-73738ec0cbc6
//b9972a76-21af-0d46-d2df-57aeb54cbe44
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
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
[<AutoOpen; System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
module SoupLenses = 
    type Soup with
        static member important_ = ((fun (self : Soup) -> self.important), (fun (value : LibraryModel.Thing) (self : Soup) -> { self with important = value }))
        static member things_ = ((fun (self : Soup) -> self.things), (fun (value : FSharp.Data.Adaptive.HashMap<Microsoft.FSharp.Core.int, LibraryModel.Thing>) (self : Soup) -> { self with things = value }))

