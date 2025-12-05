//6fd1a83f-0ea3-9dd9-6dd5-afb1245bab0b
//8b89ddd2-5304-c707-8f33-913247101307
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
    let _many_ =
        let inline __arg3 (m : LibraryModel.AdaptiveThing) (v : LibraryModel.Thing) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelListList(value.many, (fun (va : LibraryModel.Thing) (vb : LibraryModel.Thing) -> FSharp.Data.Adaptive.DefaultEqualityComparer.Equals(((fun (__self : LibraryModel.Thing) -> __self.name) va), ((fun (__self : LibraryModel.Thing) -> __self.name) vb))), (fun (v : LibraryModel.Thing) -> LibraryModel.AdaptiveThing(v)), __arg3, (fun (m : LibraryModel.AdaptiveThing) -> m))
    let _testy_ = Adaptify.ChangeableListList(value.testy)
    let _things_ =
        let inline __arg2 (m : LibraryModel.AdaptiveThing) (v : LibraryModel.Thing) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelMap(value.things, (fun (v : LibraryModel.Thing) -> LibraryModel.AdaptiveThing(v)), __arg2, (fun (m : LibraryModel.AdaptiveThing) -> m))
    let _bla_ = Adaptify.ChangeableValueCustomEquality(value.bla, (fun (va : (Microsoft.FSharp.Core.int)[]) (vb : (Microsoft.FSharp.Core.int)[]) -> FSharp.Data.Adaptive.ShallowEqualityComparer<(Microsoft.FSharp.Core.int)[]>.ShallowEquals(va, vb)))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Soup) = AdaptiveSoup(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Soup) -> AdaptiveSoup(value)) (fun (adaptive : AdaptiveSoup) (value : Soup) -> adaptive.Update(value))
    member __.Update(value : Soup) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Soup>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _important_.Update(value.important)
            _many_.Update(value.many)
            _testy_.Update(value.testy)
            _things_.Update(value.things)
            _bla_.Value <- value.bla
    member __.Current = __adaptive
    member __.important = _important_
    member __.many = _many_ :> FSharp.Data.Adaptive.alist<LibraryModel.AdaptiveThing>
    member __.testy = _testy_ :> FSharp.Data.Adaptive.alist<Microsoft.FSharp.Core.int>
    member __.things = _things_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.int, LibraryModel.AdaptiveThing>
    member __.bla = _bla_ :> FSharp.Data.Adaptive.aval<(Microsoft.FSharp.Core.int)[]>

