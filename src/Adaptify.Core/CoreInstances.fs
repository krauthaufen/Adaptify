namespace Adaptify.FSharp.Core

#nowarn "1337"
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open Adaptify

type AdaptiveOptionCase<'T, 'paT, 'aT> =
    abstract member update : Option<'T> -> AdaptiveOptionCase<'T, 'paT, 'aT>

type private AdaptiveOptionNone<'T, 'paT, 'aT>(primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT) =
    member __.update() = ()
    interface AdaptiveOptionCase<'T, 'paT, 'aT> with
        member x.update(value : Option<'T>) =
            match value with
            | Option.None ->
                x.update()
                x :> AdaptiveOptionCase<'T, 'paT, 'aT>
            | Option.Some(value) ->
                AdaptiveOptionSome(value, primTinit, primTupdate, primTview, Tinit, Tupdate, Tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>

and private AdaptiveOptionSome<'T, 'paT, 'aT>(Value : 'T, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT) =
    let _Value_ = Tinit Value
    let mutable __Value = Value
    member __.update(value : 'T) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T>.ShallowEquals(value, __Value))) then
            __Value <- value
            ignore (Tupdate _Value_ value)
    member __.Value = Tview _Value_
    interface AdaptiveOptionCase<'T, 'paT, 'aT> with
        member x.update(value : Option<'T>) =
            match value with
            | Option.None ->
                let inline __arg4 (o : System.Object) (v : 'T) =
                    ignore (Tupdate (unbox<System.Object> o) v)
                    o
                AdaptiveOptionNone(primTinit, primTupdate, primTview, Tinit, Tupdate, Tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>
            | Option.Some(value) ->
                x.update(value)
                x :> AdaptiveOptionCase<'T, 'paT, 'aT>

type AdaptiveOption<'T, 'paT, 'aT>(value : Option<'T>, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, Tinit : 'T -> System.Object, Tupdate : System.Object -> 'T -> System.Object, Tview : System.Object -> 'aT) =
    inherit Adaptify.AdaptiveValue<AdaptiveOptionCase<'T, 'paT, 'aT>>()
    let mutable __value =
        match value with
        | Option.None ->
            let inline __arg4 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionNone(primTinit, primTupdate, primTview, Tinit, Tupdate, Tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>
        | Option.Some(value) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionSome(value, primTinit, primTupdate, primTview, Tinit, Tupdate, Tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>
    static member CreateAdaptiveCase(value : Option<'T>, primTinit : 'T -> System.Object, primTupdate : System.Object -> 'T -> System.Object, primTview : System.Object -> 'paT, tinit : 'T -> System.Object, tupdate : System.Object -> 'T -> System.Object, tview : System.Object -> 'aT) =
        match value with
        | Option.None ->
            AdaptiveOptionNone(primTinit, primTupdate, primTview, tinit, tupdate, tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>
        | Option.Some(value) ->
            AdaptiveOptionSome(value, primTinit, primTupdate, primTview, tinit, tupdate, tview) :> AdaptiveOptionCase<'T, 'paT, 'aT>
    member __.update(value : Option<'T>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveOptionCase<'T, 'paT, 'aT>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value

[<AutoOpen>]
module AdaptiveOption =
    let (|AdaptiveNone|AdaptiveSome|) (o : AdaptiveOptionCase<'T, 'paT, 'aT>) =
        match o with
        | :? AdaptiveOptionNone<'T, 'paT, 'aT> -> AdaptiveNone
        | :? AdaptiveOptionSome<'T, 'paT, 'aT> as a -> AdaptiveSome a.Value
        | _ -> failwith "unreachable"


//26c44b10-4ede-8698-8ca5-65d960aec542
//7cb2f1ac-1ab0-6c9a-33d5-1668933d8b7b
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec Adaptify.FSharp.Core

open System
open FSharp.Data.Adaptive
open Adaptify
type AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2> =
    abstract member update : Choice<'T1, 'T2> -> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
type private AdaptiveChoiceChoice1Of2<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>(Item : 'T1, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2) =
    let _Item_ = T1init Item
    let mutable __Item = Item
    member __.update(Item : 'T1) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T1update _Item_ Item)
    member __.Item = T1view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2> with
        member x.update(value : Choice<'T1, 'T2>) =
            match value with
            | Choice.Choice1Of2(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
            | Choice.Choice2Of2(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
type private AdaptiveChoiceChoice2Of2<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>(Item : 'T2, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2) =
    let _Item_ = T2init Item
    let mutable __Item = Item
    member __.update(Item : 'T2) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T2update _Item_ Item)
    member __.Item = T2view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2> with
        member x.update(value : Choice<'T1, 'T2>) =
            match value with
            | Choice.Choice1Of2(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
            | Choice.Choice2Of2(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
type AdaptiveChoice<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>(value : Choice<'T1, 'T2>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>>()
    let mutable __value =
        match value with
        | Choice.Choice1Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
        | Choice.Choice2Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2) =
        match value with
        | Choice.Choice1Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
        | Choice.Choice2Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>
    member __.update(value : Choice<'T1, 'T2>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value

type AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> =
    abstract member update : Choice<'T1, 'T2, 'T3> -> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
type private AdaptiveChoiceChoice1Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>(Item : 'T1, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3) =
    let _Item_ = T1init Item
    let mutable __Item = Item
    member __.update(Item : 'T1) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T1update _Item_ Item)
    member __.Item = T1view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> with
        member x.update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice2Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice3Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
type private AdaptiveChoiceChoice2Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>(Item : 'T2, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3) =
    let _Item_ = T2init Item
    let mutable __Item = Item
    member __.update(Item : 'T2) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T2update _Item_ Item)
    member __.Item = T2view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> with
        member x.update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice2Of3(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice3Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
type private AdaptiveChoiceChoice3Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>(Item : 'T3, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3) =
    let _Item_ = T3init Item
    let mutable __Item = Item
    member __.update(Item : 'T3) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T3>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T3update _Item_ Item)
    member __.Item = T3view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> with
        member x.update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice2Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
            | Choice.Choice3Of3(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
type AdaptiveChoice<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>(value : Choice<'T1, 'T2, 'T3>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>>()
    let mutable __value =
        match value with
        | Choice.Choice1Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
        | Choice.Choice2Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
        | Choice.Choice3Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2, 'T3>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3) =
        match value with
        | Choice.Choice1Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
        | Choice.Choice2Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
        | Choice.Choice3Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>
    member __.update(value : Choice<'T1, 'T2, 'T3>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value
[<AutoOpen>]

type AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> =
    abstract member update : Choice<'T1, 'T2, 'T3, 'T4> -> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
type private AdaptiveChoiceChoice1Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>(Item : 'T1, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
    let _Item_ = T1init Item
    let mutable __Item = Item
    member __.update(Item : 'T1) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T1update _Item_ Item)
    member __.Item = T1view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> with
        member x.update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
type private AdaptiveChoiceChoice2Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>(Item : 'T2, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
    let _Item_ = T2init Item
    let mutable __Item = Item
    member __.update(Item : 'T2) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T2update _Item_ Item)
    member __.Item = T2view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> with
        member x.update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice2Of4(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
type private AdaptiveChoiceChoice3Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>(Item : 'T3, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
    let _Item_ = T3init Item
    let mutable __Item = Item
    member __.update(Item : 'T3) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T3>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T3update _Item_ Item)
    member __.Item = T3view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> with
        member x.update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice3Of4(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
type private AdaptiveChoiceChoice4Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>(Item : 'T4, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
    let _Item_ = T4init Item
    let mutable __Item = Item
    member __.update(Item : 'T4) =
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<'T4>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (T4update _Item_ Item)
    member __.Item = T4view _Item_
    interface AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> with
        member x.update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
            | Choice.Choice4Of4(Item) ->
                x.update(Item)
                x :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
type AdaptiveChoice<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>(value : Choice<'T1, 'T2, 'T3, 'T4>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>>()
    let mutable __value =
        match value with
        | Choice.Choice1Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice2Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice3Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice4Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2, 'T3, 'T4>, primT1init : 'T1 -> System.Object, primT1update : System.Object -> 'T1 -> System.Object, primT1view : System.Object -> 'paT1, T1init : 'T1 -> System.Object, T1update : System.Object -> 'T1 -> System.Object, T1view : System.Object -> 'aT1, primT2init : 'T2 -> System.Object, primT2update : System.Object -> 'T2 -> System.Object, primT2view : System.Object -> 'paT2, T2init : 'T2 -> System.Object, T2update : System.Object -> 'T2 -> System.Object, T2view : System.Object -> 'aT2, primT3init : 'T3 -> System.Object, primT3update : System.Object -> 'T3 -> System.Object, primT3view : System.Object -> 'paT3, T3init : 'T3 -> System.Object, T3update : System.Object -> 'T3 -> System.Object, T3view : System.Object -> 'aT3, primT4init : 'T4 -> System.Object, primT4update : System.Object -> 'T4 -> System.Object, primT4view : System.Object -> 'paT4, T4init : 'T4 -> System.Object, T4update : System.Object -> 'T4 -> System.Object, T4view : System.Object -> 'aT4) =
        match value with
        | Choice.Choice1Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice2Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice3Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
        | Choice.Choice4Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> T1init v :> System.Object), __arg5, (fun (o : System.Object) -> T1view (unbox<System.Object> o)), (fun (v : 'T2) -> primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> T2init v :> System.Object), __arg11, (fun (o : System.Object) -> T2view (unbox<System.Object> o)), (fun (v : 'T3) -> primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> T3init v :> System.Object), __arg17, (fun (o : System.Object) -> T3view (unbox<System.Object> o)), (fun (v : 'T4) -> primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> T4init v :> System.Object), __arg23, (fun (o : System.Object) -> T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>
    member __.update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
        let __n = __value.update(value)
        if Microsoft.FSharp.Core.Operators.not((Adaptify.ShallowEqualityComparer<AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>>.ShallowEquals(__n, __value))) then
            __value <- __n
            __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __value


[<AutoOpen>]
module AdaptiveChoice = 

    let (|AdaptiveChoice1Of2|AdaptiveChoice2Of2|) (value : AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of2<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2> as Choice1Of2) -> AdaptiveChoice1Of2(Choice1Of2.Item)
        | (:? AdaptiveChoiceChoice2Of2<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2> as Choice2Of2) -> AdaptiveChoice2Of2(Choice2Of2.Item)
        | _ -> failwith "unreachable"

    let (|AdaptiveChoice1Of3|AdaptiveChoice2Of3|AdaptiveChoice3Of3|) (value : AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> as Choice1Of3) -> AdaptiveChoice1Of3(Choice1Of3.Item)
        | (:? AdaptiveChoiceChoice2Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> as Choice2Of3) -> AdaptiveChoice2Of3(Choice2Of3.Item)
        | (:? AdaptiveChoiceChoice3Of3<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3> as Choice3Of3) -> AdaptiveChoice3Of3(Choice3Of3.Item)
        | _ -> failwith "unreachable"

    let (|AdaptiveChoice1Of4|AdaptiveChoice2Of4|AdaptiveChoice3Of4|AdaptiveChoice4Of4|) (value : AdaptiveChoiceCase<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> as Choice1Of4) -> AdaptiveChoice1Of4(Choice1Of4.Item)
        | (:? AdaptiveChoiceChoice2Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> as Choice2Of4) -> AdaptiveChoice2Of4(Choice2Of4.Item)
        | (:? AdaptiveChoiceChoice3Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> as Choice3Of4) -> AdaptiveChoice3Of4(Choice3Of4.Item)
        | (:? AdaptiveChoiceChoice4Of4<'T1, 'paT1, 'aT1, 'T2, 'paT2, 'aT2, 'T3, 'paT3, 'aT3, 'T4, 'paT4, 'aT4> as Choice4Of4) -> AdaptiveChoice4Of4(Choice4Of4.Item)
        | _ -> failwith "unreachable"


