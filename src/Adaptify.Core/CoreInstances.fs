//665f2cca-ce09-bb13-4859-89507228254b
//7c9e74a5-28c4-4bf7-f079-ba4bd8c70436
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec Adaptify.FSharp.Core

open System
open FSharp.Data.Adaptive
open Adaptify
type AdaptiveOptionCase<'T, '_primT, '_aT> =
    abstract member Update : Option<'T> -> AdaptiveOptionCase<'T, '_primT, '_aT>
type private AdaptiveOptionNone<'T, '_primT, '_aT>(_primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT) =
    member __.Update() = ()
    interface AdaptiveOptionCase<'T, '_primT, '_aT> with
        member x.Update(value : Option<'T>) =
            match value with
            | Option.None ->
                x.Update()
                x :> AdaptiveOptionCase<'T, '_primT, '_aT>
            | Option.Some(Value) ->
                let inline __arg5 (o : System.Object) (v : 'T) =
                    ignore (_Tupdate (unbox<System.Object> o) v)
                    o
                AdaptiveOptionSome(Value, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
type private AdaptiveOptionSome<'T, '_primT, '_aT>(Value : 'T, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT) =
    let _Value_ = _Tinit Value
    let mutable __Value = Value
    member __.Update(Value : 'T) =
        if Operators.not((ShallowEqualityComparer<'T>.ShallowEquals(Value, __Value))) then
            __Value <- Value
            ignore (_Tupdate _Value_ Value)
    member __.Value = _Tview _Value_
    interface AdaptiveOptionCase<'T, '_primT, '_aT> with
        member x.Update(value : Option<'T>) =
            match value with
            | Option.None ->
                let inline __arg4 (o : System.Object) (v : 'T) =
                    ignore (_Tupdate (unbox<System.Object> o) v)
                    o
                AdaptiveOptionNone((fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg4, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
            | Option.Some(Value) ->
                x.Update(Value)
                x :> AdaptiveOptionCase<'T, '_primT, '_aT>
type AdaptiveOption<'T, '_primT, '_aT>(value : Option<'T>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT) =
    inherit Adaptify.AdaptiveValue<AdaptiveOptionCase<'T, '_primT, '_aT>>()
    let mutable __value = value
    let mutable __current =
        match value with
        | Option.None ->
            let inline __arg4 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionNone((fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg4, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
        | Option.Some(Value) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionSome(Value, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : Option<'T>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT) =
        match value with
        | Option.None ->
            let inline __arg4 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionNone((fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg4, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
        | Option.Some(Value) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            AdaptiveOptionSome(Value, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o))) :> AdaptiveOptionCase<'T, '_primT, '_aT>
    static member Create(value : Option<'T>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT) = AdaptiveOption<'T, '_primT, '_aT>(value, _primTinit, _primTupdate, _primTview, _Tinit, _Tupdate, _Tview)
    member __.Current = __adaptive
    member __.Update(value : Option<'T>) =
        if Operators.not((ShallowEqualityComparer<Option<'T>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Operators.not((ShallowEqualityComparer<AdaptiveOptionCase<'T, '_primT, '_aT>>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current
[<AutoOpen>]
module AdaptiveOption = 
    let (|AdaptiveNone|AdaptiveSome|) (value : AdaptiveOptionCase<'T, '_primT, '_aT>) =
        match value with
        | (:? AdaptiveOptionNone<'T, '_primT, '_aT> as None) -> AdaptiveNone
        | (:? AdaptiveOptionSome<'T, '_primT, '_aT> as Some) -> AdaptiveSome(Some.Value)
        | _ -> failwith "unreachable"
type AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2> =
    abstract member Update : Choice<'T1, 'T2> -> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
type private AdaptiveChoiceChoice1Of2<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>(Item : 'T1, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2) =
    let _Item_ = _T1init Item
    let mutable __Item = Item
    member __.Update(Item : 'T1) =
        if Operators.not((ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T1update _Item_ Item)
    member __.Item = _T1view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2> with
        member x.Update(value : Choice<'T1, 'T2>) =
            match value with
            | Choice.Choice1Of2(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
            | Choice.Choice2Of2(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
type private AdaptiveChoiceChoice2Of2<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>(Item : 'T2, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2) =
    let _Item_ = _T2init Item
    let mutable __Item = Item
    member __.Update(Item : 'T2) =
        if Operators.not((ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T2update _Item_ Item)
    member __.Item = _T2view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2> with
        member x.Update(value : Choice<'T1, 'T2>) =
            match value with
            | Choice.Choice1Of2(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
            | Choice.Choice2Of2(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
type AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>(value : Choice<'T1, 'T2>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>>()
    let mutable __value = value
    let mutable __current =
        match value with
        | Choice.Choice1Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
        | Choice.Choice2Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2) =
        match value with
        | Choice.Choice1Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
        | Choice.Choice2Of2(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of2(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>
    static member Create(value : Choice<'T1, 'T2>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2) = AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>(value, _primT1init, _primT1update, _primT1view, _T1init, _T1update, _T1view, _primT2init, _primT2update, _primT2view, _T2init, _T2update, _T2view)
    member __.Current = __adaptive
    member __.Update(value : Choice<'T1, 'T2>) =
        if Operators.not((ShallowEqualityComparer<Choice<'T1, 'T2>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Operators.not((ShallowEqualityComparer<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current

type AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> =
    abstract member Update : Choice<'T1, 'T2, 'T3> -> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
type private AdaptiveChoiceChoice1Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>(Item : 'T1, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) =
    let _Item_ = _T1init Item
    let mutable __Item = Item
    member __.Update(Item : 'T1) =
        if Operators.not((ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T1update _Item_ Item)
    member __.Item = _T1view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> with
        member x.Update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice2Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice3Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
type private AdaptiveChoiceChoice2Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>(Item : 'T2, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) =
    let _Item_ = _T2init Item
    let mutable __Item = Item
    member __.Update(Item : 'T2) =
        if Operators.not((ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T2update _Item_ Item)
    member __.Item = _T2view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> with
        member x.Update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice2Of3(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice3Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
type private AdaptiveChoiceChoice3Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>(Item : 'T3, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) =
    let _Item_ = _T3init Item
    let mutable __Item = Item
    member __.Update(Item : 'T3) =
        if Operators.not((ShallowEqualityComparer<'T3>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T3update _Item_ Item)
    member __.Item = _T3view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> with
        member x.Update(value : Choice<'T1, 'T2, 'T3>) =
            match value with
            | Choice.Choice1Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice2Of3(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
            | Choice.Choice3Of3(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
type AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>(value : Choice<'T1, 'T2, 'T3>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>>()
    let mutable __value = value
    let mutable __current =
        match value with
        | Choice.Choice1Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
        | Choice.Choice2Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
        | Choice.Choice3Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2, 'T3>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) =
        match value with
        | Choice.Choice1Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
        | Choice.Choice2Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
        | Choice.Choice3Of3(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of3(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>
    static member Create(value : Choice<'T1, 'T2, 'T3>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3) = AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>(value, _primT1init, _primT1update, _primT1view, _T1init, _T1update, _T1view, _primT2init, _primT2update, _primT2view, _T2init, _T2update, _T2view, _primT3init, _primT3update, _primT3view, _T3init, _T3update, _T3view)
    member __.Current = __adaptive
    member __.Update(value : Choice<'T1, 'T2, 'T3>) =
        if Operators.not((ShallowEqualityComparer<Choice<'T1, 'T2, 'T3>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Operators.not((ShallowEqualityComparer<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current

type AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> =
    abstract member Update : Choice<'T1, 'T2, 'T3, 'T4> -> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
type private AdaptiveChoiceChoice1Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(Item : 'T1, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
    let _Item_ = _T1init Item
    let mutable __Item = Item
    member __.Update(Item : 'T1) =
        if Operators.not((ShallowEqualityComparer<'T1>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T1update _Item_ Item)
    member __.Item = _T1view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> with
        member x.Update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
type private AdaptiveChoiceChoice2Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(Item : 'T2, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
    let _Item_ = _T2init Item
    let mutable __Item = Item
    member __.Update(Item : 'T2) =
        if Operators.not((ShallowEqualityComparer<'T2>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T2update _Item_ Item)
    member __.Item = _T2view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> with
        member x.Update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice2Of4(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
type private AdaptiveChoiceChoice3Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(Item : 'T3, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
    let _Item_ = _T3init Item
    let mutable __Item = Item
    member __.Update(Item : 'T3) =
        if Operators.not((ShallowEqualityComparer<'T3>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T3update _Item_ Item)
    member __.Item = _T3view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> with
        member x.Update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice3Of4(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice4Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
type private AdaptiveChoiceChoice4Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(Item : 'T4, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
    let _Item_ = _T4init Item
    let mutable __Item = Item
    member __.Update(Item : 'T4) =
        if Operators.not((ShallowEqualityComparer<'T4>.ShallowEquals(Item, __Item))) then
            __Item <- Item
            ignore (_T4update _Item_ Item)
    member __.Item = _T4view _Item_
    interface AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> with
        member x.Update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
            match value with
            | Choice.Choice1Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice2Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice3Of4(Item) ->
                let inline __arg5 (o : System.Object) (v : 'T1) =
                    ignore (_T1update (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'T2) =
                    ignore (_T2update (unbox<System.Object> o) v)
                    o
                let inline __arg17 (o : System.Object) (v : 'T3) =
                    ignore (_T3update (unbox<System.Object> o) v)
                    o
                let inline __arg23 (o : System.Object) (v : 'T4) =
                    ignore (_T4update (unbox<System.Object> o) v)
                    o
                AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
            | Choice.Choice4Of4(Item) ->
                x.Update(Item)
                x :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
type AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(value : Choice<'T1, 'T2, 'T3, 'T4>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
    inherit Adaptify.AdaptiveValue<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>>()
    let mutable __value = value
    let mutable __current =
        match value with
        | Choice.Choice1Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice2Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice3Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice4Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : Choice<'T1, 'T2, 'T3, 'T4>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) =
        match value with
        | Choice.Choice1Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice1Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice2Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice2Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice3Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice3Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
        | Choice.Choice4Of4(Item) ->
            let inline __arg5 (o : System.Object) (v : 'T1) =
                ignore (_T1update (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'T2) =
                ignore (_T2update (unbox<System.Object> o) v)
                o
            let inline __arg17 (o : System.Object) (v : 'T3) =
                ignore (_T3update (unbox<System.Object> o) v)
                o
            let inline __arg23 (o : System.Object) (v : 'T4) =
                ignore (_T4update (unbox<System.Object> o) v)
                o
            AdaptiveChoiceChoice4Of4(Item, (fun (v : 'T1) -> _primT1init v :> System.Object), (fun (o : System.Object) (v : 'T1) -> _primT1update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT1view (unbox<System.Object> o)), (fun (v : 'T1) -> _T1init v :> System.Object), __arg5, (fun (o : System.Object) -> _T1view (unbox<System.Object> o)), (fun (v : 'T2) -> _primT2init v :> System.Object), (fun (o : System.Object) (v : 'T2) -> _primT2update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT2view (unbox<System.Object> o)), (fun (v : 'T2) -> _T2init v :> System.Object), __arg11, (fun (o : System.Object) -> _T2view (unbox<System.Object> o)), (fun (v : 'T3) -> _primT3init v :> System.Object), (fun (o : System.Object) (v : 'T3) -> _primT3update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT3view (unbox<System.Object> o)), (fun (v : 'T3) -> _T3init v :> System.Object), __arg17, (fun (o : System.Object) -> _T3view (unbox<System.Object> o)), (fun (v : 'T4) -> _primT4init v :> System.Object), (fun (o : System.Object) (v : 'T4) -> _primT4update (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primT4view (unbox<System.Object> o)), (fun (v : 'T4) -> _T4init v :> System.Object), __arg23, (fun (o : System.Object) -> _T4view (unbox<System.Object> o))) :> AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>
    static member Create(value : Choice<'T1, 'T2, 'T3, 'T4>, _primT1init : 'T1 -> System.Object, _primT1update : System.Object -> 'T1 -> System.Object, _primT1view : System.Object -> '_primT1, _T1init : 'T1 -> System.Object, _T1update : System.Object -> 'T1 -> System.Object, _T1view : System.Object -> '_aT1, _primT2init : 'T2 -> System.Object, _primT2update : System.Object -> 'T2 -> System.Object, _primT2view : System.Object -> '_primT2, _T2init : 'T2 -> System.Object, _T2update : System.Object -> 'T2 -> System.Object, _T2view : System.Object -> '_aT2, _primT3init : 'T3 -> System.Object, _primT3update : System.Object -> 'T3 -> System.Object, _primT3view : System.Object -> '_primT3, _T3init : 'T3 -> System.Object, _T3update : System.Object -> 'T3 -> System.Object, _T3view : System.Object -> '_aT3, _primT4init : 'T4 -> System.Object, _primT4update : System.Object -> 'T4 -> System.Object, _primT4view : System.Object -> '_primT4, _T4init : 'T4 -> System.Object, _T4update : System.Object -> 'T4 -> System.Object, _T4view : System.Object -> '_aT4) = AdaptiveChoice<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>(value, _primT1init, _primT1update, _primT1view, _T1init, _T1update, _T1view, _primT2init, _primT2update, _primT2view, _T2init, _T2update, _T2view, _primT3init, _primT3update, _primT3view, _T3init, _T3update, _T3view, _primT4init, _primT4update, _primT4view, _T4init, _T4update, _T4view)
    member __.Current = __adaptive
    member __.Update(value : Choice<'T1, 'T2, 'T3, 'T4>) =
        if Operators.not((ShallowEqualityComparer<Choice<'T1, 'T2, 'T3, 'T4>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Operators.not((ShallowEqualityComparer<AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current
[<AutoOpen>]
module AdaptiveChoice = 
    let (|AdaptiveChoice1Of2|AdaptiveChoice2Of2|) (value : AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of2<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2> as Choice1Of2) -> AdaptiveChoice1Of2(Choice1Of2.Item)
        | (:? AdaptiveChoiceChoice2Of2<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2> as Choice2Of2) -> AdaptiveChoice2Of2(Choice2Of2.Item)
        | _ -> failwith "unreachable"

    let (|AdaptiveChoice1Of3|AdaptiveChoice2Of3|AdaptiveChoice3Of3|) (value : AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> as Choice1Of3) -> AdaptiveChoice1Of3(Choice1Of3.Item)
        | (:? AdaptiveChoiceChoice2Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> as Choice2Of3) -> AdaptiveChoice2Of3(Choice2Of3.Item)
        | (:? AdaptiveChoiceChoice3Of3<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3> as Choice3Of3) -> AdaptiveChoice3Of3(Choice3Of3.Item)
        | _ -> failwith "unreachable"

    let (|AdaptiveChoice1Of4|AdaptiveChoice2Of4|AdaptiveChoice3Of4|AdaptiveChoice4Of4|) (value : AdaptiveChoiceCase<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4>) =
        match value with
        | (:? AdaptiveChoiceChoice1Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> as Choice1Of4) -> AdaptiveChoice1Of4(Choice1Of4.Item)
        | (:? AdaptiveChoiceChoice2Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> as Choice2Of4) -> AdaptiveChoice2Of4(Choice2Of4.Item)
        | (:? AdaptiveChoiceChoice3Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> as Choice3Of4) -> AdaptiveChoice3Of4(Choice3Of4.Item)
        | (:? AdaptiveChoiceChoice4Of4<'T1, '_primT1, '_aT1, 'T2, '_primT2, '_aT2, 'T3, '_primT3, '_aT3, 'T4, '_primT4, '_aT4> as Choice4Of4) -> AdaptiveChoice4Of4(Choice4Of4.Item)
        | _ -> failwith "unreachable"
type AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError> =
    abstract member Update : Result<'T, 'TError> -> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
type private AdaptiveResultOk<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>(ResultValue : 'T, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT, _primTErrorinit : 'TError -> System.Object, _primTErrorupdate : System.Object -> 'TError -> System.Object, _primTErrorview : System.Object -> '_primTError, _TErrorinit : 'TError -> System.Object, _TErrorupdate : System.Object -> 'TError -> System.Object, _TErrorview : System.Object -> '_aTError) =
    let _ResultValue_ = _Tinit ResultValue
    let mutable __ResultValue = ResultValue
    member __.Update(ResultValue : 'T) =
        if Operators.not((ShallowEqualityComparer<'T>.ShallowEquals(ResultValue, __ResultValue))) then
            __ResultValue <- ResultValue
            ignore (_Tupdate _ResultValue_ ResultValue)
    member __.ResultValue = _Tview _ResultValue_
    interface AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError> with
        member x.Update(value : Result<'T, 'TError>) =
            match value with
            | Result.Ok(ResultValue) ->
                x.Update(ResultValue)
                x :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
            | Result.Error(ErrorValue) ->
                let inline __arg5 (o : System.Object) (v : 'T) =
                    ignore (_Tupdate (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'TError) =
                    ignore (_TErrorupdate (unbox<System.Object> o) v)
                    o
                AdaptiveResultError(ErrorValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
type private AdaptiveResultError<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>(ErrorValue : 'TError, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT, _primTErrorinit : 'TError -> System.Object, _primTErrorupdate : System.Object -> 'TError -> System.Object, _primTErrorview : System.Object -> '_primTError, _TErrorinit : 'TError -> System.Object, _TErrorupdate : System.Object -> 'TError -> System.Object, _TErrorview : System.Object -> '_aTError) =
    let _ErrorValue_ = _TErrorinit ErrorValue
    let mutable __ErrorValue = ErrorValue
    member __.Update(ErrorValue : 'TError) =
        if Operators.not((ShallowEqualityComparer<'TError>.ShallowEquals(ErrorValue, __ErrorValue))) then
            __ErrorValue <- ErrorValue
            ignore (_TErrorupdate _ErrorValue_ ErrorValue)
    member __.ErrorValue = _TErrorview _ErrorValue_
    interface AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError> with
        member x.Update(value : Result<'T, 'TError>) =
            match value with
            | Result.Ok(ResultValue) ->
                let inline __arg5 (o : System.Object) (v : 'T) =
                    ignore (_Tupdate (unbox<System.Object> o) v)
                    o
                let inline __arg11 (o : System.Object) (v : 'TError) =
                    ignore (_TErrorupdate (unbox<System.Object> o) v)
                    o
                AdaptiveResultOk(ResultValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
            | Result.Error(ErrorValue) ->
                x.Update(ErrorValue)
                x :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
type AdaptiveResult<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>(value : Result<'T, 'TError>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT, _primTErrorinit : 'TError -> System.Object, _primTErrorupdate : System.Object -> 'TError -> System.Object, _primTErrorview : System.Object -> '_primTError, _TErrorinit : 'TError -> System.Object, _TErrorupdate : System.Object -> 'TError -> System.Object, _TErrorview : System.Object -> '_aTError) =
    inherit Adaptify.AdaptiveValue<AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>>()
    let mutable __value = value
    let mutable __current =
        match value with
        | Result.Ok(ResultValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (_TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveResultOk(ResultValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
        | Result.Error(ErrorValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (_TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveResultError(ErrorValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (t : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member CreateAdaptiveCase(value : Result<'T, 'TError>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT, _primTErrorinit : 'TError -> System.Object, _primTErrorupdate : System.Object -> 'TError -> System.Object, _primTErrorview : System.Object -> '_primTError, _TErrorinit : 'TError -> System.Object, _TErrorupdate : System.Object -> 'TError -> System.Object, _TErrorview : System.Object -> '_aTError) =
        match value with
        | Result.Ok(ResultValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (_TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveResultOk(ResultValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
        | Result.Error(ErrorValue) ->
            let inline __arg5 (o : System.Object) (v : 'T) =
                ignore (_Tupdate (unbox<System.Object> o) v)
                o
            let inline __arg11 (o : System.Object) (v : 'TError) =
                ignore (_TErrorupdate (unbox<System.Object> o) v)
                o
            AdaptiveResultError(ErrorValue, (fun (v : 'T) -> _primTinit v :> System.Object), (fun (o : System.Object) (v : 'T) -> _primTupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTview (unbox<System.Object> o)), (fun (v : 'T) -> _Tinit v :> System.Object), __arg5, (fun (o : System.Object) -> _Tview (unbox<System.Object> o)), (fun (v : 'TError) -> _primTErrorinit v :> System.Object), (fun (o : System.Object) (v : 'TError) -> _primTErrorupdate (unbox<System.Object> o) v :> System.Object), (fun (o : System.Object) -> _primTErrorview (unbox<System.Object> o)), (fun (v : 'TError) -> _TErrorinit v :> System.Object), __arg11, (fun (o : System.Object) -> _TErrorview (unbox<System.Object> o))) :> AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>
    static member Create(value : Result<'T, 'TError>, _primTinit : 'T -> System.Object, _primTupdate : System.Object -> 'T -> System.Object, _primTview : System.Object -> '_primT, _Tinit : 'T -> System.Object, _Tupdate : System.Object -> 'T -> System.Object, _Tview : System.Object -> '_aT, _primTErrorinit : 'TError -> System.Object, _primTErrorupdate : System.Object -> 'TError -> System.Object, _primTErrorview : System.Object -> '_primTError, _TErrorinit : 'TError -> System.Object, _TErrorupdate : System.Object -> 'TError -> System.Object, _TErrorview : System.Object -> '_aTError) = AdaptiveResult<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>(value, _primTinit, _primTupdate, _primTview, _Tinit, _Tupdate, _Tview, _primTErrorinit, _primTErrorupdate, _primTErrorview, _TErrorinit, _TErrorupdate, _TErrorview)
    member __.Current = __adaptive
    member __.Update(value : Result<'T, 'TError>) =
        if Operators.not((ShallowEqualityComparer<Result<'T, 'TError>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            let __n = __current.Update(value)
            if Operators.not((ShallowEqualityComparer<AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>>.ShallowEquals(__n, __current))) then
                __current <- __n
                __.MarkOutdated()
    override __.Compute(t : FSharp.Data.Adaptive.AdaptiveToken) = __current
[<AutoOpen>]
module AdaptiveResult = 
    let (|AdaptiveOk|AdaptiveError|) (value : AdaptiveResultCase<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError>) =
        match value with
        | (:? AdaptiveResultOk<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError> as Ok) -> AdaptiveOk(Ok.ResultValue)
        | (:? AdaptiveResultError<'T, '_primT, '_aT, 'TError, '_primTError, '_aTError> as Error) -> AdaptiveError(Error.ErrorValue)
        | _ -> failwith "unreachable"