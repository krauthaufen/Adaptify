//open Model
open FSharp.Data.Adaptive
open Adaptify.FSharp.Core

[<EntryPoint>]
let main _argv =
    //let face1 = { new Blubber with member x.Sepp = 100 }
    //let face2 = { new Blubber with member x.Sepp = 56 }

    //let r = 
    //    AdaptiveRecord { 
    //        fa = Some face1
    //        fb = IndexList.empty 
    //        fc = struct (1.0, "")
    //        x = Some 10
    //        test = Choice2Of2 2
    //    }

    //let a = r.fa |> AVal.map id

    //match AVal.force a with
    //| AdaptiveOption.AdaptiveNone -> ()
    //| AdaptiveSome v -> ()


    //printfn "%A: %A" a.OutOfDate (AVal.force a)

    //transact (fun () ->
    //    r.update { fa = Some face2; fb = IndexList.empty; fc = struct (2.0, "abc"); x = Some 123; test = Choice2Of2 2 }
    //)
    //printfn "%A: %A" a.OutOfDate (AVal.force a)



    //let ff : Adaptivefff = failwith ""
    //let a : AdaptiveRecy = failwith ""

    //match AVal.force ff with
    //| Adaptivefff.AdaptiveAaa a -> ()
    //| Adaptivefff.AdaptiveGgg g -> ()
    //| Adaptivefff.AdaptiveYYY -> ()

    //let _test =
    //    a.g.y |> AList.map (fun c ->
    //        match c with
    //        | AdaptiveCaseA(a, b) ->
    //            a |> AVal.map float
    //        | AdaptiveCaseB(x) ->
    //            x
    //    )

    //match AVal.force a.g.x with
    //| AdaptiveMyUnion.AdaptiveCaseA(a, b) -> ()
    //| AdaptiveMyUnion.AdaptiveCaseB(x) -> ()
    
    0