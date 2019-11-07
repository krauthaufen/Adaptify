open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =

    let r = 
        AdaptiveRecord { 
            fa = 1
            fb = IndexList.empty 
        }

    let a = r.fa |> AVal.map id
    printfn "%A: %A" a.OutOfDate (AVal.force a)

    transact (fun () ->
        r.update { fa = 3; fb = IndexList.empty }
    )
    printfn "%A: %A" a.OutOfDate (AVal.force a)



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