module Test

open LibraryModel
open LibraryModel2
open FSharp.Data.Adaptive

let test () =
    let model = { testy = [1;2;3]; many = [{ name = "hans"; guhtest = 1 }]; important = { name = "hans"; guhtest = 1 }; things = HashMap.empty }
    let soup = AdaptiveSoup.Create model

    let r = soup.many.GetReader()
    let rt = soup.testy.GetReader()
    let ops = r.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" (r.State |> IndexList.map (fun v -> AVal.force v.Current))
    let ops = rt.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" rt.State
    
    let model = { model with testy = [1;2;2;3]; many = { (List.head model.many) with guhtest = 2 } :: List.tail model.many }
    transact (fun () -> soup.Update model)
    let ops = r.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" (r.State |> IndexList.map (fun v -> AVal.force v.Current))
    let ops = rt.GetChanges AdaptiveToken.Top
    printfn "%A" ops
    printfn "%A" rt.State
    
    
    
    
    
    
    let res : amap<int, AdaptiveThing> = soup.things
    let res : AdaptiveThing = soup.important
    ()
