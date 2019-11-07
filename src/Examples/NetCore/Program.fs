open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =
    printfn "hi there"

    let a : RecyAdaptive = failwith ""
    a.g.y
    //let a = a.g.a.GetValue(Unchecked.defaultof<_>)

    //match a with
    //| :? MyUnionCaseAAdaptive<Recy,RecyAdaptive,RecyAdaptive> as a -> 
    //    a.


    //let x = a.a
    //let y = a.b
    //a.a

    //let a : ModelAdaptive = failwith ""

    //let (_ : alist<int>) = a.nested.list
    //let (_ : aval<int>) = a.nested.value
    
    //let (_ : alist<ThingAdaptive>) = a.nested2.list
    //let (_ : ThingAdaptive) = a.nested2.value

    0