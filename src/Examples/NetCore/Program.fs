open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =
    printfn "hi there"

    let model = 
        AdaptiveModel.create {
            all     = HashMap.empty
            value   = 0
            test    = IndexList.empty
            foo     = "asdasda"
            bar     = HashMap.empty
        }
    let _list   : alist<AdaptiveModel> = model.test
    let _value  : aval<int> = model.value
    let _all    : amap<int, AdaptiveModel> = model.all
    let _foo    : aval<string> = model.foo
    let _bar    : amap<int, string> = model.bar

    0