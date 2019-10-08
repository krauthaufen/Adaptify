open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =
    let model = 
        AdaptiveModel.create {
            all     = HashMap.empty
            value   = 0
            test    = IndexList.empty
            foo     = "asdasda"
        }

    let _list   : alist<AdaptiveModel> = model.test
    let _value  : aval<int> = model.value
    let _all    : amap<int, AdaptiveModel> = model.all
    let _foo    : aval<string> = model.foo

    0