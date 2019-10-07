open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =
    let model = 
        AdaptiveModel.create {
            all     = HashSet.empty
            value   = 0
            test    = IndexList.empty
            foo     = "asdasda"
        }

    let _list   : alist<int> = model.test
    let _value  : aval<int> = model.value
    let _all    : aset<int> = model.all
    let _foo    : aval<string> = model.foo

    0