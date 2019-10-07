// Learn more about F# at http://fsharp.org

open System
open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main argv =
    let model = 
        AdaptiveModel.create {
            all     = HashSet.empty
            value   = 0
            test    = IndexList.empty
        }

    let _list : alist<int> = model.test
    let _value : aval<int> = model.value
    let _all : aset<int> = model.all

    0 // return an integer exit code
