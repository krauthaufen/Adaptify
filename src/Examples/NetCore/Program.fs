open Model
open FSharp.Data.Adaptive


[<EntryPoint>]
let main _argv =
    printfn "hi there"

    let model = 
        AdaptiveModel.create {
            set     = HashSet.empty
            all     = HashMap.empty
            value   = 0
            test    = IndexList.empty
            foo     = "asdasda"
            bar     = HashMap.empty
            nested  = Unchecked.defaultof<_>
        }

    let res = AdaptiveMyUnion.create (CaseA(10, 0.2))

    match AVal.force res with
    | AdaptiveCaseA a -> ()
    | AdaptiveCaseB v -> ()
    model.nested.list
    let _list   : alist<AdaptiveModel> = model.test
    let _value  : aval<int> = model.value
    let _all    : amap<int, AdaptiveModel> = model.all
    let _foo    : aval<string> = model.foo
    let _bar    : amap<int, string> = model.bar

    0