namespace Model

open FSharp.Data.Adaptive
open Adaptify

[<ModelType>]
type Thing =
    {
        name : string
    }

[<ModelType>]
type Foo<'a> =
    {
        list : IndexList<'a>
    }


[<ModelType>]
type Model =
    {
        set     : HashSet<int>
        all     : HashMap<int, Model>
        value   : int
        test    : IndexList<Model>
        foo     : string
        bar     : HashMap<int, string>
        nested  : Foo<int>
    }

[<ModelType>]
type MyUnion =
    | CaseA of value : int * dst : float
    | CaseB of Model
