namespace Model

open FSharp.Data.Adaptive
open Adaptify

[<ModelType>]
type Model =
    {
        all     : HashSet<int>
        value   : int
        test    : IndexList<int>
        foo     : string
    }
