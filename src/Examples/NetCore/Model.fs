namespace Model

open FSharp.Data.Adaptive
open Adaptify

[<ModelType>]
type Model =
    {
        all     : HashMap<int, Model>
        value   : int
        test    : IndexList<Model>
        foo     : string
    }
