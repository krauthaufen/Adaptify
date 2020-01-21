namespace LibraryModel

open Adaptify
open FSharp.Data.Adaptive



[<ModelType>]
type MyUnion =
    | A of int
    | B of float

[<ModelType>]
type MyModel =  
    {
        a : Result<MyModel, string>
        b : float
        map : HashMap<int, MyModel>
    }

