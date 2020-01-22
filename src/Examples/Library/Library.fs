namespace LibraryModel

open Adaptify
open FSharp.Data.Adaptive



[<ModelType>]
type MyUnion =
    | A of int
    | B of float

[<ModelType>]
type Object =  
    {
        a : Result<Object, string>
        b : float
        map : HashMap<int, Object>
    }

