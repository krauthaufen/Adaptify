namespace LibraryModel

open Adaptify
open FSharp.Data.Adaptive



[<ModelType>]
type MyUnion =
    | A of int
    | B of float

[<ModelType>]
type Object<'a> =  
    {
        value : 'a
        a : Result<Object<'a>, string>
        b : float
        map : HashMap<int, Object<'a>>
    }

