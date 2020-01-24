namespace LibraryModel2

open Adaptify
open FSharp.Data.Adaptive
open LibraryModel

[<ModelType>]
type Soup =    
    {
        important : Thing
        things : HashMap<int, Thing>
    }
    