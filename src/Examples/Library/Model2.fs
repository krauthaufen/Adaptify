namespace LibraryModel2

open Adaptify
open FSharp.Data.Adaptive
open LibraryModel

[<ModelType>]
type Soup =    
    {
        important : Thing
        many : list<Thing>
        testy : list<int>
        things : HashMap<int, Thing>
    }
    