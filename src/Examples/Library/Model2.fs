namespace LibraryModel2

open Adaptify
open FSharp.Data.Adaptive
open LibraryModel

[<ModelType>]
type Soup =    
    {
        important : Thing
        [<TreatAsAList>]
        many : list<Thing>
        [<TreatAsAList>]
        testy : list<int>
        things : HashMap<int, Thing>
        
        bla : int[]
    }
    