namespace rec LibraryModel2

open Adaptify
open FSharp.Data.Adaptive
open LibraryModel

[<ModelType>]
type Blubber =
    | Hans of id : int
    | Hugo of name : string * surname : string

[<ModelType>]
type Thing =    
    {
        blubber : Blubber
        name : string
        someProp : int
    }
    
[<ModelType>]
type Soup =    
    {
        important : Thing
        things : HashMap<int, Thing>
        names : HashSet<string>
    }
    
type BlubberHugoDelta =
    | SetName of name : string
    | SetSurname of surname : string

type BlubberDelta =
    | SetHans of value : int
    | UpdateHugo of delta : Microsoft.FSharp.Collections.list<BlubberHugoDelta>
    | SetHugo of name : string * surname : string

type ThingDelta =
    | UpdateBlubber of delta : Microsoft.FSharp.Collections.list<BlubberDelta>
    | SetName of name : string
    | SetSomeProp of someProp : int

type SoupDelta =
    | UpdateImportant of delta : Microsoft.FSharp.Collections.list<ThingDelta>
    | UpdateThings of delta : HashMap<int, Adaptify.ElementDelta<Thing, Microsoft.FSharp.Collections.list<ThingDelta>>>
    | UpdateNames of delta : HashSetDelta<string>

module Blubber =
    let computeDelta (a : Blubber) (b : Blubber) =
        match b with
        | LibraryModel2.Blubber.Hans(bid) ->
            match a with
            | LibraryModel2.Blubber.Hans(aid) ->
                if Operators.not((ShallowEqualityComparer<int>.ShallowEquals(aid, bid))) then
                    [
                        BlubberDelta.SetHans(bid)
                    ]
                else
                    List.empty<BlubberDelta>
            | _ ->
                [
                    BlubberDelta.SetHans(bid)
                ]
        | LibraryModel2.Blubber.Hugo(bname, bsurname) ->
            match a with
            | LibraryModel2.Blubber.Hugo(aname, asurname) ->
                if Operators.not((ShallowEqualityComparer<string>.ShallowEquals(aname, bname) && ShallowEqualityComparer<string>.ShallowEquals(asurname, bsurname))) then
                    let result =
                        [
                            if Operators.not((ShallowEqualityComparer<string>.ShallowEquals(aname, bname))) then LibraryModel2.BlubberHugoDelta.SetName(bname)
                            if Operators.not((ShallowEqualityComparer<string>.ShallowEquals(asurname, bsurname))) then LibraryModel2.BlubberHugoDelta.SetSurname(bsurname)
                        ]
                    if Microsoft.FSharp.Collections.List.isEmpty(result) then
                        List.empty<BlubberDelta>
                    else
                        [
                            BlubberDelta.UpdateHugo(result)
                        ]
                else
                    List.empty<BlubberDelta>
            | _ ->
                [
                    BlubberDelta.SetHugo(bname, bsurname)
                ]
module Thing =
    let computeDelta (a : Thing) (b : Thing) =
        [
            if Operators.not((ShallowEqualityComparer<LibraryModel2.Blubber>.ShallowEquals(a.blubber, b.blubber))) then
                let delta = LibraryModel2.Blubber.computeDelta a.blubber b.blubber
                if Operators.not((Microsoft.FSharp.Collections.List.isEmpty(delta))) then ThingDelta.UpdateBlubber(delta)
            if Operators.not((DefaultEqualityComparer.Instance.Equals(a.name, b.name))) then ThingDelta.SetName(b.name)
            if Operators.not((DefaultEqualityComparer.Instance.Equals(a.someProp, b.someProp))) then ThingDelta.SetSomeProp(b.someProp)
        ]
module Soup =
    let computeDelta (a : Soup) (b : Soup) =
        [
            if Operators.not((ShallowEqualityComparer<LibraryModel2.Thing>.ShallowEquals(a.important, b.important))) then
                let delta = LibraryModel2.Thing.computeDelta a.important b.important
                if Operators.not((Microsoft.FSharp.Collections.List.isEmpty(delta))) then SoupDelta.UpdateImportant(delta)
            if Operators.not((ShallowEqualityComparer<HashMap<int, LibraryModel2.Thing>>.ShallowEquals(a.things, b.things))) then
                let delta = HashMapDelta.toHashMap((HashMap.computeDelta a.things b.things))
                let vv =
                    let inline __arg2 (a : LibraryModel2.Thing) (b : LibraryModel2.Thing) =
                        let result = LibraryModel2.Thing.computeDelta a b
                        if Operators.not((Microsoft.FSharp.Collections.List.isEmpty(result))) then Option.Some(result) else Option.None
                    Adaptify.HashMap.getElementDeltas a.things delta __arg2
                if Operators.not((HashMap.isEmpty(vv))) then SoupDelta.UpdateThings(vv)
            if Operators.not((ShallowEqualityComparer<HashSet<string>>.ShallowEquals(a.names, b.names))) then
                let delta = HashSet.computeDelta a.names b.names
                if Operators.not((HashSetDelta.isEmpty(delta))) then SoupDelta.UpdateNames(delta)
        ]