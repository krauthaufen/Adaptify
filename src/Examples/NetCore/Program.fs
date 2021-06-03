open FSharp.Data.Adaptive
open Adaptify
open LibraryModel2

// Input Code for illustration purposed copied here:
module rec GeneratedFile =

    let lastSave : Model = failwith ""

    let writeTodo (index : int) (t : Todo) =
        ()
        
    let deleteTodo (index : int) =
        ()

    type TodoDelta =
        | SetTitle of string
        | SetFinished of bool

    type ModelDelta =
        | Insert of int * Todo
        | Delete of int
        | Update of int * list<TodoDelta>

    module Model =
        let computeDelta (a : Model) (b : Model) : list<ModelDelta> =
            failwith ""

  

    module Persistence =
        let write (name : string) (value : bool) =
            ()
        let delete (name : string) =
            ()

        let move (from : string) (target : string) =
            ()


    let fileName (i : int) (m : Model) : string =
        m.todos.[i].title |> sprintf "%s.txt"


    type Todo = { title : string; finished : bool }
    type Model = { todos : list<Todo> }
    let persistDelta (lastSave : Model) (model : Model) =
        Model.computeDelta lastSave model |> List.iter (function
            | Insert(id, todo)      -> Persistence.write (fileName id model) todo.finished
            | Delete id             -> Persistence.delete (fileName id lastSave)
            | Update(id, ops) ->
                ops |> List.iter (function
                | SetTitle t        -> Persistence.move (fileName id lastSave) (fileName id model)
                | SetFinished f     -> Persistence.write (fileName id model) f
            )
        )

    // D({ f0 : 'a ...})              = [ |UpdateF0 of D('a) |... ]
    // D('a * 'b * ...)               = [ |Update0 of D('a) |Update1 of D('b) |... ]
    // D(|X of 'a |Y of 'a ...)       = [ |SetX of 'a |UpdateX of D('a) |SetY of 'b |UpdateY of D('b) |... ]
    // D(Map<'a, 'b>)                 = [ |Insert of 'a * 'b |Delete of 'a |Update of 'a * D('b) ]
    // D(Set<'a>)                     = [ |Add of 'a | Rem of 'a ]
    // D(Array<'a>)                   = [ |Insert of Index * 'a | Delete of Index | Update of Index * D('a) ]
    // D('a)                          = 'a

    // [ ... |UpdateX of 'a |SetX of 'a ... ] -> [ ... |SetX of 'a ... ]

    type Blubber =
        | Hans of id : int
        | Hugo of name : string * surname : string

    // The Generator transforms these Types:
    type Thing =    
        {
            blubber : Blubber
            name : string
            someProp : int
        }

    type Soup =    
        {
            important : Thing
            things : HashMap<int, Thing>
            names : HashSet<string>
        }

    // Into these Deltas (and creates `computeDelta` functions)
    type BlubberHugoDelta =
        | SetName of name : string
        | SetSurname of surname : string

    //type BlubberHansDelta =
    //    | SetId of id : int

    type BlubberDelta =
        //| UpdateHans of delta : list<BlubberHansDelta>
        | SetHans of value : int
        | UpdateHugo of delta : list<BlubberHugoDelta>
        | SetHugo of name : string * surname : string


    type ThingDelta =
        | UpdateBlubber of delta : list<BlubberDelta>
        | SetName of name : string
        | SetSomeProp of someProp : int

    type SoupDelta =
        | UpdateImportant of delta : list<ThingDelta>
        | UpdateThings of delta : HashMap<int, ElementDelta<Thing, list<ThingDelta>>>
        | UpdateNames of delta : HashSetDelta<string> // HashMap<string, Add|Remove>


// let's define some `Soups`

let soup1 =
    {
        Soup.important = 
            { 
                Thing.name = "Thing1"
                Thing.someProp = 10
                Thing.blubber = Blubber.Hans 25
            }
        Soup.things = 
            HashMap.single 0 { 
                Thing.name = "Thing1"
                Thing.someProp = 10
                Thing.blubber = Blubber.Hans 25
            }
        Soup.names = HashSet.single "yeah"
    }

let soup2 =
    { soup1 with
        important = { soup1.important with name = "Thing2" }
        things = 
            soup1.things |> HashMap.alter 0 (function
                | Some o -> 
                    Some { o with blubber = Blubber.Hugo("a", "b") }
                | None ->
                    None
            )
    }
        
let soup3 =
    { soup2 with
        important = { soup2.important with name = "Thing2" }
        things = 
            soup2.things |> HashMap.alter 0 (function
                | Some o -> 
                    Some { o with blubber = Blubber.Hugo("a", "x") }
                | None ->
                    None
            )
    }
    
let soup4 =
    { soup3 with
        things = 
            soup3.things |> HashMap.add 1 {
                Thing.name = "Jenny"
                Thing.someProp = 123
                Thing.blubber = Blubber.Hans 42
            }
    }
    
let soup5 =
    { soup4 with
        names = HashSet.add "name" soup4.names
    }

let soup6 =
    { soup5 with
        names = HashSet.remove "name" soup5.names
        things = HashMap.remove 0 soup5.things
    }

[<EntryPoint>]
let main _argv =
    // and compute their deltas

    printfn "1 -> 2"
    let delta = Soup.computeDelta soup1 soup2
    for d in delta do
        printfn "  %A" d
    // > UpdateImportant [SetName "Thing2"]
    // > UpdateThings HashMap [(0, Update [UpdateBlubber [SetHugo ("a","b")]])]
    

    printfn "2 -> 3"
    let delta = Soup.computeDelta soup2 soup3
    for d in delta do
        printfn "  %A" d
    // > UpdateThings HashMap [(0, Update [UpdateBlubber [UpdateHugo [SetSurname "x"]]])]
    
    
    printfn "3 -> 4"
    let delta = Soup.computeDelta soup3 soup4
    for d in delta do
        printfn "  %A" d
    // > UpdateThings
    // > HashMap [(1, Insert { blubber = Hans 42
    // >            name = "Jenny"
    // >            someProp = 123 })]
    
    printfn "4 -> 5"
    let delta = Soup.computeDelta soup4 soup5
    for d in delta do
        printfn "  %A" d
    // > UpdateNames HashSetDelta [Add("name")]

    
    printfn "5 -> 6"
    let delta = Soup.computeDelta soup5 soup6
    for d in delta do
        printfn "  %A" d
    0