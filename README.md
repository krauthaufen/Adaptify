# Adaptify

[![Publish](https://github.com/krauthaufen/Adaptify/actions/workflows/publish.yml/badge.svg)](https://github.com/krauthaufen/Adaptify/actions/workflows/publish.yml) [![Discord](https://discordapp.com/api/guilds/611129394764840960/widget.png)](https://discord.gg/UyecnhM)

Adaptify provides a MSBuild plugin for automatically *incrementalizing* F# types using [FSharp.Data.Adaptive](https://github.com/fsprojects/FSharp.Data.Adaptive). It generates associated types for all types in the project that are marked with the `ModelType` attribute. It currently achieves that via inserting generated files (suffixed with `.g.fs`) in your project that contain the adaptive reprensentations for all necessary types.

### Notes

This project also contains a command line version of the generator which will be published as a dotnet tool when its done.

Note that the MSBuild plugin cannot be built on Linux/MacOS currently. Nonetheless it should work on these platforms.
Currently there are efforts to improve the MSBuild integration, so in case I break something for VS/VSCode let me know...

The generated files (and their base library) are fable-compatible and the generated package includes the `fable/` folder, so all of this should simply be usable in fable projects.

### Example

Consider the following definitions:
```fsharp
// Model.fs
namespace Model

[<ModelType>]
type Thing =
    {
        name  : string
        value : int
    }

[<ModelType>]
type Model =
    {
        foo : int
        bar : string
        things : IndexList<Thing>
    }
```

Adaptify will generate two associated types that conceptually have the following signature:

```fsharp
// Model.g.fs
namespace Model

type AdaptiveThing =
    // all properties are adaptified
    member name : aval<string>
    member value : aval<int>
    
    // update to a new value 
    // (changing the contained avals/alists/etc.)
    member Update : Thing -> unit
    
    // the current value as aval of the input-type
    member Current : aval<Thing>

    // creates a new AdaptiveThing given an initial state
    static member Create : Thing -> AdaptiveThing


type AdaptiveModel =
    member foo : aval<int>
    member bar : aval<string>
    member things : alist<AdaptiveThing>
    
    member Update : Model -> unit
    member Current : aval<Model>
    static member Create : Model -> AdaptiveModel

```

### Semantics
Since this type association can be quite involved at times here's a complete table showing how the translation works (where `α` denotes the adaptify-type-function)


| Immutable                 | Adaptive                          | Remarks                                  |
| ------------------------- | --------------------------------- | ---------------------------------------- |
| `α(IndexList<'T>)`        | `alist<α('T)>`                    | `alist<aval<'T>>` -> `alist<'T>`         |
| `α(HashSet<'T>)`          | `aset<α('T)>`                     | `aset<aval<'T>>` -> `aset<'T>`           |
| `α(HashMap<'K, 'V>)`      | `amap<'K, α('V)>`                 | `amap<'K, aval<'V>>` -> `amap<'K, 'V>`   |
| `α({ a:'T;.. })`          | `{ a:α('T);..}`                   | product types                            |
| `α('T * ..)`              | `(α('T) * ..)`                    | tuples                                   |
| `α(struct('T * ..))`      | `(α('T) * ..)`                    | struct tuples                            |
| `α(\|A of 'T..)`          | `aval<\|AdaptiveA of α('T)..>`    | sum types                                |
| `α('T)`                   | `aval<'T>`                        | *opaque* types                           |

### Compilation
The MSBuild plugin inserts the generated files in your project directly after the original files. This means that you need to put  **model types in a separate file** in order to *see* their adaptified version in subsequent files for e.g. view functions.
We worked hard to avoid this, but couldn't figure out a way of working around this limitation yet. Once type providers are capable of taking types as arguments (see https://github.com/fsharp/fslang-suggestions/issues/212) this limitation might be overcome.

### Adaptifying third party types
Foreign types (for example from third party libraries, etc.) can be adaptified afterwards! Simply create a type alias for the foreign type to be adaptified.

```fsharp 
[<ModelType>]
type MyTime = System.DateTime

..

let t = AdaptiveMyTime(System.DateTime.Now)
t.Day |> AVal.map (fun d -> "Today is "+string d)
```

### Namespaces
The generated types will be defined in the same namespace as your original types if the original ones were not defined inside a module.

If they are defined in a module the generated types will get defined in a somewhat strange scope. Consider:
```fsharp
namespace A
module B =
    [<ModelType>]
    type Foo = { bar : int }
```

The generator will then emit
```fsharp
namespace A

[<AutoOpen>]
module Adaptify =
    module B =
        [<ModelType>]
        type AdaptiveFoo = // ...
```

We sadly couldn't figure out a better way for modules, since they can only be declared once per assembly (no partial modules).  
Generally speaking we **strongly recommend that you put your model types directly in namespaces** to avoid confusion.


### Sum Types (Unions)
Union types receive special treatment and it can be irritating how they capture two kinds of changes.
Therefore I'll give a short overview here.

```fsharp
[<ModelType>]
type Union =
    | A of int
    | B of string
```

Let's consider translating that in a *natural* way:
```fsharp
type AdaptiveUnionCase =
    | AdaptiveA of aval<int>
    | AdaptiveB of aval<string>
```

This cannot capture all changes happening, since there is no adaptive reprensentation of the current union case.
Therefore our system generates an additional type:

```fsharp
type AdaptiveUnion =
    interface aval<AdaptiveUnionCase>
    member update : Union -> unit
    new : Union -> AdaptiveUnion
    // ...
```

This way we can capture both kinds of changes:
* inner field changes are handled by the respective case-types
* case/constructor changes are handled by the outer wrapper

Note that these `aval` instances will be removed whenever they occur inside changeable collections 
Take for example `alist<AdaptiveUnion>` (which is conceptually identical to `alist<aval<AdaptiveUnionCase>>`) will be turned into `alist<AdaptiveUnionCase>`, since the outer list can take care of the case changes here.



### Generics
Adaptify has support for generic model-types but the compilation process is quite involved. However top-level model-types should generally not be generic, since their creation requires a lot of information users cannot easily supply themselves.
Here's an example for illustration purposes: (you don't have to understand this to use the library)

```fsharp
[<ModelType>]
type Model<'T> =
    { 
        value : 'T
        list : IndexList<'T>
    }
```

When instantiating `Model` with `int` the desired output is:

```fsharp
type AdaptiveModelOfInt =
    value : aval<int>
    list : alist<int> // note that this is not alist<aval<int>>!
```
However when instantiating `Model` with a model-type `Thing` the desired output is:

```fsharp
type AdaptiveModelOfThing  =
    value : AdaptiveThing
    list : alist<AdaptiveThing> // same inner type like value!
```

To account for this kind of flexibility our compiled generic model-types replicate their type arguments s.t. the new type-arguments can  be used to represent these different variants.

So the *real* generated type will look like:
```fsharp
type AdaptiveModel<'T, 'T1, 'T2> =
    member value : 'T2
    member list : alist<'T1>
    
    new : 
        Model<'T> * 
        ('T -> obj) * (obj -> 'T -> obj) * (obj -> 'T1) * 
        ('T -> obj) * (obj -> 'T -> obj) * (obj -> 'T2) -> AdaptiveModel<'T, 'T1, 'T2>
```

As you see there are lots of functions in the constructor that account for inner changes of the generic values. 
For more information please see the generator code or feel free to contact us on gitter/discord.
However as long as the top-level model is not generic you should never need to worry about these details.
