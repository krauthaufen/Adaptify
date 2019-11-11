namespace Model


open FSharp.Data.Adaptive
open Adaptify


[<ModelType>]
type IFace =
    abstract member Sepp: int

type Blubber = IFace
//type Bar = (struct(float * string))

[<ModelType>]
type MyResult<'a, 'b> = Result<'a, 'b>

[<ModelType>]
type Record =
    {
        foo : int
        fa : Choice<Blubber, Blubber>
        fb : IndexList<Record>
        fc : IndexList<Blubber>
        x : Option<int>
        test : MyResult<Record, int>
    }


[<ModelType>]
type MyUnion<'a, 'b> =
    | [<NonAdaptive>] CaseA of value : int * dst : 'a
    | CaseB of 'b
    

[<ModelType>]
type Geny<'a, 'b> =
    {
        a : MyUnion<'a, 'b>
        b : IndexList<MyUnion<'a, 'b>>
    }


[<ModelType>]
type Seppy<'a> =
    {
        x : 'a
        y : IndexList<'a>
    }

[<ModelType>]
type Recy =
    {
        u : MyUnion<int, Recy>
    }


[<ModelType>]
type fff =
    | Ggg of int
    | Aaa of string
    | YYY 
[<AutoOpen>]
module Test =
    [<ModelType; Struct>]
    type X(a : int) =
        member x.A = (a, "asd")
    

//    [<ModelType>]
//    type Y = { ya : struct (int * float) }

//[<ModelType>]
//type Thing =
//    {
//        name : IndexList<IndexList<string>>
//    }

//[<ModelType>]
//type Foo<'a> =
//    {
//        arr     : HashSet<int>[,]
//        value   : 'a
//        list    : IndexList<'a>
//    }
//    member x.listHead = Seq.tryHead x.list

//[<ModelType>]
//type Model =
//    {
//        [<NonAdaptive>]
//        set     : HashSet<int>
//        [<TreatAsValue>]
//        all     : HashMap<int, Model>
//        value   : int
//        test    : IndexList<Model>
//        foo     : string
//        bar     : HashMap<int, string>
//        nested  : Foo<int>
//        nested2 : Foo<Thing>
//    }
    
