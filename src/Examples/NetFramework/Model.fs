namespace Model

open Adaptify
open FSharp.Data.Adaptive

[<ModelType>]
type MyUnion =
    | A of int
    | B of float

[<ModelType>]
type MyModel =  
    {    
        i : int   
        a : Result<MyModel, string> 

        b : float 
        //list : HashSet<MyModel>
        map : HashMap<int, MyModel> 
    }

// THIS MODULES CAUSES CRASH! (type with same name is created witihn ModuleMixUp.fs)
module BadBoy = 
    
    let this () = "asdf"

//open FSharp.Data.Adaptive
//open Adaptify


//[<ModelType>]
//type IFace =
//    abstract member Sepp: int

//type Blubber = IFace
////type Bar = (struct(float * string))

//[<ModelType>]
//type Record =
//    {
//        fa : Choice<Blubber, Record>
//        fb : IndexList<Record>
//        fc : IndexList<Blubber>
//        x : Option<int>
//        [<TreatAsValue>]
//        test : Choice<Record, int>
//    }


//[<ModelType>]
//type MyUnion =
//    | CaseA of value : int * dst : string
//    | CaseB of float
    

//[<ModelType>]
//type Geny<'a, 'b> =
//    {
//        a : MyUnion<'a, 'b>
//        b : IndexList<MyUnion<'a, 'b>>
//    }


//[<ModelType>]
//type Seppy<'a> =
//    {
//        x : 'a
//        y : IndexList<'a>
//    }

//[<ModelType>]
//type Recy =
//    {
//        f : Geny<int, Recy>
//        g : Seppy<MyUnion<int, float>>
//    }


//[<ModelType>]
//type fff =
//    | Ggg of int
//    | Aaa of string
//    | YYY 
//[<AutoOpen>]
//module Test =
//    [<ModelType; Struct>]
//    type X(a : int) =
//        member x.A = (a, "asd")
    

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
    
