namespace Adaptify.Compiler

[<AutoOpen>]
module AdaptiveTypes =
    let private fsCollections = Namespace "Microsoft.FSharp.Collections"
    let private fda = Namespace "FSharp.Data.Adaptive"
    let private fdt = Namespace "FSharp.Data.Traceable"
    let private adaptify = Namespace "Adaptify"

    module Option =
        let typ (t : TypeRef) =
            TExtRef(Namespace "Microsoft.FSharp.Core", "Option", [t])
            
        let some (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = true
                name = "Some"
                parameters = [ t ]
                returnType = typ t
            }

        let none (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = true
                name = "get_None"
                parameters = []
                returnType = typ t
            }
 

    module HashSetDelta =
        let typ (t : TypeRef) =    
            TExtRef(fda, "HashSetDelta", [t])
        
        let isEmptyMeth (t : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fda, "HashSetDelta", false, false))
                isStatic = true
                name = "isEmpty"
                parameters = [ t ]
                returnType = TBool
            }
            
    module HashSet =
        let typ (t : TypeRef) =    
            TExtRef(fda, "HashSet", [t])
        
        let computeDeltaExpr (a : Expr) (b : Expr) =
            let t =
                match a.Type with
                | TExtRef(_, _, [t])
                | TRef(_, _, [t])
                | TModel(_, _, [t]) ->
                    t
                | _ ->
                    failwith "not a HashSet"
            let meth = 
                {
                    declaringType = Choice1Of2 (Module(fda, "HashSet", false, false))
                    isStatic = true
                    name = "get_computeDelta"
                    parameters = [ ]
                    returnType = TFunc(a.Type, TFunc(b.Type, HashSetDelta.typ t))
                }
            Expr.Application(
                Expr.Application(
                    Expr.Call(None, meth, []),
                    a
                ),
                b
            )
            
    module ElementDelta =
        let typ (value : TypeRef) (delta : TypeRef) =
            TExtRef(Namespace "Adaptify", "ElementDelta", [value; delta])
            
        let newInsert (value : TypeRef) (delta : TypeRef) (e : Expr) =
            let meth =
                {
                    declaringType = Choice2Of2 (typ value delta)
                    isStatic = true
                    name = "Insert"
                    parameters = [ e.Type ]
                    returnType = typ value delta
                }
            Expr.Call(None, meth, [e])
            
        let newDelete (value : TypeRef) (delta : TypeRef) =
            let meth =
                {
                    declaringType = Choice2Of2 (typ value delta)
                    isStatic = true
                    name = "Delete"
                    parameters = [ ]
                    returnType = typ value delta
                }
            Expr.Call(None, meth, [])
            
        let newUpdate (value : TypeRef) (delta : TypeRef) (e : Expr) =
            let meth =
                {
                    declaringType = Choice2Of2 (typ value delta)
                    isStatic = true
                    name = "Update"
                    parameters = [ e.Type ]
                    returnType = typ value delta
                }
            Expr.Call(None, meth, [e])

    module ElementOperation =
        let typ (t : TypeRef) =
            TExtRef(fda, "ElementOperation", [t])
            
        let newSet (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = true
                name = "Set"
                parameters = [ t ]
                returnType = typ t
            }
            
        let newRemove (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = true
                name = "Remove"
                parameters = [ ]
                returnType = typ t
            }
            

    module HashMapDelta =   
        let typ (k : TypeRef) (op : TypeRef) =
            TExtRef(fda, "HashMapDelta", [k;op])
            
        let toHashMapMeth (k : TypeRef) (v : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fda, "HashMapDelta", false, false))
                isStatic = true
                name = "toHashMap"
                parameters = [ typ k v ]
                returnType = TExtRef(fda, "HashMap", [k;ElementOperation.typ v])
            }
            


    module HashMap =
        let typ (k : TypeRef) (v : TypeRef) =    
            TExtRef(fda, "HashMap", [k;v])
            
        let isEmptyMeth (k : TypeRef) (v : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fda, "HashMap", false, false))
                isStatic = true
                name = "isEmpty"
                parameters = [ typ k v ]
                returnType = TBool
            }
        let computeDeltaExpr (a : Expr) (b : Expr) =
            let k, v = 
                match a.Type with
                    | TRef(_, e, [k;v]) -> k, v
                    | TModel(_, _, [k;v]) -> k, v
                    | TExtRef(Namespace "FSharp.Data.Adaptive", "HashMap", [k;v]) -> k, v
                    | _ -> failwith "not a HashMap"
            let meth = 
                {
                    declaringType = Choice1Of2 (Module(fda, "HashMap", false, false))
                    isStatic = true
                    name = "get_computeDelta"
                    parameters = [ ]
                    returnType = TFunc(a.Type, TFunc(b.Type, HashMapDelta.typ k v))
                }

            Expr.Application(
                Expr.Application(
                    Expr.Call(None, meth, []),
                    a
                ),
                b
            )
            
        let choose2Meth (k : TypeRef) (v1 : TypeRef) (v2 : TypeRef) (ret : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fda, "HashMap", false, false))
                isStatic = true
                name = "choose2"
                parameters = [ TFunc(k, TFunc(Option.typ v1, TFunc(Option.typ v2, Option.typ ret))); typ k v1; typ k v2 ]
                returnType = HashMapDelta.typ k ret
            }

        let getElementDeltasExpr (a : Expr) (b : Expr) (compute : Expr) =

            let k =
                match a.Type with
                | TExtRef(_,_,[k;_]) 
                | TRef(_,_,[k;_]) 
                | TModel(_,_,[k;_]) ->  
                    k
                | _ -> failwith "not a Hashmap"

            let v, d =
                match compute.Type with
                | TFunc(v, TFunc(_, (TRef(_,_,[d]) | TExtRef(_,_,[d]) |TModel(_,_,[d])))) -> v, d
                | _ -> failwith "not a function"
                
            let meth =
                {
                    declaringType = Choice1Of2 (Module(adaptify, "HashMap", false, false))
                    isStatic = true
                    name = "get_getElementDeltas"
                    parameters = [ ]
                    returnType = 
                        TFunc(a.Type, 
                            TFunc(b.Type, 
                                TFunc(      
                                    compute.Type, 
                                    HashMapDelta.typ k (ElementDelta.typ v d)
                                )
                            )
                        )
                    
                }
            Expr.Application(
                Expr.Application(
                    Expr.Application(
                        Expr.Call(None, meth, []),
                        a
                    ),
                    b
                ),
                compute
            )
            

    module IndexList =
        let typ (t : TypeRef) =    
            TExtRef(fda, "IndexList", [t])
            
    module IndexListDelta =
        let typ (t : TypeRef) =    
            TExtRef(fda, "IndexListDelta", [t])

    module AdaptiveToken =
        let typ  =    
            TExtRef(fda, "AdaptiveToken", [])
             
    module AVal =
        let typ (t : TypeRef) =    
            TExtRef(fda, "aval", [t])
            
        let custom (t : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fda, "AVal", false, false))
                isStatic = true
                name = "custom"
                parameters = [ TFunc(AdaptiveToken.typ, t) ]
                returnType = typ t
            }
            
    module Transaction =
        let transact (t : TypeRef) = 
            {
                declaringType = Choice1Of2 (Module(fda, "Transaction", false, false))
                isStatic = true
                name = "transact"
                parameters = [ TFunc(TTuple(false, []), t) ]
                returnType = t
            }

    module List =
        let typ (t : TypeRef) =
            TExtRef(fsCollections, "list", [t])

        let isEmptyMeth (t : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(fsCollections, "List", false, false))
                isStatic = true
                name = "isEmpty"
                parameters = [ t ]
                returnType = TBool
            }
            

    module AdaptiveObject =
        let typ  =    
            TExtRef(fda, "AdaptiveObject", [])
            
        let markOutdated =
            {
                declaringType = Choice2Of2 typ
                isStatic = false
                name = "MarkOutdated"
                parameters = [ ]
                returnType = TTuple(false, [])
            }
            

    module AbstractAdaptiveValue =
        let typ (t : TypeRef) =    
            TExtRef(adaptify, "AdaptiveValue", [t])

    module Lazy =
        let typ (t : TypeRef) =
            TExtRef(Namespace "System", "Lazy", [t])
            
        let ll (f : unit -> 'a) = System.Lazy<_>(f)

        let ctor (t : TypeRef) =
            {
                isStatic = true
                declaringType = Choice1Of2 Global
                name = "lazy"
                parameters = []
                returnType = typ t
            }
        let value (t : TypeRef) =
            {
                isStatic = false
                declaringType = Choice2Of2 (typ t)
                name = "get_Value"
                parameters = []
                returnType = t
            }

        let isValueCreated (t : TypeRef) =
            {
                isStatic = false
                declaringType = Choice2Of2 (typ t)
                name = "get_IsValueCreated"
                parameters = []
                returnType = TBool
            }


    module CVal =
        let typ (t : TypeRef) = 
            TExtRef(fda, "cval", [t])

        let setValue (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = false
                name = "set_Value"
                parameters = [ t ]
                returnType = TTuple(false, [])
            }
            
        let ctor (t : TypeRef) =
            {
                declaringType = Choice1Of2(fda)
                isStatic = true
                name = "cval"
                parameters = [ t ]
                returnType = typ t
            }

    module ASet =
        let typ (t : TypeRef) =    
            TExtRef(fda, "aset", [t])

    module CSet =
        let typ (t : TypeRef) =  
            TExtRef(fda, "cset", [t])

        let setValue (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = false
                name = "set_Value"
                parameters = [ HashSet.typ t ]
                returnType = TTuple(false, [])
            }
            
        let ctor (t : TypeRef) =
            {
                declaringType = Choice1Of2(fda)
                isStatic = true
                name = "cset"
                parameters = [ HashSet.typ t ]
                returnType = typ t
            }

    module AList =
        let typ (t : TypeRef) =    
            TExtRef(fda, "alist", [t])

    module CList =
        let typ (t : TypeRef) =  
            TExtRef(fda, "clist", [t])

        let setValue (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = false
                name = "set_Value"
                parameters = [ IndexList.typ t ]
                returnType = TTuple(false, [])
            }
            
        let ctor (t : TypeRef) =
            {
                declaringType = Choice1Of2(fda)
                isStatic = true
                name = "clist"
                parameters = [ IndexList.typ t ]
                returnType = typ t
            }

    module AMap =
        let typ (k : TypeRef) (v : TypeRef) =    
            TExtRef(fda, "amap", [k; v])

    module CMap =
        let typ (k : TypeRef) (v : TypeRef) =  
            TExtRef(fda, "cmap", [k; v])

        let setValue (k : TypeRef) (v : TypeRef) =
            {
                declaringType = Choice2Of2 (typ k v)
                isStatic = false
                name = "set_Value"
                parameters = [ HashMap.typ k v ]
                returnType = TTuple(false, [])
            }
            
        let ctor (k : TypeRef) (v : TypeRef) =
            {
                declaringType = Choice1Of2(fda)
                isStatic = true
                name = "cmap"
                parameters = [ HashMap.typ k v ]
                returnType = typ k v
            }


    module ChangeableModelMap =
        let typ (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(fdt, "ChangeableModelMap", [k; a; ca; aa])

        let setValue (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ k a ca aa)
                isStatic = false
                name = "Update"
                parameters = [ HashMap.typ k a ]
                returnType = TTuple(false, [])
            }
            
        let ctor (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(fdt)
                isStatic = true
                name = "ChangeableModelMap"
                parameters = 
                    [ 
                        HashMap.typ k a 
                        TFunc(a, ca)
                        TFunc(ca, TFunc(a, ca))
                        TFunc(ca, aa)
                    ]
                returnType = typ k a ca aa
            }

    module Adaptor = 
        let typ (a : TypeRef) (aa : TypeRef) =
            TExtRef(adaptify, "Unpersist", [a; aa])

        let create (a : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(adaptify, "Unpersist", false, false))
                isStatic = true
                name = "get_create"
                parameters = []//[ TFunc(a, aa); TFunc(aa, TFunc(a, TTuple(false, []))) ]
                returnType = TFunc(TFunc(a, aa), TFunc(TFunc(aa, TFunc(a, TTuple(false, []))), typ a aa))
            }

    module ChangeableModelList =
        let typ (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(fdt, "ChangeableModelList", [a; ca; aa])

        let setValue (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ a ca aa)
                isStatic = false
                name = "Update"
                parameters = [ IndexList.typ a ]
                returnType = TTuple(false, [])
            }
            
        let ctor (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(fdt)
                isStatic = true
                name = "ChangeableModelList"
                parameters = 
                    [ 
                        IndexList.typ a 
                        TFunc(a, ca)
                        TFunc(ca, TFunc(a, ca))
                        TFunc(ca, aa)
                    ]
                returnType = typ a ca aa
            }

    module Object =
        let typ =
            TExtRef(Namespace "System", "Object", [])

        //let refEquals =
        //    {
        //        declaringType = Choice2Of2(typ)
        //        isStatic = true
        //        name = "ReferenceEquals"
        //        parameters = [ typ; typ ]
        //        returnType = TBool
        //    }
            

    let notMeth =
        {
            declaringType = Choice1Of2(Module(Namespace "Microsoft.FSharp.Core", "Operators", true, false))
            isStatic = true
            name = "not"
            parameters = [ TBool ]
            returnType = TBool
        }
  
    let shallowEquals (t : TypeRef) =
        {
            declaringType = Choice2Of2(TExtRef(fda, "ShallowEqualityComparer", [t]))
            isStatic = true
            name = "ShallowEquals"
            parameters = [ t; t ]
            returnType = TBool
        }
  
    let defaultEquals (t : TypeRef) =
        {
            declaringType = Choice2Of2(TExtRef(fda, "DefaultEqualityComparer.Instance", []))
            isStatic = true
            name = "Equals"
            parameters = [ t; t ]
            returnType = TBool
        }


