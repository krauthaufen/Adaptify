namespace Adaptify.Compiler

[<AutoOpen>]
module AdaptiveTypes =
    let private fda = Namespace "FSharp.Data.Adaptive"
    let private fdt = Namespace "FSharp.Data.Traceable"
    let private adaptify = Namespace "Adaptify"
    let private system = Namespace "System"

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
 
    module List =
        
        let typ (t : TypeRef) =
            TExtRef(Namespace "Microsoft.FSharp.Collections", "list", [t])
    
    module HashSet =
        let typ (t : TypeRef) =    
            TExtRef(fda, "HashSet", [t])
        
    module HashMap =
        let typ (k : TypeRef) (v : TypeRef) =    
            TExtRef(fda, "HashMap", [k;v])

    module IndexList =
        let typ (t : TypeRef) =    
            TExtRef(fda, "IndexList", [t])
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

    module ChangeableListList =
        let typ (t : TypeRef) =    
            TExtRef(adaptify, "ChangeableListList", [t])
    
        let setValue (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = false
                name = "Update"
                parameters = [ List.typ t ]
                returnType = TTuple(false, [])
            }
            
        let ctor (t : TypeRef) =
            {
                declaringType = Choice1Of2(adaptify)
                isStatic = true
                name = "ChangeableListList"
                parameters = 
                    [ 
                        List.typ t
                    ]
                returnType = typ t
            }
            
            
    module ChangeableListArray=
        let typ (t : TypeRef) =    
            TExtRef(adaptify, "ChangeableListArray", [t])
    
        let setValue (t : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t)
                isStatic = false
                name = "Update"
                parameters = [ TArray(t, 1) ]
                returnType = TTuple(false, [])
            }
            
        let ctor (t : TypeRef) =
            {
                declaringType = Choice1Of2(adaptify)
                isStatic = true
                name = "ChangeableListArray"
                parameters = 
                    [ 
                        TArray(t, 1)
                    ]
                returnType = typ t
            }
            
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
            
    module CValCustomEquality =
        let typ (t : TypeRef) = 
            TExtRef(adaptify, "ChangeableValueCustomEquality", [t])

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
                declaringType = Choice1Of2(adaptify)
                isStatic = true
                name = "ChangeableValueCustomEquality"
                parameters = [ t; TFunc(t, TFunc(t, TBool)) ]
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

    module ChangeableModelListList =
        let typ (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(fdt, "ChangeableModelListList", [a; ca; aa])


        let setValue (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ a ca aa)
                isStatic = false
                name = "Update"
                parameters = [ List.typ a ]
                returnType = TTuple(false, [])
            }
            
        let ctor (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(fdt)
                isStatic = true
                name = "ChangeableModelListList"
                parameters = 
                    [ 
                        List.typ a
                        TFunc(a, TFunc(a, TBool))
                        TFunc(a, ca)
                        TFunc(ca, TFunc(a, ca))
                        TFunc(ca, aa)
                    ]
                returnType = typ a ca aa
            }

    
    module ChangeableModelListArray =
        let typ (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(fdt, "ChangeableModelListArray", [a; ca; aa])


        let setValue (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ a ca aa)
                isStatic = false
                name = "Update"
                parameters = [ TArray(a, 1) ]
                returnType = TTuple(false, [])
            }
            
        let ctor (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(fdt)
                isStatic = true
                name = "ChangeableModelListArray"
                parameters = 
                    [ 
                        TArray(a, 1)
                        TFunc(a, TFunc(a, TBool))
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
            declaringType = Choice2Of2(TExtRef(fda, "DefaultEqualityComparer", []))
            isStatic = true
            name = "Equals"
            parameters = [ t; t ]
            returnType = TBool
        }
    let referenceEquals (t : TypeRef) =
        {
            declaringType = Choice2Of2(TExtRef(system, "Object", [t]))
            isStatic = true
            name = "ReferenceEquals"
            parameters = [ t; t ]
            returnType = TBool
        }


