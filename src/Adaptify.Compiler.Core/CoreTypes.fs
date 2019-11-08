namespace Adaptify.Compiler

[<AutoOpen>]
module AdaptiveTypes =
    let private fda = Namespace "FSharp.Data.Adaptive"
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
 
    module HashSet =
        let typ (t : TypeRef) =    
            TExtRef(fda, "HashSet", [t])
        
    module HashMap =
        let typ (k : TypeRef) (v : TypeRef) =    
            TExtRef(fda, "HashMap", [k;v])

    module IndexList =
        let typ (t : TypeRef) =    
            TExtRef(fda, "IndexList", [t])
        
    module AVal =
        let typ (t : TypeRef) =    
            TExtRef(fda, "aval", [t])
            
    module AdaptiveToken =
        let typ  =    
            TExtRef(fda, "AdaptiveToken", [])
       
    module AdaptiveObject =
        let typ  =    
            TExtRef(fda, "AdaptiveObject", [])

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

    module ChangeableModelOption =
        let typ (t : TypeRef) (tc : TypeRef) (ta : TypeRef) =
            TExtRef(adaptify, "ChangeableModelOption", [t; tc; ta])

        let setValue (t : TypeRef) (tc : TypeRef) (ta : TypeRef) =
            {
                declaringType = Choice2Of2 (typ t tc ta)
                isStatic = false
                name = "update"
                parameters = [ Option.typ t ]
                returnType = TTuple(false, [])
            }

        let ctor (t : TypeRef) (tc : TypeRef) (ta : TypeRef) =
            {
                declaringType = Choice1Of2(adaptify)
                isStatic = true
                name = "ChangeableModelOption"
                parameters = 
                    [ 
                        Option.typ t
                        TFunc(t, tc)
                        TFunc(tc, TFunc(t, tc))
                        TFunc(tc, ta)
                    ]
                returnType = typ t tc ta
            }


    module ChangeableModelMap =
        let typ (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(adaptify, "ChangeableModelMap", [k; a; ca; aa])

        let setValue (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ k a ca aa)
                isStatic = false
                name = "update"
                parameters = [ HashMap.typ k a ]
                returnType = TTuple(false, [])
            }
            
        let ctor (k : TypeRef) (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(adaptify)
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

    module ChangeableModelList =
        let typ (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            TExtRef(adaptify, "ChangeableModelList", [a; ca; aa])

        let setValue (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice2Of2 (typ a ca aa)
                isStatic = false
                name = "update"
                parameters = [ IndexList.typ a ]
                returnType = TTuple(false, [])
            }
            
        let ctor (a : TypeRef) (ca : TypeRef) (aa : TypeRef) =
            {
                declaringType = Choice1Of2(adaptify)
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

        let refEquals =
            {
                declaringType = Choice2Of2(typ)
                isStatic = true
                name = "ReferenceEquals"
                parameters = [ typ; typ ]
                returnType = TBool
            }
            

    let notMeth =
        {
            declaringType = Choice1Of2(Module(Namespace "Microsoft.FSharp.Core", "Operators", true, false))
            isStatic = true
            name = "not"
            parameters = [ TBool ]
            returnType = TBool
        }
    
    let uncheckedEquals (t : TypeRef) =
        {
            declaringType = Choice1Of2(Module(Module(Namespace "Microsoft.FSharp.Core", "Operators", true, false), "Unchecked", false, false))
            isStatic = true
            name = "get_equals"
            parameters = [ ]
            returnType = TFunc(t, TFunc(t, TBool))
        }
