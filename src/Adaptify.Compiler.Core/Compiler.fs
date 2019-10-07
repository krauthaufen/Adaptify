namespace Adaptify.Compiler

open System
open FSharp.Compiler.SourceCodeServices


type GenericArguments = Map<string, DomainTypeDescription>

and DomainTypeDescription =
    {
        /// is the type treated as value?
        isTrivial   : GenericArguments -> bool

        /// the Adaptive definition (if any)
        definition  : GenericArguments -> Option<string>

        /// the view-type (as seen from outside)
        name        : GenericArguments -> string

        /// create a new adaptive cell given the initial value.
        init        : GenericArguments -> string -> string

        /// update the target (1st string) to the value (2nd string) and optionally return the target.
        update      : GenericArguments -> bool -> string -> string -> string

        /// convert the internal representation to the public visible type.
        view        : GenericArguments -> string -> string

        /// the internal-type (cval/cset/etc.)
        internalName: GenericArguments -> string
    }

module FSharpAttribute =
    let isAdaptor (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some "Adaptify.ModelTypeAttribute" -> true
        | _ -> false
        
    let isNonAdaptive (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some "Adaptify.NonAdaptiveAttribute" -> true
        | _ -> false
        
    let isValue (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some "Adaptify.TreatAsValueAttribute" -> true
        | _ -> false

module FSharpEntity =
    let hasAdaptorAttribute (entity : FSharpEntity) =
        entity.Attributes |> Seq.exists FSharpAttribute.isAdaptor

    let isGeneric (entity : FSharpEntity) =
        entity.GenericParameters.Count > 0

module FSharpType =
    let hasAdaptorAttribute (t : FSharpType) =
        if t.HasTypeDefinition then FSharpEntity.hasAdaptorAttribute t.TypeDefinition
        else false

module FSharpField =
    let hasNonAdaptiveAttribute (f : FSharpField) =
        Seq.exists FSharpAttribute.isNonAdaptive f.FieldAttributes ||
        Seq.exists FSharpAttribute.isNonAdaptive f.PropertyAttributes

    let hasTreatAsValueAttribute (f : FSharpField) =
        Seq.exists FSharpAttribute.isValue f.FieldAttributes ||
        Seq.exists FSharpAttribute.isValue f.PropertyAttributes

[<AutoOpen>]
module FSharpTypePatterns = 
    let (|StructTuple|_|) (typ : FSharpType) =
        if typ.IsStructTupleType then
            typ.GenericArguments |> Seq.toList |> Some
        else
            None

    let (|Tuple|_|) (typ : FSharpType) =
        if typ.IsTupleType then
            typ.GenericArguments |> Seq.toList |> Some
        else
            None



    let (|Adaptor|_|) (typ : FSharpType) =
        if FSharpType.hasAdaptorAttribute typ then
            Some typ
        else
            None

    let (|FullName|) (typ : FSharpType) =
        let targs = typ.GenericArguments |> Seq.toList
        if typ.HasTypeDefinition then
            let path = typ.TypeDefinition.AccessPath
            FullName(path + "." + typ.TypeDefinition.DisplayName, targs)
        else
            let name = typ.Format(FSharpDisplayContext.Empty)
            FullName (name, targs)

    let (|FromFSharpDataAdaptive|_|) (typ : FSharpType) =
        if typ.HasTypeDefinition then
            let targs = typ.GenericArguments |> Seq.toList
            let def = typ.TypeDefinition
            if def.Namespace = Some "FSharp.Data.Adaptive" then
                Some (def.DisplayName, targs)
            else
                None
        else
            None

    let (|HashSet|_|) (typ : FSharpType) =
        match typ with
        | FromFSharpDataAdaptive("HashSet", [elementType]) -> Some elementType
        | _ -> None

    let (|HashMap|_|) (typ : FSharpType) =
        match typ with
        | FromFSharpDataAdaptive("HashMap", [key; value]) -> Some (key, value)
        | _ -> None
       
    let (|Option|_|) (typ : FSharpType) =
        match typ with
        | FullName("Microsoft.FSharp.Core.Option", [t])
        | FullName("Microsoft.FSharp.Core.option", [t]) -> Some t
        | _ -> None

    let (|IndexList|_|) (typ : FSharpType) =
        match typ with
        | FromFSharpDataAdaptive("IndexList", [elementType]) -> Some elementType
        | _ -> None
    
    let (|GenericParameter|_|) (typ : FSharpType) =
        if typ.IsGenericParameter then Some typ.GenericParameter.DisplayName
        else None

    let rec (|AnyAdaptive|_|) (t : FSharpType) =
        match t with
        | HashMap _ | HashSet _ | IndexList _ | Adaptor _ | GenericParameter _ ->
            Some ()
        | Tuple elements ->
            if elements |> List.exists (function AnyAdaptive -> true | _ -> false) then Some ()
            else None
        | _ ->
            None

module Ast = 

    type QualifiedPath =
        | Namespace of list<string>
        | Module of msuffix : bool * name : string * parent : Option<QualifiedPath>
        | Type of name : string * parent : QualifiedPath

    type TypeRef =
        | FSharpType of FSharpType
        | FSharpEntity of FSharpEntity
        | Adaptor of FSharpEntity

    type Method =
        {
            declaring : TypeRef
            isStatic  : bool
            name      : string
            generic   : list<FSharpType>
        }

    type Expr =
        | Call of method : Method * args : list<Expr>
        | Lambda of var : Var * body : Expr
        | Application of lambda : Expr * arg : Expr
        | Let of Var * Expr * Expr
        | Sequential of list<Expr>
        | Value of string
        | Var of Var

    and Var(name : string) =
        member x.Name = name

    module QualifiedPath =
        let private typeFormat = sprintf "Adaptive%s"

        let rec ofEntity (e : FSharpEntity) =
            if e.IsFSharpModule then    
                let name = e.DisplayName
                let msuffix = e.HasFSharpModuleSuffix
                let parent = e.DeclaringEntity |> Option.map ofEntity
                Module(msuffix, name, parent)
            elif e.IsNamespace then
                match e.Namespace with
                | Some ns -> ns.Split([|"."|], StringSplitOptions.None) |> Array.toList |> Namespace
                | None -> Namespace []
            else
                let parent = 
                    match e.DeclaringEntity with
                    | Some e -> ofEntity e
                    | None -> 
                        match e.Namespace with
                        | Some ns ->  ns.Split([|"."|], StringSplitOptions.None) |> Array.toList |> Namespace
                        | None -> Namespace []
                Type(e.DisplayName, parent)

    

        let rec private insertAdaptor (p : QualifiedPath) =
            match p with
            | Namespace ns -> 
                Module(false, "Adaptor", Some (Namespace ns))
            | Module(m, name, Some p) ->
                Module(m, name, Some (insertAdaptor p))
            | Module(m, name, None) ->
                Module(m, name, Some (Namespace ["Adaptor"]))
            | Type(name, parent) ->
                Type(name, insertAdaptor parent)

        let rec toAdaptivePath (p : QualifiedPath) =
            match p with
            | Type(name, parent) ->
                Type(typeFormat name, insertAdaptor parent)
            | _ ->
                insertAdaptor p
    
        let rec toString (p : QualifiedPath) =
            match p with
            | Namespace ns -> String.concat "." ns
            | Module(_, name, Some parent) -> sprintf "%s.%s" (toString parent) name
            | Module(_, name, None) -> name
            | Type(name, parent) -> sprintf "%s.%s" (toString parent) name


    module TypeRef =

        let rec toString (r : TypeRef) =
            match r with
            | FSharpType t ->
                let fmt = t.Format(FSharpDisplayContext.Empty.WithShortTypeNames true)
                if t.GenericArguments.Count > 0 then
                    sprintf "%s<%s>" fmt (t.GenericArguments |> Seq.map (FSharpType >> toString) |> String.concat ", ")
                else
                    fmt
            | FSharpEntity e ->
                match e.TryGetFullDisplayName() with
                | Some n ->
                    n
                | None ->
                    e.DisplayName
            | Adaptor e ->
                QualifiedPath.ofEntity e 
                |> QualifiedPath.toAdaptivePath 
                |> QualifiedPath.toString


            

    module Expr =

        let private lines (str : string) =
            str.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

        let rec toString (e : Expr) =
            match e with
            | Var v -> v.Name
            | Value s -> s
            | Sequential es ->
                es |> Seq.map toString |> String.concat "\r\n"
            | Application(Lambda(v, b), e) ->
                toString (Let(v, e, b))
            | Application(l, e) ->
                let l = toString l |> lines
                let e = toString e |> lines
                match l, e with
                | [], _ | _, [] -> failwith "empty"

                | [l], [e] -> sprintf "(%s)(%s)" l e
                | l0 :: ls, [e] ->
                    let ls = ls |> Seq.map (sprintf " %s") |> String.concat "\r\n"
                    sprintf "(%s\r\n%s) (%s)" l0 ls e

                | [l], es ->
                    let es = es |> Seq.map (sprintf "    %s") |> String.concat "\r\n"
                    sprintf "(%s)(\r\n%s\r\n)" l es
                
                | ls, es ->
                    failwith "not implemented"
            | Let(v, e, b) ->
                let e = toString e |> lines
                match e with
                | [] -> failwith "bad"
                | [e] -> sprintf "let %s = %s\r\n%s" v.Name e (toString b)
                | es -> sprintf "let %s =\r\n%s\r\n%s" v.Name (es |> Seq.map (sprintf "    %s") |> String.concat "\r\n") (toString b)
            | Lambda(v, b) ->
                let b = toString b |> lines
                match b with
                | [] -> failwith "bad"
                | [b] -> sprintf "fun %s -> %s" v.Name b
                | bs -> sprintf "fun %s ->\r\n%s" v.Name (bs |> Seq.map (sprintf "    %s") |> String.concat "\r\n")
            | Call(method, args) ->
                if method.isStatic then
                    let args = args |> Seq.map toString |> String.concat ", "
                    let typ = method.declaring |> TypeRef.toString
                    sprintf "%s.%s(%s)" typ method.name args
                else
                    match args with
                    | this :: args ->
                        let this = toString this
                        let args = args |> Seq.map toString |> String.concat ", "
                        sprintf "%s.%s(%s)" this method.name args
                    | _ ->
                        failwith "no this for member"
                    



type AdaptorContext =
    {
        file                : string
        qualifiedPath       : list<string>
    }


module DomainTypeDescription =
    
    let private updateMemberName = "update"
    let private createMemberName = "create"
    let private typeFormat = sprintf "Adaptive%s"

    let private cache = System.Collections.Generic.Dictionary<FSharpType, DomainTypeDescription>()

    let private cached (t : FSharpType) (create : FSharpType -> DomainTypeDescription) =
        lock cache (fun () ->
            match cache.TryGetValue t with
            | (true, res) ->
                res
            | _ -> 
                let mutable real = Unchecked.defaultof<DomainTypeDescription>
                let trampoline = 
                    {
                        isTrivial = fun a -> real.isTrivial a
                        name = fun a -> real.name a
                        init = fun a b -> real.init a b
                        view = fun a b -> real.view a b
                        update = fun a b c -> real.update a b c
                        definition = fun a -> real.definition a
                        internalName = fun a -> real.internalName a
                    }
                cache.[t] <- trampoline
                real <- create t
                cache.[t] <- real
                real
        )

    let rec private fullName (t : FSharpType) =
        if t.IsGenericParameter then
            t.GenericParameter.DisplayName
        else
            let name = t.Format(FSharpDisplayContext.Empty.WithShortTypeNames true)
            let hasGen = t.GenericArguments |> Seq.exists (fun t -> t.IsGenericParameter)

            if hasGen then sprintf "%s<%s>" name (t.GenericArguments |> Seq.map fullName |> String.concat ", ")
            else name
           
    let rec helper (ret : string -> string) (defName : string) (args : list<string * Option<string>>) (code : string) =
        let args =
            match args with
            | [] -> "()"
            | _ -> args |> List.map (function (name, Some typ) -> sprintf "(%s : %s)" name typ | (name, None) -> name) |> String.concat " "
        let lines = code.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
        if lines.Length = 1 then
            let ret = ret lines.[0]
            let ret = ret.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
            if ret.Length = 1 then
                [], sprintf "(fun %s -> %s)" args ret.[0]
            else
                let def =
                    [
                        yield sprintf "let %s %s =" defName args
                        for l in ret do
                            yield "    " + l
                    ]
                def, defName
                
        else
            let ret = ret(lines.[lines.Length - 1]).Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
            let lines = Array.append (Array.take (lines.Length - 1) lines) ret
            let def =
                [
                    yield sprintf "let %s %s =" defName args
                    for l in lines do
                        yield "    " + l
                ]
            def, defName

    let rec getAssociatedType (ctx : AdaptorContext) (t : FSharpType) : DomainTypeDescription =
        cached t (fun t ->
            match t with
            | Option (AnyAdaptive as t) ->
                let desc = getAssociatedType ctx t
                optionAdaptor desc

            | HashMap (k, (AnyAdaptive as v)) ->
                let t = getAssociatedType ctx v
                if t.isTrivial Map.empty then cmap k v
                else cmapAdaptor k t

            | IndexList (AnyAdaptive as v) -> 
                let t = getAssociatedType ctx v
                if t.isTrivial Map.empty then clist v
                else clistAdaptor t

            | HashSet (AnyAdaptive as v) -> 
                failwithf "HashSet of Adaptors not implemented yet %A." v

            | GenericParameter name         -> tpar name
            | StructTuple elements          -> tuple ctx true elements
            | Tuple elements                -> tuple ctx false elements
            | HashSet t                     -> cset t
            | HashMap (k, v)                -> cmap k v
            | IndexList t                   -> clist t
            | Adaptor t                     -> adaptor ctx t
            | _                             -> cval t
        )

    and optionAdaptor (t : DomainTypeDescription) =
        let init (targs : GenericArguments) (str : string) =
            String.concat "\r\n" [
                let init = (t.init targs "value").Split([|"\r\n"|], StringSplitOptions.None)

                yield sprintf "let cell = "
                yield sprintf "    match %s with" str
                yield sprintf "    | Some value ->"

                if init.Length = 1 then
                    yield sprintf "        let res = %s" init.[0]
                else
                    yield sprintf "        let res = "
                    for i in init do yield sprintf "        %s" i

                yield sprintf "        cval(Some res)"
                yield sprintf "    | None ->"
                yield sprintf "        cval None"

                let view = (t.view targs "inner").Split([|"\r\n"|], StringSplitOptions.None)
                if view.Length = 1 then 
                    yield sprintf "let inline view inner = %s" view.[0]
                else
                    yield sprintf "let inline view inner ="
                    for i in view do yield sprintf "    %s" i

                yield sprintf "(cell, AVal.map (Option.map view) cell)"

            ]

        let update (targs : GenericArguments) (retValue : bool) (dst : string) (str : string) =
            let innerFullname = t.name targs

            String.concat "\r\n" [
                let init = (t.init targs "value").Split([|"\r\n"|], StringSplitOptions.None)
                let view = (t.view targs "res").Split([|"\r\n"|], StringSplitOptions.None)
                yield sprintf "let (dst,_) = %s" dst

                yield sprintf "match dst.Value, %s with" str
                yield sprintf "| Some dst, Some value -> "
                let update = (t.update targs false "dst" "value").Split([|"\r\n"|], StringSplitOptions.None)
                for u in update do yield sprintf "    %s" u
                yield sprintf "| None, Some value ->"

                if init.Length = 1 then 
                    yield sprintf "    let res = %s" init.[0]
                else
                    yield sprintf "    let res = "
                    for i in init do yield sprintf "        %s" i

                yield sprintf "    dst.Value <- Some res"
                yield sprintf "| _, None ->"
                yield sprintf "    dst.Value <- None"

                if retValue then
                    yield dst
            ]
        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            init = init
            update = update
            view = fun _ v -> sprintf "snd %s" v
            name = fun _ -> "aval<_>"
            internalName = fun _ -> "cval<_> * aval<_>"
        }

    and cmapAdaptor (key : FSharpType) (valueType : DomainTypeDescription) =    

        let init (gen : GenericArguments) (value : string) =
            String.concat "\r\n" [
                let initCode = valueType.init gen "v"
                let def, arg = helper (valueType.view gen) "initValue" ["_", None; "v", None] initCode
                yield! def
                yield sprintf "cmap(%s |> HashMap.map %s)" value arg
            ]

        let update (gen : GenericArguments) (retValue : bool) (target : string) (value : string) =
            String.concat "\r\n" [
                let initCode = valueType.init gen "v"

                let def, initValue = helper (valueType.view gen) "initValue" ["v", None] (valueType.init gen "v")
                yield! def

                let def, updateValue = 
                    let updateCode = sprintf "let t = unbox<%s> t\r\n%s" (valueType.internalName gen) (valueType.update gen true "t" "v")
                    helper (valueType.view gen) "updateValue" ["t", None; "v", None] updateCode
                yield! def

                yield sprintf "let merge k (o : option<_>) (n : option<_>) ="
                yield sprintf "    match o with"
                yield sprintf "    | Some t -> "
                yield sprintf "        match n with"
                yield sprintf "        | Some v -> %s t v |> Some" updateValue
                yield sprintf "        | None -> None"
                yield sprintf "    | None ->"
                yield sprintf "        match n with"
                yield sprintf "        | Some v -> Some (%s v)" initValue
                yield sprintf "        | None -> None"
                    
                yield sprintf "%s.Value <- HashMap.choose2 merge %s.Value %s" target target value
                if retValue then 
                    yield target

            ]

        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            init = init
            update = update
            view = fun _ v -> sprintf "%s :> amap<_,_>" v
            name = fun _ -> "amap<_,_>"
            internalName = fun _ -> "cmap<_,_>"
        }
    
    and clistAdaptor (valueType : DomainTypeDescription) =    

        let init (gen : GenericArguments) (value : string) =
            String.concat "\r\n" [
                let initCode = valueType.init gen "v"
                let def, arg = helper (valueType.view gen) "initValue" ["v", None] initCode
                yield! def
                yield sprintf "clist(%s |> IndexList.map %s)" value arg
            ]

        let update (gen : GenericArguments) (retValue : bool) (target : string) (value : string) =
            String.concat "\r\n" [
                let def, initValue = helper (valueType.view gen) "initValue" ["v", None] (valueType.init gen "v")
                yield! def

                let def, updateValue = 
                    let updateCode = sprintf "let t = unbox<%s> t\r\n%s" (valueType.internalName gen) (valueType.update gen true "t" "v")
                    helper (valueType.view gen) "updateValue" ["t", None; "v", None] updateCode
                yield! def

                yield sprintf "let merge k (o : option<_>) (n : option<_>) ="
                yield sprintf "    match o with"
                yield sprintf "    | Some t -> "
                yield sprintf "        match n with"
                yield sprintf "        | Some v -> %s t v |> Some" updateValue
                yield sprintf "        | None -> None"
                yield sprintf "    | None ->"
                yield sprintf "        match n with"
                yield sprintf "        | Some v -> Some (%s v)" initValue
                yield sprintf "        | None -> None"
                    
                yield sprintf "%s.Value <- IndexList.choose2 merge %s.Value %s" target target value
                if retValue then 
                    yield target

            ]

        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            init = init
            update = update
            view = fun _ v -> sprintf "%s :> alist<_>" v
            name = fun _ -> "amap<_>"
            internalName = fun _ -> "clist<_>"
        }

    and tpar (name : string) =
        {
            isTrivial = fun m -> match Map.tryFind name m with | Some g -> g.isTrivial m | None -> false
            definition = fun _ -> None
            init = fun _ str -> sprintf "init%s(%s)" name str
            update = fun _ retValue t v -> if retValue then sprintf "update%s (%s) (%s)" name t v else sprintf "ignore (update%s (%s) (%s))" name t v
            view = fun _ v -> sprintf "view%s (%s)" name v
            name = fun _ -> sprintf "'%sView" name
            internalName = fun _ -> sprintf "'%sAdaptive" name
        }

    and adaptor (ctx : AdaptorContext) (t : FSharpType) =  
        let template = generate ctx (Some t) t.TypeDefinition
        let targs = t.GenericArguments |> Seq.toList
        match targs with
        | [] -> template
        | targs ->   
            let tpars = t.TypeDefinition.GenericParameters |> Seq.toList
            let m = (tpars, targs) ||> List.map2 (fun p a -> (p.DisplayName, getAssociatedType ctx a)) |> Map.ofList
            { 
                isTrivial = fun g -> template.isTrivial m
                name = fun _ -> template.name m
                definition = fun _  -> template.definition m
                init = fun _ v -> template.init m v
                update = fun _ t v -> template.update m t v
                view = fun _ v -> template.view m v
                internalName = fun _ -> template.name m
            }



    /// emitting a simple cval
    and cval (t : FSharpType) =
        let typeName = fullName t

        let name (targs : GenericArguments) =
            sprintf "aval<%s>" typeName

        let init (targs : GenericArguments) (value : string) =  
            sprintf "cval(%s)" value

        let update (targs : GenericArguments) (retValue : bool) (target : string) (value : string) =  
            let str = sprintf "%s.Value <- %s" target value
            if retValue then sprintf "%s; %s" str target else str
        {
            isTrivial = fun _ -> true
            definition = fun _ -> None
            name = name
            init = init
            update = update
            view = fun _ s -> sprintf "%s :> aval<_>" s
            internalName = fun _ -> "cval<_>"
        }
        
    /// emitting a simple cset
    and cset (t : FSharpType) =
        let typeName = fullName t

        let name (_targs : GenericArguments) =
            sprintf "aset<%s>" typeName

        let init (_targs : GenericArguments) (value : string) =  
            sprintf "cset(%s)" value

        let update (_targs : GenericArguments) (retValue : bool) (target : string) (value : string) =  
            let str = sprintf "%s.Value <- %s" target value
            if retValue then sprintf "%s; %s" str target else str

        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            name = name
            init = init
            update = update
            view = fun _ n -> sprintf "%s :> aset<_>" n
            internalName = fun _ -> "cset<_>"
        }
        
    /// emitting a simple clist
    and clist (t : FSharpType) =
        let typeName = fullName t

        let name (targs : GenericArguments) =
            sprintf "alist<%s>" typeName

        let init (targs : GenericArguments) (value : string) =  
            sprintf "clist(%s)" value

        let update (targs : GenericArguments) (retValue : bool) (target : string) (value : string) =  
            let str = sprintf "%s.Value <- %s" target value
            if retValue then sprintf "%s; %s" str target else str

        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            name = name
            init = init
            update = update
            view = fun _ n -> sprintf "%s :> alist<_>" n
            internalName = fun _ -> "clist<_>"
        }
        
    /// emitting a simple cmap
    and cmap (key : FSharpType) (value : FSharpType) =
        let keyName = fullName key
        let valueName = fullName value

        let name (targs : GenericArguments) =
            sprintf "amap<%s, %s>" keyName valueName

        let init (targs : GenericArguments) (value : string) =  
            sprintf "cmap %s" value

        let update (targs : GenericArguments) (retValue : bool) (target : string) (value : string) =  
            let str = sprintf "%s.Value <- %s" target value
            if retValue then sprintf "%s; %s" str target else str

        {
            isTrivial = fun _ -> false
            definition = fun _ -> None
            name = name
            init = init
            update = update
            view = fun _ n -> sprintf "%s :> amap<_,_>" n
            internalName = fun _ -> "cmap<_,_>"
        }
        
    /// emitting non-adaptive value
    and nonadaptive (t : FSharpType) =
        let typeName = fullName t
        {
            isTrivial = fun _ -> true
            definition = fun _ -> None
            name = fun _ -> typeName
            init = fun _ v -> sprintf "ref(%s)" v
            update = fun _ r t v -> if r then sprintf "%s := %s; %s" t v t else sprintf "%s := %s" t v
            view = fun _ n -> sprintf "!%s" n 
            internalName = fun _ -> "ref<_>"
        }
        
    /// emitting a tuple of values
    and tuple (ctx : AdaptorContext) (isStruct : bool) (elements : list<FSharpType>) =
        let elements = elements |> List.map (getAssociatedType ctx)

        let prefix = if isStruct then "struct" else ""

        let name (targs : GenericArguments) =
            elements |> List.map (fun e -> e.name targs) |> String.concat " * " |> sprintf "%s(%s)" prefix
            
        let internalName (targs : GenericArguments) =
            elements |> List.map (fun e -> e.internalName targs) |> String.concat " * " |> sprintf "%s(%s)" prefix

        let init (targs : GenericArguments) (value : string) =
            let names = elements |> List.mapi (fun i _ -> sprintf "v%d" i)                        
            let outNames = elements |> List.mapi (fun i _ -> sprintf "a%d" i)

            String.concat "\r\n" [
                yield sprintf "let %s(%s) = %s" prefix (String.concat ", " names) value
                for (name, outName, element) in List.zip3 names outNames elements do
                    let init = element.init targs name
                    let initLines = init.Split([|"\r\n"|], System.StringSplitOptions.None)
                    if initLines.Length = 1 then
                        yield sprintf "let %s = %s" outName initLines.[0]
                    else
                        yield sprintf "let %s =" outName
                        for l in initLines do
                            yield "    " + l

                yield sprintf "%s(%s)" prefix (String.concat ", " outNames)
            ]

        let update (targs : GenericArguments) (retValue : bool) (target : string) (value : string) =
            let names = elements |> List.mapi (fun i _ -> sprintf "v%d" i)                        
            let outNames = elements |> List.mapi (fun i _ -> sprintf "a%d" i)
                        
            String.concat "\r\n" [
                yield sprintf "let %s(%s) = %s" prefix (String.concat ", " outNames) target
                yield sprintf "let %s(%s) = %s" prefix (String.concat ", " names) value

                if retValue then                   
                    let resNames = elements |> List.mapi (fun i _ -> sprintf "n%d" i)
                    for ((name, outName), (element, resName)) in List.zip (List.zip names outNames) (List.zip elements resNames) do
                        let update = element.update targs true outName name
                        let updateLines = update.Split([|"\r\n"|], StringSplitOptions.None)
                        for l in 0 .. updateLines.Length - 2 do
                            yield updateLines.[l]
                        yield sprintf "let %s = %s" resName updateLines.[updateLines.Length - 1]

                    let res = resNames |> String.concat ", "
                    yield sprintf "(%s)" res
                else

                    for (name, outName, element) in List.zip3 names outNames elements do
                        let update = element.update targs false outName name
                        yield update
            ]

        let view (targs : GenericArguments) (target : string) =                   
            let outNames = elements |> List.mapi (fun i _ -> sprintf "a%d" i)
            String.concat "\r\n" [
                yield sprintf "let %s(%s) = %s" prefix (String.concat ", " outNames) target

                for (name, e) in List.zip outNames elements do
                    let view = e.view targs name
                    let viewLines = view.Split([|"\r\n"|], System.StringSplitOptions.None)
                    if viewLines.Length = 1 then
                        yield sprintf "let %s = %s" name viewLines.[0]
                    else
                        yield sprintf "let %s = " name
                        for l in viewLines do
                            yield sprintf "    %s" l
                yield sprintf "%s(%s)" prefix (String.concat ", " outNames)
            ]
        {
            isTrivial = fun g -> elements |> List.forall (fun e -> e.isTrivial g)
            definition = fun _ -> None
            name = name
            init = init
            update = update
            view = view
            internalName = internalName
        }

    /// emitting an AdaptiveType for the given type
    and generate (ctx : AdaptorContext) (typ : Option<FSharpType>) (entity : FSharpEntity) =   
        if entity.IsFSharpRecord then
            record ctx entity
        elif entity.IsFSharpUnion then
            union ctx entity
        else
            match typ with
            | Some typ -> cval typ
            | None -> failwith "bad"


    and union (ctx : AdaptorContext) (entity : FSharpEntity) =
        if entity.GenericParameters.Count > 0 then
            failwith "generic union types not supported yet"

        let cases =
            entity.UnionCases
            |> Seq.toList
            |> List.map (fun c ->
                let fields = 
                    c.UnionCaseFields
                    |> Seq.toList
                    |> List.map (fun f ->
                        if FSharpField.hasNonAdaptiveAttribute f then
                            f.Name, nonadaptive f.FieldType, fullName f.FieldType
                        elif FSharpField.hasTreatAsValueAttribute f then
                            f.Name, cval f.FieldType, fullName f.FieldType
                        else
                            f.Name, getAssociatedType ctx f.FieldType, fullName f.FieldType
                    )
                c.DisplayName, fields
            )

        let typeName = entity.DisplayName
        
        let fullName = 
            let name = entity.TryGetFullDisplayName() |> Option.get
            if entity.GenericParameters.Count > 0 then
                let pars = entity.GenericParameters |> Seq.map (fun p -> sprintf "'%s" p.DisplayName) |> String.concat ", "
                sprintf "%s<%s>" name pars
            else
                name

        let definition (targs : GenericArguments) = 
            String.concat "\r\n" [

                yield sprintf "type %s private(__current : %s, __value : %sConstructor) = " (typeFormat typeName) fullName (typeFormat typeName)
                yield sprintf "    inherit AdaptiveObject()"
                yield sprintf "    let mutable __current = __current"
                yield sprintf "    let mutable __value = __value"
                yield sprintf "    member x.GetValue(token: AdaptiveToken) = "
                yield sprintf "        x.EvaluateAlways token (fun _ -> __value)"
                yield sprintf "    interface aval<%sConstructor> with" (typeFormat typeName)
                yield sprintf "        member x.IsConstant = false"
                yield sprintf "        member x.GetValue t = x.GetValue t"

                yield sprintf "    static member %s(value : %s) =" createMemberName fullName
                yield sprintf "        match value with"
                for (caseName, fields) in cases do
                    let ctorName = sprintf "%s%s" (typeFormat typeName) caseName
                    match fields with
                    | [] -> 
                        yield sprintf "        |  %s -> %s(value, %s())" caseName (typeFormat typeName) ctorName
                    | _ -> 
                        let dArgs = fields |> Seq.map (fun (n,_,_) -> sprintf "_%s" n) |> String.concat ", "
                        yield sprintf "        |  %s(%s) -> %s(value, %s(%s))" caseName dArgs (typeFormat typeName) ctorName dArgs

                yield sprintf "    member x.%s(value : %s) =" updateMemberName fullName
                yield sprintf "        if not (System.Object.ReferenceEquals(__current, value)) then"
                yield sprintf "            __current <- value"
                yield sprintf "            match __value, value with"
                for (caseName, fields) in cases do
                    let ctorName = sprintf "%s%s" (typeFormat typeName) caseName
                    match fields with
                    | [] -> 
                        yield sprintf "            | (:? %s), %s -> ()" ctorName caseName
                    | _ -> 
                        let dArgs = fields |> Seq.map (fun (n,_,_) -> sprintf "_%s" n) |> String.concat ", "
                        yield sprintf "            | (:? %s as __dst), %s(%s) -> __dst.%s(%s)" ctorName caseName dArgs updateMemberName dArgs
                for (caseName, fields) in cases do
                    let ctorName = sprintf "%s%s" (typeFormat typeName) caseName
                    match fields with
                    | [] -> 
                        yield sprintf "            | _, %s -> __value <- %s(); x.MarkOutdated()" caseName ctorName
                    | _ ->
                        let dArgs = fields |> Seq.map (fun (n,_,_) -> sprintf "_%s" n) |> String.concat ", "
                        yield sprintf "            | _, %s(%s) -> __value <- %s(%s); x.MarkOutdated()" caseName dArgs ctorName dArgs


                yield sprintf "and %sConstructor =" (typeFormat typeName)
                yield sprintf "    abstract member Tag : int"
                for (name, _) in cases do
                    yield sprintf "    abstract member Is%s : bool" name

                let mutable tag = 0
                for (case, fields) in cases do
                    
                    let fieldArgs =
                        fields |> List.map (fun (name, desc, inputType) ->
                            sprintf "_%s : %s" name inputType
                        )

                    yield sprintf "and private %s%s(%s) =" (typeFormat typeName) case (String.concat ", " fieldArgs)
                    for (fieldName, desc, inputType) in fields do
                        let initCode = desc.init targs (sprintf "_%s" fieldName)
                        let initLines = initCode.Split([|"\r\n"|], StringSplitOptions.None)
                        if initLines.Length = 1 then
                            yield sprintf "    let _%s = %s" fieldName initLines.[0]
                        else
                            yield sprintf "    let _%s =" fieldName
                            for l in initLines do yield sprintf "        %s" l

                    for (fieldName, desc, inputType) in fields do
                        let view = desc.view targs (sprintf "_%s" fieldName)
                        let view = view.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
                        if view.Length = 1 then
                            yield sprintf "    member x.%s = %s" fieldName view.[0]
                        else
                            yield sprintf "    member x.%s = " fieldName
                            for l in view do yield sprintf "        %s" l

                    if not (List.isEmpty fields) then
                        let updateArgs = fields |> List.map (fun (n, _, t) ->  sprintf "_n%s : %s" n t)
                        yield sprintf "    member x.%s(%s) =" updateMemberName (String.concat ", " updateArgs)
                        for (fieldName, desc, inputType) in fields do
                            let t = sprintf "_%s" fieldName
                            let v = sprintf "_n%s" fieldName
                            let lines = (desc.update targs false t v).Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)
                            for l in lines do
                                yield sprintf "         %s" l

                    // interface impl
                    yield sprintf "    interface %sConstructor with" (typeFormat typeName)
                    yield sprintf "        member x.Tag = %d" tag
                    for (ci, _) in cases do
                        yield sprintf "        member x.Is%s = %s" ci (if ci = case then "true" else "false")

                    tag <- tag + 1


                yield ""
                let pattern = cases |> Seq.map (fun (caseName, _) -> typeFormat caseName) |> String.concat "|" |> sprintf "(|%s|)"

                yield sprintf "let %s (value : %sConstructor) = " pattern (typeFormat typeName)
                yield "    match value with"
                for (caseName, fields) in cases do
                    let aName = typeFormat caseName
                    let ctorName = sprintf "%s%s" (typeFormat typeName) caseName
                    match fields with
                    | [] ->
                        yield sprintf "    | :? %s -> %s" ctorName aName
                    | _ -> 
                        let dArgs = fields |> Seq.map (fun (n,_,_) -> sprintf "value.%s" n) |> String.concat ", "
                        yield sprintf "    | :? %s as value -> %s(%s)" ctorName aName dArgs
                yield "    | _ -> failwith \"not a union case\""
                    
            ]


        { 
            isTrivial = fun _ -> false  
            name = fun _ -> typeFormat typeName
            definition = definition >> Some
            init = fun _ v -> sprintf "%s.%s(%s)" (typeFormat typeName) createMemberName v
            update = fun _ rv t v -> 
                if rv then sprintf "%s.%s(%s); %s" t updateMemberName v t
                else sprintf "%s.%s(%s)" t updateMemberName v
            view = fun _ n -> n
            internalName = fun _ -> typeFormat typeName
        }


    and record (ctx : AdaptorContext) (entity : FSharpEntity) =
        let fields =
            entity.FSharpFields
            |> Seq.toList
            |> List.map (fun f ->
                if FSharpField.hasNonAdaptiveAttribute f then
                    f.DisplayName, nonadaptive f.FieldType
                elif FSharpField.hasTreatAsValueAttribute f then
                    f.DisplayName, cval f.FieldType
                else
                    f.DisplayName, getAssociatedType ctx f.FieldType
            )

        let name = entity.DisplayName
        let fullName = 
            let name = entity.TryGetFullDisplayName() |> Option.get
            if entity.GenericParameters.Count > 0 then
                let pars = entity.GenericParameters |> Seq.map (fun p -> sprintf "'%s" p.DisplayName) |> String.concat ", "
                sprintf "%s<%s>" name pars
            else
                name

        let tpars = Seq.toList entity.GenericParameters

        let tparNames =
            tpars |> List.collect (fun t -> [ t.DisplayName; sprintf "%sAdaptive" t.DisplayName; sprintf "%sView" t.DisplayName ])

        let tparDef = 
            match tparNames with
            | [] -> ""
            | tparNames ->
                let def = 
                    tparNames
                    |> List.map (sprintf "'%s")
                    |> String.concat ", "
                        
                let constraints = 
                    tpars
                    |> List.collect (fun p -> 
                        let name = p.DisplayName
                        p.Constraints |> Seq.toList |> List.map (fun c ->
                            sprintf "'%s %s" name (c.ToString())
                        )
                    )

                match constraints with
                | c0 :: ((_ :: _) as rest) ->
                    let rest = String.concat " and " rest
                    sprintf "<%s when %s and %s>" def c0 rest
                | [c] ->
                    sprintf "<%s when %s>" def c
                        
                | [] ->
                    sprintf "<%s>" def

        let tparArgDef =
            match tpars with
            | [] -> ""
            | tpars ->
                tpars |> List.collect (fun a ->
                    [
                        sprintf "init%s : '%s -> '%sAdaptive" a.DisplayName a.DisplayName a.DisplayName
                        sprintf "update%s : '%sAdaptive -> '%s -> '%sAdaptive" a.DisplayName a.DisplayName a.DisplayName a.DisplayName
                        sprintf "view%s : '%sAdaptive -> '%sView" a.DisplayName a.DisplayName a.DisplayName
                    ]
                )
                |> String.concat ", "
                |> sprintf ", %s"
        let tparArgs =
            match tpars with
            | [] -> []
            | tpars ->
                tpars |> List.collect (fun a ->
                    [
                        sprintf "init%s" a.DisplayName
                        sprintf "update%s" a.DisplayName
                        sprintf "view%s" a.DisplayName
                    ]
                )
                

        let definition (targs : GenericArguments) = 
            String.concat "\r\n" [
                yield sprintf "/// Adaptive representation for `%s`" entity.DisplayName
                yield sprintf "type %s%s private(__initial : %s%s) =" (typeFormat name) tparDef fullName tparArgDef
                for (name, desc) in fields do
                    let initCode = desc.init targs (sprintf "__initial.%s" name)
                    let initLines = initCode.Split([|"\r\n"|], StringSplitOptions.None)

                    if initLines.Length = 1 then
                        yield sprintf "    let _%s = %s" name initLines.[0]
                    else
                        yield sprintf "    let _%s =" name
                        for l in initLines do
                            yield "        " + l

                for (name, desc) in fields do
                    let view = desc.view targs (sprintf "_%s" name)
                    let viewLines = view.Split([|"\r\n"|], System.StringSplitOptions.None)
                    yield sprintf "    /// The current value of %s as `%s`." name (desc.name targs)
                    if viewLines.Length = 1 then
                        yield sprintf "    member __.%s = %s" name viewLines.[0]
                    else
                        yield sprintf "    member __.%s = " name
                        for l in viewLines do
                            yield sprintf "        %s" l
                                
                yield sprintf "    /// Updates all values in the `%s` to the given `%s`." (typeFormat name) entity.DisplayName
                yield sprintf "    /// Note that it expects a Transaction to be current." 
                yield sprintf "    member __.%s(value : %s) : unit =" updateMemberName fullName
                yield sprintf "        let __value = value"
                for (name, desc) in fields do
                    let updateCode = desc.update targs false (sprintf "_%s" name) (sprintf "__value.%s" name)
                    let updateLines = updateCode.Split([|"\r\n"|], StringSplitOptions.None)
                    for l in updateLines do
                        yield sprintf "        %s" l

                yield sprintf "    /// Creates a new `%s` using the given `%s`." (typeFormat name) entity.DisplayName
                yield sprintf "    static member %s(value : %s%s) : %s%s = " createMemberName fullName tparArgDef (typeFormat name) tparDef
                    
                let all = "value" :: tparArgs |> String.concat ", "
                yield sprintf "        %s(%s)" (typeFormat name) all


            ]

        let init (targs : GenericArguments) (value : string) =  
            let adaptors = 
                tpars |> List.choose (fun p ->
                    match Map.tryFind p.DisplayName targs with
                    | Some a -> Some(p.DisplayName, a)
                    | None -> None  
                )

            String.concat "\r\n" [
                for (p, a) in adaptors do
                    if a.isTrivial targs then
                        yield sprintf "let inline init%s v = v" p
                        yield sprintf "let inline update%s _t v = v" p
                        yield sprintf "let inline view%s v = v" p
                    else
                        let initCode = a.init targs "v"
                        let initLines = initCode.Split([|"\r\n"|], StringSplitOptions.None)
                        if initLines.Length = 1 then
                            yield sprintf "let init%s v = %s" p initLines.[0]
                        else
                            yield sprintf "let init%s v =" p
                            for l in initLines do
                                yield sprintf "    %s" l

                        
                        let updateCode = a.update targs true "t" "v"
                        let updateLines = updateCode.Split([|"\r\n"|], StringSplitOptions.None)
                        if updateLines.Length = 1 then
                            yield sprintf "let update%s (t : %s) v = %s" p (a.internalName targs) updateLines.[0]
                        else
                            yield sprintf "let update%s (t : %s) v =" p (a.internalName targs)
                            for l in updateLines do
                                yield sprintf "    %s" l

                        
                        let viewCode = a.view targs "v"
                        let viewLines = viewCode.Split([|"\r\n"|], StringSplitOptions.None)
                        if viewLines.Length = 1 then
                            yield sprintf "let view%s v = %s" p viewLines.[0]
                        else
                            yield sprintf "let view%s v =" p
                            for l in viewLines do
                                yield sprintf "    %s" l

                            
                let additionalArgs = 
                    adaptors |> List.collect (fun (p, _) -> 
                        [
                            sprintf "init%s" p
                            sprintf "update%s" p
                            sprintf "view%s" p
                        ]
                    )

                let args = value :: additionalArgs |> String.concat ", "
                yield sprintf "%s.%s(%s)" (typeFormat name) createMemberName args
            ]

        let update (targs : GenericArguments) (retValue : bool) (target : string) (value : string) =
            if retValue then
                sprintf "%s.%s(%s); %s" target updateMemberName value target
            else
                sprintf "%s.%s(%s)" target updateMemberName value
                    

        { 
            isTrivial = fun _ -> false  
            name = fun _ -> typeFormat name
            definition = definition >> Some
            init = init
            update = update
            view = fun _ n -> n
            internalName = fun _ -> typeFormat name
        }
module Adaptor = 
    let rec generate (ctx : AdaptorContext) (entity : FSharpEntity) =
        if entity.IsFSharpModule then
            let name = entity.DisplayName
            entity.NestedEntities
            |> Seq.toList
            |> List.collect (generate { ctx with qualifiedPath = ctx.qualifiedPath @ [name] })
        else
            let createAdaptor = FSharpEntity.hasAdaptorAttribute entity
            if createAdaptor then
                let def = DomainTypeDescription.generate ctx None entity

                match def.definition Map.empty with
                | Some def ->
                    let lines = def.Split([|"\r\n"|], StringSplitOptions.None) 

                    let ns =
                        match ctx.qualifiedPath with
                        | [] -> "global"
                        | ns -> String.concat "." ns

                    let moduleName =
                        sprintf "%sAdaptor" (System.IO.Path.GetFileNameWithoutExtension ctx.file)
                        
                    [ns, moduleName, lines]


                    //[
                    //    match ctx.qualifiedPath with
                    //    | [] -> 
                    //        yield "namespace global"
                    //    | ns ->
                    //        yield sprintf "namespace %s" (String.concat "." ns)

                    //    yield ""
                    //    yield "open FSharp.Data.Adaptive"
                    //    yield ""
                    //    yield sprintf "module %sAdaptor =" (System.IO.Path.GetFileNameWithoutExtension ctx.file)
                    //    for l in lines do
                    //        yield "    " + l
                    //]

                | None ->
                    []

                //[DomainTypeDescription.generate ctx None entity]
            else
                []




