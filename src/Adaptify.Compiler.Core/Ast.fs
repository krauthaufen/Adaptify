module rec Ast

open System
open FSharp.Compiler.SourceCodeServices

module Map =
    let union (l : Map<_,_>) (r : Map<_,_>) =
        let mutable res = l
        for (KeyValue(k,v)) in r do
            res <- Map.add k v res
        res

module FSharpAttribute =
    let isAutoOpen (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = typeof<AutoOpenAttribute>.FullName
        | None ->
            false

    let isModelType (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.ModelTypeAttribute"
        | None ->
            false
            
    let isTreatAsValue (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.TreatAsValueAttribute"
        | None ->
            false

    let isNonAdaptive (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.NonAdaptiveAttribute"
        | None ->
            false


type Scope =
    | Global
    | Namespace of string
    | Module of parent : Scope * name : string * isAutoOpen : bool * hasModuleSuffix : bool

module Scope =
    let rec ofFSharpEntity (e : FSharpEntity) =
        if e.IsNamespace then
            match e.DeclaringEntity with
            | Some d ->
                match ofFSharpEntity d with
                | Namespace p -> p + "." + e.DisplayName |> Namespace
                | _ -> failwith "invalid F#"

            | None ->
                Namespace(e.DisplayName)

        elif e.IsFSharpModule then
            let autoOpen = e.Attributes |> Seq.exists FSharpAttribute.isAutoOpen
            let moduleSuffix = e.HasFSharpModuleSuffix
            let name = e.DisplayName
            match e.DeclaringEntity with
            | Some d ->
                let parent = ofFSharpEntity d
                Module(parent, name, autoOpen, moduleSuffix)
            | None ->
                match e.Namespace with
                | Some ns -> Module(Namespace ns, name, autoOpen, moduleSuffix)
                | None -> Module(Global, name, autoOpen, moduleSuffix)
        else
            failwithf "%s is neither a module nor a namespace" e.DisplayName
         
    let rec ofFSharpEntityOpt (e : option<FSharpEntity>) (ns : option<string>) =  
        match e with
        | None ->
            match ns with
            | Some ns -> Namespace ns
            | None -> Global
        | Some e -> ofFSharpEntity e

    let rec fullName (scope : Scope) =
        match scope with
        | Global -> None
        | Namespace ns -> Some ns
        | Module(parent, name, _, _) ->
            match fullName parent with
            | None -> Some name
            | Some v -> Some (v + "." + name)
        
    let private opened =
        [
            "System"
            "Microsoft.FSharp.Core"
            "Microsoft.FSharp.Collections"
            "FSharp.Data.Adaptive"
            "Adaptify"
        ]

    let rec shortName (scope : Scope) =
        match fullName scope with
        | Some full ->
            let shortened = 
                opened |> Seq.tryPick (fun o ->
                    if full.StartsWith o then Some (full.Substring o.Length)
                    else None
                )
            match shortened with
            | Some s -> 
                if String.IsNullOrWhiteSpace s then None
                else Some s
            | None -> 
                Some full
        | None ->
            None

    let rec relativeName (relativeTo : Scope) (scope : Scope) =
        let rec find (s : Scope) =
            if s = relativeTo then
                None
            else
                match s with
                | Module(parent, name, _, _) ->
                    match find parent with
                    | Some str -> str + "." + name |> Some
                    | None -> name |> Some
                | Namespace ns ->
                    Some ns
                | Global ->
                    None
        match find scope with
        | Some str -> Some str
        | None -> None

type TypeVar(name : string) =
    member x.Name = name
    override x.ToString() = sprintf "'%s" name

[<StructuredFormatDisplay("{AsString}")>]
type TypeRef =
    | TBool
    | TTuple of isStruct : bool * elements : TypeRef list
    | TFunc of domain : TypeRef * codomain : TypeRef
    | TArray of element : TypeRef * rank : int
    | TModel of def : Lazy<TypeDef> * targs : list<TypeRef>
    | TExtRef of scope : Scope * name : string * targs : list<TypeRef>
    | TRef of ent : FSharpEntity * targs : list<TypeRef>
    | TVar of var : TypeVar
    
    member private x.AsString = x.ToString()

    override x.ToString() =
        match x with
        | TBool ->
            "bool"
        | TTuple(false, ts) ->
            ts |> Seq.map string |> String.concat " * " |> sprintf "(%s)"
        | TTuple(true, ts) ->
            ts |> Seq.map string |> String.concat " * " |> sprintf "struct (%s)"
        | TArray(t, rank) ->
            let str = System.String(',', rank - 1)
            sprintf "(%s)[%s]" (string t) str
        | TFunc(a, b) ->
            sprintf "%s -> %s" (string a) (string b)
        | TModel(def, []) ->
            sprintf "%s" def.Value.FullName
        | TModel(def, targs) ->
            let targs = targs |> Seq.map string |> String.concat ", "
            sprintf "%s<%s>" def.Value.FullName targs
        | TRef(r, []) ->
            let s = Scope.ofFSharpEntityOpt r.DeclaringEntity r.Namespace
            match Scope.shortName s with
            | Some n -> n + "." + r.DisplayName
            | None -> r.DisplayName
        | TRef(r, targs) ->
            let targs = targs |> Seq.map string |> String.concat ", "
            
            let s = Scope.ofFSharpEntityOpt r.DeclaringEntity r.Namespace
            match Scope.shortName s with
            | Some n -> sprintf "%s.%s<%s>" n r.DisplayName targs
            | None -> sprintf "%s<%s>" r.DisplayName targs
        | TExtRef(s, r, []) ->
            match Scope.shortName s with
            | Some n -> n + "." + r
            | None -> r

        | TExtRef(s, r, targs) ->
            let targs = targs |> Seq.map string |> String.concat ", "
            match Scope.shortName s with
            | Some n -> sprintf "%s.%s<%s>" n r targs
            | None -> sprintf "%s<%s>" r targs
                      
        | TVar v ->
            string v

let inline (|TUnit|_|) (t : TypeRef) =
    match t with
    | TTuple(false, []) -> Some ()
    | _ -> None

[<StructuredFormatDisplay("{AsString}")>]
type TypeDef =
    | ProductType of isValueType : bool * scope : Scope * name : string * properties : list<Prop>
    | Union of scope : Scope * name : string * properties : list<Prop> * cases : list<string * list<Prop>>
    | Generic of tpars : list<TypeVar> * def : TypeDef
    
    member private x.AsString = x.ToString()

    member x.Name =
        match x with
        | ProductType(_, _, n, _)
        | Union(_, n, _, _) ->
            n
        | Generic(_,d) ->
            d.Name
            
    member x.RelativeName (scope : Scope) =
        match x with
        | ProductType(_, s, n, _)
        | Union(s, n, _, _) ->
            match Scope.relativeName scope s with
            | Some s -> sprintf "%s.%s" s n
            | None -> n

        | Generic(_,d) ->
            d.RelativeName scope
        

    member x.FullName =
        match x with
        | ProductType(_, s, n, _)
        | Union(s, n, _, _) ->
            match Scope.fullName s with
            | Some s -> sprintf "%s.%s" s n
            | None -> n

        | Generic(_,d) ->
            d.FullName

    override x.ToString() =
        match x with
        | ProductType(_, scope, name, []) ->
            match Scope.fullName scope with
            | Some n -> sprintf "type %s.%s" n name
            | None -> sprintf "type %s" name

            
        | ProductType(_, scope, name, props) ->
            let props = props |> Seq.map Prop.printDef |> String.concat "; "
            match Scope.fullName scope with
            | Some n -> sprintf "type %s.%s = { %s }" n name props
            | None -> sprintf "type %s = { %s }" name props


        | Union(scope, name, _, cases) ->  
            let name =
                match Scope.fullName scope with
                | Some n -> sprintf "%s.%s" n name
                | None -> name


            let caseNames = 
                cases |> Seq.map (fun (n, f) ->
                    match f with
                    | [] -> 
                        n
                    | fs ->
                        sprintf "%s of %s" n (fs |> Seq.map Prop.printDef |> String.concat " * ")
                )

            sprintf "type %s = %s" name (String.concat " | " caseNames)
        | Generic(pars, def) ->
            let str = def.ToString()
            let prefix = sprintf "type %s" def.FullName

            if str.StartsWith prefix then
                let str = str.Substring(prefix.Length)
                let decl = sprintf "%s<%s>" prefix (pars |> Seq.map string |> String.concat ", ")
                decl + str
            else
                sprintf "(%s)<%s>" str (pars |> Seq.map string |> String.concat ", ")


type AdaptifyMode = 
    | Default
    | Value
    | NonAdaptive

type Prop =
    {
        name            : string
        typ             : TypeRef
        mode            : AdaptifyMode
    }

module Prop =
    let ofFSharpField (targs : Map<_,_>) (f : FSharpField) =
        let mode = 
            if f.PropertyAttributes |> Seq.exists FSharpAttribute.isNonAdaptive then NonAdaptive
            elif f.PropertyAttributes |> Seq.exists FSharpAttribute.isTreatAsValue then Value
            else Default

        let typ = TypeRef.ofType targs f.FieldType
        let name = f.Name
        {
            name = name
            typ = typ
            mode = mode
        }

    let ofMemberOrFunctionOrValue (targs : Map<_,_>) (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.IsProperty && mfv.HasGetterMethod && not mfv.HasSetterMethod then
            let mode = 
                if mfv.Attributes |> Seq.exists FSharpAttribute.isNonAdaptive then NonAdaptive
                elif mfv.Attributes |> Seq.exists FSharpAttribute.isTreatAsValue then Value
                else Default

            let typ = TypeRef.ofType targs mfv.GetterMethod.ReturnParameter.Type
            let name = mfv.DisplayName
            Some {
                name = name
                typ = typ
                mode = mode
            }
        else
            None

    let printDef (p : Prop) =
        let prefix =
            match p.mode with
            | Value -> "[<TreatAsValue>] "
            | NonAdaptive -> "[<NonAdaptive>] "
            | Default -> ""
        sprintf "%s%s : %s" prefix p.name (string p.typ)

module TypeRef =
    
    let rec ofType (args : Map<string, TypeRef>) (t : FSharpType) =
        if t.IsGenericParameter then
            let name = t.GenericParameter.DisplayName
            //let name = t.Format(FSharpDisplayContext.Empty).Substring(1)
            match Map.tryFind name args with
            | Some t -> t
            | None -> failwithf "generic parameter '%s not found" name

        elif t.IsStructTupleType then
            let els = 
                t.GenericArguments 
                |> Seq.toList
                |> List.map (ofType args)
            TTuple(true, els)

        elif t.IsTupleType then
            let els = 
                t.GenericArguments 
                |> Seq.toList
                |> List.map (ofType args)
            TTuple(false, els)

        elif t.IsAbbreviation && not (t.Format(FSharpDisplayContext.Empty).StartsWith "Microsoft.FSharp.") then
            ofType args t.AbbreviatedType

        elif t.IsFunctionType then
            let targs = t.GenericArguments
            let cd = targs.[targs.Count - 1] |> ofType args
            let d = Seq.take (targs.Count - 1) targs |> Seq.map (ofType args) |> Seq.toList

            let rec build (d : list<TypeRef>) (cd : TypeRef) =
                match d with
                | [] -> cd
                | h :: t -> TFunc(h, build t cd)

            build d cd

        elif t.HasTypeDefinition then
            let def = t.TypeDefinition

            if def.IsArrayType then
                let el = ofType args t.GenericArguments.[0]
                TArray(el, def.ArrayRank)
            else
                let targs = 
                    t.GenericArguments
                    |> Seq.toList
                    |> List.map (ofType args)

                match TypeDef.ofEntity def with
                | Some def ->
                    TModel(def, targs) 
                | None ->
                    TRef(def, targs)

        else
            failwithf "could not find definition for %s" (t.Format(FSharpDisplayContext.Empty))

    let rec toString (currentScope : Scope) (t : TypeRef) =
        match t with
        | TBool ->
            "bool"
        | TTuple(false, ts) ->
            ts |> Seq.map (toString currentScope) |> String.concat " * " |> sprintf "(%s)"

        | TTuple(true, ts) ->
            ts |> Seq.map (toString currentScope) |> String.concat " * " |> sprintf "struct (%s)"

        | TArray(t, rank) ->
            let str = System.String(',', rank - 1)
            sprintf "(%s)[%s]" (toString currentScope t) str

        | TFunc(a, b) ->
            sprintf "%s -> %s" (toString currentScope a) (toString currentScope b)

        | TModel(def, []) ->
            sprintf "%s" (def.Value.RelativeName currentScope)

        | TModel(def, targs) ->
            let targs = targs |> Seq.map (toString currentScope) |> String.concat ", "
            sprintf "%s<%s>" (def.Value.RelativeName currentScope) targs

        | TRef(r, []) ->
            let s = Scope.ofFSharpEntityOpt r.DeclaringEntity r.Namespace
            match Scope.relativeName currentScope s with
            | Some n -> n + "." + r.DisplayName
            | None -> r.DisplayName

        | TRef(r, targs) ->
            let targs = targs |> Seq.map (toString currentScope) |> String.concat ", "
            
            let s = Scope.ofFSharpEntityOpt r.DeclaringEntity r.Namespace
            match Scope.relativeName currentScope s with
            | Some n -> sprintf "%s.%s<%s>" n r.DisplayName targs
            | None -> sprintf "%s<%s>" r.DisplayName targs
        | TExtRef(s, r, []) ->
            match Scope.relativeName currentScope s with
            | Some n -> n + "." + r
            | None -> r

        | TExtRef(s, r, targs) ->
            let targs = targs |> Seq.map (toString currentScope) |> String.concat ", "
            match Scope.relativeName currentScope s with
            | Some n -> sprintf "%s.%s<%s>" n r targs
            | None -> sprintf "%s<%s>" r targs
                      
        | TVar v ->
            v.ToString()


module TypeDef =
    let dict = System.Collections.Concurrent.ConcurrentDictionary<FSharpEntity, Lazy<TypeDef>>()

    let rec private create (e : FSharpEntity) =
        let tpars = 
            e.GenericParameters 
            |> Seq.toList
            |> List.map (fun p -> TypeVar p.DisplayName)

        let parMap =
            tpars
            |> Seq.map (fun p -> p.Name, TVar p)
            |> Map.ofSeq

        let ret (t : TypeDef) =
            match tpars with
            | [] -> t
            | _ -> Generic(tpars, t)

        let props = 
            e.MembersFunctionsAndValues
            |> Seq.toList
            |> List.choose (Prop.ofMemberOrFunctionOrValue parMap)

        if e.IsFSharpRecord then
            let fields = 
                e.FSharpFields
                |> Seq.toList
                |> List.map (Prop.ofFSharpField parMap)

            let parent = 
                Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace

            ProductType(e.IsValueType, parent, e.DisplayName, props @ fields) |> ret

        elif e.IsFSharpUnion then
            let cases = 
                e.UnionCases
                |> Seq.toList
                |> List.map (fun c ->
                    let name = c.DisplayName
                    let fields = 
                        c.UnionCaseFields
                        |> Seq.toList
                        |> List.map (Prop.ofFSharpField parMap)

                    name, fields
                )
                
            let parent = Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace

            Union(parent, e.DisplayName, props, cases) |> ret

        elif e.IsClass || e.IsValueType then
            let parent = Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace
            ProductType(e.IsValueType, parent, e.DisplayName, props) |> ret

        else
            failwith "asdasdsad"

    and ofEntity (e : FSharpEntity) : option<Lazy<TypeDef>> =
        if e.Attributes |> Seq.exists FSharpAttribute.isModelType then
            dict.GetOrAdd(e, fun e -> lazy (create e)) |> Some
        else    
            None


type Var(name : string, t : TypeRef, isMutable : bool) =
    member x.Name = name
    member x.Type = t
    member x.IsMutable = isMutable

    new(name, t) = Var(name, t, false)

type Method =
    {
        declaringType   : Choice<Scope, TypeRef>
        isStatic        : bool
        name            : string
        parameters      : list<TypeRef>
        returnType      : TypeRef
    }



type Pattern =
    | UnionCaseTest of TypeRef * string * list<Var>
    | TypeTest of TypeRef * Var
    | Any

type Expr =
    | Unit
    | Var of Var
    | Lambda of list<Var> * Expr
    | NewTuple of bool * list<Expr>
    | Application of Expr * Expr
    | Seq of Expr * Expr
    | Upcast of Expr * TypeRef
    | Call of option<Expr> * Method * list<Expr>
    | Fail of TypeRef * reason : string
    | Let of bool * list<Var> * Expr * Expr
    | PropertyGet of Expr * Prop
    | VarSet of Var * Expr
    | IfThenElse of Expr * Expr * Expr
    | Unbox of TypeRef * Expr
    | Ignore of Expr
    | And of list<Expr>
    | Match of Expr * list<Pattern * Expr>


    static member Many (s : list<Expr>) =
        match s with
        | [] -> Unit
        | [s] -> s
        | h :: t -> Seq(h, Expr.Many t)

    member x.Type =
        match x with
        | Match(_,[]) ->
            failwith "bad match"
        | Match(_, (_,c) :: _) ->
            c.Type
        | And _ -> 
            TBool
        | Unbox(t, _) ->
            t
        | Ignore _ ->
            TTuple(false, [])

        | NewTuple(isStruct, es) ->
            let types = es |> List.map (fun e -> e.Type)
            TTuple(isStruct, types)

        | Application(e, _) ->
            match e.Type with
            | TFunc(_, r) -> r
            | _ -> failwith "bad application"

        | Seq(_, r) ->
            r.Type

        | VarSet _ ->
            TTuple(false, [])

        | IfThenElse(_,i,_) ->
            i.Type

        | Upcast(_, t) ->
            t
        | Unit -> 
            TTuple(false, [])

        | Call(_, m, _) ->
            m.returnType

        | Var v -> 
            v.Type
        | Let(_,_,_,b) ->
            b.Type
        | Lambda(vs, b) ->
            let argTypes = vs |> List.map (fun v -> v.Type)
            let argType = TTuple(false, argTypes)
            TFunc(argType, b.Type)
        | PropertyGet(_,p) -> 
            p.typ
        | Fail(t,_) ->
            t


module Expr =

    let rec (|Lambdas|_|) (e : Expr) =
        match e with
        | Lambda(v, b) -> 
            match b with
            | Lambdas(vs, b) -> Some(v::vs, b)
            | _ -> Some ([v], b)
        | _ -> 
            None
            
    let rec (|Applications|_|) (e : Expr) =
        match e with
        | Application(l, v) -> 
            match l with
            | Applications(l, vs) -> Some(l, vs @ [v])
            | _ -> Some (l, [v])
        | _ -> 
            None

    let private lines (str : string) =
        str.Split([|"\r\n"|], StringSplitOptions.None)

    let indent (lines : string[]) =
        lines |> Array.map (fun s -> "    " + s)
        
    let indentStr (str : string) =
        str |> lines |> indent |> String.concat "\r\n"

    let private argDef (scope : Scope) (args : list<list<Var>>) =
        args 
        |> List.map (
            List.map (fun v -> sprintf "%s : %s" v.Name (TypeRef.toString scope v.Type)) 
            >> String.concat ", "
            >> sprintf "(%s)"
        )
        |> String.concat " "

    let private bracketed (str : string) =
        if str.Contains "(" || str.Contains " " || str.Contains "fun" then sprintf "(%s)" str
        else str


    let private toOneLineArgs (scope : Scope) (args : list<Expr>) =
        let temporary = System.Collections.Generic.List()
        let args = 
            args |> List.mapi (fun i a ->
                match a with
                | Lambdas(vs, b) ->
                    let b = toString scope b |> lines
                    if b.Length = 1 then
                        sprintf "(fun %s -> %s)" (argDef scope vs) b.[0]
                    else
                        let name = sprintf "__arg%d" i

                        let code = sprintf "let inline %s %s =\r\n%s" name (argDef scope vs) (indent b |> String.concat "\r\n")

                        temporary.Add(code)
                        name
                | _ ->
                    let a = toString scope a |> lines
                    if a.Length = 1 then    
                        bracketed a.[0]
                    else
                        let name = sprintf "__arg%d" i
                        let code = sprintf "let %s =\r\n%s" name (indent a |> String.concat "\r\n")

                        temporary.Add(code)
                        name

            )

        if temporary.Count > 0 then
            let prefix = String.concat "\r\n" temporary + "\r\n"
            prefix, args
        else
            "", args


    let rec toString (scope : Scope) (e : Expr) =
        match e with
        | Match(e, cases) ->
            let prefix, e = toOneLineArgs scope [e]
            let e = List.head e

            let pat (p : Pattern) =
                match p with
                | Any -> "_"
                | TypeTest(t, v) -> sprintf "(:? %s as %s)" (TypeRef.toString scope t) v.Name
                | UnionCaseTest(t, n, vs) ->
                    let t = 
                        match t with
                        | TExtRef(s, n, _) -> TExtRef(s, n, []) |> TypeRef.toString scope
                        | TRef(e, _) -> TRef(e, []) |> TypeRef.toString scope
                        | TModel(d, _) -> TModel(d, []) |> TypeRef.toString scope
                        | _ -> t |> TypeRef.toString scope
                    match vs with
                    | [] ->
                        sprintf "%s.%s" t n 
                    | _ -> 
                        sprintf "%s.%s(%s)" t n (vs |> List.map (fun v -> v.Name) |> String.concat ", ")

            let cases =
                cases |> List.map (fun (p, b) ->
                    let b = toString scope b |> lines |> indent
                    sprintf "| %s ->\r\n%s" (pat p) (String.concat "\r\n" b)
                )

            sprintf "%smatch %s with\r\n%s" prefix e (String.concat "\r\n" cases)


        | And vs ->
            vs |> List.map (toString scope) |> String.concat " && "

        | Lambda([v], Application(b, Var v1)) when v.Name = v1.Name ->
            toString scope b

        | Unbox(t, e) ->
            let prefix, e = toOneLineArgs scope [e]
            let e = List.head e
            sprintf "%sunbox<%s> %s" prefix (TypeRef.toString scope t) e

        | Ignore(e) ->
            let prefix, e = toOneLineArgs scope [e]
            let e = List.head e
            sprintf "%signore %s"prefix e

        | Upcast(e, t) ->
            let e = toString scope e
            let t = TypeRef.toString scope t
            sprintf "%s :> %s" e t
        | Unit -> 
            "()"

        | Var v -> 
            v.Name

        | Seq(l, r) ->
            sprintf "%s\r\n%s" (toString scope l) (toString scope r)

        | Fail(_, reason) ->
            sprintf "failwith \"%s\"" reason

        | Call(Some t, m, args) ->
            let prefix, t = toOneLineArgs scope [t]
            let t = List.head t

            if m.name.StartsWith "set_" then
                let v = List.head args |> toString scope |> lines
                if v.Length = 1 then
                    sprintf "%s%s.%s <- %s" prefix t (m.name.Substring 4) v.[0]
                else
                    sprintf "%s%s.%s <-\r\n%s" prefix t (m.name.Substring 4) (indent v |> String.concat "\r\n")
                    
            elif m.name.StartsWith "get_" && args = [] then
                sprintf "%s%s.%s" prefix t (m.name.Substring 4)

            else
                let prefix2, args = args |> toOneLineArgs scope
                sprintf "%s%s%s.%s(%s)" prefix prefix2 t m.name (String.concat ", " args)
            
        | Call(None, m, args) ->
            let qual = 
                match m.declaringType with
                | Choice1Of2 a ->
                    match Scope.relativeName scope a with
                    | Some n -> n + "."
                    | None -> ""
                | Choice2Of2 b ->
                    TypeRef.toString scope b + "."

            if m.name.StartsWith "set_" then
                let v = List.head args |> toString scope |> lines
                if v.Length = 1 then
                    sprintf "%s%s <- %s" qual (m.name.Substring 4) v.[0]
                else
                    sprintf "%s%s <-\r\n%s" qual (m.name.Substring 4) (indent v |> String.concat "\r\n")
                
            elif m.name.StartsWith "get_" && args = [] then
                sprintf "%s%s" qual (m.name.Substring 4)

            else
                let prefix, args = toOneLineArgs scope args
                sprintf "%s%s%s(%s)" prefix qual m.name (String.concat ", " args)
                

        | Lambdas(args, body) ->
            let args = argDef scope args

            let body =
                toString scope body
                |> lines

            if body.Length = 1 then
                sprintf "fun %s -> %s" args body.[0]
            else
                sprintf "fun %s ->\r\n%s" args (indent body |> String.concat "\r\n")

        | Lambda _ -> failwith "unreachable"

        | Applications(lambda, args) ->
            let lambda = toString scope lambda |> lines
            let prefix, args = toOneLineArgs scope args

            if lambda.Length = 1 then
                sprintf "%s%s %s" prefix (bracketed lambda.[0]) (String.concat " " args)
            else
                sprintf "%s(%s\r\n%s) %s" prefix lambda.[0] (Array.skip 1 lambda |> indent |> String.concat "\r\n") (String.concat " " args)
                
        | Application _ -> failwith "unreachable"

        | Let(_, [v], e, b) ->
            let e = toString scope e |> lines
            let b = toString scope b

            let vp = 
                if v.IsMutable then "mutable "
                else ""

            if e.Length = 1 then
                sprintf "let %s%s = %s\r\n%s" vp v.Name e.[0] b
            else
                sprintf "let %s%s =\r\n%s\r\n%s" vp v.Name (String.concat "\r\n" (indent e)) b
                
        | Let(isStruct, vs, e, b) ->
            let e = toString scope e |> lines
            let b = toString scope b

            let vp = 
                if vs |> List.exists (fun v -> v.IsMutable) then "mutable "
                else ""

            let pattern =
                let els = vs |> Seq.map (fun v -> sprintf "%s : %s" v.Name (TypeRef.toString scope v.Type)) |> String.concat ", "
                if isStruct then 
                    sprintf "struct(%s)" els
                else
                    sprintf "(%s)" els

            if e.Length = 1 then
                sprintf "let %s%s = %s\r\n%s" vp pattern e.[0] b
            else
                sprintf "let %s%s =\r\n%s\r\n%s" vp pattern (String.concat "\r\n" (indent e)) b

        | PropertyGet(t, p) ->
            let t = toString scope t |> lines
            if t.Length = 1 then sprintf "%s.%s" (bracketed t.[0]) p.name
            else
                let v = new Var(sprintf "___%s" p.name, p.typ)
                sprintf "let %s =\r\n%s\r\n%s.%s" v.Name (String.concat "\r\n" (indent t)) v.Name p.name

        | VarSet (v, e) ->
            let e = toString scope e |> lines

            if e.Length = 1 then
                sprintf "%s <- %s" v.Name e.[0]
            else
                sprintf "%s <-\r\n%s" v.Name (indent e |> String.concat "\r\n")
            
        | IfThenElse(c, i, Unit) ->
            let c = toString scope c |> lines
            let i = toString scope i |> lines

            if c.Length = 1 then    
                if i.Length = 1 then
                    sprintf "if %s then %s" c.[0] i.[0]
                else 
                    sprintf "if %s then\r\n%s" c.[0] (indent i |> String.concat "\r\n")

            else
                sprintf "if (\r\n%s) then\r\n%s" (indent c |> String.concat "\r\n") (indent i |> String.concat "\r\n")


        | IfThenElse(c, i, e) ->
            let c = toString scope c |> lines
            let i = toString scope i |> lines
            let e = toString scope e |> lines

            if c.Length = 1 then    
                if i.Length = 1 && e.Length = 1 then
                    sprintf "if %s then %s else %s" c.[0] i.[0] e.[0]
                else 
                    sprintf "if %s then\r\n%s\r\nelse\r\n%s" c.[0] (indent i |> String.concat "\r\n") (indent e |> String.concat "\r\n")

            else
                sprintf "if (\r\n%s) then\r\n%s\r\nelse\r\n%s" (indent c |> String.concat "\r\n") (indent i |> String.concat "\r\n") (indent e |> String.concat "\r\n")

        | NewTuple(isStruct, args) ->
            let prefix, args = toOneLineArgs scope args

            if isStruct then
                sprintf "%sstruct(%s)" prefix (String.concat ", " args)
            else
                sprintf "%s(%s)" prefix (String.concat ", " args)


type Adaptor =
    {
        vType   : TypeRef
        mType   : TypeRef
        aType   : TypeRef
        init    : Expr -> Expr
        update  : Expr -> Expr -> Expr
        view    : Expr -> Expr
    }

[<AutoOpen>]
module Adaptive =
    let private fda = Namespace "FSharp.Data.Adaptive"
    let private adaptify = Namespace "Adaptify"

    module Option =
        let typ (t : TypeRef) =
            TExtRef(Namespace "Microsoft.FSharp.Core", "Option", [t])
            
        let map (a : TypeRef) (b : TypeRef) =
            {
                declaringType = Choice1Of2 (Module(Namespace "Microsoft.FSharp.Core", "Option", false, true))
                isStatic = true
                name = "map"
                parameters = [ TFunc(a, b); typ a ]
                returnType = typ b
            }
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

module Adaptify =
    let adaptorScope (o : Scope) =
        match o with
        | Module _ ->
            let rec insert (moduleName : string) (o : Scope) =
                match o with
                | Module(parent, name, isAutoOpen, hasModuleSuffix) ->
                    Module(insert moduleName parent, name, isAutoOpen, hasModuleSuffix)
                | _ ->
                    Module(o, moduleName, true, false)
            insert "Adaptify" o
        | o ->
            o

    [<AutoOpen>]
    module Patterns =
        let (|Option|_|) (t : TypeRef) =
            match t with
            | TRef(e, [t]) ->
                match e.TryFullName with
                | Some "Microsoft.FSharp.Core.Option`1" -> Some t
                | _ -> None
            | _ ->
                None

        let (|HashSet|_|) (t : TypeRef) =
            match t with
            | TRef(e, [t]) ->
                match e.TryFullName with
                | Some "FSharp.Data.Adaptive.HashSet`1" -> Some t
                | _ -> None
            | _ ->
                None
                
        let (|HashMap|_|) (t : TypeRef) =
            match t with
            | TRef(e, [k;v]) ->
                match e.TryFullName with
                | Some "FSharp.Data.Adaptive.HashMap`2" -> Some(k,v)
                | _ -> None
            | _ ->
                None

        let (|IndexList|_|) (t : TypeRef) =
            match t with
            | TRef(e, [t]) ->
                match e.TryFullName with
                | Some "FSharp.Data.Adaptive.IndexList`1" -> Some t
                | _ -> None
            | _ ->
                None
                
        let (|AVal|_|) (t : TypeRef) =
            match t with
            | TRef(e, [t]) ->
                match e.TryFullName with
                | Some "FSharp.Data.Adaptive.AdaptiveValue`1" -> Some t
                | Some "FSharp.Data.Adaptive.aval`1" -> Some t
                | Some "FSharp.Data.Adaptive.AdaptiveValue" -> Some t
                | Some "FSharp.Data.Adaptive.aval" -> Some t
                | _ -> None
            | TExtRef(scope, name, [t]) ->
                match Scope.fullName scope with
                | Some "FSharp.Data.Adaptive" ->
                    if name = "aval" || name = "AdaptiveValue" then Some t
                    else None
                | _ -> 
                    None
            | _ ->
                None

        let rec (|AnyAdaptive|_|) (t : TypeRef) =
            match t with
            | Option (AnyAdaptive _) -> 
                Some t

            | TTuple (_, ts) ->
                if ts |> List.exists (function AnyAdaptive _ -> true | _ -> false) then Some t
                else None

            | TVar _ -> Some t
            | TModel _ -> Some t
            | HashSet _ -> Some t
            | IndexList _ -> Some t
            | HashMap _ -> Some t
            | AVal _ -> Some t
            | _ -> None
            
        let (|PlainValue|_|) (t : TypeRef) =
            match t with
            | AnyAdaptive _ -> None
            | _ -> Some t

    module Adaptor =    
        let varPrimitive (t : TypeVar) =
            let mat = TypeVar("pa" + t.Name)

            let tinit = new Var(sprintf "prim%sinit" t.Name, TFunc(TVar t, Object.typ))
            let tupdate = new Var(sprintf "prim%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
            let tview = new Var(sprintf "prim%sview" t.Name, TFunc(Object.typ, TVar mat))

            {
                vType = TVar t
                mType = Object.typ
                aType = TVar mat
                init = fun v -> Application(Var tinit, v)
                update = fun c v -> Application(Application(Var tupdate, c), v)
                view = fun c -> Application(Var tview, c)
            } 
    
        let varModel (t : TypeVar) =
            let mat = TypeVar("pa" + t.Name)
            let at = TypeVar("a" + t.Name)

            let tinit = new Var(sprintf "%sinit" t.Name, TFunc(TVar t, Object.typ))
            let tupdate = new Var(sprintf "%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
            let tview = new Var(sprintf "%sview" t.Name, TFunc(Object.typ, TVar at))

            {
                vType = TVar t
                mType = Object.typ
                aType = TVar at
                init = fun v -> Application(Var tinit, v)
                update = fun c v -> Ignore(Application(Application(Var tupdate, c), v))
                view = fun c -> Application(Var tview, c)
            } 

        let identity (t : TypeRef) =
            {
                vType = t
                mType = t
                aType = t
                init = fun v -> v
                update = fun c v -> v
                view = fun c -> c
            } 

        let aval (t : TypeRef) =
            {
                vType = t
                mType = CVal.typ t
                aType = AVal.typ t
                init = fun v -> Call(None, CVal.ctor t, [v])
                update = fun c v -> Call(Some c, CVal.setValue t, [v])
                view = fun c -> Upcast(c, AVal.typ t)
            } 
            
        let asetPrimitive (t : TypeRef) =
            {
                vType = HashSet.typ t
                mType = CSet.typ t
                aType = ASet.typ t
                init = fun v -> Call(None, CSet.ctor t, [v])
                update = fun c v -> Call(Some c, CSet.setValue t, [v])
                view = fun c -> Upcast(c, ASet.typ t)
            } 
            
        let alistPrimitive (t : TypeRef) =
            {
                vType = IndexList.typ t
                mType = CList.typ t
                aType = AList.typ t
                init = fun v -> Call(None, CList.ctor t, [v])
                update = fun c v -> Call(Some c, CList.setValue t, [v])
                view = fun c -> Upcast(c, AList.typ t)
            } 
            
        let alistModel (a : Adaptor) =
            let v = new Var("v", a.vType)
            let m = new Var("m", a.mType)

            let init = Expr.Lambda([v], a.init (Var v))
            let update = 
                let b = a.update (Var m) (Var v)
                match b.Type with
                | TUnit -> Expr.Lambda([m], Expr.Lambda([v], Seq(b, Var m)))
                | _ -> Expr.Lambda([m], Expr.Lambda([v], b))

            let view = 
                Expr.Lambda([m], a.view (Var m))

            {
                vType = IndexList.typ a.vType
                mType = ChangeableModelList.typ a.vType a.mType a.aType
                aType = AList.typ a.aType
                init = fun v -> Call(None, ChangeableModelList.ctor a.vType a.mType a.aType, [v; init; update; view])
                update = fun c v -> Call(Some c, ChangeableModelList.setValue a.vType a.mType a.aType, [v])
                view = fun c -> Upcast(c, AList.typ a.aType)
            } 

        let amapPrimitive (k : TypeRef) (v : TypeRef) =
            {
                vType = HashMap.typ k v
                mType = CMap.typ k v
                aType = AMap.typ k v
                init = fun vv -> Call(None, CMap.ctor k v, [vv])
                update = fun c vv -> Call(Some c, CMap.setValue k v, [vv])
                view = fun c -> Upcast(c, AMap.typ k v)
            } 

        let amapModel (k : TypeRef) (a : Adaptor) =
            let v = new Var("v", a.vType)
            let m = new Var("m", a.mType)

            let init = Expr.Lambda([v], a.init (Var v))
            let update = Expr.Lambda([m], Expr.Lambda([v], Seq(a.update (Var m) (Var v), Var m)))
            let view = Expr.Lambda([m], a.view (Var m))

            {
                vType = HashMap.typ k a.vType
                mType = ChangeableModelMap.typ k a.vType a.mType a.aType
                aType = AMap.typ k a.aType
                init = fun v -> Call(None, ChangeableModelMap.ctor k a.vType a.mType a.aType, [v; init; update; view])
                update = fun c v -> Call(Some c, ChangeableModelMap.setValue k a.vType a.mType a.aType, [v])
                view = fun c -> Upcast(c, AMap.typ k a.aType)
            } 

        let optionModel (a : Adaptor) =
            let v = new Var("v", a.vType)
            let m = new Var("m", a.mType)

            let init = Expr.Lambda([v], a.init (Var v))
            let update = Expr.Lambda([m], Expr.Lambda([v], Seq(a.update (Var m) (Var v), Var m)))
            let view = Expr.Lambda([m], a.view (Var m))

            {
                vType = Option.typ a.vType
                mType = ChangeableModelOption.typ a.vType a.mType a.aType
                aType = AVal.typ (Option.typ a.aType)
                init = fun v -> Call(None, ChangeableModelOption.ctor a.vType a.mType a.aType, [v; init; update; view])
                update = fun c v -> Call(Some c, ChangeableModelOption.setValue a.vType a.mType a.aType, [v])
                view = fun c -> Upcast(c, AVal.typ (Option.typ a.aType))
            } 

        let tuple (isStruct : bool) (adaptors : list<Adaptor>) =
            let vTypes = adaptors |> List.map (fun a -> a.vType)
            let cTypes = adaptors |> List.map (fun a -> a.mType)
            let aTypes = adaptors |> List.map (fun a -> a.aType)


            let vVars = vTypes |> List.mapi (fun i t -> new Var(sprintf "__v%d" i, t))
            let cVars = cTypes |> List.mapi (fun i t -> new Var(sprintf "__c%d" i, t))
            let aVars = aTypes |> List.mapi (fun i t -> new Var(sprintf "__a%d" i, t))

            let v = new Var("v", TTuple(isStruct, vTypes))
            let m = new Var("m", TTuple(false, cTypes))
            let a = new Var("a", TTuple(false, aTypes))

            let init (e : Expr) = 
                Expr.Let(isStruct, vVars, e, 
                    Expr.NewTuple(false, (adaptors, vVars) ||> List.map2 (fun a v ->
                        a.init (Var v)
                    ))
                )
            let update (m : Expr) (v : Expr) = 
                Expr.Let(false, cVars, m,
                    Expr.Let(isStruct, vVars, v,
                        Expr.Many (
                            (adaptors, cVars, vVars) |||> List.map3 (fun a c v ->
                                a.update (Var c) (Var v)
                            )
                        )
                    )
                )
            let view  (m : Expr) =
                Expr.Let(false, cVars, m,
                    Expr.NewTuple(false, (adaptors, cVars) ||> List.map2 (fun a c -> a.view (Var c)))
                )

            {
                vType = v.Type
                mType = m.Type
                aType = a.Type
                init = init
                update = update
                view = view
            } 


    let rec getAdaptor (mutableScope : bool) (typ : TypeRef) : Adaptor =
        match typ with
        | TVar v ->
            if mutableScope then Adaptor.varPrimitive v
            else Adaptor.varModel v

        | Option (PlainValue t) ->
            if mutableScope then Adaptor.identity typ
            else Adaptor.aval t

        | Option t ->
            let a = getAdaptor mutableScope t
            if mutableScope then failwith ""
            else Adaptor.optionModel a

        | HashSet (PlainValue t) ->
            Adaptor.asetPrimitive t

        | HashSet _t ->
            failwith "HashSet<ModelType> not implemented!"

        | IndexList (PlainValue t) ->
            Adaptor.alistPrimitive t

        | IndexList t ->
            let a = getAdaptor true t
            Adaptor.alistModel a

        | HashMap (k, PlainValue v) ->
            Adaptor.amapPrimitive k v

        | HashMap(k, v) ->
            let a = getAdaptor true v
            Adaptor.amapModel k a

        | TModel(def, targs) ->
            let rec parsAndDef (def : TypeDef) =
                match def with
                | Generic(tpars, def) ->
                    let pp, def = parsAndDef def
                    tpars @ pp, def
                | _ ->
                    [], def

            let _tpars, def = parsAndDef def.Value  
            match def with
            | Generic _ -> failwith "unreachable"

            | Union(scope, name, _, _) ->
                let newName = name + "Adaptive"
                let newScope = adaptorScope scope
                
                let adaptors =
                    targs |> List.map (fun a ->
                        let pat = getAdaptor true a
                        let at = getAdaptor false a
                        pat, at
                    )

                let tAdaptiveArgs = 
                    adaptors |> List.collect (fun (pa, a) -> 
                        let pat = pa.aType
                        let at = a.aType
                        [a.vType; pat; at]
                    )
                    
                let mt = TExtRef(newScope, sprintf "%sAdaptiveCase" name, tAdaptiveArgs)
                let at = TExtRef(newScope, newName, tAdaptiveArgs)

                let ctorArgs =
                    adaptors |> List.collect (fun (pa, a) ->
                        let v = new Var("v", pa.vType)
                        let p = new Var("o", Object.typ)
                        [
                            Lambda([v], Expr.Upcast(pa.init (Var v), Object.typ))
                            Lambda([p], Lambda([v], Expr.Upcast(pa.update (Unbox(pa.mType, Var p)) (Var v), Object.typ)))
                            Lambda([p], pa.view (Expr.Unbox(pa.mType, Var p)))

                            Lambda([v], Expr.Upcast(a.init (Var v), Object.typ))
                            Lambda([p], Lambda([v], Expr.Seq(a.update (Unbox(a.mType, Var p)) (Var v), Var p)))
                            Lambda([p], a.view (Expr.Unbox(a.mType, Var p)))
                        ]
                    )


                if mutableScope then

                    let ctor =
                        {
                            declaringType   = Choice2Of2 at
                            isStatic        = true
                            name            = "CreateAdaptiveCase"
                            parameters      = [typ]
                            returnType      = mt
                        }

                    let update =
                        {
                            declaringType   = Choice2Of2 mt
                            isStatic        = false
                            name            = "update"
                            parameters      = [typ]
                            returnType      = mt
                        }


                    {
                        vType = typ
                        mType = mt
                        aType = mt
                        init = fun v -> Expr.Call(None, ctor, v :: ctorArgs)
                        update = fun c v -> Expr.Call(Some c, update, [v])
                        view = fun c -> c
                    } 
                    

                else

                    let ctor =
                        {
                            declaringType   = Choice1Of2 newScope
                            isStatic        = true
                            name            = TypeRef.toString newScope at
                            parameters      = [typ]
                            returnType      = at
                        }

                    let update =
                        {
                            declaringType   = Choice2Of2 at
                            isStatic        = false
                            name            = "update"
                            parameters      = [typ]
                            returnType      = TTuple(false, [])
                        }

                    {
                        vType = typ
                        mType = at
                        aType = AVal.typ mt
                        init = fun v -> Expr.Call(None, ctor, v :: ctorArgs)
                        update = fun c v -> Expr.Call(Some c, update, [v])
                        view = fun c -> Expr.Upcast(c, AVal.typ mt)
                    } 

                
            | ProductType(_, scope, name, _) ->
                let newName = name + "Adaptive"
                let newScope = adaptorScope scope

                let adaptors =
                    targs |> List.map (fun a ->
                        let pat = getAdaptor true a
                        let at = getAdaptor false a
                        pat, at
                    )


                let tAdaptiveArgs = 
                    adaptors |> List.collect (fun (pa, a) -> 
                        let pat = pa.aType
                        let at = a.aType
                        [a.vType; pat; at]
                    )

                let at = TExtRef(newScope, newName, tAdaptiveArgs)

                let ctorArgs =
                    adaptors |> List.collect (fun (pa, a) ->
                        let v = new Var("v", pa.vType)
                        let p = new Var("o", Object.typ)
                        [
                            Lambda([v], Expr.Upcast(pa.init (Var v), Object.typ))
                            Lambda([p], Lambda([v], Expr.Upcast(pa.update (Unbox(pa.mType, Var p)) (Var v), Object.typ)))
                            Lambda([p], pa.view (Expr.Unbox(pa.mType, Var p)))
                            Lambda([v], Expr.Upcast(a.init (Var v), Object.typ))
                            Lambda([p], Lambda([v], Expr.Seq(a.update (Unbox(a.mType, Var p)) (Var v), Var p)))
                            Lambda([p], a.view (Expr.Unbox(a.mType, Var p)))
                        ]
                    )

                let ctor =
                    {
                        declaringType   = Choice1Of2 newScope
                        isStatic        = true
                        name            = TypeRef.toString newScope at
                        parameters      = [typ]
                        returnType      = at
                    }

                let update =
                    {
                        declaringType   = Choice2Of2 at
                        isStatic        = false
                        name            = "update"
                        parameters      = [typ]
                        returnType      = TTuple(false, [])
                    }

                {
                    vType = typ
                    mType = at
                    aType = at
                    init = fun v -> Expr.Call(None, ctor, v :: ctorArgs)
                    update = fun c v -> Expr.Call(Some c, update, [v])
                    view = id
                } 

        | TTuple(isStruct, els) ->
            let adaptors = els |> List.map (getAdaptor false)

            Adaptor.tuple isStruct adaptors


        | _ -> 
            if mutableScope then Adaptor.identity typ
            else Adaptor.aval typ



    type TypeDefinition =
        {
            isInterface : bool
            baseType    : Option<TypeRef>
            scope       : Scope
            name        : string
            tpars       : list<TypeVar>
            ctorArgs    : list<Var>
            ctor        : Expr
            statics     : list<string * list<Var> * Expr>
            members     : list<string * list<Var> * Expr>
            interfaces  : list<TypeRef * list<string * list<Var> * (Var -> Expr)>>
        }

    module TypeDefinition =
        [<AutoOpen>]
        module private Helpers =
        
            let lines (str : string) =
                str.Split([|"\r\n"|], StringSplitOptions.None)

            let indent (lines : string[]) =
                lines |> Array.map (fun s -> "    " + s)
        
            let indentStr (str : string) =
                str |> lines |> indent |> String.concat "\r\n"

        let toString (d : TypeDefinition) =
            let defaultOpens =
                [|
                    "open System"
                    "open FSharp.Data.Adaptive"
                    "open Adaptify"
                |]
            let rec wrap (s : Scope) (str : string[]) =
                match s with
                | Global ->
                    
                    sprintf "namespace global\r\n\r\n%s" (String.concat "\r\n" (Array.append defaultOpens str))
                    
                | Namespace ns ->
                    sprintf "namespace %s\r\n\r\n%s" ns (String.concat "\r\n" (Array.append defaultOpens str))

                | Module(parent, name, autoOpen, moduleSuffix) ->

                    let attrs =
                        [
                            if autoOpen then "AutoOpen"
                            if moduleSuffix then "CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)"
                        ]

                    match attrs with
                    | [] ->
                        let code =
                            [|
                                yield sprintf "module %s =" name
                                yield! indent str
                            |]
                        wrap parent code
                    | atts ->
                        let atts = atts |> String.concat "; " |> sprintf "[<%s>]" 
                        let code =
                            [|
                                yield atts
                                yield sprintf "module %s =" name
                                yield! indent str
                            |]
                        wrap parent code
                      
            let tpars =
                match d.tpars with
                | [] -> ""
                | ts -> ts |> Seq.map string |> String.concat ", " |> sprintf "<%s>"

            if d.isInterface then

                let code =
                    [|
                        yield sprintf "type %s%s =" d.name tpars 
                        for (name, args, b) in d.members do
                            let typ = b.Type
                            if args = [] && name.StartsWith "get_" then
                                yield sprintf "    abstract member %s : %s" (name.Substring 4) (TypeRef.toString d.scope typ)
                            else
                                let args = 
                                    List.concat [
                                        args |> List.map (fun a -> TypeRef.toString d.scope a.Type)
                                        [TypeRef.toString d.scope typ]
                                    ]
                                yield sprintf "    abstract member %s : %s" name (String.concat " -> " args)
                                
                    |]
            
                wrap d.scope code
            else


                let ctorArgs =
                    d.ctorArgs 
                    |> Seq.map (fun v -> sprintf "%s : %s" v.Name (TypeRef.toString d.scope v.Type)) |> String.concat ", "

                let ctorLines = Expr.toString d.scope d.ctor |> lines |> indent

                let ctorLines =
                    if ctorLines.Length > 0 && ctorLines.[ctorLines.Length - 1].Trim() = "()" then Array.take (ctorLines.Length - 1) ctorLines
                    else ctorLines

                let selfType =
                    TExtRef(d.scope, d.name, List.map TVar d.tpars)

                let code =
                    [|
                        yield sprintf "type %s%s(%s) =" d.name tpars ctorArgs
                        match d.baseType with
                        | Some b -> yield sprintf "    inherit %s()" (TypeRef.toString d.scope b)
                        | None -> ()


                        yield! ctorLines

                        for (name, args, body) in d.statics do
                            let body = Expr.toString d.scope body |> lines
                            let args = args |> Seq.map (fun a -> sprintf "%s : %s" a.Name (TypeRef.toString d.scope a.Type)) |> String.concat ", "
                            
                            if body.Length = 1 then
                                yield sprintf "    static member %s(%s) = %s" name args body.[0]
                            else
                                yield sprintf "    static member %s(%s) =" name args
                                yield! body |> indent |> indent

                            
                        for (name, args, body) in d.members do
                            let body = Expr.toString d.scope body |> lines
                            if args = [] && name.StartsWith "get_" then
                            
                                if body.Length = 1 then
                                    yield sprintf "    member __.%s = %s" (name.Substring 4) body.[0]
                                else
                                    yield sprintf "    member __.%s =" (name.Substring 4)
                                    yield! body |> indent |> indent
                            else
                                let args = args |> Seq.map (fun a -> sprintf "%s : %s" a.Name (TypeRef.toString d.scope a.Type)) |> String.concat ", "
                            
                                if body.Length = 1 then
                                    yield sprintf "    member __.%s(%s) = %s" name args body.[0]
                                else
                                    yield sprintf "    member __.%s(%s) =" name args
                                    yield! body |> indent |> indent

                        for (iface, mems) in d.interfaces do
                            let name = TypeRef.toString d.scope iface
                            yield sprintf "    interface %s with" name
                            let this = new Var("x", selfType)
                            for (name, args, body) in mems do
                                let body = Expr.toString d.scope (body this) |> lines
                                let args = args |> Seq.map (fun a -> sprintf "%s : %s" a.Name (TypeRef.toString d.scope a.Type)) |> String.concat ", "
                            
                                if body.Length = 1 then
                                    yield sprintf "        member x.%s(%s) = %s" name args body.[0]
                                else
                                    yield sprintf "        member x.%s(%s) =" name args
                                    yield! body |> indent |> indent |> indent

                    |]
                
                wrap d.scope code
        let rec isValueType (t : FSharpEntity) =
            if t.IsFSharpAbbreviation then
                let t = t.AbbreviatedType
                if t.HasTypeDefinition then
                    isValueType t.TypeDefinition
                elif t.IsStructTupleType then
                    true
                else
                    false
            else
                t.IsValueType

        let eq (a : Expr) (b : Expr) = 
            let isValueType =
                match a.Type with
                | TRef(e,_) -> isValueType e
                | TModel(d, _) -> 
                    match d.Value with
                    | ProductType(isValueType, _, _, _) -> isValueType
                    | _ -> false
                | _ ->
                    false
            if isValueType then
                Expr.Application(
                    Expr.Application(
                        Expr.Call(None, uncheckedEquals a.Type, []),
                        a
                    ),
                    b
                )
            else 
                Expr.Call(None, Object.refEquals, [a; b])

        let productType (args : list<Var>) (tpars : list<TypeVar>) (s : Scope) (n : string) (props : list<Prop * Expr>) =

            

            let adaptors = 
                props |> List.map (fun (p,b) -> new Var("_" + p.name + "_", p.typ), p, b, getAdaptor false p.typ)

            let newScope = 
                adaptorScope s

            let newName = 
                n + "Adaptive"
                        

            let cache = args |> List.map (fun v -> new Var(sprintf "__%s" v.Name, v.Type, true))

            //let currentVar = new Var("__current", valueType, true)
            //let initialVar = new Var("value", valueType)

            let rec ctor (adaptors : list<Var * Prop * Expr * Adaptor>) =
                match adaptors with
                | [] -> 
                    (Expr.Unit, cache, args) |||> List.fold2 (fun s c a ->
                        Expr.Let(false, [c], Var a, s)
                    )
                | (v, p, get, a) :: rest ->
                    Expr.Let(
                        false, [v], a.init get,
                        ctor rest
                    )
                    
            let ctor = ctor adaptors
            let rec update (adaptors : list<Var * Prop * Expr * Adaptor>) =
                match adaptors with
                | [] -> 
                    Expr.Unit

                | [(v, p, get, a)] -> 
                    a.update (Var v) get

                | (v, p, get, a) :: rest ->
                    Expr.Seq(
                        a.update (Var v) get,
                        update rest
                    )

            
         


            let update =
                match args with
                | [] ->
                    Expr.Unit
                | _ -> 
                    let allEq = (args, cache) ||> List.map2 (fun a c -> eq (Var a) (Var c)) |> And
              
                    Expr.IfThenElse(
                        Expr.Call(None, notMeth, [allEq]),
                        Expr.Many [
                            for (args, cache) in List.zip args cache do
                                yield Expr.VarSet(cache, Var args)
                            yield update adaptors
                        ],
                        Expr.Unit
                    )
                        
            let members =
                [
                    yield "update", args, update

                    for (v, p, _, a) in adaptors do
                        yield sprintf "get_%s" p.name, [], a.view (Var v)
                ]
                    



            let tAdaptivePars =
                tpars |> List.collect (fun t -> [t; TypeVar("pa" + t.Name); TypeVar("a" + t.Name)])

            let ctorArgs = 
                tpars |> List.collect (fun t ->
                    let mat = TypeVar("pa" + t.Name)
                    let at = TypeVar("a" + t.Name)

                    [
                        new Var(sprintf "prim%sinit" t.Name, TFunc(TVar t, Object.typ))
                        new Var(sprintf "prim%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
                        new Var(sprintf "prim%sview" t.Name, TFunc(Object.typ, TVar mat))
                        new Var(sprintf "%sinit" t.Name, TFunc(TVar t, Object.typ))
                        new Var(sprintf "%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
                        new Var(sprintf "%sview" t.Name, TFunc(Object.typ, TVar at))
                    ]
                )

            {
                isInterface = false
                baseType    = None
                scope       = newScope
                name        = newName
                tpars       = tAdaptivePars
                ctorArgs    = args @ ctorArgs
                ctor        = ctor
                statics     = []
                members     = members
                interfaces  = []
            }


        let rec ofTypeDef (tpars : list<TypeVar>) (d : TypeDef) =
            match d with
            | Generic(tpars, def) ->
                ofTypeDef tpars def

            | ProductType(_, s, n, props) ->
                let valueType = TModel(lazy d, List.map TVar tpars)
                let arg = new Var("value", valueType)

                let props =
                    props |> List.map (fun p ->
                        (p, Expr.PropertyGet(Var arg, p))
                    )

                [ productType [arg] tpars s n props ]
              
            
            | Union(scope, name, props, cases) ->
                //if props <> [] then failwith ""
                // TODO: props

                let newScope = 
                    adaptorScope scope

                let newName = 
                    name + "Adaptive"
                    
                let tAdaptivePars =
                    tpars |> List.collect (fun t -> [t; TypeVar("pa" + t.Name); TypeVar("a" + t.Name)])


                let caseTypes =
                    cases |> List.map (fun (caseName, props) ->

                        let args =
                            props |> List.map (fun p ->
                                new Var(p.name, p.typ)
                            )

                        let props = 
                            (args, props) ||> List.map2 (fun a p ->
                                p, Var a
                            )

                        //let valueType = TTuple(false, props |> List.map (fun p -> p.typ))
                        let typeName = sprintf "%s%s" name caseName
                        productType args tpars scope typeName props
                    )


                let ctorTypeRef = TExtRef(newScope, sprintf "%sAdaptiveCase" name, List.map TVar tAdaptivePars)
                let valueType = TExtRef(scope, name, List.map TVar tpars)

                let ctorType =
                    {
                        isInterface     = true
                        baseType        = None
                        scope           = newScope
                        name            = sprintf "%sAdaptiveCase" name
                        tpars           = tAdaptivePars
                        ctorArgs        = []
                        ctor            = Expr.Unit
                        statics         = []
                        members =
                            [
                                "update", [new Var("value", valueType)], Expr.Unbox(ctorTypeRef, Expr.Unit)
                            ]
                        interfaces = []
                    }

                let createNew (caseName : string) =

                    let props = cases |> List.pick (fun (n, p) -> if n = caseName then Some p else None)
                    let vars = props |> List.map (fun p -> new Var(p.name, p.typ))
                    let pat = UnionCaseTest(valueType, caseName, vars)

                    let adaptors =
                        tpars |> List.map (fun a ->
                            let pat = getAdaptor true (TVar a)
                            let at = getAdaptor false (TVar a)
                            pat, at
                        )

                    let ctorArgs =
                        adaptors |> List.collect (fun (pa, a) ->
                            let v = new Var("v", pa.vType)
                            let p = new Var("o", Object.typ)
                            [
                                Lambda([v], Expr.Upcast(pa.init (Var v), Object.typ))
                                Lambda([p], Lambda([v], Expr.Upcast(pa.update (Unbox(pa.mType, Var p)) (Var v), Object.typ)))
                                Lambda([p], pa.view (Expr.Unbox(pa.mType, Var p)))
                                Lambda([v], Expr.Upcast(a.init (Var v), Object.typ))
                                Lambda([p], Lambda([v], Expr.Seq(a.update (Unbox(a.mType, Var p)) (Var v), Var p)))
                                Lambda([p], a.view (Expr.Unbox(a.mType, Var p)))
                            ]
                        )

                    let otherType =
                        TExtRef(newScope, sprintf "%s%s" name caseName, List.map TVar tAdaptivePars)

                    let meth =     
                        {
                            declaringType   = Choice1Of2 newScope
                            isStatic        = true
                            name            = sprintf "%s%sAdaptive" name caseName
                            parameters      = 
                                List.append
                                    (props |> List.map (fun v -> v.typ))
                                    (ctorArgs |> List.map (fun v -> v.Type))
                            returnType      = otherType
                        }


                    pat, Expr.Upcast(Expr.Call(None, meth, (List.map Var vars) @ ctorArgs), ctorTypeRef)

                let caseTypes =
                    (cases, caseTypes) ||> List.map2 (fun (selfCase,_) ct ->
                        let value = new Var("value", valueType)
                        { ct with
                            interfaces =
                                [
                                    ctorTypeRef, [
                                        "update", [value], (fun this ->
                                            Expr.Match(
                                                Var value,
                                                [
                                                    for (caseName, props) in cases do
                                                        let vars = props |> List.map (fun p -> new Var(p.name, p.typ))
                                                        let pat = UnionCaseTest(valueType, caseName, vars)
                                                        if caseName = selfCase then

                                                            let meth =     
                                                                {
                                                                    declaringType   = Choice2Of2 this.Type
                                                                    isStatic        = false
                                                                    name            = "update"
                                                                    parameters      = vars |> List.map (fun v -> v.Type)
                                                                    returnType      = TTuple(false, [])
                                                                }

                                                            let update =    
                                                                Expr.Seq(
                                                                    Expr.Call(Some (Var this), meth, List.map Var vars),
                                                                    Expr.Upcast(Var this, ctorTypeRef)
                                                                )
                                                            yield pat, update
                                                        else
                                                            yield createNew caseName
                                                ]    
                                            )
                                        )
                                    ]
                                ]
                        }
                    )

                let adaptiveType =
                    let value = new Var("value", valueType)
                    let current = new Var("__value", ctorTypeRef, true)
                    let n = new Var("__n", ctorTypeRef)

                    let ctor =
                        Expr.Match(
                            Var value, 
                            cases |> List.map (fun (c,_) -> createNew c)
                        )

                    let updateMeth =
                        {
                            isStatic = false
                            declaringType = Choice2Of2 ctorTypeRef
                            returnType = ctorTypeRef
                            parameters = [valueType]
                            name = "update"
                        }

                    let selfType = TExtRef(newScope, sprintf "%sAdaptive" name, List.map TVar tAdaptivePars)

                    let markOutdated =
                        {
                            isStatic = false
                            declaringType = Choice2Of2 selfType
                            returnType = TTuple(false, [])
                            parameters = []
                            name = "MarkOutdated"
                        }

                    let eval =
                        {
                            isStatic = false
                            declaringType = Choice2Of2 selfType
                            returnType = TFunc(AdaptiveToken.typ, TFunc(TFunc(AdaptiveToken.typ, ctorTypeRef), ctorTypeRef))
                            parameters = []
                            name = "get_EvaluateAlways"
                        }

                    let token = new Var("t", AdaptiveToken.typ)

                    //let tAdaptivePars =
                    //    tpars |> List.collect (fun t -> [t; TypeVar("pa" + t.Name); TypeVar("a" + t.Name)])

                    let ctorArgs = 
                        tpars |> List.collect (fun t ->
                            let mat = TypeVar("pa" + t.Name)
                            let at = TypeVar("a" + t.Name)

                            [
                                new Var(sprintf "prim%sinit" t.Name, TFunc(TVar t, Object.typ))
                                new Var(sprintf "prim%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
                                new Var(sprintf "prim%sview" t.Name, TFunc(Object.typ, TVar mat))
                                new Var(sprintf "%sinit" t.Name, TFunc(TVar t, Object.typ))
                                new Var(sprintf "%supdate" t.Name, TFunc(Object.typ, TFunc(TVar t, Object.typ)))
                                new Var(sprintf "%sview" t.Name, TFunc(Object.typ, TVar at))
                            ]
                        )


                    {
                        isInterface     = false
                        baseType        = Some AdaptiveObject.typ
                        scope           = newScope
                        name            = sprintf "%sAdaptive" name
                        tpars           = tAdaptivePars
                        ctorArgs        = value :: ctorArgs
                        ctor            = Expr.Let(false, [current], ctor, Expr.Unit)
                            
                        statics =
                            [
                                "CreateAdaptiveCase", value :: ctorArgs, ctor
                            ]

                        members =
                            [
                                "update", [value], 
                                    Expr.Let(false, [n], Expr.Call(Some (Var current), updateMeth, [Var value]), 
                                        Expr.IfThenElse(
                                            Call(None, notMeth, [eq (Var n) (Var current)]),
                                            Expr.Seq(
                                                VarSet(current, Var n),
                                                Call(Some (Var (new Var("__", selfType))), markOutdated, [])
                                            ),
                                            Expr.Unit
                                        )
                                    )
                            ]
                        interfaces = 
                            [
                                AVal.typ ctorTypeRef, [
                                    "GetValue", [token], (fun this ->
                                        Application(Application(Expr.Call(Some (Var this), eval, []), Var token),
                                            Lambda([token], Var current)
                                        )
                                    )
                                ]
                            ]
                    }
                    


                [ctorType] @ caseTypes @ [adaptiveType]
            

    let test (defs : list<TypeDef>) =
        let all = defs |> List.collect (TypeDefinition.ofTypeDef [])
        printfn "#nowarn \"49\" // upper case patterns"
        printfn "#nowarn \"66\" // upcast is unncecessary"

        for a in all do 
            let str = TypeDefinition.toString a
            printfn "%s" str
            printfn ""
            printfn ""
            printfn ""










