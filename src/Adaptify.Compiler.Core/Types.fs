namespace rec Adaptify.Compiler


open System
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range
open Adaptify.Compiler

type TypeVar(name : string) =
    member x.Name = name
    override x.ToString() = sprintf "'%s" name

[<StructuredFormatDisplay("{AsString}")>]
type TypeRef =
    | TBool
    | TTuple of isStruct : bool * elements : TypeRef list
    | TFunc of domain : TypeRef * codomain : TypeRef
    | TArray of element : TypeRef * rank : int
    | TModel of range : range * def : Lazy<TypeDef> * targs : list<TypeRef>
    | TExtRef of scope : Scope * name : string * targs : list<TypeRef>
    | TRef of range : range * ent : FSharpEntity * targs : list<TypeRef>
    | TVar of var : TypeVar
    
    member private x.AsString = x.ToString()
    override x.ToString() = TypeRef.toString Global x
    
[<StructuredFormatDisplay("{AsString}")>]
type TypeDef =
    | ProductType of lenses : bool * range : range * isValueType : bool * scope : Scope * name : string * properties : list<Prop>
    | Union of range : range * scope : Scope * name : string * properties : list<Prop> * cases : list<string * list<Prop>>
    | Generic of tpars : list<TypeVar> * def : TypeDef
    
    member private x.AsString = x.ToString()
    override x.ToString() = 
        let rec print (targs : list<TypeVar>) (t : TypeDef) =
            match t with
            | Generic(t, d) -> print (targs @ t) d
            | _ ->  
                TModel(range0, lazy x, List.map TVar targs)
                |> TypeRef.toString Global
        print [] x

    member x.Name =
        match x with
        | ProductType(_, _, _, _, n, _)
        | Union(_, _, n, _, _) ->
            n
        | Generic(_,d) ->
            d.Name
            
    member x.RelativeName (scope : Scope) =
        match x with
        | ProductType(_, _, _, s, n, _)
        | Union(_, s, n, _, _) ->
            match Scope.relativeName scope s with
            | Some s -> sprintf "%s.%s" s n
            | None -> n

        | Generic(_,d) ->
            d.RelativeName scope
        

    member x.FullName =
        match x with
        | ProductType(_, _, _, s, n, _)
        | Union(_, s, n, _, _) ->
            match Scope.fullName s with
            | Some s -> sprintf "%s.%s" s n
            | None -> n

        | Generic(_,d) ->
            d.FullName

type AdaptifyMode = 
    | Default
    | Value
    | Lazy
    | NonAdaptive

module AdaptifyMode =
    let ofAttributes (atts : seq<FSharpAttribute>) =
        if atts |> Seq.exists FSharpAttribute.isNonAdaptive then AdaptifyMode.NonAdaptive
        elif atts |> Seq.exists FSharpAttribute.isTreatAsValue then AdaptifyMode.Value
        else AdaptifyMode.Default


type Prop =
    {
        range           : FSharp.Compiler.Range.range
        name            : string
        typ             : TypeRef
        mode            : AdaptifyMode
        isRecordField   : bool
    }

[<AutoOpen>]
module TypeRefPatterns =
    let inline (|TUnit|_|) (t : TypeRef) =
        match t with
        | TTuple(false, []) -> Some ()
        | _ -> None


module Prop =
    let ofFSharpField (log : ILog)(targs : Map<_,_>) (f : FSharpField) =
        let mode = AdaptifyMode.ofAttributes f.PropertyAttributes
        let typ = TypeRef.ofType log targs f.FieldType
        let name = f.Name
        {
            range = try f.DeclarationLocation with _ -> range0
            name = name
            typ = typ
            mode = mode
            isRecordField = match f.DeclaringEntity with | Some e -> e.IsFSharpRecord | _ -> false
        }

    let ofMemberOrFunctionOrValue (log : ILog) (targs : Map<_,_>) (mfv : FSharpMemberOrFunctionOrValue) =
        if mfv.IsProperty && mfv.HasGetterMethod && mfv.IsInstanceMember then
            let mode = 
                if mfv.Attributes |> Seq.exists FSharpAttribute.isNonAdaptive then NonAdaptive
                elif mfv.Attributes |> Seq.exists FSharpAttribute.isTreatAsValue then Value
                else Lazy

            let typ = TypeRef.ofType log targs mfv.GetterMethod.ReturnParameter.Type
            let name = mfv.DisplayName
            Some {
                range = try mfv.DeclarationLocation with _ -> range0
                name = name
                typ = typ
                mode = mode
                isRecordField = false
            }
        else
            None

    let printDef (p : Prop) =
        let prefix =
            match p.mode with
            | Value -> "[<TreatAsValue>] "
            | NonAdaptive -> "[<NonAdaptive>] "
            | Lazy -> ""
            | Default -> ""
        sprintf "%s%s : %s" prefix p.name (string p.typ)

module TypeRef =
    
    let rec ofType (log : ILog) (args : Map<string, TypeRef>) (t : FSharpType) =
        if t.IsGenericParameter then
            let name = t.GenericParameter.DisplayName
            match Map.tryFind name args with
            | Some t -> t
            | None -> failwithf "generic parameter '%s not found" name

        elif t.IsStructTupleType then
            let els = 
                t.GenericArguments 
                |> Seq.toList
                |> List.map (ofType log args)
            TTuple(true, els)

        elif t.IsTupleType then
            let els = 
                t.GenericArguments 
                |> Seq.toList
                |> List.map (ofType log args)
            TTuple(false, els)

        //elif t.IsAbbreviation && not (t.Format(FSharpDisplayContext.Empty).StartsWith "Microsoft.FSharp.") then
        //    ofType log args t.AbbreviatedType

        elif t.IsFunctionType then
            let targs = t.GenericArguments
            let cd = targs.[targs.Count - 1] |> ofType log args
            let d = Seq.take (targs.Count - 1) targs |> Seq.map (ofType log args) |> Seq.toList

            let rec build (d : list<TypeRef>) (cd : TypeRef) =
                match d with
                | [] -> cd
                | h :: t -> TFunc(h, build t cd)

            build d cd

        elif t.HasTypeDefinition then
            let def = t.TypeDefinition
            let range = try def.DeclarationLocation with _ -> range0
            if def.IsArrayType then
                let el = ofType log args t.GenericArguments.[0]
                TArray(el, def.ArrayRank)
            else
                let targs = 
                    t.GenericArguments
                    |> Seq.toList
                    |> List.map (ofType log args)

                match TypeDef.ofEntity log def with
                | Some def ->
                    TModel(range, def, targs) 
                | None ->
                    TRef(range, def, targs)

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

        | TModel(_, def, []) ->
            sprintf "%s" (def.Value.RelativeName currentScope)

        | TModel(_, def, targs) ->
            let targs = targs |> Seq.map (toString currentScope) |> String.concat ", "
            sprintf "%s<%s>" (def.Value.RelativeName currentScope) targs

        | TRef(_, r, []) ->
            let s = Scope.ofFSharpEntityOpt r.DeclaringEntity r.Namespace
            match Scope.relativeName currentScope s with
            | Some n -> n + "." + r.DisplayName
            | None -> r.DisplayName

        | TRef(_, r, targs) ->
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

    let rec containedModelTypes (t : TypeRef) =
        match t with
        | TBool -> []
        | TFunc _ -> []
        | TTuple(_, els) -> els |> List.collect containedModelTypes
        | TArray(e, _) -> containedModelTypes e
        | TExtRef(_, _, targs) -> targs |> List.collect containedModelTypes
        | TRef(_, _, targs) -> targs |> List.collect containedModelTypes
        | TModel(_, _, _) -> [t]
        | TVar _ -> []

module TypeDef =
    let dict = System.Collections.Concurrent.ConcurrentDictionary<FSharpEntity, Lazy<TypeDef>>()

    let rec private withInfo (range : range) (scope : Scope) (name : string) (d : TypeDef) =
        match d with
        | ProductType(lenses, _, v, _, _, p) -> ProductType(lenses, range, v, scope, name, p)
        | Union(_, _, _, p, c) -> Union(range, scope, name, p, c)
        | Generic(pars, t) -> Generic(pars, withInfo range scope name t)
        

    let rec private create (log : ILog) (e : FSharpEntity) =
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
            |> List.choose (Prop.ofMemberOrFunctionOrValue log parMap)

        let range = try e.DeclarationLocation with _ -> range0

        if e.IsFSharpRecord then
            let fields = 
                e.FSharpFields
                |> Seq.toList
                |> List.map (Prop.ofFSharpField log parMap)

            let parent = 
                Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace

            ProductType(true, range, e.IsValueType, parent, e.DisplayName, props @ fields) |> ret

        elif e.IsFSharpUnion then
            let cases = 
                e.UnionCases
                |> Seq.toList
                |> List.map (fun c ->
                    let name = c.DisplayName

                    let mode = 
                        AdaptifyMode.ofAttributes c.Attributes

                    let fields = 
                        c.UnionCaseFields
                        |> Seq.toList
                        |> List.map (Prop.ofFSharpField log parMap)
                        |> List.map (fun p -> { p with mode = mode })

                    name, fields
                )
                
            let parent = Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace

            Union(range, parent, e.DisplayName, props, cases) |> ret

        elif e.IsFSharpAbbreviation then
            let parent = Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace

            let real = e.AbbreviatedType
            if real.HasTypeDefinition then
                let def = create log real.TypeDefinition
                withInfo range parent e.DisplayName def |> ret
            else
                ProductType(false, range, e.IsValueType, parent, e.DisplayName, props) |> ret
        else
            let parent = Scope.ofFSharpEntityOpt e.DeclaringEntity e.Namespace
            ProductType(false, range, e.IsValueType, parent, e.DisplayName, props) |> ret

    and ofEntity (log : ILog) (e : FSharpEntity) : option<Lazy<TypeDef>> =
        let isModel =
            e.TryFullName = Some "Microsoft.FSharp.Core.FSharpOption`1" ||
            e.TryFullName = Some "Microsoft.FSharp.Core.FSharpChoice`2" ||
            e.TryFullName = Some "Microsoft.FSharp.Core.FSharpChoice`3" ||
            e.TryFullName = Some "Microsoft.FSharp.Core.FSharpChoice`4" ||
            e.TryFullName = Some "Microsoft.FSharp.Core.FSharpResult`2" ||
            e.Attributes |> Seq.exists FSharpAttribute.isModelType

        if isModel then 
            let loc = try e.DeclarationLocation with _ -> range0
            if e.IsArrayType then log.warn loc "2413" "arrays cannot be model types"; None
            elif e.IsByRef then log.warn loc "2413" "byrefs cannot be model types"; None
            elif e.IsDelegate then log.warn loc "2413" "delegates cannot be model types"; None
            elif e.IsEnum then log.warn loc "2413" "enums cannot be model types"; None
            //elif e.IsFSharpAbbreviation then log.warn loc "abbreviations cannot be model types"; None
            elif e.IsFSharpExceptionDeclaration then log.warn loc "2413" "exceptions cannot be model types"; None
            elif e.IsFSharpModule then log.warn loc "2413" "modules cannot be model types"; None
            elif e.IsMeasure then log.warn loc "2413" "measures cannot be model types"; None
            else dict.GetOrAdd(e, fun e -> lazy (create log e)) |> Some
        else    
            None


