namespace rec Adaptify.Compiler


open System
open FSharp.Compiler.SourceCodeServices
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
    | TModel of def : Lazy<TypeDef> * targs : list<TypeRef>
    | TExtRef of scope : Scope * name : string * targs : list<TypeRef>
    | TRef of ent : FSharpEntity * targs : list<TypeRef>
    | TVar of var : TypeVar

type TypeDef =
    | ProductType of isValueType : bool * scope : Scope * name : string * properties : list<Prop>
    | Union of scope : Scope * name : string * properties : list<Prop> * cases : list<string * list<Prop>>
    | Generic of tpars : list<TypeVar> * def : TypeDef

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

[<AutoOpen>]
module TypeRefPatterns =
    let inline (|TUnit|_|) (t : TypeRef) =
        match t with
        | TTuple(false, []) -> Some ()
        | _ -> None


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


