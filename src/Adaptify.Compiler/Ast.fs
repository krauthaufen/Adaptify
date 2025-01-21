﻿namespace rec Adaptify.Compiler

open System
open FSharp.Compiler.Symbols
open Adaptify.Compiler

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
    | RecordUpdate of Expr * Prop * Expr

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
        | RecordUpdate(r,_,_) -> 
            r.Type

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

    let private argDef (log : ILog) (scope : Scope) (args : list<list<Var>>) =
        args 
        |> List.map (
            List.map (fun v -> sprintf "%s : %s" v.Name (TypeRef.toString log scope v.Type)) 
            >> String.concat ", "
            >> sprintf "(%s)"
        )
        |> String.concat " "

    let private bracketed (str : string) =
        if str.Contains "(" || str.Contains " " || str.Contains "fun" then sprintf "(%s)" str
        else str


    let private toOneLineArgs (log : ILog) (scope : Scope) (args : list<Expr>) =
        let temporary = System.Collections.Generic.List()
        let args = 
            args |> List.mapi (fun i a ->
                match a with
                | Lambdas(vs, b) ->
                    let b = toString log scope b |> lines
                    if b.Length = 1 then
                        sprintf "(fun %s -> %s)" (argDef log scope vs) b.[0]
                    else
                        let name = sprintf "__arg%d" i

                        let code = sprintf "let inline %s %s =\r\n%s" name (argDef log scope vs) (indent b |> String.concat "\r\n")

                        temporary.Add(code)
                        name
                | _ ->
                    let a = toString log scope a |> lines
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

    let private patternString (log : ILog) (scope : Scope) (p : Pattern) =
        match p with
        | Any -> "_"
        | TypeTest(t, v) -> sprintf "(:? %s as %s)" (TypeRef.toString log scope t) v.Name
        | UnionCaseTest(t, n, vs) ->
            let t = 
                match t with
                | TExtRef(s, n, _) -> TExtRef(s, n, []) |> TypeRef.toString log scope
                | TRef(r, e, _) -> TRef(r, e, []) |> TypeRef.toString log scope
                | TModel(r, d, _) -> TModel(r, d, []) |> TypeRef.toString log scope
                | _ -> t |> TypeRef.toString log scope
            match vs with
            | [] ->
                sprintf "%s.%s" t n 
            | _ -> 
                sprintf "%s.%s(%s)" t n (vs |> List.map (fun v -> v.Name) |> String.concat ", ")

    let rec toString (log : ILog) (scope : Scope) (e : Expr) =
        match e with
        | Match(e, cases) ->
            let prefix, e = toOneLineArgs log scope [e]
            let e = List.head e


            let cases =
                cases |> List.map (fun (p, b) ->
                    let b = toString log scope b |> lines 
                    if b.Length = 1 then
                        sprintf "| %s -> %s" (patternString log scope p) b.[0]
                    else
                        let b = b |> indent
                        sprintf "| %s ->\r\n%s" (patternString log scope p) (String.concat "\r\n" b)
                )

            sprintf "%smatch %s with\r\n%s" prefix e (String.concat "\r\n" cases)


        | And vs ->
            vs |> List.map (toString log scope) |> String.concat " && "

        | Lambda([v], Application(b, Var v1)) when v.Name = v1.Name ->
            toString log scope b

        | Unbox(t, e) ->
            let prefix, e = toOneLineArgs log scope [e]
            let e = List.head e
            sprintf "%sunbox<%s> %s" prefix (TypeRef.toString log scope t) e

        | Ignore(e) ->
            let prefix, e = toOneLineArgs log scope [e]
            let e = List.head e
            sprintf "%signore %s"prefix e

        | Upcast(e, t) ->
            let e = toString log scope e
            let t = TypeRef.toString log scope t
            sprintf "%s :> %s" e t
        | Unit -> 
            "()"

        | Var v -> 
            v.Name

        | Seq(l, r) ->
            sprintf "%s\r\n%s" (toString log scope l) (toString log scope r)

        | Fail(_, reason) ->
            sprintf "failwith \"%s\"" reason

        | Call(Some t, m, args) ->
            let prefix, t = toOneLineArgs log scope [t]
            let t = List.head t

            if m.name.StartsWith "set_" then
                let v = List.head args |> toString log scope |> lines
                if v.Length = 1 then
                    sprintf "%s%s.%s <- %s" prefix t (m.name.Substring 4) v.[0]
                else
                    sprintf "%s%s.%s <-\r\n%s" prefix t (m.name.Substring 4) (indent v |> String.concat "\r\n")
                    
            elif m.name.StartsWith "get_" && args = [] then
                sprintf "%s%s.%s" prefix t (m.name.Substring 4)

            else
                let prefix2, args = args |> toOneLineArgs log scope
                sprintf "%s%s%s.%s(%s)" prefix prefix2 t m.name (String.concat ", " args)
            
        | Call(None, m, args) ->
            let qual = 
                match m.declaringType with
                | Choice1Of2 a ->
                    match Scope.relativeName scope a with
                    | Some n -> n + "."
                    | None -> ""
                | Choice2Of2 b ->
                    TypeRef.toString log scope b + "."

            if m.name.StartsWith "set_" then
                let v = List.head args |> toString log scope |> lines
                if v.Length = 1 then
                    sprintf "%s%s <- %s" qual (m.name.Substring 4) v.[0]
                else
                    sprintf "%s%s <-\r\n%s" qual (m.name.Substring 4) (indent v |> String.concat "\r\n")
                
            elif m.name.StartsWith "get_" && args = [] then
                sprintf "%s%s" qual (m.name.Substring 4)

            else
                let prefix, args = toOneLineArgs log scope args
                sprintf "%s%s%s(%s)" prefix qual m.name (String.concat ", " args)
                

        | Lambdas(args, body) ->
            let args = argDef log scope args

            let body =
                toString log scope body
                |> lines

            if body.Length = 1 then
                sprintf "fun %s -> %s" args body.[0]
            else
                sprintf "fun %s ->\r\n%s" args (indent body |> String.concat "\r\n")

        | Lambda _ -> failwith "unreachable"

        | Applications(lambda, args) ->
            let lambda = toString log scope lambda |> lines
            let prefix, args = toOneLineArgs log scope args

            if lambda.Length = 1 then
                sprintf "%s%s %s" prefix (bracketed lambda.[0]) (String.concat " " args)
            else
                sprintf "%s(%s\r\n%s) %s" prefix lambda.[0] (Array.skip 1 lambda |> indent |> String.concat "\r\n") (String.concat " " args)
                
        | Application _ -> failwith "unreachable"

        | Let(_, [v], e, b) ->
            let e = toString log scope e |> lines
            let b = toString log scope b

            let vp = 
                if v.IsMutable then "mutable "
                else ""

            if e.Length = 1 then
                sprintf "let %s%s = %s\r\n%s" vp v.Name e.[0] b
            else
                sprintf "let %s%s =\r\n%s\r\n%s" vp v.Name (String.concat "\r\n" (indent e)) b
                
        | Let(isStruct, vs, e, b) ->
            let e = toString log scope e |> lines
            let b = toString log scope b

            let vp = 
                if vs |> List.exists (fun v -> v.IsMutable) then "mutable "
                else ""

            let pattern =
                let els = vs |> Seq.map (fun v -> sprintf "%s : %s" v.Name (TypeRef.toString log scope v.Type)) |> String.concat ", "
                if isStruct then 
                    sprintf "struct(%s)" els
                else
                    sprintf "(%s)" els

            if e.Length = 1 then
                sprintf "let %s%s = %s\r\n%s" vp pattern e.[0] b
            else
                sprintf "let %s%s =\r\n%s\r\n%s" vp pattern (String.concat "\r\n" (indent e)) b

        | PropertyGet(t, p) ->
            let t = toString log scope t |> lines
            if t.Length = 1 then sprintf "%s.%s" (bracketed t.[0]) p.name
            else
                let v = new Var(sprintf "___%s" p.name, p.typ)
                sprintf "let %s =\r\n%s\r\n%s.%s" v.Name (String.concat "\r\n" (indent t)) v.Name p.name

        | VarSet (v, e) ->
            let e = toString log scope e |> lines

            if e.Length = 1 then
                sprintf "%s <- %s" v.Name e.[0]
            else
                sprintf "%s <-\r\n%s" v.Name (indent e |> String.concat "\r\n")
            
        | IfThenElse(c, i, Unit) ->
            let c = toString log scope c |> lines
            let i = toString log scope i |> lines

            if c.Length = 1 then    
                if i.Length = 1 then
                    sprintf "if %s then %s" c.[0] i.[0]
                else 
                    sprintf "if %s then\r\n%s" c.[0] (indent i |> String.concat "\r\n")

            else
                sprintf "if (\r\n%s) then\r\n%s" (indent c |> String.concat "\r\n") (indent i |> String.concat "\r\n")


        | IfThenElse(c, i, e) ->
            let c = toString log scope c |> lines
            let i = toString log scope i |> lines
            let e = toString log scope e |> lines

            if c.Length = 1 then    
                if i.Length = 1 && e.Length = 1 then
                    sprintf "if %s then %s else %s" c.[0] i.[0] e.[0]
                else 
                    sprintf "if %s then\r\n%s\r\nelse\r\n%s" c.[0] (indent i |> String.concat "\r\n") (indent e |> String.concat "\r\n")

            else
                sprintf "if (\r\n%s) then\r\n%s\r\nelse\r\n%s" (indent c |> String.concat "\r\n") (indent i |> String.concat "\r\n") (indent e |> String.concat "\r\n")

        | NewTuple(isStruct, args) ->
            let prefix, args = toOneLineArgs log scope args

            if isStruct then
                sprintf "%sstruct(%s)" prefix (String.concat ", " args)
            else
                sprintf "%s(%s)" prefix (String.concat ", " args)

        | RecordUpdate(record, prop, value) ->
            let prefix1, record = toOneLineArgs log scope [record]
            let prefix2, value = toOneLineArgs log scope [value]

            sprintf "%s%s{ %s with %s = %s }" prefix1 prefix2 (List.exactlyOne record) prop.name (List.exactlyOne value)



    let rec substitute (mapping : Var -> Option<Expr>) (e : Expr) =
        match e with
        | Unit -> e
        | Var v ->
            match mapping v with
            | Some e -> e
            | None -> e
        | Lambda(vs, b) ->  
            Lambda(vs, substitute mapping b)
        | NewTuple(s, f) ->
            NewTuple(s, f |> List.map (substitute mapping))
        | Application(a, b) ->
            Application(substitute mapping a, substitute mapping b)
        | Seq(a, b) ->
            Seq(substitute mapping a, substitute mapping b)
        | Upcast(e,t) ->
            Upcast(substitute mapping e, t)
        | Call(t, m, args) ->
            let t = t |> Option.map (substitute mapping)
            let args = args |> List.map (substitute mapping)
            Call(t, m, args)
        | Fail _ ->
            e
        | Let(s, vs, e, b) ->
            Let(s, vs, substitute mapping e, substitute mapping b)
        | PropertyGet(e, p) ->
            PropertyGet(substitute mapping e, p)
        | VarSet(v, e) ->
            VarSet(v, substitute mapping e)
        | IfThenElse(c,i,e) ->
            IfThenElse(substitute mapping c, substitute mapping i, substitute mapping e)
        | Unbox(t, e) ->
            Unbox(t, substitute mapping e)
        | Ignore(e) ->
            Ignore(substitute mapping e)
        | And(es) ->
            And(es |> List.map (substitute mapping))
        | Match(e, pats) ->
            Match(substitute mapping e, pats |> List.map (fun (h,b) -> h, substitute mapping b))
        | RecordUpdate(r,p,v) ->
            RecordUpdate(substitute mapping r, p, substitute mapping v)










