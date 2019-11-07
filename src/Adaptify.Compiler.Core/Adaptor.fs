namespace Adaptify.Compiler


open System
open FSharp.Compiler.SourceCodeServices
open Adaptify.Compiler

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
module TypePatterns =
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

    let rec get (mutableScope : bool) (typ : TypeRef) : Adaptor =
        match typ with
        | TVar v ->
            if mutableScope then varPrimitive v
            else varModel v

        | Option (PlainValue t) ->
            if mutableScope then identity typ
            else aval t

        | Option t ->
            let a = get mutableScope t
            if mutableScope then failwith ""
            else optionModel a

        | HashSet (PlainValue t) ->
            asetPrimitive t

        | HashSet _t ->
            failwith "HashSet<ModelType> not implemented!"

        | IndexList (PlainValue t) ->
            alistPrimitive t

        | IndexList t ->
            let a = get true t
            alistModel a

        | HashMap (k, PlainValue v) ->
            amapPrimitive k v

        | HashMap(k, v) ->
            let a = get true v
            amapModel k a

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
                let newName = "Adaptive" + name
                let newScope = adaptorScope scope
                
                let adaptors =
                    targs |> List.map (fun a ->
                        let pat = get true a
                        let at = get false a
                        pat, at
                    )

                let tAdaptiveArgs = 
                    adaptors |> List.collect (fun (pa, a) -> 
                        let pat = pa.aType
                        let at = a.aType
                        [a.vType; pat; at]
                    )
                    
                let mt = TExtRef(newScope, sprintf "Adaptive%sCase" name, tAdaptiveArgs)
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
                let newName = "Adaptive" + name 
                let newScope = adaptorScope scope

                let adaptors =
                    targs |> List.map (fun a ->
                        let pat = get true a
                        let at = get false a
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
            let adaptors = els |> List.map (get false)

            tuple isStruct adaptors


        | _ -> 
            if mutableScope then identity typ
            else aval typ


[<RequireQualifiedAccess>]
type TypeKind =
    | Class
    | Interface
    | Module

type TypeDefinition =
    {
        kind        : TypeKind
        priv        : bool
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
    let toString (d : TypeDefinition) =
   
        let tpars =
            match d.tpars with
            | [] -> ""
            | ts -> ts |> Seq.map string |> String.concat ", " |> sprintf "<%s>"
        let priv =
            if d.priv then "private "
            else ""

        if d.kind = TypeKind.Interface then
            [|
                yield sprintf "type %s%s%s =" priv d.name tpars 
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

        elif d.kind = TypeKind.Module then
            [|
                yield sprintf "[<AutoOpen>]"
                yield sprintf "module %s%s = " priv d.name
                for (name, args, b) in d.statics do
                    let typ = b.Type
                    let args = args |> List.map (fun a -> sprintf "(%s : %s)" a.Name (TypeRef.toString d.scope a.Type))
                    yield sprintf "    let %s %s =" name (String.concat " " args) 
                    yield! Expr.toString d.scope b |> lines |> indent |> indent
            |]


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
                    yield sprintf "type %s%s%s(%s) =" priv d.name tpars ctorArgs
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
                
            code

    let private adaptorScope (o : Scope) =
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

    let rec private isValueType (t : FSharpEntity) =
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

    let private eq (a : Expr) (b : Expr) = 
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

    let private productType (args : list<Var>) (tpars : list<TypeVar>) (s : Scope) (n : string) (props : list<Prop * Expr>) =

        let adaptors = 
            props |> List.map (fun (p,b) -> new Var("_" + p.name + "_", p.typ), p, b, Adaptor.get false p.typ)

        let newScope = 
            adaptorScope s

        let newName = 
            "Adaptive" + n
                        

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
            kind        = TypeKind.Class
            priv        = false
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
                "Adaptive" + name 
                    
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


            let ctorTypeRef = TExtRef(newScope, sprintf "Adaptive%sCase" name, List.map TVar tAdaptivePars)
            let valueType = TExtRef(scope, name, List.map TVar tpars)

            let ctorType =
                {
                    kind            = TypeKind.Interface
                    priv            = false
                    baseType        = None
                    scope           = newScope
                    name            = sprintf "Adaptive%sCase" name
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
                        let pat = Adaptor.get true (TVar a)
                        let at = Adaptor.get false (TVar a)
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
                        name            = sprintf "Adaptive%s%s" name caseName
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
                        priv = true
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

                let selfType = TExtRef(newScope, sprintf "Adaptive%s" name, List.map TVar tAdaptivePars)

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
                    kind            = TypeKind.Class
                    priv            = false
                    baseType        = Some AdaptiveObject.typ
                    scope           = newScope
                    name            = sprintf "Adaptive%s" name
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
                    


            let patterns =

                let patternName =  cases |> List.map (fun (n,_) -> sprintf "Adaptive%s" n) |> String.concat "|" |> sprintf "(|%s|)"

                let value = new Var("value", ctorTypeRef)

                {
                    kind            = TypeKind.Module
                    priv            = false
                    baseType        = None
                    scope           = newScope
                    name            = sprintf "Adaptive%s" name
                    tpars           = []
                    ctorArgs        = []
                    ctor            = Expr.Unit
                    members = []
                    interfaces = []
                            
                    statics =
                        [
                            patternName, [value], 
                                Expr.Match(Var value, [
                                    for (c, props) in cases do
                                        let n = TExtRef(newScope, sprintf "Adaptive%s%s" name c, List.map TVar tAdaptivePars)
                                        let ctor = 
                                            {
                                                isStatic = true
                                                declaringType = Choice1Of2 newScope
                                                name = sprintf "Adaptive%s" c
                                                parameters = props |> List.map (fun p -> p.typ)
                                                returnType = TTuple(false, [])
                                            }
                                        let self = new Var(c, n)
                                        let pat = TypeTest(n, self)
                                        match props with
                                        | [] ->
                                            yield pat, Expr.Var(new Var(sprintf "Adaptive%s" c, TTuple(false, [])))

                                        | _ ->
                                            let body = Call(None, ctor, props |> List.map (fun p -> Expr.PropertyGet(Var self, p)))
                                            yield pat, body

                                    yield Any, Fail(TTuple(false, []), "unreachable")
                                ])
                        ]

                }




            [ctorType] @ caseTypes @ [adaptiveType; patterns]
         



    let private defaultOpens =
        [|
            "open System"
            "open FSharp.Data.Adaptive"
            "open Adaptify"
        |]

    let rec private wrap (s : Scope) (str : string[]) =
        match s with
        | Global ->  
            sprintf "namespace rec global\r\n\r\n%s" (String.concat "\r\n" (Array.append defaultOpens str))
                    
        | Namespace ns ->
            sprintf "namespace rec %s\r\n\r\n%s" ns (String.concat "\r\n" (Array.append defaultOpens str))

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
                        yield sprintf "module rec %s =" name
                        yield! indent str
                    |]
                wrap parent code
            | atts ->
                let atts = atts |> String.concat "; " |> sprintf "[<%s>]" 
                let code =
                    [|
                        yield atts
                        yield sprintf "module rec %s =" name
                        yield! indent str
                    |]
                wrap parent code
                    
    let toFile (all : list<TypeDefinition>) =
        String.concat "\r\n" [
            yield "#nowarn \"49\" // upper case patterns"
            yield "#nowarn \"66\" // upcast is unncecessary"


            let groups = all |> List.groupBy (fun d -> d.scope)

            for (scope, ts) in groups do 
                let code = ts |> Seq.map toString |> Array.concat
                let str = wrap scope code
                yield str
                yield ""
                yield ""
        ]



//module Adaptify =
//    let private defaultOpens =
//        [|
//            "open System"
//            "open FSharp.Data.Adaptive"
//            "open Adaptify"
//        |]

//    let rec private wrap (s : Scope) (str : string[]) =
//        match s with
//        | Global ->
                    
//            sprintf "namespace rec global\r\n\r\n%s" (String.concat "\r\n" (Array.append defaultOpens str))
                    
//        | Namespace ns ->
//            sprintf "namespace rec %s\r\n\r\n%s" ns (String.concat "\r\n" (Array.append defaultOpens str))

//        | Module(parent, name, autoOpen, moduleSuffix) ->

//            let attrs =
//                [
//                    if autoOpen then "AutoOpen"
//                    if moduleSuffix then "CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)"
//                ]

//            match attrs with
//            | [] ->
//                let code =
//                    [|
//                        yield sprintf "module rec %s =" name
//                        yield! indent str
//                    |]
//                wrap parent code
//            | atts ->
//                let atts = atts |> String.concat "; " |> sprintf "[<%s>]" 
//                let code =
//                    [|
//                        yield atts
//                        yield sprintf "module rec %s =" name
//                        yield! indent str
//                    |]
//                wrap parent code
                   
//    let test (defs : list<TypeDef>) =
//        let all = 
//            String.concat "\r\n" [

//                let all = defs |> List.collect (TypeDefinition.ofTypeDef [])
//                yield "#nowarn \"49\" // upper case patterns"
//                yield "#nowarn \"66\" // upcast is unncecessary"


//                let groups = all |> List.groupBy (fun d -> d.scope)

//                for (scope, ts) in groups do 
//                    let code = ts |> Seq.mapi( fun i t -> TypeDefinition.toString t) |> Array.concat
//                    let str = wrap scope code
//                    yield str
//                    yield ""
//                    yield ""
//                    yield ""
//            ]
//        System.IO.File.WriteAllText(@"C:\Users\Schorsch\Development\Adaptify\src\Examples\NetCore\Test.fs", all)
