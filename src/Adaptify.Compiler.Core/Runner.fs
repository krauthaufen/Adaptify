namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.Range
open Aardvark.Compiler
open FSharp.Compiler.SourceCodeServices

module Adaptify =
    let private modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

    let private waitUntilExisting (log : ILog) (timeout : int) (files : seq<string>) =
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let rec run (iter : int) (files : string[]) = 
            if files.Length = 0 then
                [||]
            elif iter = 0 || (iter < 50 && sw.Elapsed.TotalMilliseconds <= float timeout) then
                if iter > 0 then log.debug range0 "%d references missing -> retry" files.Length
                let remaining = 
                    files |> Array.filter (fun f ->
                        if File.Exists f then false
                        else true
                    )

                if remaining.Length > 0 then
                    Threading.Thread.Sleep 32
                    run (iter + 1) remaining
                else
                    [||]
            else
                if iter > 0 then 
                    log.debug range0 "%d references missing" files.Length
                    for i,f in Seq.indexed files do
                        log.debug range0 "    %d: %s" i f
                files

        run 0 (Seq.toArray files)

    let private toProjectOptions (projectInfo : ProjectInfo) =
        let otherOptions =
            [|
                yield! projectInfo.additional

                for r in projectInfo.references do
                    yield sprintf "-r:%s" r
            |]
        {
            LoadTime = DateTime.Now
            ProjectFileName = projectInfo.project
            ProjectId = None
            ExtraProjectInfo = None
            SourceFiles = List.toArray projectInfo.files
            OtherOptions = otherOptions
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = true
            UseScriptResolutionRules = false
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }

    let getReplacementCode (log : ILog) (createLenses : bool) (res : FSharpCheckFileResults) (code : string) =

        let errs, wrns = res.Errors |> Array.partition (fun err -> err.Severity = FSharpErrorSeverity.Error)
        if errs.Length > 0 then
            let errorStrings =
                errs |> Seq.map (fun err ->
                    sprintf "  (%d,%d): %s" err.StartLineAlternate err.StartColumn err.Message
                )
                |> String.concat "\r\n"

            let range = mkRange "internal" pos0 pos0
            log.warn range "internal" "compiler errors:\r\n%s" errorStrings

        for err in wrns do
            let p0 = mkPos err.StartLineAlternate err.StartColumn
            let p1 = mkPos err.EndLineAlternate err.EndColumn
            let range = mkRange err.FileName p0 p1
            log.warn range (sprintf "%04d" err.ErrorNumber) "%s" err.Message

        let rec allEntities (d : FSharpImplementationFileDeclaration) =
            match d with
            | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                e :: List.collect allEntities ds
            | _ ->
                []

        let entities = 
            match res.ImplementationFile with
            | Some impl ->
                impl.Declarations
                |> Seq.toList
                |> List.collect allEntities
            | None ->
                []

        let definitions =   
            entities 
            |> List.choose (TypeDef.ofEntity log)
            |> List.map (fun l -> l.Value)
            |> List.collect (TypeDefinition.ofTypeDef log createLenses [])

        match definitions with
        | [] ->
            code
        | _ -> 
            let defs = definitions |> List.toArray |> Array.collect (TypeDefinition.toString log)


            let rx = System.Text.RegularExpressions.Regex @"^([ \t\r\n]*)(namespace|module)[ \t\r\n]+(rec[ \t\r\n]+)?([^\r\n]+)"

            let nowarns =
                String.concat "\r\n" [
                    yield "#nowarn \"49\" // upper case patterns"
                    yield "#nowarn \"66\" // upcast is unncecessary"
                    yield "#nowarn \"1337\" // internal types"
                ]
            let m = rx.Match code
            if m.Success then
                let _isRec = m.Groups.[3].Success
                sprintf "%s%s rec %s\r\n%s%s\r\n%s" 
                    m.Groups.[1].Value 
                    m.Groups.[2].Value 
                    m.Groups.[4].Value
                    (nowarns + "\r\n#line 2")
                    (code.Substring(m.Index + m.Length))
                    (String.concat "\r\n" defs)
            else
                String.concat "\r\n" [
                    nowarns
                    "[<AutoOpen>]"
                    "module rec Hans ="
                    "#line 1"
                    indentStr code
                    yield! indent defs
                ]
    open Humanizer
    
    
    let rec getScope (d : TypeDef) =
        match d with
        | TypeDef.ProductType(_,_,_,s,_,_) -> s
        | TypeDef.Generic(_, t) -> getScope t
        | TypeDef.Union(_,s,_,_,_) -> s


    let rec deltaType (t : TypeRef) =
        match t with
        | TModel(_, def, targs) ->
            let def = def.Value
            TExtRef(getScope def, sprintf "%sDelta" def.Name, targs) 
            |> List.typ 
            |> Some

        | HashSet t ->
            HashSetDelta.typ t |> Some

        | IndexList t ->
            match t with
            | TModel _ ->
                match deltaType t with
                | Some deltaType ->
                    IndexList.typ (ElementDelta.typ t deltaType) |> Some
                | None ->
                    IndexListDelta.typ t |> Some
            | _ ->
                IndexListDelta.typ t |> Some

        | HashMap(k, v) ->
            match v with
            | TModel _ ->
                match deltaType v with
                | Some deltaType ->
                    HashMap.typ k (ElementDelta.typ v deltaType) |> Some
                | None ->
                    HashMapDelta.typ k v |> Some
            | _ ->
                HashMapDelta.typ k v |> Some


        | _ ->
            None

    let newValues = false

    let rec propUpdateCase (scope : Scope) (p : Prop) =
        let inline tstr t = TypeRef.toString Log.empty scope t
        let name = p.name.Pascalize()
        let valueName = name.Camelize()
        
        match p.typ with
        | TTuple(_, elems) ->
            elems |> List.indexed |> List.collect (fun (i, t) ->
                propUpdateCase scope { p with name = sprintf "%s%d" p.name i; typ = t}
            )
        | _ -> 
            match deltaType p.typ with
            | Some deltaType ->
                [
                    sprintf "Update%s" name, [
                        if newValues then
                            {
                                Prop.name = "newValue"
                                Prop.typ = p.typ
                                Prop.isRecordField = false
                                Prop.mode = AdaptifyMode.Default
                                Prop.range = range0
                            }
                        {
                            Prop.name = "delta"
                            Prop.typ = deltaType
                            Prop.isRecordField = false
                            Prop.mode = AdaptifyMode.Default
                            Prop.range = range0
                        }
                    ]
                ]
            | None ->
                [
                    sprintf "Set%s" name, [
                        {
                            Prop.name = valueName
                            Prop.typ = p.typ
                            Prop.isRecordField = false
                            Prop.mode = AdaptifyMode.Default
                            Prop.range = range0
                        }
                    ]
                ]

    let rec operationType (tpars : list<TypeVar>) (t : TypeDef) =
        let rec cases (t : TypeDef) =
            match t with
            | ProductType _ -> []
            | Union(_,_,_,_,cases) -> cases
            | Generic(_, t) -> cases t

        match t with
        | ProductType(_, range, isValueType, scope, name, properties) ->
            [
                TypeDef.Union(range0, scope, sprintf "%sDelta" name, [], 
                    properties |> List.collect (fun p -> propUpdateCase scope p)
                )
            ]
        | TypeDef.Union(range, scope, typeName, _props, cases) ->
            [
                let singleCase = System.Collections.Generic.List<_>()
                let others = System.Collections.Generic.List()
                for name, props in cases do 
                    match props with
                    | [_] ->
                        singleCase.Add(name, props)
                    | _ -> 
                        others.Add(name, props)
                        let def = TypeDef.ProductType(false, range0, false, scope, sprintf "%s%s" typeName name, props)
                        let ops = operationType tpars def
                        yield! ops

                TypeDef.Union(
                    range0, scope, sprintf "%sDelta" typeName, [], [

                        for name, props in singleCase do    
                            let s = List.head props
                            match s.typ with
                            | TModel _ ->
                                let d = deltaType s.typ |> Option.get
                                yield sprintf "Update%s" name, [
                                    if newValues then
                                        {
                                            Prop.name = "newValue"
                                            Prop.typ = s.typ
                                            range = range0
                                            mode = AdaptifyMode.Default
                                            isRecordField = false
                                        }
                                    {
                                        Prop.name = "delta"
                                        Prop.typ = d
                                        range = range0
                                        mode = AdaptifyMode.Default
                                        isRecordField = false
                                    }
                                ]
                            | _ ->      
                                yield sprintf "Set%s" name, [
                                    {
                                        Prop.name = "value"
                                        Prop.typ = s.typ
                                        range = range0
                                        mode = AdaptifyMode.Default
                                        isRecordField = false
                                    }
                                ]

                        for name, props in others do
                           
                            yield sprintf "Update%s" name, [
                                if newValues then
                                    yield {
                                        Prop.name = "newValue"
                                        Prop.typ = TTuple(false, props |> List.map (fun p -> p.typ))
                                        range = range0
                                        mode = AdaptifyMode.Default
                                        isRecordField = false
                                    }
                                yield {
                                    Prop.name = "delta"
                                    Prop.typ = List.typ (TExtRef(scope, sprintf "%s%sDelta" typeName name, tpars |> List.map TVar))
                                    range = range0
                                    mode = AdaptifyMode.Default
                                    isRecordField = false
                                }
                            ]
                            
                            yield sprintf "Set%s" name, props
                    ])
            ]
        | TypeDef.Generic(tpars, t) ->  
            operationType tpars t |> List.map (fun t -> TypeDef.Generic(tpars, t))

    let isEmptyExpr (e : Expr) =
        let typ = e.Type
        match typ with
        | Option et ->
            let v = new Var("value", et)
            Expr.Match(e, 
                [
                    Pattern.UnionCaseTest(typ, "Some", [v]), Expr.Boolean true
                    Pattern.Any, Expr.Boolean false
                ]
            )
        | List et ->
            Expr.Call(None, List.isEmptyMeth et, [e])
        | HashSetDelta et ->
            Expr.Call(None, HashSetDelta.isEmptyMeth et, [e])
        | HashMap(k, v) ->
            Expr.Call(None, HashMap.isEmptyMeth k v, [e])
        //| HashMapDelta
        | _ ->
            // TODO
            Expr.Boolean false

    let rec diff (a : Var) (b : Var) (t : TypeDef) =
        
        let tDelta = deltaType (TypeRef.TModel(range0, lazy t, [])) |> Option.get
        let selfDelta = TExtRef(Global, sprintf "%sDelta" t.Name, [])

        match t with
        | ProductType(_, range, isValueType, scope, name, properties) ->
            
            let deltas =
                properties |> List.map (fun p ->
                    let pa = Expr.PropertyGet(Expr.Var a, p)
                    let pb = Expr.PropertyGet(Expr.Var b, p)

                    match p.typ with
                    | TModel(_,def,targs) ->
                        let n = def.Value.Name
                        let s = getScope def.Value
                        let tPropDelta = deltaType p.typ |> Option.get

                        let newCase =
                            {
                                Method.declaringType = Choice2Of2 selfDelta
                                Method.isStatic = true
                                Method.name = sprintf "Update%s" (p.name.Pascalize())
                                Method.parameters = 
                                    if newValues then [ p.typ; tPropDelta ]
                                    else [ tPropDelta ]
                                Method.returnType = deltaType p.typ |> Option.get
                            }

                        let retType = deltaType p.typ |> Option.get
                        let meth =
                            {
                                Method.declaringType = Choice1Of2 (Module(scope, n, false, false))
                                Method.isStatic = true
                                Method.name = "get_computeDelta"
                                Method.parameters = [ ]
                                Method.returnType = TFunc(p.typ, TFunc(p.typ, retType))
                            }
                        let delta = new Var("delta", retType)
                        Expr.IfThenElse(
                            Expr.Call(None, notMeth, [Expr.Call(None, shallowEquals p.typ, [pa; pb])]),
                            Expr.Let(false, [delta], Expr.Application(Expr.Application(Expr.Call(None, meth, []), pa), pb), 
                                Expr.IfThenElse(
                                    Expr.Call(None, notMeth, [isEmptyExpr (Expr.Var delta)]),
                                    Expr.Call(None, newCase, 
                                        if newValues then [pb; Expr.Var delta]
                                        else [ Expr.Var delta ]
                                    ),
                                    Expr.Unit
                                )
                            ),
                            Expr.Unit
                        )

                    | HashSet t ->
                        let tPropDelta = deltaType p.typ |> Option.get

                        let newCase =
                            {
                                Method.declaringType = Choice2Of2 selfDelta
                                Method.isStatic = true
                                Method.name = sprintf "Update%s" (p.name.Pascalize())
                                Method.parameters = 
                                    if newValues then [ p.typ; tPropDelta ]
                                    else [tPropDelta]
                                Method.returnType = deltaType p.typ |> Option.get
                            }

                        let retType = HashSetDelta.typ t
                        let delta = new Var("delta", retType)
                        Expr.IfThenElse(
                            Expr.Call(None, notMeth, [Expr.Call(None, shallowEquals p.typ, [pa; pb])]),
                            Expr.Let(false, [delta], HashSet.computeDeltaExpr pa pb, 
                                Expr.IfThenElse(
                                    Expr.Call(None, notMeth, [isEmptyExpr (Expr.Var delta)]),
                                    Expr.Call(None, newCase, 
                                        if newValues then [pb; Expr.Var delta]
                                        else [ Expr.Var delta ]
                                    ),
                                    Expr.Unit
                                )
                            ),
                            Expr.Unit
                        )
                        
                    | HashMap(k, (TModel(_, def, _) as v)) ->
                        let allDelta = deltaType p.typ |> Option.get
                        let delta = deltaType v |> Option.get
                        let vd = new Var("delta", HashMap.typ k (ElementOperation.typ v))

                        let computeValueDelta =
                            {
                                Method.declaringType = Choice1Of2 (Module(scope, def.Value.Name, false, false))
                                Method.isStatic = true
                                Method.name = "get_computeDelta"
                                Method.parameters = [ ]
                                Method.returnType = TFunc(v, TFunc(v, delta))
                            }
                            
                        let newCase =
                            {
                                Method.declaringType = Choice2Of2 selfDelta
                                Method.isStatic = true
                                Method.name = sprintf "Update%s" (p.name.Pascalize())
                                Method.parameters = 
                                    if newValues then [ p.typ; HashMap.typ k (ElementDelta.typ v delta) ]
                                    else [ HashMap.typ k (ElementDelta.typ v delta) ]
                                Method.returnType = allDelta
                            }

                        let vv = new Var("vv", newCase.returnType)

                        let getInnerDelta =
                            let someMeth =
                                {
                                    declaringType = Choice1Of2(Module(Scope.Namespace("Microsoft.FSharp.Core"), "Option", false, false))
                                    name = "Some"
                                    parameters = []
                                    isStatic = true
                                    returnType = Option.typ allDelta
                                }
                                
                            let noneMeth =
                                {
                                    declaringType = Choice1Of2(Module(Scope.Namespace("Microsoft.FSharp.Core"), "Option", false, false))
                                    name = "get_None"
                                    parameters = []
                                    isStatic = true
                                    returnType = Option.typ allDelta
                                }
                            let a = new Var("a", v)
                            let b = new Var("b", v)
                            let res = new Var("result", delta)
                            Expr.Lambda([a],
                                Expr.Lambda([b],
                                    Expr.Let(
                                        false, [res],
                                        Expr.Application(
                                            Expr.Application(
                                                Expr.Call(None, computeValueDelta, []),
                                                Expr.Var a
                                            ),
                                            Expr.Var b
                                        ),
                                        Expr.IfThenElse(
                                            Expr.Call(None, notMeth, [isEmptyExpr (Expr.Var res)]),
                                            Expr.Call(None, someMeth, [Expr.Var res]),
                                            Expr.Call(None, noneMeth, [])
                                        )
                                    )
                                )
                            )

                        Expr.IfThenElse(
                            Expr.Call(None, notMeth, [Expr.Call(None, shallowEquals p.typ, [pa; pb])]),
                            Expr.Let(
                                false, [vd], Expr.Call(None, HashMapDelta.toHashMapMeth k v, [HashMap.computeDeltaExpr pa pb]),
                                
                                Expr.Let(
                                    false, [vv], HashMap.getElementDeltasExpr pa (Expr.Var vd) getInnerDelta,
                                    
                                    Expr.IfThenElse(
                                        Expr.Call(None, notMeth, [isEmptyExpr (Expr.Var vv)]),
                                        Expr.Call(None, newCase, 
                                            if newValues then [pb; Expr.Var vv]
                                            else [ Expr.Var vv ]
                                        ),
                                        Expr.Unit
                                    )
                                )
                                //Expr.Call(
                                //    None, HashMap.applyOperationsMeth k v delta,
                                //    [
                                //        pa; Expr.Var vd
                                //        Expr.Call(None, computeValueDelta, [])
                                //    ]
                                //)
                            ),
                            Expr.Unit
                        )
                    
                    | HashMap(k, v) ->
                        let retType = HashMapDelta.typ k v

                        let newCase =
                            {
                                Method.declaringType = Choice2Of2 selfDelta
                                Method.isStatic = true
                                Method.name = sprintf "Update%s" (p.name.Pascalize())
                                Method.parameters = 
                                    if newValues then [ p.typ; retType ]
                                    else [retType]
                                Method.returnType = deltaType p.typ |> Option.get
                            }

                        let delta = new Var("delta", retType)
                        Expr.IfThenElse(
                            Expr.Call(None, notMeth, [Expr.Call(None, shallowEquals p.typ, [pa; pb])]),
                            Expr.Let(false, [delta], HashMap.computeDeltaExpr pa pb, 
                                Expr.IfThenElse(
                                    Expr.Call(None, notMeth, [isEmptyExpr (Expr.Var delta)]),
                                    Expr.Call(None, newCase, 
                                        if newValues then [pb; Expr.Var delta]
                                        else [ Expr.Var delta ]
                                    ),
                                    Expr.Unit
                                )
                            ),
                            Expr.Unit
                        )

                    | _ ->
                        
                        let newCase =
                            {
                                Method.declaringType = Choice2Of2 selfDelta
                                Method.isStatic = true
                                Method.name = sprintf "Set%s" (p.name.Pascalize())
                                Method.parameters = [ p.typ ]
                                Method.returnType = selfDelta
                            }
                        Expr.IfThenElse(
                            Expr.Call(None, notMeth, [Expr.Call(None, defaultEquals pa.Type, [pa; pb])]),
                            Expr.Call(None, newCase, [pb]),
                            Expr.Unit
                        )
                )

            Expr.NewList deltas

        | Union(range, scope, name, _, cases) ->

            let setCtors =
                cases |> List.map (fun (name, props) ->
                    let meth =
                        {
                            Method.declaringType = Choice2Of2 selfDelta
                            Method.isStatic = true
                            Method.name = sprintf "Set%s" (name.Pascalize())
                            Method.parameters = props |> List.map (fun p -> p.typ)
                            Method.returnType = selfDelta
                        }

                    let create (values : list<Expr>) =
                        Expr.Call(None, meth, values)

                    name, create
                )
                |> Map.ofList

            Expr.Match(
                Expr.Var b,
                [
                    for caseName, props in cases do
                        let ba = props |> List.map (fun p -> new Var("b" + p.name, p.typ))
                        let aa = props |> List.map (fun p -> new Var("a" + p.name, p.typ))
                        
                       
                        let changed =
                            match props with
                            | [_] ->
                                Expr.NewList [
                                    setCtors.[caseName] (ba |> List.map Expr.Var)
                                ]
                            | _ ->

                                let tCaseDelta =
                                    TExtRef(scope, sprintf "%s%sDelta" name caseName, [])

                                let newCase =
                                    {
                                        Method.declaringType = Choice2Of2 selfDelta
                                        Method.isStatic = true
                                        Method.name = sprintf "Update%s" (caseName.Pascalize())
                                        Method.parameters = 
                                            if newValues then [ TTuple(false, props |> List.map (fun p -> p.typ)); List.typ tCaseDelta]
                                            else [ List.typ tCaseDelta ]
                                        Method.returnType = selfDelta
                                    }
                                    
                                let res = new Var("result", List.typ tCaseDelta)

                                let inner =
                                    Expr.NewList (
                                        (props, aa, ba) |||> List.map3 (fun p a b ->

                                            let updateCaseCtor =
                                                {
                                                    Method.declaringType = Choice2Of2 tCaseDelta
                                                    Method.isStatic = true
                                                    Method.name = sprintf "Set%s" (p.name.Pascalize())
                                                    Method.parameters = [ a.Type ]
                                                    Method.returnType = tCaseDelta
                                                }
                                                

                                            Expr.IfThenElse(
                                                Expr.Call(None, notMeth, [Expr.Call(None, shallowEquals a.Type, [Expr.Var a; Expr.Var b])]),
                                                Expr.Call(None, updateCaseCtor, [Expr.Var b]),
                                                Expr.Unit
                                            )
                                        )
                                    )

                                Expr.Let(
                                    false, [res], inner,
                                    Expr.IfThenElse(
                                        isEmptyExpr (Expr.Var res), Expr.EmptyList selfDelta, 
                                        Expr.NewList [
                                            Expr.Call(
                                                None, newCase,
                                                [
                                                    if newValues then Expr.NewTuple(false, ba |> List.map Expr.Var)
                                                    Expr.Var res
                                                ]
                                            )
                                        ]
                                    )
                                )

                        let code =
                            let allEqual = 
                                (aa, ba) ||> List.map2 (fun a b -> 
                                    Expr.Call(None, shallowEquals a.Type, [Expr.Var a; Expr.Var b])
                                ) |> Expr.And

                            Expr.IfThenElse(
                                Expr.Call(None, notMeth, [allEqual]),
                                changed,
                                Expr.EmptyList selfDelta
                            )

                        let body =
                            Expr.Match(
                                Expr.Var a,
                                [
                                    Pattern.UnionCaseTest(a.Type, caseName, aa), code
                                    Pattern.Any, Expr.NewList [setCtors.[caseName] (List.map Expr.Var ba)]
                                    //for ca, props in cases do
                                    //    if ca <> caseName then
                                    //        let ctor = setCtors.[caseNam]
                                    //        let va = props |> List.map (fun p -> new Var(p.name, p.typ))
                                    //        Pattern.UnionCaseTest(a.Type, ca, va), Expr.NewList [ctor (va |> List.map Expr.Var)]
                                ]
                            )
                        
                        Pattern.UnionCaseTest(b.Type, caseName, ba), body
                ]
            )

        | _ ->
            Expr.Fail(List.typ tDelta, "not implemented")

    let generateDeltaTypes (checker : FSharpChecker) (outputPath : string) (designTime : bool) (useCache : bool) (createLenses : bool) (log : ILog) (projectInfo : ProjectInfo) =
        async {
            do! Async.SwitchToThreadPool()
            let projectInfo = ProjectInfo.normalize projectInfo
            let options = toProjectOptions projectInfo
            
            for file in projectInfo.files do
                let content = File.ReadAllText file
                let mayDefineModelTypes = modelTypeRx.IsMatch content
                if mayDefineModelTypes then
                    let text = FSharp.Compiler.Text.SourceText.ofString content
                    let! (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options)
        
                    match answer with
                    | FSharpCheckFileAnswer.Succeeded res -> 
                        let rec allEntities (d : FSharpImplementationFileDeclaration) =
                            match d with
                            | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                                e :: List.collect allEntities ds
                            | _ ->
                                []

                        let entities = 
                            res.ImplementationFile.Value.Declarations
                            |> Seq.toList
                            |> List.collect allEntities
                                        
                        let definitions =   
                            entities 
                            |> List.choose (TypeDef.ofEntity log)
                            |> List.choose (fun l -> 
                                try 
                                    l.Value |> Some
                                with e -> 
                                    log.error range0 "1337" "[Adaptify] could not get type entity:%s" e.Message
                                    None
                                )

                        let defs =
                            definitions |> List.groupBy getScope


                        for scope, definitions in defs do
                            let rec toList (scope : Scope) =
                                match scope with
                                | Scope.Global -> 
                                    true,[]
                                | Scope.Namespace ns -> 
                                    true, [ns]
                                | Scope.Module(parent, name,_, _) ->
                                    let _, p = toList parent 
                                    false, p @ [name]

                            let isNamespace, parts =
                                toList scope

                            match parts with
                            | [] -> printfn "namespace rec global"
                            | ps -> 
                                if isNamespace then printfn "namespace rec %s" (String.concat "." ps)
                                else printfn "module rec %s" (String.concat "." ps)
                            printfn ""

                            let defs = definitions |> List.collect (operationType [])
                            
                            let inline tstr t = TypeRef.toString Log.empty scope t

                            let rec printDef (tpars : list<TypeVar>) (def : TypeDef) =
                                match def with
                                | ProductType _ ->
                                    failwith "unexpected"
                                | Union(_, _, name, _, cases) ->
                                    match tpars with
                                    | [] -> 
                                        printfn "type %s =" name
                                    | pars ->
                                        let pars = pars |> List.map (fun v -> sprintf "'%s" v.Name) |> String.concat ", "
                                        printfn "type %s<%s> =" name pars

                                    for name, args in cases do
                                        let args = args |> List.map (fun p -> sprintf "%s : %s" p.name (tstr p.typ)) |> String.concat " * "
                                        printfn "    | %s of %s" name (args)
                                | Generic(tpars, t) ->
                                    printDef tpars t

                            for def in defs do   
                                printDef [] def


                            for def in definitions do
                                printfn "module %s =" def.Name

                                let t = TModel(range0, lazy def, [])
                                let a = new Var("a", t)
                                let b = new Var("b", t)

                                let code = diff a b def
                                let string =
                                    Expr.toString Log.empty Global code
                                    |> lines

                                printfn "    let computeDelta (a : %s) (b : %s) =" (tstr t) (tstr t)
                                for l in string do
                                    printfn "        %s" l

                            

                        ()
                    | _ ->
                        failwith "bad"
        }

    let runAsync (checker : FSharpChecker) (outputPath : string) (designTime : bool) (useCache : bool) (createLenses : bool) (log : ILog) (projectInfo : ProjectInfo) =
        async {
            do! Async.SwitchToThreadPool()
            let projectInfo = ProjectInfo.normalize projectInfo

            let hash = ProjectInfo.computeHash projectInfo
            let projectFile = projectInfo.project
            let projDir = Path.GetDirectoryName projectFile
            let outputDirectory = 
                let dir = Path.Combine(Path.GetTempPath(), hash)
                File.ensureDirectory dir
                dir

            let relativePath (name : string) =
                let dirFull = Path.GetFullPath projDir
                let nameFull = Path.GetFullPath name
                if nameFull.StartsWith dirFull then
                    nameFull.Substring(dirFull.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
                else
                    name

            let getOutputFile (file : string) =
                let rel = relativePath file
                let path = Path.ChangeExtension(Path.Combine(outputDirectory, rel), ".g.fs")
                File.ensureDirectory path
                path



            if Path.GetExtension projectFile = ".fsproj" then
            
                let realFiles = projectInfo.files

                let inFiles =
                    let rec appendGenerated (f : list<string>) =
                        match f with
                        | [] -> []
                        | [s] when projectInfo.target = Target.Exe -> [s]
                        | [s] -> 
                            if s.EndsWith ".g.fs" then 
                                [s]
                            else
                                let content = File.ReadAllText s
                                let mayDefineModelTypes = modelTypeRx.IsMatch content
                                if mayDefineModelTypes then [s; getOutputFile s]
                                else [s]
                        | h :: t ->
                            match t with
                            | hh :: t when hh = getOutputFile h ->
                                h :: hh :: appendGenerated t
                            | _ ->
                                
                                let content = File.ReadAllText h
                                let mayDefineModelTypes = modelTypeRx.IsMatch content
                                if mayDefineModelTypes then h :: getOutputFile h :: appendGenerated t
                                else h :: appendGenerated t

                    appendGenerated projectInfo.files

                let projectInfo = { projectInfo with files = inFiles }

                let projHash = ProjectInfo.computeHash projectInfo
                let cacheFile = Path.Combine(outputDirectory, ".adaptifycache")
                let cache = if useCache then CacheFile.tryRead log cacheFile else None

                
                let projectChanged = 
                    match cache with
                    | Some cache -> projHash <> cache.projectHash || createLenses <> cache.lenses
                    | None -> 
                        if useCache then
                            log.debug range0 "[Adaptify]   no cache file for %s" (Path.GetFileName projectFile)
                        true

                let oldHashes = 
                    match cache with
                    | Some c -> c.fileHashes
                    | None -> Map.empty

                let mutable newHashes = Map.empty
            


                log.info range0 "[Adaptify] %s" (Path.GetFileName projectFile)

                let info = 
                    String.concat "; " [
                        if useCache then "cache"
                        if createLenses then "lenses"
                        if designTime then "designTime"
                    ]

                log.debug range0 "[Adaptify] project info"
                log.debug range0 "[Adaptify]   Name:     %s" (Path.GetFileName projectInfo.project)
                log.debug range0 "[Adaptify]   Style:    %s" (if projectInfo.isNewStyle then "new" else "old")
                log.debug range0 "[Adaptify]   Target:   %A" projectInfo.target
                log.debug range0 "[Adaptify]   Debug:    %A" projectInfo.debug
                log.debug range0 "[Adaptify]   Defines:  [%s]" (String.concat "; " projectInfo.defines)
                log.debug range0 "[Adaptify]   Flags:    [%s]" (String.concat " " projectInfo.additional)
                log.debug range0 "[Adaptify]   Params:   [%s]" info

                let nuget = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile), ".nuget", "packages") |> Path.GetFullPath

                let dotnetPack =
                    Path.Combine(
                        Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles)),
                        "dotnet", "packs"
                    )

                let cleanReferences =
                    projectInfo.references |> List.map (fun f ->
                        let f = Path.GetFullPath f
                        if f.StartsWith nuget then 
                            let nugetPath = f.Substring(nuget.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
                            let parts = nugetPath.Split([| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |])
                            if parts.Length >= 2 then
                                let name = parts.[0]
                                let version = parts.[1]
                                sprintf "nuget %s (%s)" name version
                            else
                                f
                        elif f.StartsWith dotnetPack then
                            let packPath = f.Substring(dotnetPack.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
                            let parts = packPath.Split([| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |])
                            if parts.Length >= 2 then
                                let name = 
                                    let name = parts.[0]
                                    if name.EndsWith ".Ref" then name.Substring(0, name.Length - 4)
                                    else name
                                let version = parts.[1]
                                sprintf "pack %s (%s)" name version
                            else
                                f
                        else 
                            relativePath f
                    ) |> Set.ofList
                
                log.debug range0 "[Adaptify]   References:"
                for f in cleanReferences do
                    log.debug range0 "[Adaptify]     %s" f

                log.debug range0 "[Adaptify]   Files:"
                for f in realFiles do
                    log.debug range0 "[Adaptify]     %s" (relativePath f)



                let newFiles = System.Collections.Generic.List<string>()
                let md5 = System.Security.Cryptography.MD5.Create()
                let inline hash (str : string) = 
                    md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string

                let options = toProjectOptions projectInfo
                let mutable changed = false

                let noGeneration =
                    if designTime then  
                        true
                    else
                        let missing = waitUntilExisting log 200 projectInfo.references
                        missing.Length > 0

                for file in projectInfo.files do
                    if not (file.EndsWith ".g.fs") then
                        let content = File.ReadAllText file
                        let mayDefineModelTypes = modelTypeRx.IsMatch content
                        let outputExists = File.Exists (getOutputFile file)

                        // just diagnostic output for strange case, which is handled with additional care.
                        if noGeneration && not outputExists then 
                            // normally this would be bad, handled in next if. we report this happened.
                            log.info range0 "[Adaptify]   the output for file %s was not found in output during a design time build. this should not happen, as the build should have generated this one. maybe design time and compile time project infos do not match" file

                        if noGeneration && outputExists then // no matter what, if output file does not exist, create it - non-existing files should not be returned
                            newFiles.Add file
                            let generated = getOutputFile file
                            if mayDefineModelTypes then
                                newFiles.Add generated
                        else
                            let fileHash = hash content

                            let mayDefineModelTypes = modelTypeRx.IsMatch content

                            let needsUpdate, containsModels =
                                if not mayDefineModelTypes then
                                    newFiles.Add file
                                    newHashes <- Map.add file { fileHash = fileHash; hasModels = false; warnings = [] } newHashes
                                    false, false
                                elif projectChanged then 
                                    let old = match cache with | Some p -> p.projectHash | None -> ""
                                    if old <> "" then
                                        log.debug range0 "[Adaptify]   project for %s changed (%A vs %A)" (relativePath file) projHash old
                                    true, true
                                elif not changed then
                                    match Map.tryFind file oldHashes with
                                    | Some oldEntry ->
                                        if oldEntry.fileHash = fileHash then
                                            if oldEntry.hasModels then
                                                let generated = getOutputFile file

                                                let readGeneratedHash (file : string) = 
                                                    use s = File.OpenRead file
                                                    use r = new StreamReader(s)
                                                    try
                                                        let inputHash = 
                                                            let line = r.ReadLine()
                                                            if line.StartsWith "//" then line.Substring 2
                                                            else ""

                                                        let selfHash =
                                                            let line = r.ReadLine()
                                                            if line.StartsWith "//" then line.Substring 2
                                                            else ""

                                                        let restHash = 
                                                            let str = r.ReadToEnd()
                                                            hash str

                                                        if selfHash = restHash then
                                                            inputHash
                                                        else
                                                            ""
                                                    with _ ->
                                                        ""

                                                if File.Exists(generated) && readGeneratedHash generated = fileHash then
                                                    let mutable hadErrors = false
                                                    newFiles.Add file
                                                    newFiles.Add generated
                                                    newHashes <- Map.add file oldEntry newHashes
                                                    for w in oldEntry.warnings do   
                                                        if w.isError then hadErrors <- true
                                                        if w.code <> "internal" then
                                                            let range = mkRange file (mkPos w.startLine w.startCol) (mkPos w.endLine w.endCol)
                                                            if w.isError then log.error range w.code "%s" w.message
                                                            else log.warn range w.code "%s" w.message
                                                
                                                    if hadErrors then changed <- true
                                                    hadErrors, true
                                                else
                                                    changed <- true
                                                    log.debug range0 "[Adaptify]   %s: generated file invalid" (relativePath file)
                                                    true, true
                                            else
                                                newFiles.Add file
                                                newHashes <- Map.add file oldEntry newHashes

                                                false, true
                                        else
                                            changed <- true
                                            log.debug range0 "[Adaptify]   %s: file hash changed" (relativePath file)
                                            true, true

                                    | None ->   
                                        changed <- true
                                        log.debug range0 "[Adaptify]   %s: no old hash" (relativePath file)
                                        true, true
                                else
                                    true, true
                                
                            if needsUpdate then
                                let warnings = System.Collections.Generic.List<Warning>()
                                let addWarning (isError : bool) (r : range) (code : string) (str : string) =
                                    warnings.Add {
                                        isError = isError
                                        startLine = r.StartLine
                                        startCol = r.StartColumn
                                        code = code
                                        endLine = r.EndLine
                                        endCol = r.EndColumn
                                        message = str
                                    }

                                //log.info range0 "[Adaptify]   update file %s" (relativePath projDir file)
                                let text = FSharp.Compiler.Text.SourceText.ofString content
                                let! (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options)
        
                                match answer with
                                | FSharpCheckFileAnswer.Succeeded res ->  
                                    let localLogger =
                                        { new ILog with
                                            member __.debug r fmt = log.debug r fmt
                                            member __.info r fmt = log.debug r fmt
                                            member __.warn r c fmt = Printf.kprintf (fun str -> addWarning false r c str; log.warn r c "%s" str) fmt
                                            member __.error r c fmt = Printf.kprintf (fun str -> addWarning true r c str; log.error r c "%s" str) fmt
                                        }

                                    let errs, wrns = res.Errors |> Array.partition (fun err -> err.Severity = FSharpErrorSeverity.Error)
                                    if errs.Length > 0 then
                                        let errorStrings =
                                            errs |> Seq.map (fun err ->
                                                sprintf "  (%d,%d): %s" err.StartLineAlternate err.StartColumn err.Message
                                            )
                                            |> String.concat "\r\n"
                                            |> sprintf "compiler errors:\r\n%s"

                                        let range = mkRange (relativePath file) pos0 pos0
                                        addWarning true range "internal" errorStrings

                                    for err in wrns do
                                        let p0 = mkPos err.StartLineAlternate err.StartColumn
                                        let p1 = mkPos err.EndLineAlternate err.EndColumn
                                        let range = mkRange (relativePath err.FileName) p0 p1
                                        localLogger.warn range (sprintf "%04d" err.ErrorNumber) "%s" err.Message

                                    let rec allEntities (d : FSharpImplementationFileDeclaration) =
                                        match d with
                                        | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                                            e :: List.collect allEntities ds
                                        | _ ->
                                            []

                                    let entities = 
                                        res.ImplementationFile.Value.Declarations
                                        |> Seq.toList
                                        |> List.collect allEntities
                                        
                                    let definitions =   
                                        entities 
                                        |> List.choose (TypeDef.ofEntity localLogger)
                                        |> List.choose (fun l -> 
                                            try 
                                                l.Value |> Some
                                            with e -> 
                                                log.error range0 "1337" "[Adaptify] could not get type entity:%s" e.Message
                                                None)
                                        |> List.collect (TypeDefinition.ofTypeDef localLogger createLenses [])

                                    let warnings = Seq.toList warnings

                                    newHashes <- Map.add file { fileHash = fileHash; hasModels = not (List.isEmpty definitions); warnings = warnings } newHashes
                                    newFiles.Add file
                                    match definitions with
                                    | [] ->
                                        log.info range0 "[Adaptify]   no models in %s" (relativePath file)
                                    | defs ->
                                        let outputFile = getOutputFile file

                                        let content = TypeDefinition.toFile log defs
                                        let result = sprintf "//%s\r\n//%s\r\n" fileHash (hash content) + content

                                        File.WriteAllText(outputFile, result)
                                        newFiles.Add outputFile
                                        log.info range0 "[Adaptify]   gen  %s" (relativePath outputFile)

                                | FSharpCheckFileAnswer.Aborted ->
                                    log.error range0 "587" "[Adaptify]   could not parse %s" (relativePath file)
                                    newFiles.Add file
                                    ()
                            else
                                if containsModels then
                                    log.info range0 "[Adaptify]   skip %s (up to date)" (relativePath file)
                                else
                                    log.info range0 "[Adaptify]   skip %s (no model types)" (relativePath file)

                if not designTime then
                    CacheFile.save { lenses = createLenses; projectHash = projHash; fileHashes = newHashes } cacheFile


                let files = newFiles |> Seq.map relativePath |> String.concat "; " |> sprintf "[%s]"
                log.debug range0 "[Adaptify]   files: %s" files

                return Seq.toList newFiles
            else
                log.info range0 "[Adaptify] skipping project %s" projectFile
                return Seq.toList projectInfo.files
        }

    let run (checker : FSharpChecker) (outputPath : string) (designTime : bool) (useCache : bool) (createLenses : bool) (log : ILog) (projectInfo : ProjectInfo) =
        try runAsync checker outputPath designTime useCache createLenses log projectInfo  |> Async.RunSynchronously
        with e -> 
            log.warn range0 "Internal error?" "[Adaptify]   internal error: %s" (e.Message)
            []
