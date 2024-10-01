namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.Text
open Aardvark.Compiler
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

module Adaptify =
    let private modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

    let private waitUntilExisting (log : ILog) (timeout : int) (files : seq<string>) =
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let rec run (iter : int) (files : string[]) = 
            if files.Length = 0 then
                [||]
            elif iter = 0 || (iter < 50 && sw.Elapsed.TotalMilliseconds <= float timeout) then
                if iter > 0 then log.debug Range.range0 "%d references missing -> retry" files.Length
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
                    log.debug Range.range0 "%d references missing" files.Length
                    for i,f in Seq.indexed files do
                        log.debug Range.range0 "    %d: %s" i f
                files

        run 0 (Seq.toArray files)

    let private toProjectOptions (projectInfo : ProjectInfo) : FSharpProjectOptions =
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

        let errs, wrns = 
            res.Diagnostics 
            |> Array.filter (fun e -> e.Severity = FSharpDiagnosticSeverity.Warning || e.Severity = FSharpDiagnosticSeverity.Error)
            |> Array.partition (fun err -> err.Severity = FSharpDiagnosticSeverity.Error)
        if errs.Length > 0 then
            let errorStrings =
                errs |> Seq.map (fun err ->
                    sprintf "  (%d,%d): %s" err.StartLine err.StartColumn err.Message
                )
                |> String.concat "\r\n"

            let range = Range.mkRange "internal" Position.pos0 Position.pos0
            log.warn range "internal" "compiler errors:\r\n%s" errorStrings

        for err in wrns do
            let p0 = Position.mkPos err.StartLine err.StartColumn
            let p1 = Position.mkPos err.EndLine err.EndColumn
            let range = Range.mkRange err.FileName p0 p1
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
                    yield "#nowarn \"1182\" // value is unused"
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
               

    let runAsync (checker : FSharpChecker) (outputPath : string) (designTime : bool) (useCache : bool) (createLenses : bool) (log : ILog) (local : bool) (release : bool) (projectInfo : ProjectInfo) =
        async {
            do! Async.SwitchToThreadPool()
            let projectInfo = ProjectInfo.normalize projectInfo

            let hash = ProjectInfo.computeHash projectInfo
            let projectFile = projectInfo.project
            let projDir = Path.GetDirectoryName projectFile
            let outputDirectory =
                if local then
                    let dir = Path.Combine(Path.GetTempPath(), "adaptify", hash)
                    Directory.ensure dir |> ignore
                    dir
                else
                    outputPath

            let relativePath (name : string) =
                let dirFull = Path.GetFullPath projDir
                let nameFull = Path.GetFullPath name
                if nameFull.StartsWith dirFull then
                    nameFull.Substring(dirFull.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
                else
                    name

            let getOutputFile (file : string) =
                if local then
                    Path.ChangeExtension(file, ".g.fs")
                else
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
                            log.debug Range.range0 "[Adaptify]   no cache file for %s" (Path.GetFileName projectFile)
                        true

                let oldHashes = 
                    match cache with
                    | Some c -> c.fileHashes
                    | None -> Map.empty

                let mutable newHashes = Map.empty
            


                log.info Range.range0 "[Adaptify] %s" (Path.GetFileName projectFile)

                let info = 
                    String.concat "; " [
                        if useCache then "cache"
                        if createLenses then "lenses"
                        if designTime then "designTime"
                    ]

                log.debug Range.range0 "[Adaptify] project info"
                log.debug Range.range0 "[Adaptify]   Name:     %s" (Path.GetFileName projectInfo.project)
                log.debug Range.range0 "[Adaptify]   Style:    %s" (if projectInfo.isNewStyle then "new" else "old")
                log.debug Range.range0 "[Adaptify]   Target:   %A" projectInfo.target
                log.debug Range.range0 "[Adaptify]   Debug:    %A" projectInfo.debug
                log.debug Range.range0 "[Adaptify]   Defines:  [%s]" (String.concat "; " projectInfo.defines)
                log.debug Range.range0 "[Adaptify]   Flags:    [%s]" (String.concat " " projectInfo.additional)
                log.debug Range.range0 "[Adaptify]   Params:   [%s]" info

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
                
                log.debug Range.range0 "[Adaptify]   References:"
                for f in cleanReferences do
                    log.debug Range.range0 "[Adaptify]     %s" f

                log.debug Range.range0 "[Adaptify]   Files:"
                for f in realFiles do
                    log.debug Range.range0 "[Adaptify]     %s" (relativePath f)



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
                        if missing.Length > 0 then

                            log.info Range.range0 "  %s is missing %d references" projectInfo.project missing.Length
                            log.info Range.range0 "  building referenced projects"
                            for _, path in projectInfo.projRefs do
                                log.info Range.range0 "    building %s" (Path.GetFileName path)
                                let args =
                                    if release then
                                        ["build"; "-c"; "Release"; Path.GetFileName path]
                                    else
                                        ["build"; "-c"; "Debug"; Path.GetFileName path]

                                let proc = 
                                    Process.tryStart {
                                        file        = "dotnet"
                                        workDir     = Path.GetDirectoryName path
                                        args        = args
                                        output      = OutputMode.Custom (fun s m -> log.debug Range.range0 "      %s" m)
                                    }
                                match proc with
                                | Some (p, d) -> 
                                    p.WaitForExit()
                                    d.Dispose()
                                    if p.ExitCode <> 0 then log.error Range.range0 "1" "    failed"
                                | None -> 
                                    log.info Range.range0 "    success"
                                    ()

                            let missing = waitUntilExisting log 200 projectInfo.references
                            missing.Length > 0 && useCache
                        else
                            useCache

                for file in projectInfo.files do
                    if not (file.EndsWith ".g.fs") then
                        let content = File.ReadAllText file
                        let mayDefineModelTypes = modelTypeRx.IsMatch content
                        let outputFile = getOutputFile file
                        let outputExists = File.Exists outputFile
                        let fileHash = hash content
                        let hashEquals = 
                            match Map.tryFind file oldHashes with
                            | Some oldHash -> oldHash.fileHash = fileHash
                            | _ -> false

                        // just diagnostic output for strange case, which is handled with additional care.
                        if noGeneration && not outputExists && mayDefineModelTypes then 
                            // normally this would be bad, handled in next if. we report this happened.
                            log.info Range.range0 "[Adaptify]   the output %s for file %s was not found in output during a design time build. this should not happen, as the build should have generated this one. maybe design time and compile time project infos do not match" outputFile file

                        if noGeneration && outputExists && hashEquals then // no matter what, if output file does not exist, create it - non-existing files should not be returned
                            newFiles.Add file
                            let generated = getOutputFile file
                            if mayDefineModelTypes then
                                newFiles.Add generated
                        else

                            let mayDefineModelTypes = modelTypeRx.IsMatch content

                            let needsUpdate, containsModels =
                                if not mayDefineModelTypes then
                                    newFiles.Add file
                                    newHashes <- Map.add file { fileHash = fileHash; hasModels = false; warnings = [] } newHashes
                                    false, false
                                elif projectChanged then 
                                    let old = match cache with | Some p -> p.projectHash | None -> ""
                                    if old <> "" then
                                        log.debug Range.range0 "[Adaptify]   project for %s changed (%A vs %A)" (relativePath file) projHash old
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
                                                            let range = Range.mkRange file (Position.mkPos w.startLine w.startCol) (Position.mkPos w.endLine w.endCol)
                                                            if w.isError then log.error range w.code "%s" w.message
                                                            else log.warn range w.code "%s" w.message
                                                
                                                    if hadErrors then changed <- true
                                                    hadErrors, true
                                                else
                                                    changed <- true
                                                    log.debug Range.range0 "[Adaptify]   %s: generated file invalid" (relativePath file)
                                                    true, true
                                            else
                                                newFiles.Add file
                                                newHashes <- Map.add file oldEntry newHashes

                                                false, true
                                        else
                                            changed <- true
                                            log.debug Range.range0 "[Adaptify]   %s: file hash changed" (relativePath file)
                                            true, true

                                    | None ->   
                                        changed <- true
                                        log.debug Range.range0 "[Adaptify]   %s: no old hash" (relativePath file)
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

                                    let errs, wrns = 
                                        res.Diagnostics
                                        |> Array.filter (fun e -> e.Severity = FSharpDiagnosticSeverity.Warning || e.Severity = FSharpDiagnosticSeverity.Error)
                                        |> Array.partition (fun err -> err.Severity = FSharpDiagnosticSeverity.Error)

                                    if errs.Length > 0 then
                                        let errorStrings =
                                            errs |> Seq.map (fun err ->
                                                sprintf "  (%d,%d): %s" err.StartLine err.StartColumn err.Message
                                            )
                                            |> String.concat "\r\n"
                                            |> sprintf "compiler errors:\r\n%s"

                                        let range = Range.mkRange (relativePath file) Position.pos0 Position.pos0
                                        addWarning true range "internal" errorStrings

                                    for err in wrns do
                                        let p0 = Position.mkPos err.StartLine err.StartColumn
                                        let p1 = Position.mkPos err.EndLine err.EndColumn
                                        let range = Range.mkRange (relativePath err.FileName) p0 p1
                                        localLogger.warn range (sprintf "%04d" err.ErrorNumber) "%s" err.Message

                                    let rec allEntities (d : FSharpImplementationFileDeclaration) =
                                        match d with
                                        | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                                            e :: List.collect allEntities ds
                                        | _ ->
                                            []

                                    let entities = 
                                        match res.ImplementationFile with
                                        | None -> 
                                            let range = Range.mkRange (relativePath file) Position.pos0 Position.pos0
                                            log.error range "1338" "[Adaptify] no implementation file for: %A, assuming no model types" res
                                            []
                                        | Some implementationFile -> 
                                            implementationFile.Declarations
                                            |> Seq.toList
                                            |> List.collect allEntities
                                        
                                    let definitions =   
                                        entities 
                                        |> List.choose (TypeDef.ofEntity localLogger)
                                        |> List.choose (fun l -> 
                                            try 
                                                l.Value |> Some
                                            with e -> 
                                                log.error Range.range0 "1337" "[Adaptify] could not get type entity:%s" e.Message
                                                None)
                                        |> List.collect (TypeDefinition.ofTypeDef localLogger createLenses [])

                                    let warnings = Seq.toList warnings

                                    newHashes <- Map.add file { fileHash = fileHash; hasModels = not (List.isEmpty definitions); warnings = warnings } newHashes
                                    newFiles.Add file
                                    match definitions with
                                    | [] ->
                                        log.info Range.range0 "[Adaptify]   no models in %s" (relativePath file)
                                    | defs ->
                                        let outputFile = getOutputFile file

                                        let content = TypeDefinition.toFile log defs
                                        let result = sprintf "//%s\r\n//%s\r\n" fileHash (hash content) + content

                                        File.WriteAllText(outputFile, result)
                                        newFiles.Add outputFile
                                        log.info Range.range0 "[Adaptify]   gen  %s" (relativePath outputFile)

                                | FSharpCheckFileAnswer.Aborted ->
                                    log.error Range.range0 "587" "[Adaptify]   could not parse (aborted) %s" (relativePath file)
                                    newFiles.Add file
                                    ()
                            else
                                if containsModels then
                                    log.info Range.range0 "[Adaptify]   skip %s (up to date)" (relativePath file)
                                else
                                    log.info Range.range0 "[Adaptify]   skip %s (no model types)" (relativePath file)

                if not designTime then
                    CacheFile.save { lenses = createLenses; projectHash = projHash; fileHashes = newHashes } cacheFile


                let files = newFiles |> Seq.map relativePath |> String.concat "; " |> sprintf "[%s]"
                log.debug Range.range0 "[Adaptify]   files: %s" files

                return Seq.toList newFiles
            else
                log.info Range.range0 "[Adaptify] skipping project %s" projectFile
                return Seq.toList projectInfo.files
        }

    let run (checker : FSharpChecker) (outputPath : string) (designTime : bool) (useCache : bool) (createLenses : bool) (log : ILog) (local : bool) (release : bool) (projectInfo : ProjectInfo) =
        try runAsync checker outputPath designTime useCache createLenses log local release projectInfo  |> Async.RunSynchronously
        with e -> 
            log.warn Range.range0 "Internal error?" "[Adaptify]   internal error: %s" (e.Message)
            []
