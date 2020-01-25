﻿namespace Adaptify.Compiler

open System.IO
open FSharp.Compiler.Range
open Aardvark.Compiler
open FSharp.Compiler.SourceCodeServices

module Adaptify =
    let private modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

    let private waitUntilExisting (log : ILog) (timeout : int) (files : seq<string>) =
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let rec run (iter : int) (infos : FileInfo[]) = 
            if infos.Length = 0 then
                [||]
            elif iter = 0 || sw.Elapsed.TotalMilliseconds <= float timeout then
                if iter > 0 then log.debug range0 "%d references missing -> retry" infos.Length
                let remaining = 
                    infos |> Array.filter (fun f ->
                        f.Refresh()
                        if f.Exists then false
                        else true
                    )

                if remaining.Length > 0 then
                    run (iter + 1) remaining
                else
                    [||]
            else
                if iter > 0 then log.debug range0 "%d references missing" infos.Length
                infos |> Array.map (fun f -> f.FullName)

        let infos = files |> Seq.map FileInfo |> Seq.toArray
        run 0 infos


    let run (checker : FSharpChecker) (useCache : bool) (createLenses : bool) (log : ILog) (projectInfo : ProjectInfo) =
        let projectInfo = ProjectInfo.normalize projectInfo

        let projectFile = projectInfo.project
        if Path.GetExtension projectFile = ".fsproj" then
            
            let realFiles = projectInfo.files

            let inFiles =
                let rec appendGenerated (f : list<string>) =
                    match f with
                    | [] -> []
                    | [s] when projectInfo.target = Target.Exe -> [s]
                    | [s] -> 
                        if s.EndsWith ".g.fs" then [s]
                        else [s; Path.ChangeExtension(s, ".g.fs")]
                    | h :: t ->
                        match t with
                        | hh :: t when hh = Path.ChangeExtension(h, ".g.fs") ->
                            h :: hh :: appendGenerated t
                        | _ ->
                            h :: Path.ChangeExtension(h, ".g.fs") :: appendGenerated t
                appendGenerated projectInfo.files

            let projectInfo = { projectInfo with files = inFiles }

            let projHash = ProjectInfo.computeHash projectInfo
            let cacheFile = Path.Combine(Path.GetDirectoryName projectFile, ".adaptifycache")
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
            
            let projDir = Path.GetDirectoryName projectFile

            let relativePath (dir : string) (name : string) =
                let dirFull = Path.GetFullPath dir
                let nameFull = Path.GetFullPath name
                if nameFull.StartsWith dirFull then
                    nameFull.Substring(dirFull.Length).TrimStart [| System.IO.Path.DirectorySeparatorChar; System.IO.Path.AltDirectorySeparatorChar |]
                else
                    name


            log.info range0 "[Adaptify] %s" (Path.GetFileName projectFile)

            log.debug range0 "[Adaptify] project info"
            log.debug range0 "[Adaptify]   Name:     %s" (Path.GetFileName projectInfo.project)
            log.debug range0 "[Adaptify]   Style:    %s" (if projectInfo.isNewStyle then "new" else "old")
            log.debug range0 "[Adaptify]   Target:   %A" projectInfo.target
            log.debug range0 "[Adaptify]   Debug:    %A" projectInfo.debug
            log.debug range0 "[Adaptify]   Defines:  [%s]" (String.concat "; " projectInfo.defines)
            log.debug range0 "[Adaptify]   Flags:    [%s]" (String.concat " " projectInfo.additional)

            let nuget = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile), ".nuget", "packages") |> Path.GetFullPath

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
                    else 
                        relativePath projDir f
                ) |> Set.ofList
                
            log.debug range0 "[Adaptify]   References:"
            for f in cleanReferences do
                log.debug range0 "[Adaptify]     %s" f

            log.debug range0 "[Adaptify]   Files:"
            for f in realFiles do
                log.debug range0 "[Adaptify]     %s" (relativePath projDir f)



            let newFiles = System.Collections.Generic.List<string>()
            let md5 = System.Security.Cryptography.MD5.Create()
            let inline hash (str : string) = 
                md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string

            let options = 
                lazy (
                    checker.GetProjectOptionsFromCommandLineArgs(
                        projectFile, 
                        ProjectInfo.toFscArgs projectInfo |> List.toArray
                    )
                )

            let mutable changed = false

            let wait =
                lazy (
                    waitUntilExisting log 100 projectInfo.references
                )

            for file in projectInfo.files do
                if not (file.EndsWith ".g.fs") then
                    let content = File.ReadAllText file
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
                                log.debug range0 "[Adaptify]   project for %s changed (%A vs %A)" (relativePath projDir file) projHash old
                            true, true
                        elif not changed then
                            match Map.tryFind file oldHashes with
                            | Some oldEntry ->
                                if oldEntry.fileHash = fileHash then
                                    if oldEntry.hasModels then
                                        let generated = Path.ChangeExtension(file, ".g.fs")

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
                                            log.debug range0 "[Adaptify]   %s: generated file invalid" (relativePath projDir file)
                                            true, true
                                    else
                                        newFiles.Add file
                                        newHashes <- Map.add file oldEntry newHashes

                                        false, true
                                else
                                    changed <- true
                                    log.debug range0 "[Adaptify]   %s: file hash changed" (relativePath projDir file)
                                    true, true

                            | None ->   
                                changed <- true
                                log.debug range0 "[Adaptify]   %s: no old hash" (relativePath projDir file)
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
                        let missingReferences = wait.Value
                        if missingReferences.Length > 0 then
                            let range = mkRange (relativePath projDir file) pos0 pos0
                            let missing =
                                missingReferences |> Array.map (fun m ->
                                    relativePath projDir m |> sprintf "  %s"
                                )
                                |> String.concat "\r\n"
                                |> sprintf "missing references:\r\n%s"

                            addWarning true range "internal" missing
                            log.debug range "%s" missing
                        else
                            let (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options.Value) |> Async.RunSynchronously
        
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

                                    let range = mkRange (relativePath projDir file) pos0 pos0
                                    addWarning true range "internal" errorStrings

                                for err in wrns do
                                    let p0 = mkPos err.StartLineAlternate err.StartColumn
                                    let p1 = mkPos err.EndLineAlternate err.EndColumn
                                    let range = mkRange (relativePath projDir err.FileName) p0 p1
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
                                    |> List.map (fun l -> l.Value)
                                    |> List.collect (TypeDefinition.ofTypeDef localLogger createLenses [])

                                let warnings = Seq.toList warnings

                                newHashes <- Map.add file { fileHash = fileHash; hasModels = not (List.isEmpty definitions); warnings = warnings } newHashes
                                newFiles.Add file
                                match definitions with
                                | [] ->
                                    log.info range0 "[Adaptify]   no models in %s" (relativePath projDir file)
                                | defs ->
                                    let file = Path.ChangeExtension(file, ".g.fs")

                                    let content = TypeDefinition.toFile defs
                                    let result = sprintf "//%s\r\n//%s\r\n" fileHash (hash content) + content

                                    File.WriteAllText(file, result)
                                    newFiles.Add file
                                    log.info range0 "[Adaptify]   gen  %s" (relativePath projDir file)

                            | FSharpCheckFileAnswer.Aborted ->
                                log.error range0 "587" "[Adaptify]   could not parse %s" (relativePath projDir file)
                                ()
                    else
                        if containsModels then
                            log.info range0 "[Adaptify]   skip %s (up to date)" (relativePath projDir file)
                        else
                            log.info range0 "[Adaptify]   skip %s (no model types)" (relativePath projDir file)

            CacheFile.save { lenses = createLenses; projectHash = projHash; fileHashes = newHashes } cacheFile


            let files = newFiles |> Seq.map (relativePath projDir) |> String.concat "; " |> sprintf "[%s]"
            log.debug range0 "[Adaptify]   files: %s" files

            Seq.toList newFiles
        else
            log.info range0 "[Adaptify] skipping project %s" projectFile
            Seq.toList projectInfo.files