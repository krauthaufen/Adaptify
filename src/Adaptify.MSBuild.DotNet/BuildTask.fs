namespace Adaptify.MSBuild

open Adaptify.Compiler
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range


type AdaptifyTask() =
    inherit Task()

    let mutable debug = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]
    let mutable framework : string = ""
    let mutable outputType : string = ""
    let mutable rebuild : bool = false

    let mutable log : option<ILog> = None


    member x.Logger =
        match log with
        | Some l -> l
        | None ->
            let msg (range : FSharp.Compiler.Range.range) imp fmt =
                fmt |> Printf.kprintf (fun str ->
                    x.Log.LogMessage(
                        "", 
                        "", 
                        "", 
                        range.FileName, 
                        range.StartLine, range.StartColumn + 1, 
                        range.EndLine, range.EndColumn + 1, 
                        imp, 
                        str,
                        [||]
                    )
                )
            let l =
                { new ILog with
                    member __.debug range fmt = msg range MessageImportance.Low fmt
                    member __.info range fmt = msg range MessageImportance.Normal fmt
                    member __.warn (range : FSharp.Compiler.Range.range) code fmt =
                        fmt |> Printf.kprintf (fun str ->
                            x.Log.LogWarning(
                                "Adaptify", 
                                code, 
                                "", 
                                range.FileName, 
                                range.StartLine, range.StartColumn + 1, 
                                range.EndLine, range.EndColumn + 1, 
                                str,
                                [||]
                            )
                        )
                    member __.error range code fmt =
                        fmt |> Printf.kprintf (fun str ->
                            x.Log.LogError(
                                "Adaptify", 
                                code,  
                                "", 
                                range.FileName, 
                                range.StartLine, range.StartColumn + 1, 
                                range.EndLine, range.EndColumn + 1, 
                                str,
                                [||]
                            )
                        )
                }
            log <- Some l
            l

    override x.Execute() =
        if debug then
            System.Diagnostics.Debugger.Launch() |> ignore
            
        let modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

        let md5 = System.Security.Cryptography.MD5.Create()
        let inline hash (str : string) = 
            md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string

        match Path.GetExtension projectFile with
            | ".fsproj" -> 
                try
                    let targetType = 
                        match outputType.ToLower() with
                            | "winexe" -> Target.WinExe
                            | "exe" -> Target.Exe
                            | _ -> Target.Library

                    let isNetFramework = references |> Array.exists (fun r -> Path.GetFileNameWithoutExtension(r).ToLower() = "mscorlib")
                    
                    let inFiles =
                        let rec appendGenerated (f : list<string>) =
                            match f with
                            | [] -> []
                            | [s] when targetType = Target.Exe -> [s]
                            | [s] -> 
                                if s.EndsWith ".g.fs" then [s]
                                else [s; Path.ChangeExtension(s, ".g.fs")]
                            | h :: t ->
                                match t with
                                | hh :: t when hh = Path.ChangeExtension(h, ".g.fs") ->
                                    h :: hh :: appendGenerated t
                                | _ ->
                                    h :: Path.ChangeExtension(h, ".g.fs") :: appendGenerated t
                        appendGenerated (Array.toList files)

                    let projInfo =
                        {
                            project = projectFile
                            isNewStyle = not isNetFramework
                            references = Array.toList references
                            files = inFiles
                            defines = []
                            target = targetType
                            output = Some (Path.GetTempFileName() + ".dll")
                            additional = []
                            debug = DebugType.Off
                        }

                    //let projHash = ProjectInfo.computeHash projInfo
                    //let cacheFile = Path.Combine(Path.GetDirectoryName projectFile, ".adaptifycache")
                    //let cache = CacheFile.tryRead cacheFile

                    //let projectChanged = 
                    //    match cache with
                    //    | Some cache -> projHash <> cache.projectHash
                    //    | None -> true

                    //let oldHashes = 
                    //    match cache with
                    //    | Some c -> c.fileHashes
                    //    | None -> Map.empty

                    //let mutable newHashes = Map.empty
                    
                    //let projDir = Path.GetDirectoryName projectFile
                    //let newFiles = System.Collections.Generic.List<string>()

                    //let md5 = System.Security.Cryptography.MD5.Create()
                    //let inline hash (str : string) = 
                    //    md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string
                    
                    //let checker = 
                    //    lazy (
                    //        let sw = System.Diagnostics.Stopwatch.StartNew()
                    //        let res = FSharpChecker.Create(keepAssemblyContents = true)
                    //        sw.Stop()
                    //        x.Logger.info range0 "[Adaptify] creating FSharpChecker took: %.0fm" sw.Elapsed.TotalMilliseconds
                    //        res
                    //    )
                    
                    //let options = 
                    //    lazy (
                    //        checker.Value.GetProjectOptionsFromCommandLineArgs(
                    //            projectFile, 
                    //            ProjectInfo.toFscArgs projInfo |> List.toArray
                    //        )
                    //    )
                    //for file in files do
                    //    if not (file.EndsWith ".g.fs") then
                    //        let path = Path.Combine(projDir, file)
                    //        let content = File.ReadAllText path
                    //        let fileHash = hash content

                    //        let mayDefineModelTypes = modelTypeRx.IsMatch content


                    //        let needsUpdate =
                    //            if not mayDefineModelTypes then
                    //                newFiles.Add file
                    //                newHashes <- Map.add file (fileHash, false) newHashes
                    //                false
                    //            elif projectChanged then 
                    //                true
                    //            else
                    //                match Map.tryFind file oldHashes with
                    //                | Some (oldHash, hasModels) ->
                    //                    if oldHash = fileHash then
                    //                        if hasModels then
                    //                            let generated = Path.ChangeExtension(path, ".g.fs")

                    //                            let readGeneratedHash (file : string) = 
                    //                                use s = File.OpenRead file
                    //                                use r = new StreamReader(s)
                    //                                try
                    //                                    let inputHash = 
                    //                                        let line = r.ReadLine()
                    //                                        if line.StartsWith "//" then line.Substring 2
                    //                                        else ""

                    //                                    let selfHash =
                    //                                        let line = r.ReadLine()
                    //                                        if line.StartsWith "//" then line.Substring 2
                    //                                        else ""

                    //                                    let restHash = 
                    //                                        let str = r.ReadToEnd()
                    //                                        hash str

                    //                                    if selfHash = restHash then
                    //                                        inputHash
                    //                                    else
                    //                                        ""
                    //                                with _ ->
                    //                                    ""

                    //                            if File.Exists(generated) && readGeneratedHash generated = fileHash then
                    //                                newFiles.Add file
                    //                                newFiles.Add generated
                    //                                newHashes <- Map.add file (fileHash, true) newHashes
                    //                                false
                    //                            else
                    //                                true
                    //                        else
                    //                            newFiles.Add file
                    //                            newHashes <- Map.add file (fileHash, false) newHashes
                    //                            false
                    //                    else
                    //                        true

                    //                | None ->   
                    //                    true
                                
                    //        if needsUpdate then
                    //            x.Logger.info range0 "[Adaptify] update file %s" file
                    //            let text = FSharp.Compiler.Text.SourceText.ofString content
                    //            let (_parseResult, answer) = checker.Value.ParseAndCheckFileInProject(file, 0, text, options.Value) |> Async.RunSynchronously
        
                    //            match answer with
                    //            | FSharpCheckFileAnswer.Succeeded res ->
                    //                let rec allEntities (d : FSharpImplementationFileDeclaration) =
                    //                    match d with
                    //                    | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                    //                        e :: List.collect allEntities ds
                    //                    | _ ->
                    //                        []

                    //                let entities = 
                    //                    res.ImplementationFile.Value.Declarations
                    //                    |> Seq.toList
                    //                    |> List.collect allEntities
                                        


                    //                let definitions = 
                    //                    entities 
                    //                    |> List.choose (TypeDef.ofEntity x.Logger)
                    //                    |> List.map (fun l -> l.Value)
                    //                    |> List.collect (TypeDefinition.ofTypeDef x.Logger [])

                    //                newHashes <- Map.add file (fileHash, not (List.isEmpty definitions)) newHashes
                    //                newFiles.Add file
                    //                match definitions with
                    //                | [] ->
                    //                    x.Logger.info range0 "[Adaptify] no models in %s" file
                    //                | defs ->
                    //                    let file = Path.ChangeExtension(path, ".g.fs")

                    //                    let content = TypeDefinition.toFile defs
                    //                    let result = sprintf "//%s\r\n//%s\r\n" fileHash (hash content) + content

                    //                    File.WriteAllText(file, result)
                    //                    newFiles.Add file
                    //                    x.Logger.info range0 "[Adaptify] generated %s" file

                    //            | FSharpCheckFileAnswer.Aborted ->
                    //                x.Logger.warn range0 "587" "[Adaptify] could not parse %s" file
                    //                ()
                    //        else
                    //            x.Logger.info range0 "[Adaptify] skipping %s" file

                    //CacheFile.save { projectHash = projHash; fileHashes = newHashes } cacheFile

                    let logMessage (m : Message) =
                        match m.severity with
                        | Severity.Debug -> x.Logger.debug m.range "%s" m.message
                        | Severity.Info -> x.Logger.info m.range "%s" m.message
                        | Severity.Warn -> x.Logger.warn m.range m.code.Value "%s" m.message
                        | Severity.Error -> x.Logger.error m.range m.code.Value "%s" m.message
                        | _ -> ()



                    let path = Path.GetDirectoryName(typeof<AdaptifyTask>.Assembly.Location)

                    let fileHash = hash projInfo.project
                    let cmd = Command.AdaptifyInfo(rebuild, projInfo)
                    match Client.run path fileHash cmd with
                    | Some (Reply.Success(info, messages)) ->
                        Seq.iter logMessage messages
                        results <- Seq.toArray info.files
                        true
                    | Some (Reply.Error messages) ->  
                        Seq.iter logMessage messages  
                        false
                    | None ->
                        false

                with e ->
                    x.Logger.error range0 "587" "failed: %A" e
                    false
              
            | _other -> 
                let item = TaskItem("Compile")
                item.SetMetadata("AutoGen", "True")

                results <- files
                true
                

    member x.Debug
        with get() = debug
        and set i = debug <- i
        
    member x.Rebuild
        with get() = rebuild
        and set i = rebuild <- i
        
    member x.TargetFramework
        with get() = framework
        and set i = framework <- i

    member x.OutputType
        with get() = outputType
        and set t = outputType <- t

    [<Required>]
    member x.Files
        with get() = files
        and set i = files <- i

    [<Required>]
    member x.References
        with get() = references
        and set c = references <- c

    [<Required>]
    member x.ProjectFile
        with get() = projectFile
        and set f = projectFile <- f

   
    [<Output>]
    member x.Results
        with get() = results
        and set r = results <- r
