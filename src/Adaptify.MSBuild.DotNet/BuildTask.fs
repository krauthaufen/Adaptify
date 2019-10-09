namespace Adaptify.MSBuild

open Adaptify.Compiler
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open System.IO
open FSharp.Compiler.SourceCodeServices

type CacheFile =
    {
        projectHash : string
        fileHashes : Map<string, string * bool>
    }

module CacheFile =
    let tryRead (path : string) =
        try
            let lines = File.ReadAllLines path
            let fileHashes =
                Array.skip 1 lines |> Seq.map (fun l ->
                    let comp = l.Split([|";"|], System.StringSplitOptions.None)
                    comp.[0], (comp.[1], System.Boolean.Parse comp.[2])
                )
                |> Map.ofSeq
            Some {
                projectHash = lines.[0]
                fileHashes = fileHashes
            }
        with _ ->
            None

    let save (cache : CacheFile) (path : string) =
        try
            File.WriteAllLines(path, [|
                yield cache.projectHash
                for (file, (hash, modelTypes)) in Map.toSeq cache.fileHashes do
                    yield sprintf "%s;%s;%A" file hash modelTypes
            |])
        with _ ->
            ()

type AdaptifyTask() =
    inherit Task()

    let mutable debug = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]
    let mutable framework : string = ""
    let mutable outputType : string = ""

    member x.info fmt =
        fmt |> Printf.kprintf (fun str ->
            x.Log.LogMessage(MessageImportance.Normal, str)
        )

        
    member x.warn fmt =
        fmt |> Printf.kprintf (fun str ->
            x.Log.LogWarning(str)
        )
    member x.error fmt =
        fmt |> Printf.kprintf (fun str ->
            x.Log.LogError(str)
        )

    override x.Execute() =
        if debug then
            System.Diagnostics.Debugger.Launch() |> ignore
            
        let modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

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

                    let projHash = ProjectInfo.computeHash projInfo
                    let cacheFile = Path.Combine(Path.GetDirectoryName projectFile, ".adaptifycache")
                    let cache = CacheFile.tryRead cacheFile

                    let projectChanged = 
                        match cache with
                        | Some cache -> projHash <> cache.projectHash
                        | None -> true

                    let oldHashes = 
                        match cache with
                        | Some c -> c.fileHashes
                        | None -> Map.empty

                    let mutable newHashes = Map.empty
                    
                    let projDir = Path.GetDirectoryName projectFile
                    let newFiles = System.Collections.Generic.List<string>()

                    let md5 = System.Security.Cryptography.MD5.Create()
                    let inline hash (str : string) = 
                        md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string
                    
                    let checker = 
                        lazy (
                            let sw = System.Diagnostics.Stopwatch.StartNew()
                            let res = FSharpChecker.Create(keepAssemblyContents = true)
                            sw.Stop()
                            x.info "[Adaptify] creating FSharpChecker took: %.0fm" sw.Elapsed.TotalMilliseconds
                            res
                        )
                    
                    let options = 
                        lazy (
                            checker.Value.GetProjectOptionsFromCommandLineArgs(
                                projectFile, 
                                ProjectInfo.toFscArgs projInfo |> List.toArray
                            )
                        )
                    for file in files do
                        if not (file.EndsWith ".g.fs") then
                            let path = Path.Combine(projDir, file)
                            let content = File.ReadAllText path
                            let fileHash = hash content

                            let mayDefineModelTypes = modelTypeRx.IsMatch content


                            let needsUpdate =
                                if not mayDefineModelTypes then
                                    newFiles.Add file
                                    newHashes <- Map.add file (fileHash, false) newHashes
                                    false
                                elif projectChanged then 
                                    true
                                else
                                    match Map.tryFind file oldHashes with
                                    | Some (oldHash, hasModels) ->
                                        if oldHash = fileHash then
                                            if hasModels then
                                                let generated = Path.ChangeExtension(path, ".g.fs")

                                                let readGeneratedHash (file : string) =
                                                    use s = File.OpenRead file
                                                    use r = new StreamReader(s)
                                                    try
                                                        let line = r.ReadLine()
                                                        if line.StartsWith "//" then line.Substring 2
                                                        else ""
                                                    with _ ->
                                                        ""

                                                if File.Exists(generated) && readGeneratedHash generated = fileHash then
                                                    newFiles.Add file
                                                    newFiles.Add generated
                                                    newHashes <- Map.add file (fileHash, true) newHashes
                                                    false
                                                else
                                                    true
                                            else
                                                newFiles.Add file
                                                newHashes <- Map.add file (fileHash, false) newHashes
                                                false
                                        else
                                            true

                                    | None ->   
                                        true
                                
                            if needsUpdate then
                                x.info "[Adaptify] update file %s" file
                                let text = FSharp.Compiler.Text.SourceText.ofString content
                                let (_parseResult, answer) = checker.Value.ParseAndCheckFileInProject(file, 0, text, options.Value) |> Async.RunSynchronously
        
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


                                    let adaptors = 
                                        entities
                                        |> List.collect (fun e -> Adaptor.generate { qualifiedPath = Option.toList e.Namespace; file = path } e)
                                        |> List.groupBy (fun (f, _, _) -> f)
                                        |> List.map (fun (f, els) -> 
                                            f, els |> List.map (fun (_,m,c) -> m, c) |> List.groupBy fst |> List.map (fun (m,ds) -> m, List.map snd ds) |> Map.ofList
                                        )
                                        |> Map.ofList

                                    let builder = System.Text.StringBuilder()

                                    for (ns, modules) in Map.toSeq adaptors do
                                        sprintf "namespace %s" ns |> builder.AppendLine |> ignore
                                        sprintf "open FSharp.Data.Adaptive" |> builder.AppendLine |> ignore
                                        sprintf "open Adaptify" |> builder.AppendLine |> ignore
                                        for (m, def) in Map.toSeq modules do
                                            sprintf "[<AutoOpen>]" |> builder.AppendLine |> ignore
                                            sprintf "module rec %s =" m |> builder.AppendLine |> ignore
                                            for d in def do
                                                for l in d do
                                                    sprintf "    %s" l |> builder.AppendLine |> ignore

                                    let generated = not (System.String.IsNullOrWhiteSpace(string builder))
                                    newHashes <- Map.add file (fileHash, generated) newHashes
                                    newFiles.Add file
                                    if generated then
                                        let file = Path.ChangeExtension(path, ".g.fs")

                                        let result = sprintf "//%s\r\n" fileHash + builder.ToString()

                                        File.WriteAllText(file, result)
                                        newFiles.Add file
                                        x.info "[Adaptify] generated %s" file
                                    else
                                        x.info "[Adaptify] no models in %s" file

                                | FSharpCheckFileAnswer.Aborted ->
                                    x.warn "[Adaptify] could not parse %s" file
                                    ()
                            else
                                x.info "[Adaptify] skipping %s" file

                    CacheFile.save { projectHash = projHash; fileHashes = newHashes } cacheFile
                    results <- Seq.toArray newFiles
                    true
                with e ->
                    x.error "failed: %A" e
                    false
              
            | _other -> 
                results <- files
                true
                

    member x.Debug
        with get() = debug
        and set i = debug <- i
        
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
