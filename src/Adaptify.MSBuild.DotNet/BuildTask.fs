namespace Adaptify.MSBuild

open Adaptify.Compiler
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range

type Warning =
    {
        startLine : int
        startCol : int
        endLine : int
        endCol : int
        message : string
        code : string
    }

module Warning =
    let parse (str : string) : list<Warning> =
        if str.Length = 0 then 
            []
        else
            let str = System.Text.Encoding.UTF8.GetString(System.Convert.FromBase64String(str))
            str.Split([|"\r\n" |], System.StringSplitOptions.None)
            |> Array.toList
            |> List.map (fun (l : string) ->
                let c : string[] = l.Split([| ";" |], System.StringSplitOptions.None)
                { 
                    startLine = int c.[0]
                    startCol = int c.[1]
                    endLine = int c.[2]
                    endCol = int c.[3]
                    code = c.[4]
                    message = c.[5]
                }
            )

    let pickle (l : list<Warning>) =
        let string =
            l |> List.map (fun w ->
                sprintf "%d;%d;%d;%d;%s;%s" w.startLine w.startCol w.endLine w.endCol w.code w.message
            ) |> String.concat "\r\n"
            
        string
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Convert.ToBase64String


type FileCacheEntry =
    {
        fileHash    : string
        hasModels   : bool
        warnings    : list<Warning>
    }


type CacheFile =
    {
        projectHash : string
        fileHashes : Map<string, FileCacheEntry>
    }

module CacheFile =
    let tryRead (path : string) =
        try
            let lines = File.ReadAllLines path
            let fileHashes =
                Array.skip 1 lines |> Seq.map (fun l ->
                    let comp = l.Split([|";"|], System.StringSplitOptions.None)

                    let warnings =
                        if comp.Length > 3 then Warning.parse comp.[3]
                        else []

                    comp.[0], { fileHash = comp.[1]; hasModels = System.Boolean.Parse comp.[2]; warnings = warnings }
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
                for (file, entry) in Map.toSeq cache.fileHashes do
                    let wrn = Warning.pickle entry.warnings
                    yield sprintf "%s;%s;%A;%s" file entry.fileHash entry.hasModels wrn
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
            
        match Path.GetExtension projectFile with
            | ".fsproj" -> 
                try
                    let targetType = 
                        match outputType.ToLower() with
                            | "winexe" -> Target.WinExe
                            | "exe" -> Target.Exe
                            | _ -> Target.Library

                    let isNetFramework = references |> Array.exists (fun r -> Path.GetFileNameWithoutExtension(r).ToLower() = "mscorlib")
                    
                    let projInfo =
                        {
                            project = projectFile
                            isNewStyle = not isNetFramework
                            references = Array.toList references
                            files = Array.toList files
                            defines = []
                            target = targetType
                            output = Some (Path.GetTempFileName() + ".dll")
                            additional = ["--noframework"]
                            debug = DebugType.Off
                        }

                    let newFiles = Adaptify.run None false x.Logger projInfo

                    results <- List.toArray newFiles
                    true
                with e ->
                    x.Logger.error range0 "587" "failed: %A" e
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
