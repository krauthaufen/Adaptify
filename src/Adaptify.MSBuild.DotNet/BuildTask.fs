namespace Adaptify.MSBuild

open Adaptify.Compiler
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open System.IO
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Threading


type AdaptifyTask() =
    inherit Task()

    let mutable designTime = false
    let mutable debug = false
    let mutable touchFiles = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]
    let mutable framework : string = ""
    let mutable outputType : string = ""
    let mutable createLenses = false
    let mutable defines = ""
    let mutable outputPath = "."

    let mutable log : option<ILog> = None

    let mutable cancel : CancellationTokenSource = null

    member x.Cancel() =
        if not (isNull cancel) then cancel.Cancel()

    interface ICancelableTask with
        member x.Cancel() = x.Cancel()

    member x.Logger =
        match log with
        | Some l -> l
        | None ->
            let msg (range : FSharp.Compiler.Text.range) imp fmt =
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
                    member __.warn (range : FSharp.Compiler.Text.range) code fmt =
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
        cancel <- new CancellationTokenSource()
        try
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
                    
                    let defines = defines.Split([|';'|], System.StringSplitOptions.RemoveEmptyEntries)

                    let projInfo =
                        {
                            project = projectFile
                            isNewStyle = not isNetFramework
                            references = Array.toList references
                            files = Array.toList files
                            defines = Array.toList defines
                            target = targetType
                            output = Some (Path.GetTempFileName() + ".dll")
                            additional = ["--noframework"]
                            debug = DebugType.Off
                        }

                    let newFiles = Client.adaptify cancel.Token x.Logger projInfo outputPath designTime true createLenses

                    if touchFiles then
                        x.Logger.info Range.range0 "[Adaptify] touching project files to trigger code completion updates..."
                        try
                            let time = System.DateTime.Now
                            for f in newFiles do    
                                File.SetLastWriteTime(f,time)
                        with e -> 
                            x.Logger.info Range.range0 "[Adaptify] could not touch files"
    
                    results <- List.toArray newFiles
                    not cancel.IsCancellationRequested
                with e ->
                    x.Logger.error Range.range0 "587" "failed: %A" e
                    not cancel.IsCancellationRequested
              
            | _other -> 
                results <- files
                true
        finally
            cancel.Dispose()
            cancel <- null
          
    member x.DesignTime
        with get() = designTime
        and set d = designTime <- d
              
    member x.GenerateLenses
        with get() = createLenses
        and set l = createLenses <- l

    member x.Debug
        with get() = debug
        and set i = debug <- i

    member x.TouchFiles 
        with get() = touchFiles
        and set t = touchFiles <- t
        
    member x.TargetFramework
        with get() = framework
        and set i = framework <- i

    member x.OutputType
        with get() = outputType
        and set t = outputType <- t

    member x.Defines 
        with get() = defines
        and set d = defines <- d

    member x.OutputPath 
        with get() = outputPath
        and set p = outputPath <- p

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
