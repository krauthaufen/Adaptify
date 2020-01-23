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
    let mutable createLenses = false

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

                    let newFiles = Client.adaptifyTcp x.Logger projInfo true createLenses
                    //let newFiles = Adaptify.run None true createLenses x.Logger projInfo

                    results <- List.toArray newFiles
                    true
                with e ->
                    x.Logger.error range0 "587" "failed: %A" e
                    false
              
            | _other -> 
                results <- files
                true
              
              
    member x.GenerateLenses
        with get() = createLenses
        and set l = createLenses <- l

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
