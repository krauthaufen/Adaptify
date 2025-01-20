open System.IO
open System.Text.Json
open FSharp.Compiler.Text
open FSharp.Core
open Adaptify.Compiler

let msbuild (argFile: string) =
    let mutable lenses = false
    let mutable touchFiles = false
    let mutable designTime = false
    let mutable targetFramework = ""
    let mutable projectFile = ""
    let mutable defines = ""
    let mutable outputPath = ""
    let mutable outputType = ""
    let mutable files = [||]
    let mutable references = [||]
    let mutable verbose = false
    let mutable local = false

    if File.Exists argFile then
        let args = File.ReadAllText(argFile).Replace("\\", "\\\\")
        let doc = System.Text.Json.JsonDocument.Parse args
        let root = doc.RootElement

        let inline stringArr (e : JsonElement) =
            let cnt = e.GetArrayLength()
            Array.init cnt (fun i -> e.[i].GetString())

        lenses <- root.GetProperty("lenses").GetString().ToLower() = "true"
        touchFiles <- root.GetProperty("touchFiles").GetString().ToLower() = "true"
        designTime <- root.GetProperty("designTime").GetString().ToLower() = "true"
        targetFramework <- root.GetProperty("targetFramework").GetString()
        projectFile <- root.GetProperty("projectFile").GetString()
        defines <- root.GetProperty("defines").GetString()
        outputPath <- root.GetProperty("outputPath").GetString()
        outputType <- root.GetProperty("outputType").GetString()
        files <- stringArr(root.GetProperty("files"))
        references <- stringArr(root.GetProperty("references"))
        verbose <- root.GetProperty("verbose").GetString().ToLower() = "true"
        local <- root.GetProperty("local").GetString().ToLower() = "true"

    let outputPath =
        Path.Combine(Path.GetDirectoryName(projectFile), outputPath) |> Path.GetFullPath
    //
    // if verbose then
    //     printfn "lenses: %A" lenses
    //     printfn "debugHate: %A" debugHate
    //     printfn "touchFiles: %A" touchFiles
    //     printfn "designTime: %A" designTime
    //     printfn "targetFramework: %s" targetFramework
    //     printfn "projectFile: %s" projectFile
    //     printfn "defines: %s" defines
    //     printfn "outputPath: %s" outputPath
    //     printfn "outputType: %s" outputType
    //     printfn "files: %A" files
    //     printfn "references: %A" references
    //

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
            projRefs = []
            isNewStyle = not isNetFramework
            references = Array.toList references
            files = Array.toList files
            defines = Array.toList defines
            target = targetType
            output = Some (Path.GetTempFileName() + ".dll")
            additional = ["--noframework"]
            debug = DebugType.Off
        }

    let checker = newChecker()
    let log : ILog =
        { new ILog with
            member this.debug range fmt =
                fmt |> Printf.kprintf (fun str ->
                    if verbose then
                        printfn "%s(%d,%d,%d,%d): %s" range.FileName range.StartLine range.StartColumn range.EndLine range.EndColumn str
                )

            member this.info range fmt =
                fmt |> Printf.kprintf (fun str ->
                    if verbose then
                        printfn "%s(%d,%d,%d,%d): %s" range.FileName range.StartLine range.StartColumn range.EndLine range.EndColumn str
                )

            member this.error range code fmt =
                fmt |> Printf.kprintf (fun str ->
                    eprintfn "%s(%d,%d,%d,%d): error %s: %s" range.FileName range.StartLine range.StartColumn range.EndLine range.EndColumn code str
                )

            member this.warn (range : range) code fmt =
                fmt |> Printf.kprintf (fun str ->
                    eprintfn "%s(%d,%d,%d,%d): warning %s: %s" range.FileName range.StartLine range.StartColumn range.EndLine range.EndColumn code str
                )
        }

    let run = Adaptify.runAsync checker outputPath designTime true lenses log local false projInfo

    let newFiles =
        try Async.RunSynchronously(run)
        with e ->
            eprintfn "adaptify unexpected error: %A" e
            projInfo.files


    File.WriteAllLines(Path.Combine(outputPath, "adaptify.files"), newFiles)


[<EntryPoint>]
let main argv =
    if argv.Length > 0 && File.Exists argv.[0] then
        msbuild argv.[0]
        0
    else
        eprintfn "Invalid arguments: %A" argv
        eprintfn "Usage: Adaptify.MSBuild <argfile>"
        1