open System
open System.IO
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Core
open Adaptify.Compiler
open System.Runtime.CompilerServices
open System.Threading
open Ionide.ProjInfo.ProjectLoader
open Ionide.ProjInfo.Types
open System.Reflection
open Microsoft.FSharp.Reflection

module ProjectInfo =
    open Ionide.ProjInfo
    
    module internal Utils =
        let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
            let psi = System.Diagnostics.ProcessStartInfo()
            psi.FileName <- exePath
            psi.WorkingDirectory <- workingDir
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.Arguments <- args
            psi.CreateNoWindow <- true
            psi.UseShellExecute <- false

            use p = new System.Diagnostics.Process()
            p.StartInfo <- psi

            p.OutputDataReceived.Add(fun ea -> log (ea.Data))

            p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

            // printfn "running: %s %s" psi.FileName psi.Arguments

            p.Start() |> ignore
            p.BeginOutputReadLine()
            p.BeginErrorReadLine()
            p.WaitForExit()

            let exitCode = p.ExitCode

            exitCode, (workingDir, exePath, args)

    let private cases = FSharpType.GetUnionCases(typeof<ProjectLoadingStatus>, true)
    let private success = cases |> Array.find (fun c -> c.Name = "Success")
    let private error = cases |> Array.find (fun c -> c.Name = "Error")
    let private readTag = FSharpValue.PreComputeUnionTagReader(typeof<ProjectLoadingStatus>, true)
    let private readSuccess = FSharpValue.PreComputeUnionReader(success, true)
    let private readError = FSharpValue.PreComputeUnionReader(error, true)

    let tryOfProject additionalMSBuildProps (file : string) =
        let projDir = Path.GetDirectoryName file
        
       // Ionide.ProjInfo.ProjectLoader.loadProject
        
        let dotnet = Ionide.ProjInfo.Paths.dotnetRoot.Value
        let path = Init.init (DirectoryInfo projDir) dotnet

        let additionalMSBuildProps = ("GenerateDomainTypes", "false") :: additionalMSBuildProps
        let s = Ionide.ProjInfo.ProjectLoader.loadProject file BinaryLogGeneration.Off additionalMSBuildProps 
        let t = readTag s
        if t = success.Tag then
            let p = readSuccess(s).[0] :?> LoadedProject
            match ProjectLoader.getLoadedProjectInfo (Path.GetFullPath file) [] p with
            | Result.Ok info ->
                let isNewStyle =
                    info.ProjectSdkInfo.TargetFrameworks |> List.exists (fun f -> f.StartsWith "netframework")  |> not
                
                let fscArgs = ProjectLoader.getFscArgs p |> Seq.toList
                
                let projs = 
                    info.ReferencedProjects |> List.map (fun p ->
                        let path = Path.Combine(projDir, p.RelativePath.Replace('\\', Path.DirectorySeparatorChar), p.ProjectFileName) |> Path.GetFullPath
                        (Some p.TargetFramework, path)   
                    )
                    
                Ok (ProjectInfo.ofFscArgs isNewStyle file projs fscArgs)
            | Result.Error e ->
                Error [e]
                    
        else
            printfn "asdjlashdjkadjoas"
            let e = readError(s).[0] :?> string
            Error [e]


let md5 = System.Security.Cryptography.MD5.Create()
let inline hash (str : string) = 
    md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string

type Path with

    static member TryGetRelative(path : string, ?ref : string) =
        let ref =
            match ref with
            | Some ref -> Path.GetFullPath ref
            | None -> Environment.CurrentDirectory
        let path = Path.GetFullPath path

        if path.StartsWith ref then
            path.Substring(ref.Length).TrimStart([|Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar|])
            |> Some
        else
            None

    static member Glob (patterns : #seq<string>, ?directory : string) =
        let directory = defaultArg directory Environment.CurrentDirectory
        let patterns = Seq.toList patterns

        let files, patterns =
            patterns |> List.partition (fun f -> File.Exists(Path.Combine(directory, f)))

        let negative =
            patterns 
            |> List.choose (fun p ->
                if p.StartsWith "!" then
                    DotNet.Globbing.Glob.Parse (p.Substring 1)
                    |> Some
                else
                    None
            )


        let positive =
            patterns 
            |> List.choose (fun p ->
                if not (p.StartsWith "!") then
                    DotNet.Globbing.Glob.Parse p
                    |> Some
                else
                    None
            )

        let matches (file : string) (pat : DotNet.Globbing.Glob) =
            pat.IsMatch file

        let includeFile (rel : string) =
            if positive |> List.exists (matches rel) then
                negative |> List.forall (matches rel >> not)
            else
                false

        if Directory.Exists directory then
            let all = Directory.GetFiles(directory, "*", SearchOption.AllDirectories)
            all
            |> Array.filter (fun file ->
                if Path.GetExtension(file).ToLower() = ".fsproj" then
                    match Path.TryGetRelative(file, directory) with
                    | Some rel -> includeFile rel
                    | None -> false
                else
                    false
            )
            |> Array.append (List.toArray files)
        else
            [||]



let msbuild (argv : string[]) =

    let mutable lenses = false
    let mutable debugHate = false
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

    for i in 0 .. argv.Length - 1 do
        match argv.[i].ToLower().Trim() with
        | "--verbose" | "-v" ->
            verbose <- true
        | "--lenses" ->
            if i + 1 < argv.Length && argv.[i+1].Trim().ToLower() = "true" then
                lenses <- true
        | "--designtime" ->
            if i + 1 < argv.Length then
                designTime <- argv.[i+1].Trim().ToLower() = "true"
        | "--debughate" ->
            if i + 1 < argv.Length then
                debugHate <- argv.[i+1].Trim().ToLower() = "true"
        | "--touchfiles" ->
            if i + 1 < argv.Length then
                touchFiles <- argv.[i+1].Trim().ToLower() = "true"
        | "--projectfile" ->
            if i + 1 < argv.Length then
                projectFile <- argv.[i+1].Trim()
        | "--defines" ->
            if i + 1 < argv.Length then
                defines <- argv.[i+1].Trim()
        | "--outputpath" ->
            if i + 1 < argv.Length then
                outputPath <- argv.[i+1].Trim()
        | "--outputtype" ->
            if i + 1 < argv.Length then
                outputType <- argv.[i+1].Trim()
        | "--files" ->
            if i + 1 < argv.Length then
                let refs = argv.[i+1].Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
                files <- refs
                
        | "--targetframework" ->
            if i + 1 < argv.Length then
                targetFramework <- argv.[i+1].Trim()
        | "--references" ->
            if i + 1 < argv.Length then
                let refs = argv.[i+1].Split([|';'|], StringSplitOptions.RemoveEmptyEntries)
                references <- refs
        | _ ->
            ()
 
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
    
    let run = Adaptify.runAsync checker outputPath designTime true lenses log false false projInfo
    
    let newFiles = 
        try Async.RunSynchronously(run)
        with e ->
            eprintfn "adaptify unexpected error: %A" e
            projInfo.files
        
  
    File.WriteAllLines(Path.Combine(outputPath, "adaptify.files"), newFiles)
    

[<EntryPoint>]
let main argv =
    if argv.[0] = "msbuild" then
        msbuild argv
        0
    else
        //let r = ProcessManagement.dotnet (Log.console true) ["--list-sdks"]

        if argv.Length <= 0 then
            printfn "Usage: adaptify [options] [projectfiles]"
            printfn "  Version: %s" selfVersion
            printfn ""
            printfn "Options:"
            printfn "  -f|--force    ignore caches and regenerate files"
            printfn "  -v|--verbose  verbose output"
            printfn "  -l|--lenses   generate aether lenses for records"
            printfn "  -c|--client   uses or creates a local server process"
            printfn "  -r|--release  generate release files"
            printfn "  --server      runs as server"
            printfn "  --killserver  kills the currently running server"
            Environment.Exit 1

        let local =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "--local"    
            )
            
        let killserver =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "--killserver"    
            )


        let force =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "-f" || a = "--force"    
            )
            
        let lenses =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "-l" || a = "--lenses"    
            )
            
        let release =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "-r" || a = "--release"    
            )
        let verbose =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "-v" || a = "--verbose"    
            )
             
        let client =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "-c" || a = "--client"    
            )
       
        let server =
            argv |> Array.exists (fun a -> 
                let a = a.ToLower().Trim()
                a = "--server"    
            )
            
        if killserver then
            let log = Log.console verbose
            Client.shutdown log
            0
        elif server then
            let log = 
                Log.ofList [
                    Log.console verbose
                    Log.file verbose ProcessManagement.logFile
                ]

            match Server.start log with
            | Some server ->
                let hasConsole = try ignore Console.WindowHeight; true with _ -> false
                if hasConsole then
                    startThread (fun () ->
                        try
                            
                            let mutable line = ""
                            while line <> "exit" do
                                line <- Console.ReadLine().Trim().ToLower()
                                if line = "" then Thread.Sleep 100
                            server.Stop()
                        with _ ->
                            ()
                    ) |> ignore

                server.WaitForExit()
                0

            | None ->
                1

            //if running then
            //    let rec wait() =
            //        let line = Console.ReadLine().Trim().ToLower()
            //        if line <> "exit" then wait()

            //    wait()
            //    cancel()
            //    0
            //else
            //    1
        else
            let log = Log.console verbose
            let projFiles = 
                argv 
                |> Array.filter (fun a -> not (a.StartsWith "-"))
                |> Path.Glob

            if projFiles.Length = 0 then
                log.error Range.range0 "1" "no input files given"
                exit 1

            log.debug Range.range0 "CWD: %s" Environment.CurrentDirectory
            log.debug Range.range0 "%d projects" projFiles.Length
            for f in projFiles do
                log.debug Range.range0 "%s" f

            let props =
                [
                    if release then "Configuration", "Release"
                ]

            let projectInfos = 
                projFiles |> Array.choose (fun projFile ->
                    match ProjectInfo.tryOfProject props projFile with
                    | Ok info -> 
                        Some info
                    | Error err ->
                        log.error Range.range0 "" "ERRORS in %s" projFile
                        for e in err do 
                            log.error Range.range0 "" "  %s" e
                        None
                )

            let topologicalSort (projects : ProjectInfo[]) : ProjectInfo[][] =
                if projects.Length <= 1 then
                    [|projects|]
                else
                    let all =
                        projects |> Array.map (fun p -> p.project, p) |> Map.ofArray

                    let dependencies =
                        projects |> Array.map (fun p ->
                            let deps = p.projRefs |> List.map snd |> List.filter (fun p -> Map.containsKey p all) |> Set.ofList
                            p.project, deps
                        )
                        |> Map.ofArray

                    log.info Range.range0 "topological sort"
                    let rec run (level : int) (m : Map<string, Set<string>>) =    
                        if Map.isEmpty m then
                            []
                        else
                            let noDependencies = m |> Map.filter (fun k v -> Set.isEmpty v) |> Map.toSeq |> Seq.map fst |> Set.ofSeq
                            log.info Range.range0 "  level %d:" level
                            for d in noDependencies do
                                log.info Range.range0 "    %s" d
                            let newMap = 
                                (m, noDependencies) 
                                ||> Set.fold (fun m d -> m |> Map.remove d)
                                |> Map.map (fun _ d -> Set.difference d noDependencies)

                            Set.toList noDependencies :: run (level + 1) newMap

                    run 0 dependencies
                    |> List.map (List.map (fun p -> all.[p]) >> List.toArray)
                    |> List.toArray




            projectInfos |> topologicalSort |> Array.iter (Array.iter (fun info ->
                let outputPath = 
                    match info.output with
                    | Some output -> Path.GetDirectoryName output
                    | None -> "."
                if client && not local then
                    Client.adaptify CancellationToken.None log info outputPath false (not force) lenses |> ignore<list<string>>
                else
                    let checker = newChecker()
                    Adaptify.run checker outputPath false (not force) lenses log local release info |> ignore<list<string>>
            ))

            0 
