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
open Microsoft.Build.Definition
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
        let runCmd exePath args = Utils.runProcess ignore projDir exePath (args |> String.concat " ")
    
        let dotnet = Ionide.ProjInfo.Paths.dotnetRoot.Value
        let path = Init.init (DirectoryInfo Environment.CurrentDirectory) dotnet
        
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



[<EntryPoint>]
let main argv = 

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

    let addToProject =
        argv |> Array.exists (fun a -> 
            let a = a.ToLower().Trim()
            a = "--addtoproject"     
        )

    if addToProject && not local then
        printfn "--addToProject only available in local mode."

    let addToProject = addToProject && local
        
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
                let newFiles, genFiles = Adaptify.run checker outputPath false (not force) lenses log local release info
                if addToProject then
                    let genFiles = genFiles |> Seq.toArray
                    if genFiles.Length <> 0 then
                        try
                            for (modelFile, genFile) in genFiles do
                                log.info Range.range0 "[PatchProject] (%s) trying to add %s" info.project genFile  
                            
                            Adaptify.PatchProject.patchProject log info.project genFiles
                        with e -> 
                            log.error Range.range0 "" "could not add gen files to project: %s" info.project
                            log.error Range.range0 "" "the error was: %A" e
        ))

        0 
