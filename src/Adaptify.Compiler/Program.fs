open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range
open FSharp.Core
open Adaptify.Compiler

module ProjectInfo =
    open Dotnet.ProjInfo
    open Dotnet.ProjInfo.Inspect
    open Dotnet.ProjInfo.Workspace

    let rec private projInfo additionalMSBuildProps (file : string) =

        let projDir = Path.GetDirectoryName file
        let runCmd exePath args = Utils.runProcess ignore projDir exePath (args |> String.concat " ")
    
        let additionalMSBuildProps = ("GenerateDomainTypes", "false") :: additionalMSBuildProps

        let netcore =
            match file with
            | ProjectRecognizer.NetCoreSdk -> true
            | _ -> false
    
        let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
        if netcore && not(File.Exists(projectAssetsJsonPath)) then
            let (s, a) = runCmd "dotnet" ["restore"; sprintf "\"%s\"" file]
            if s <> 0 then 
                failwithf "Cannot find restored info for project %s" file
    
        let getFscArgs = 
            if netcore then
                Dotnet.ProjInfo.Inspect.getFscArgs
            else
                let asFscArgs props =
                    let fsc = Microsoft.FSharp.Build.Fsc()
                    Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
                Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> Ok)

        let results =
            let msbuildExec =
                let msbuildPath =
                    if netcore then Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
                    else 
                        let all = 
                            BlackFox.VsWhere.VsInstances.getWithPackage "Microsoft.Component.MSBuild" true

                        let probes =
                            [
                                @"MSBuild\Current\Bin\MSBuild.exe"
                                @"MSBuild\15.0\Bin\MSBuild.exe"
                            ]

                        let msbuild =
                            all |> List.tryPick (fun i ->
                                probes |> List.tryPick (fun p ->
                                    let path = Path.Combine(i.InstallationPath, p)
                                    if File.Exists path then Some path
                                    else None
                                )
                            )

                        match msbuild with
                        | Some msbuild -> Dotnet.ProjInfo.Inspect.MSBuildExePath.Path msbuild
                        | None -> failwith "no msbuild"
                Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

            let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

            let log = ignore

            let projs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs

            file
            |> Inspect.getProjectInfos log msbuildExec [projs; getFscArgs] additionalArgs

        netcore, results

    let tryOfProject (additionalMSBuildProps : list<string * string>) (file : string) =
        let (netcore, info) = projInfo additionalMSBuildProps file

        match info with
        | Ok info ->
            let mutable errors = []
            let fscArgs = 
                info |> List.tryPick (fun res ->
                    match res with
                    | Ok res ->
                        match res with
                        | GetResult.FscArgs args -> Some (Ok args)
                        | _ -> None
                    | Error err ->
                        errors <- err :: errors
                        None
                )

            match fscArgs with
            | Some args -> 
                match args with
                | Ok args -> Ok (ProjectInfo.ofFscArgs netcore file args)
                | Error e -> Error [sprintf "%A" e]
            | None -> 
                let errors = 
                    errors |> List.map (fun e ->
                        match e with
                        | MSBuildFailed (code, err) ->
                            sprintf "msbuild error %d: %A" code err
                        | MSBuildSkippedTarget ->
                            sprintf "msbuild skipped target"
                        | UnexpectedMSBuildResult res ->
                            sprintf "msbuild error: %s" res
                    )
                Error errors
        | Error e ->
            match e with
            | MSBuildFailed (code, err) ->
                Error [sprintf "msbuild error %d: %A" code err]
            | MSBuildSkippedTarget ->
                Error [sprintf "msbuild skipped target"]
            | UnexpectedMSBuildResult res ->
                Error [sprintf "msbuild error: %s" res]

let md5 = System.Security.Cryptography.MD5.Create()
let inline hash (str : string) = 
    md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string
    
    
let log (verbose : bool) =  

    let useColor (c : ConsoleColor) (f : unit -> 'a) =
        let o = Console.ForegroundColor
        Console.ForegroundColor <- c
        try f()
        finally Console.ForegroundColor <- o


    let writeRange (r : FSharp.Compiler.Range.range) =  
        if r <> range0 then
            Console.WriteLine(" @ {0} ({1},{2}--{3},{4})", r.FileName, r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
        else
            Console.WriteLine()
    { new ILog with
        member x.debug range fmt =
            
            fmt |> Printf.kprintf (fun str ->   
                if verbose then
                    Console.Write "> "
                    useColor ConsoleColor.DarkGray (fun () ->
                        Console.Write(str)
                    )
                    writeRange range
            )
            
        member x.info range fmt =
            fmt |> Printf.kprintf (fun str ->   
                Console.Write "> " 
                useColor ConsoleColor.Gray (fun () ->
                    Console.Write(str)
                )
                writeRange range
            )
            
        member x.warn range code fmt =
            fmt |> Printf.kprintf (fun str ->    
                Console.Write "> "
                useColor ConsoleColor.DarkYellow (fun () ->
                    Console.Write(str)
                )
                writeRange range
            )
            
        member x.error range code fmt =
            fmt |> Printf.kprintf (fun str ->    
                Console.Write "> "
                useColor ConsoleColor.Red (fun () ->
                    Console.Write(str)
                )
                writeRange range
            )
    }

[<EntryPoint>]
let main argv =
    
    if argv.Length <= 0 then
        printfn "Usage: adaptify [options] [projectfiles]"
        printfn ""
        printfn "Options:"
        printfn "  -f|--force    ignore caches and regenerate files"
        printfn "  -v|--verbose  verbose output"
        Environment.Exit 1

    let force =
        argv |> Array.exists (fun a -> 
            let a = a.ToLower().Trim()
            a = "-f" || a = "--force"    
        )
        
    let verbose =
        argv |> Array.exists (fun a -> 
            let a = a.ToLower().Trim()
            a = "-v" || a = "--verbose"    
        )

    let log = log verbose

    let projFiles = 
        if argv.Length > 0 then argv |> Array.filter (fun a -> not (a.StartsWith "-"))
        else [| Path.Combine(__SOURCE_DIRECTORY__, "..", "Examples", "Library", "Library.fsproj") |]


    let checker = 
        FSharpChecker.Create(
            projectCacheSize = 0,
            keepAssemblyContents = true, 
            keepAllBackgroundResolutions = false
        )

    for projFile in projFiles do
        match ProjectInfo.tryOfProject [] projFile with
        | Ok info ->
            Adaptify.run (Some checker) (not force) log info |> ignore

        | Error err ->
            log.error range0 "" "ERRORS"
            for e in err do 
                log.error range0 "" "  %s" e




    0 
