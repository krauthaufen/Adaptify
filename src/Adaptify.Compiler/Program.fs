﻿open System
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

[<EntryPoint>]
let main argv = 
    let log = Log.console true
    //let close = Server.startTcp log


    if argv.Length <= 0 then
        printfn "Usage: adaptify [options] [projectfiles]"
        printfn "  Version: %s" selfVersion
        printfn ""
        printfn "Options:"
        printfn "  -f|--force    ignore caches and regenerate files"
        printfn "  -v|--verbose  verbose output"
        printfn "  -l|--lenses   generate aether lenses for records"
        printfn "  -c|--client   uses or creates a local server process"
        printfn "  --server      runs as server"
        Environment.Exit 1


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

    if server then
        match Process.readPort 5000 with
        | Some _port ->
            ()
        | None ->
            let log = 
                Log.ofList [
                    Log.console verbose
                    //Log.file verbose Process.logFile
                ]
            let cancel = Server.startTcp log 

            let rec wait() =
                let line = Console.ReadLine().Trim().ToLower()
                if line <> "exit" then wait()

            wait()
            cancel()


        0
    else

        let projects =
            [|
                @"C:\Users\Schorsch\Development\aardvark.media\src\Aardvark.UI.Primitives\Aardvark.UI.Primitives.fsproj"
                "C:\Users\Schorsch\Development\Adaptify\src\Examples\Library\Library.fsproj"
                "C:\Users\Schorsch\Development\Adaptify\src\Examples\NetCore\NetCore.fsproj"
                "C:\Users\Schorsch\Development\Adaptify\src\Examples\NetFramework\NetFramework.fsproj"
                "C:\Users\Schorsch\Development\Adaptify\src\Examples\NetFrameworkOld\NetFrameworkOld.fsproj"
            |]

        projects 
        |> Array.Parallel.choose (fun f ->
            match ProjectInfo.tryOfProject [] f with
            | Ok info -> Some info
            | Error err ->
                log.error range0 "" "%A" err
                None
        )
        |> Array.Parallel.iter (fun info ->
            Client.adaptifyTcp log info false true |> ignore
        )
        Environment.Exit 0

        let log = Log.console verbose
        let projFiles = argv |> Array.filter (fun a -> not (a.StartsWith "-"))

        let projectInfos = 
            projFiles |> Array.choose (fun projFile ->
                match ProjectInfo.tryOfProject [] projFile with
                | Ok info -> 
                    Some info
                | Error err ->
                    log.error range0 "" "ERRORS in %s" projFile
                    for e in err do 
                        log.error range0 "" "  %s" e
                    None
            )


        projectInfos |> Array.Parallel.iter (fun info ->
            if client then
                Client.adaptifyTcp log info (not force) lenses |> ignore
            else
                Adaptify.run None (not force) lenses log info |> ignore
        )

        0 
