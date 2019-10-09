open System
open System.IO
open FSharp.Compiler.SourceCodeServices
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
                        | None ->
                            failwith "no msbuild"
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
                    
let generateFilesForProject (checker : FSharpChecker) (info : ProjectInfo) =
        
    let args = ProjectInfo.toFscArgs info

    let projDir = Path.GetDirectoryName info.project
    let options =
        checker.GetProjectOptionsFromCommandLineArgs(info.project, List.toArray args, DateTime.Now)



    for file in info.files do
        let name = Path.GetFileNameWithoutExtension file

        let path = Path.Combine(projDir, file)
        let content = File.ReadAllText path
        let text = FSharp.Compiler.Text.SourceText.ofString content
        let (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options) |> Async.RunSynchronously
        
        match answer with
        | FSharpCheckFileAnswer.Succeeded res ->
            let adaptors = 

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

            if builder.Length > 0 then
                let file = Path.ChangeExtension(path, ".g.fs")
                
                let result = sprintf "//%s\r\n" (hash content) + builder.ToString()

                File.WriteAllText(file, result)



        | FSharpCheckFileAnswer.Aborted ->
            printfn "  aborted"

[<EntryPoint>]
let main argv =
    let projFiles = 
        if argv.Length > 0 then argv
        else [| Path.Combine(__SOURCE_DIRECTORY__, "..", "Examples", "NetCore", "NetCore.fsproj") |]


    let checker = 
        FSharpChecker.Create(
            projectCacheSize = 0,
            keepAssemblyContents = true, 
            keepAllBackgroundResolutions = false
        )

    for projFile in projFiles do
        match ProjectInfo.tryOfProject [] projFile with
        | Ok info ->
            generateFilesForProject checker info

        | Error err ->
            printfn "ERRORS"
            for e in err do 
                printfn "  %s" e




    0 
