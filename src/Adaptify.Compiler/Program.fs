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
    
    
let log =  

    //let tryReadRange (r : FSharp.Compiler.Range.range) =    
    //    match Map.tryFind r.FileName fileContent with
    //    | Some c ->
    //        if r.StartLine = r.EndLine then
    //            let line = c.[r.StartLine - 1]
    //            line.Substring(r.StartColumn - 1, 1 + r.EndColumn - r.StartColumn) |> Some
    //        else
    //            let arr = c.[r.StartLine - 1 .. r.EndLine - 1]
    //            arr.[0] <- arr.[0].Substring(r.StartColumn - 1)
    //            arr.[arr.Length-1] <- arr.[arr.Length-1].Substring(r.EndColumn - 1)
    //            String.concat "\r\n" arr |> Some
    //    | None ->
    //        None

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

let generateFilesForProject (checker : FSharpChecker) (info : ProjectInfo) =
        
    let args = ProjectInfo.toFscArgs info

    let projDir = Path.GetDirectoryName info.project
    let options =
        checker.GetProjectOptionsFromCommandLineArgs(info.project, List.toArray args, DateTime.Now)


    for file in info.files do
        if not (file.EndsWith ".g.fs") then
            log.debug range0 "checking %s" file 
            let path = Path.Combine(projDir, file)
            let content = File.ReadAllText path
            let text = FSharp.Compiler.Text.SourceText.ofString content
            let (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options) |> Async.RunSynchronously

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

                let definitions = 
                    entities 
                    |> List.choose (TypeDef.ofEntity log)
                    |> List.map (fun l -> l.Value)
                    |> List.collect (TypeDefinition.ofTypeDef log [])

                match definitions with
                | [] ->
                    log.info range0 "%s defines no model types" file 
                    ()
                | defs ->
                    let modelTypes = 
                        defs |> Seq.map (fun d ->   
                            match Scope.fullName d.scope with
                            | Some s -> sprintf "%s.%s" s d.name
                            | None -> d.name
                        )
                    log.info range0 "%s defines model types: %s" file (modelTypes |> String.concat ", " |> sprintf "[%s]")
                    let generated = TypeDefinition.toFile defs
                    let file = Path.ChangeExtension(path, ".g.fs")
                    let result = sprintf "//%s\r\n//%s\r\n" (hash content) (hash generated) + generated
                    File.WriteAllText(file, result)



            | FSharpCheckFileAnswer.Aborted ->
                log.warn range0 "587" "aborted"

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
