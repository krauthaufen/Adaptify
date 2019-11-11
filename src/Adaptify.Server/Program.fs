// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.IO.Pipes
open System.Threading
open System.Threading.Tasks
open Adaptify.Compiler
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range

let checker = FSharpChecker.Create(keepAssemblyContents = true)

let md5 = System.Security.Cryptography.MD5.Create()
let inline hash (str : string) = 
    md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string

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

type ListLog() =
    let messages = System.Collections.Generic.List<Message>()



    member x.debug range fmt = fmt |> Printf.kprintf (fun str -> Console.WriteLine(str); messages.Add { time = DateTime.Now; range = range; code = None; severity = Severity.Debug; message = str })
    member x.info range fmt = fmt |> Printf.kprintf (fun str -> Console.WriteLine(str); messages.Add { time = DateTime.Now; range = range; code = None; severity = Severity.Info; message = str })
    member x.warn range code fmt = fmt |> Printf.kprintf (fun str -> Console.WriteLine(str); messages.Add { time = DateTime.Now; range = range; code = Some code; severity = Severity.Warn; message = str })
    member x.error range code fmt = fmt |> Printf.kprintf (fun str -> Console.WriteLine(str); messages.Add { time = DateTime.Now; range = range; code = Some code; severity = Severity.Error; message = str })

    interface ILog with
        member x.debug range fmt = x.debug range fmt
        member x.info range fmt = x.info range fmt
        member x.warn range code fmt = x.warn range code fmt
        member x.error range code fmt = x.error range code fmt

    member x.ToArray() = messages.ToArray()
    member x.toList() = messages |> Seq.toList

type Task with
    member x.Join() =
        try x.Wait()
        with _ -> ()
        x.Status
        
type Task<'a> with
    member x.Join() =
        try x.Wait()
        with _ -> ()
        x.Status

let modelTypeRx = System.Text.RegularExpressions.Regex @"ModelType(Attribute)?"

let generateFilesForProject (checker : FSharpChecker) (rebuild : bool) (projInfo : ProjectInfo) =
    let log = ListLog()

    let projectFile = projInfo.project

    let projHash = ProjectInfo.computeHash projInfo
    let cacheFile = Path.Combine(Path.GetDirectoryName projectFile, ".adaptifycache")
    let cache = CacheFile.tryRead cacheFile

    let projectChanged = 
        match cache with
        | Some cache -> rebuild || projHash <> cache.projectHash
        | None -> true

    let oldHashes = 
        match cache with
        | Some c -> c.fileHashes
        | None -> Map.empty

    let mutable newHashes = Map.empty
                    
    let projDir = Path.GetDirectoryName projectFile
    let newFiles = System.Collections.Generic.List<string>()

    let md5 = System.Security.Cryptography.MD5.Create()
    let inline hash (str : string) = 
        md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes str) |> System.Guid |> string
                          
    let options = 
        lazy (
            checker.GetProjectOptionsFromCommandLineArgs(
                projectFile, 
                ProjectInfo.toFscArgs projInfo |> List.toArray
            )
        )
    for file in projInfo.files do
        if not (file.EndsWith ".g.fs") then
            let path = Path.Combine(projDir, file)
            let content = File.ReadAllText path
            let fileHash = hash content

            let mayDefineModelTypes = modelTypeRx.IsMatch content


            let needsUpdate =
                if not mayDefineModelTypes then
                    newFiles.Add file
                    newHashes <- Map.add file (fileHash, false) newHashes
                    false
                elif projectChanged then 
                    true
                else
                    match Map.tryFind file oldHashes with
                    | Some (oldHash, hasModels) ->
                        if oldHash = fileHash then
                            if hasModels then
                                let generated = Path.ChangeExtension(path, ".g.fs")

                                let readGeneratedHash (file : string) = 
                                    use s = File.OpenRead file
                                    use r = new StreamReader(s)
                                    try
                                        let inputHash = 
                                            let line = r.ReadLine()
                                            if line.StartsWith "//" then line.Substring 2
                                            else ""

                                        let selfHash =
                                            let line = r.ReadLine()
                                            if line.StartsWith "//" then line.Substring 2
                                            else ""

                                        let restHash = 
                                            let str = r.ReadToEnd()
                                            hash str

                                        if selfHash = restHash then
                                            inputHash
                                        else
                                            ""
                                    with _ ->
                                        ""

                                if File.Exists(generated) && readGeneratedHash generated = fileHash then
                                    newFiles.Add file
                                    newFiles.Add generated
                                    newHashes <- Map.add file (fileHash, true) newHashes
                                    false
                                else
                                    true
                            else
                                newFiles.Add file
                                newHashes <- Map.add file (fileHash, false) newHashes
                                false
                        else
                            true

                    | None ->   
                        true
                                
            if needsUpdate then
                log.info range0 "[Adaptify] update file %s" file
                let text = FSharp.Compiler.Text.SourceText.ofString content
                let (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options.Value) |> Async.RunSynchronously
        
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

                    newHashes <- Map.add file (fileHash, not (List.isEmpty definitions)) newHashes
                    newFiles.Add file
                    match definitions with
                    | [] ->
                        log.info range0 "[Adaptify] no models in %s" file
                    | defs ->
                        let file = Path.ChangeExtension(path, ".g.fs")

                        let content = TypeDefinition.toFile defs
                        let result = sprintf "//%s\r\n//%s\r\n" fileHash (hash content) + content

                        File.WriteAllText(file, result)
                        newFiles.Add file
                        log.info range0 "[Adaptify] generated %s" file

                | FSharpCheckFileAnswer.Aborted ->
                    log.error range0 "587" "[Adaptify] could not parse %s" file
                    ()
            else
                log.info range0 "[Adaptify] skipping %s" file

    CacheFile.save { projectHash = projHash; fileHashes = newHashes } cacheFile
    
    Seq.toList newFiles, log.toList()

let run (rebuild : bool) (info : ProjectInfo) =
    let files, messages = generateFilesForProject checker rebuild info
    //let messages = { severity = Severity.Warn; range = range0; code = Some "321"; message = sprintf "%A" files; time = DateTime.Now } :: messages
    { info with files = files}, messages

let server (cmd : Command) : Async<Reply> =
    async {
        do! Async.SwitchToThreadPool()
        match cmd with
        | Command.AdaptifyFile(rebuild, file, props) ->
            printfn "adaptify %s" (Path.GetFileName file)
            match ProjectInfo.tryOfProject props file with
            | Result.Ok info -> 
                let newInfo = run rebuild info
                return Reply.Success newInfo
            | Result.Error errors ->
                let errors = errors |> List.map (fun e -> { time = DateTime.Now; code = Some "213"; message = e; severity = Severity.Error; range = range0 })
                return Reply.Error(errors)
        | Command.AdaptifyInfo(rebuild, info) ->
            printfn "adaptify %s" (Path.GetFileName info.project)
            let newInfo = run rebuild info
            return Reply.Success newInfo
            

    }

let mutable lastCommand = DateTime.Now

let runServer (pipeName : string) =
    async {
        let pipe = 
            new NamedPipeServerStream(
                pipeName, PipeDirection.InOut, NamedPipeServerStream.MaxAllowedServerInstances, 
                PipeTransmissionMode.Message, PipeOptions.Asynchronous
            )
        try
            try
                let! ct = Async.CancellationToken
                while not ct.IsCancellationRequested do
                    do! pipe.WaitForConnectionAsync(ct) |> Async.AwaitTask
                    match! pipe.ReceiveCommandAsync() with
                    | Some cmd ->
                        lastCommand <- DateTime.Now
                        let! reply = server cmd
                        if pipe.IsConnected then
                            do! pipe.SendAsync reply
                    | None ->
                        if pipe.IsConnected then
                            let msg = { time = DateTime.Now; range = range0; message = "could not parse command"; code = Some "353"; severity = Severity.Error }
                            do! pipe.SendAsync (Reply.Error [msg])
                    pipe.Disconnect()
            with e -> 
                printfn "error %A" e
        finally 
            pipe.Dispose()
    }


[<EntryPoint>]
let main argv =
    let pipeName =
        if argv.Length > 0 then argv.[0]
        else "AdaptifyServer"

    use cancel = new CancellationTokenSource()
    let task = Async.StartAsTask(runServer pipeName, cancellationToken = cancel.Token)

    let timer = 
        let tick _ = 
            if DateTime.Now - lastCommand > TimeSpan.FromMinutes 5.0 then
                cancel.Cancel()

        new Timer(TimerCallback(tick), null, 10000, 10000)

    
    Task.Run (fun () ->
        printfn "press enter to exit..."
        Console.ReadLine() |> ignore
        cancel.Cancel()
    ) |> ignore

    match task.Join() with
    | TaskStatus.Faulted -> printfn "faulted: %A" task.Exception
    | _ -> ()
    timer.Dispose()

    0 // return an integer exit code
