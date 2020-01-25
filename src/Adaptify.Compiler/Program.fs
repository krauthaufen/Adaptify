open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range
open FSharp.Core
open Adaptify.Compiler
open System.Runtime.CompilerServices

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

module IPCTest =
    open System.Diagnostics
    open System.Reflection
    open System.IO.MemoryMappedFiles
    open Microsoft.FSharp.NativeInterop
    open System.Threading

    let run (args : string[]) =
        let consoleLock = obj()
        let run (arg : int) =
            let name = sprintf "%02d" arg
            let dll = Assembly.GetEntryAssembly().Location
            let info = ProcessStartInfo("dotnet", dll + " " + name + " --server", UseShellExecute = false, CreateNoWindow = true, RedirectStandardOutput = true, RedirectStandardError = true)
            let proc = new Process(StartInfo = info)

            proc.EnableRaisingEvents <- true
            proc.OutputDataReceived.Add(fun e -> 
                if not (String.IsNullOrWhiteSpace e.Data) then
                    lock consoleLock (fun () ->
                        Console.WriteLine("{0}{1}", name, e.Data)
                    )
            )
            proc.ErrorDataReceived.Add(fun e -> 
                if not (String.IsNullOrWhiteSpace e.Data) then
                    lock consoleLock (fun () ->
                        Console.WriteLine("{0}{1}", name, e.Data)
                    )
            )

            if not (proc.Start()) then printfn "could not start process"
            proc.BeginOutputReadLine()
            proc.BeginErrorReadLine()

            name, proc

        if args.Length = 0 then
            let cnt = 24
            let all = Array.init cnt run

            let mutable line = Console.ReadLine()
            while line <> "exit" do
                line <- Console.ReadLine()

            Client.shutdown Log.empty

            for (name, a) in all do 
                if not a.HasExited then
                    try a.Kill()
                    with _ -> ()
                    printfn "%s killed" name
            Environment.Exit 0
               
      

let startThread (run : unit -> unit) =
    let thread = System.Threading.Thread(System.Threading.ThreadStart(run), IsBackground = true)
    thread.Start()
    thread


module TCP =
    open System.Net
    open System.Net.Sockets
    open System.Threading
    open System.Threading.Tasks

    let private attempt (timeout : int) (create : CancellationToken -> Task) =
        let timeout = Task.Delay(timeout)
        let cancel = new CancellationTokenSource()

        let rec retry() =
            async {
                if timeout.IsCompleted then
                    cancel.Cancel()
                    return false
                else
                    let run = 
                        try create cancel.Token |> Some
                        with _ -> None
                    match run with
                    | Some run -> 
                        let! finished = Task.WhenAny(run, timeout) |> Async.AwaitTask
                        if finished = run then
                            try 
                                finished.Wait()
                                return true
                            with _ ->
                                return! retry()
                        else
                            cancel.Cancel()
                            return false
                    | None ->
                        do! Async.Sleep 0
                        return! retry()
            }

        retry()

    let private withTimeout (timeout : int) (start : Async<'a>) =
        let cancel = new CancellationTokenSource()
        let run = Async.StartAsTask(start, cancellationToken = cancel.Token)
        let timeout = Task.Delay(timeout)

        async {
            let! fin = Task.WhenAny((run :> Task), timeout) |> Async.AwaitTask
            if fin = timeout then
                cancel.Cancel()
                return raise <| TimeoutException()
            else
                let! res = Async.AwaitTask run
                return res
        }


    type TcpClient with
        member x.TryConnectAsync(address : IPAddress, port : int, ?timeout : int, ?ct : CancellationToken) =
            match timeout with
            | Some timeout when timeout > 0 ->
                attempt timeout (fun ct ->
                    x.ConnectAsync(address, port)
                )
            | _ ->
                async {
                    try
                        do! x.ConnectAsync(address, port) |> Async.AwaitTask
                        return true
                    with _ ->
                        return false
                }
         
    type NetworkStream with
        member x.ReadForSure(arr : byte[], offset : int, len : int) =
            async {
                let! ct = Async.CancellationToken
                let mutable remaining = len
                let mutable offset = offset
                while remaining > 0 do
                    let! read = x.ReadAsync(arr, offset, remaining, ct) |> Async.AwaitTask
                    if read <= 0 then raise <| ObjectDisposedException("stream")
                    remaining <- remaining - read
                    offset <- offset + read
            }

        member x.ReadMessage() =
            async {
                let lenBuffer = Array.zeroCreate 4
                do! x.ReadForSure(lenBuffer, 0, 4)
                let len = BitConverter.ToInt32(lenBuffer, 0)

                let message = Array.zeroCreate len
                do! x.ReadForSure(message, 0, len)

                return message
            }

        member x.WriteMessage(message : byte[]) =
            async {
                let! ct = Async.CancellationToken
                let lenBuffer = BitConverter.GetBytes message.Length
                do! x.WriteAsync(lenBuffer, 0, 4, ct) |> Async.AwaitTask
                do! x.WriteAsync(message, 0, message.Length, ct) |> Async.AwaitTask
            }


    type private TaskSet() =
        let tasks = System.Collections.Generic.Dictionary<Task, Task>()

        let rem task =
            lock tasks (fun () ->
                tasks.Remove task |> ignore
            )
            
        let add task =
            lock tasks (fun () ->
                tasks.[task] <- task.ContinueWith (fun _ -> rem task)
            )

        member x.Add(task : Task) =
            add task

        member x.Wait() =
            let all = lock tasks (fun () -> Seq.toArray tasks.Values)
            if all.Length > 0 then
                try 
                    Task.WaitAll(all)
                    x.Wait()
                with _ -> 
                    ()

        member x.WaitAsync() =
            async {
                let all = lock tasks (fun () -> Seq.toArray tasks.Values)
                if all.Length > 0 then
                    try
                        do! Task.WhenAll(all) |> Async.AwaitTask
                        return! x.WaitAsync()
                    with _ ->
                        ()
            }

    type Server(address : IPAddress, processMessage : Server -> byte[] -> Async<byte[]>) as this =
        let listener = TcpListener(address, 0)
        do listener.Start()

        let listenLock = obj()
        let mutable listening = false
        let mutable shouldListen = true
        let port = (unbox<IPEndPoint> listener.LocalEndpoint).Port

        let runClient (c : TcpClient) =
            c.NoDelay <- true
            async {
                try
                    do! Async.SwitchToThreadPool()
                    use s = c.GetStream()
                    try
                        let! msg = s.ReadMessage() |> withTimeout 2000
                        do! Async.SwitchToThreadPool()
                        let! reply = processMessage this msg
                        do! s.WriteMessage reply
                        do! s.FlushAsync() |> Async.AwaitTask
                    with _ ->
                        ()
                finally
                    try c.Dispose()
                    with _ -> ()
            }

        let tasks = TaskSet()

        let stopListening() =
            if shouldListen then
                lock listenLock (fun () ->
                    if shouldListen then
                        shouldListen <- false
                        if listening then
                            try listener.Stop()
                            with _ -> ()
                )

        let run =
            async {
                try
                    while shouldListen do
                        if not listening then
                            lock listenLock (fun () ->
                                listening <- true
                                Monitor.PulseAll listenLock
                            )
                        let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                        try tasks.Add (runClient client |> Async.StartAsTask)
                        with _ -> ()
                with _ ->
                    ()
                    
                do! Async.SwitchToThreadPool()
                lock listenLock (fun () ->
                    listening <- false
                    Monitor.PulseAll listenLock
                )
                printfn "stopped listening"
                do! tasks.WaitAsync()
                printfn "clients done"
                listener.Stop()
            }

        let task = Async.StartAsTask run

        do
            lock listenLock (fun () ->
                while not listening do
                    Monitor.Wait listenLock |> ignore
            )
            printfn "server running"

        member x.WaitForListenerClosed() =
            lock listenLock (fun () ->
                while listening do
                    Monitor.Wait listenLock |> ignore
            )

        member x.WaitForExit() =
            try task.Wait()
            with _ -> ()

        member x.Stop() =
            stopListening()

        member x.Port = port


    module Client =
        let tryGetAsync (address : IPAddress) (port : int) (timeout : int) (message : byte[]) =
            async {
                use c = new TcpClient()
                c.NoDelay <- true
                match! c.TryConnectAsync(address, port, 0) with
                | true -> 
                    try
                        use s = c.GetStream()
                        do! s.WriteMessage message
                        do! s.FlushAsync() |> Async.AwaitTask
                        let! res = s.ReadMessage() |> withTimeout timeout
                        return Some res
                    with _ ->
                        return None
                | false ->
                    return None

            }
            
        let tryGet (address : IPAddress) (port : int) (timeout : int) (message : byte[]) =
            tryGetAsync address port timeout message |> Async.RunSynchronously

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let busySleep500() = 
        let mutable sum = 0
        for i in 1 .. 2 do
            for i in i .. 1 <<< 30 do
                sum <- sum + i
        0 &&& (sum % 2)

    let runTest () =
        let reply (_server : Server) (msg : byte[]) =
            async {
                do! Async.SwitchToThreadPool()
                let id = busySleep500()
                msg.[0] <- msg.[0] + byte id
                return msg
            }
        
        let server = Server(IPAddress.Loopback, reply)
        
        let thread = 
            startThread <| fun () ->
                try
                    let valid =
                        Seq.init 40 (fun i ->
                            async {
                                match! Client.tryGetAsync IPAddress.Loopback server.Port 120000 [|byte i|] with
                                | Some [| v |] when v = byte i ->
                                    return 1
                                | _ ->
                                    return 0
                            }
                        ) 
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> Array.sum

                    printfn "valid: %d" valid
                with 
                | :? ThreadInterruptedException ->
                    printfn "interrupted"
                | e ->
                    printfn "failed: %A" e
        
        Thread.Sleep(100)
        thread.Interrupt()
        //printfn "press any key to stop listening"
        //Console.ReadLine() |> ignore
        printfn "stopping server"
        server.Stop()
        server.WaitForExit()
        thread.Interrupt

        //thread.Join()

[<EntryPoint>]
let main argv = 

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
        printfn "  --killserver  kills the currently running server"
        Environment.Exit 1

        
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
                Log.file verbose Process.logFile
            ]

        match Server.start log with
        | Some server ->
            let hasConsole = try ignore Console.WindowHeight; true with _ -> false
            if hasConsole then
                startThread (fun () ->
                    let rec run() =
                        let line = Console.ReadLine().Trim().ToLower()
                        if line <> "exit" then run()
                    run()
                    server.Stop()
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
                Client.adaptify log info (not force) lenses |> ignore
            else
                let checker = newChecker()
                Adaptify.run checker (not force) lenses log info |> ignore
        )

        0 
