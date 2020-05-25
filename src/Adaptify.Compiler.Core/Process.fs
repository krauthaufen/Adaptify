namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.Range
open System.Reflection
open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading

type IPCLock(fileName : string) =
    let mutable stream : FileStream = null
    let lockObj = obj()
    let mutable isEntered = 0

    member x.Enter() =
        Monitor.Enter lockObj

        isEntered <- isEntered + 1
        if isEntered = 1 then
            File.ensureDirectory fileName
            let mutable entered = false
            while not entered do
                try
                    stream <- new FileStream(fileName, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.WriteThrough)
                    entered <- true
                with _ ->
                    Threading.Thread.Sleep 5

    member x.Exit() =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        isEntered <- isEntered - 1
        if isEntered = 0 then
            stream.Dispose()
            stream <- null

        Monitor.Exit lockObj

    member x.Write(data : byte[], offset : int, count : int) =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        stream.SetLength(int64 count)
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        stream.Write(data, offset, count)
        stream.Flush()

    member x.Write(str : string) =
        let bytes = System.Text.Encoding.UTF8.GetBytes str
        x.Write(bytes, 0, bytes.Length)

    member x.ReadAll() =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        let arr = Array.zeroCreate (int stream.Length)
        stream.Flush()
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        
        let mutable offset = 0
        let mutable rem = arr.Length
        while rem > 0 do
            let r = stream.Read(arr, offset, rem)
            rem <- rem - r
            offset <- offset + r

        arr

    member x.ReadString() =
        let data = x.ReadAll()
        System.Text.Encoding.UTF8.GetString data

    member x.Dispose() = 
        lock lockObj (fun () ->
            if isEntered > 0 then 
                isEntered <- 0
                stream.Flush()
            if not (isNull stream) then
                stream.Dispose()
                stream <- null
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()



module TCP =
    open System.Net
    open System.Net.Sockets
    open System.Threading
    open System.Threading.Tasks

    let private attempt (timeout : int) (create : CancellationToken -> Task) =
        let timeout = Task.Delay(timeout)
        let cancel = new CancellationTokenSource()

        let rec retry(level : int) =
            async {
                if timeout.IsCompleted || level > 20 then
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
                                return! retry(level + 1)
                        else
                            cancel.Cancel()
                            return false
                    | None ->
                        do! Async.Sleep 0
                        return! retry(level + 1)
            }

        retry 0

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


module ProcessManagement = 
    open TCP
    open System.Threading
    open System.Threading.Tasks

    let private executableExtension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe"
        else ""

    let private executableName =
        "adaptify" + executableExtension

    let directory() =
        let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "adaptify", string selfVersion)
        Directory.ensure path

    let private ipc = 
        let path = Path.Combine(directory(), "process.lock")
        new IPCLock(path)
        
    let logFile = Path.Combine(directory(), "log.txt")

    let locked (action : unit -> 'r) =
        ipc.Enter()
        try action()
        finally ipc.Exit()

    let private (|Int32|_|) (str : string) =
        match Int32.TryParse str with
        | (true, v) -> Some v
        | _ -> None


    let readProcessAndPort () =
        locked (fun () -> 
            let parts = ipc.ReadString().Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            match parts with
            | [| Int32 pid; Int32 port |] ->
                if pid = 0 then 
                    None
                else
                    try
                        let proc = Process.GetProcessById pid
                        if not proc.HasExited then
                            Some(proc, port)
                        else
                            None
                    with _ ->
                        None
            | _ ->
                None
        )

    let private testServer (port : int) (timeout : int) =
        try 
            use c = new System.Net.Sockets.TcpClient()
            let connected = c.TryConnectAsync(Net.IPAddress.Loopback, port, timeout) |> Async.RunSynchronously
            c.Dispose()
            if connected then
                true
            else
                false
        with e ->
            false

    let trySetPort (port : int) =
        let pid = Process.GetCurrentProcess().Id
        locked (fun () ->
            match readProcessAndPort () with
            | Some(proc, otherPort) ->
                if testServer otherPort 500 then
                    Choice2Of2(proc.Id, otherPort)
                else
                    proc.Kill()
                    ipc.Write(sprintf "%d;%d" pid otherPort)
                    Choice1Of2 ()
            | None -> 
                ipc.Write(sprintf "%d;%d" pid port)
                Choice1Of2 ()
        )

    let releasePort (port : int) =
        let pid = Process.GetCurrentProcess().Id
        locked (fun () ->
            let parts = ipc.ReadString().Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            match parts with
            | [| Int32 fpid; Int32 fport |] when fpid = pid && fport = port ->
                ipc.Write("0;0")
            | _ ->
                ()
        )

    let private dotnet (log : ILog) (args : list<string>) =  
        let output = System.Collections.Generic.List<string>()
        let proc = 
            Process.tryStart {
                file = "dotnet"
                args = args
                output = OutputMode.Custom (fun s l ->
                    lock output (fun () -> output.Add l)
                )
                workDir = ""
            }
        match proc with
        | Some proc ->
            proc.WaitForExit()
            if proc.ExitCode <> 0 then
                for line in output do
                    log.debug range0 "dotnet: %s" line
                failwith "dotnet failed"
        | None ->
            failwith "dotnet failed"
            

    let startAdaptifyServer (log : ILog) =
        let entryPath = try Assembly.GetEntryAssembly().Location with _ -> "foo.dll"
        let executablePath = try Process.GetCurrentProcess().MainModule.FileName with _ -> "bar"

        if Path.GetFileName(executablePath) = executableName then
            log.info range0 "starting self-executable"
            Process.startDaemon executablePath ["--server"]

        elif Path.GetFileName(entryPath) = "adaptify.dll" then  
            log.info range0 "starting self-dll" 
            Process.startDaemon "dotnet" [sprintf "\"%s\"" entryPath; "--server"]

        else    
            let toolPath = Path.Combine(directory(), executableName)
            if not (File.Exists toolPath) then
                log.info range0 "adaptify tool %s not found (installing)" selfVersion

                while not (File.Exists toolPath) do
                    locked (fun () ->
                        try
                            dotnet log [ 
                                "tool"; "install"; "adaptify"
                                "--no-cache"
                                "--tool-path"; sprintf "\"%s\"" (directory())
                                "--version"; sprintf "[%s]" selfVersion
                            ]
                            log.info range0 "installed tool at %s" toolPath
                        with _ ->
                            ()
                    )
            
            match readProcessAndPort() with
            | None ->
                log.info range0 "starting %s" toolPath 
                Process.startDaemon toolPath ["--server"]
            | Some(_otherProc, _) ->
                ()
                

