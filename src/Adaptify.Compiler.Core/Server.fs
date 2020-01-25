namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range
open System.IO.Pipes
open System.Threading
open System.Security.Principal

module private IPC = 
    [<RequireQualifiedAccess>]
    type MessageKind =
        | Debug
        | Info
        | Warning of string
        | Error of string

    type AdaptifyMessage =
        {
            file        : string
            kind        : MessageKind
            startLine   : int
            startCol    : int
            endLine     : int
            endCol      : int
            message     : string
        }

    type Command =
        | Adaptify of project : ProjectInfo * useCache : bool * lenses : bool
        | Exit

    module Command =
        let pickle (cmd : Command) =
            use ms = new MemoryStream()
            use w = new BinaryWriter(ms)
            match cmd with
            | Adaptify(p, c, l) ->  
                w.Write 1
                ProjectInfo.pickleTo ms p
                w.Write(c)
                w.Write(l)
            | Exit ->
                w.Write 2
            ms.ToArray()

        let tryUnpickle (data : byte[]) =
            use ms = new MemoryStream(data)
            use r = new BinaryReader(ms)
            match r.ReadInt32() with
            | 1 ->
                match ProjectInfo.tryUnpickleOf ms with
                | Some p ->
                    let c = r.ReadBoolean()
                    let l = r.ReadBoolean()
                    Some (Adaptify(p, c, l))
                | None ->
                    None
            | 2 ->
                Some Exit
            | _ ->
                None

    type Reply =
        | Success of files : list<string> * messages : list<AdaptifyMessage>
        | Fatal of message : string
        | Shutdown

    module Reply =
        let pickle (r : Reply) =
        
            use ms = new MemoryStream()
            use w = new BinaryWriter(ms)
            match r with
            | Success(files, messages) ->
                w.Write(1)
                let files = List.toArray files
                let messages = List.toArray messages
                w.Write(files.Length)
                for f in files do 
                    w.Write f

                w.Write(messages.Length)
                for m in messages do
                    match m.kind with
                    | MessageKind.Debug -> w.Write 0
                    | MessageKind.Info -> w.Write 1
                    | MessageKind.Warning code -> w.Write 2; w.Write code
                    | MessageKind.Error code -> w.Write 3; w.Write code
                    w.Write(m.file)
                    w.Write(m.startLine)
                    w.Write(m.startCol)
                    w.Write(m.endLine)
                    w.Write(m.endCol)
                    w.Write(m.message)
            | Fatal str ->
                w.Write(2)
                w.Write(str)
            | Shutdown ->
                w.Write(3)

            
            ms.ToArray()

        let tryUnpickle (data : byte[]) =
            use ms = new MemoryStream(data)
            use r = new BinaryReader(ms)
            match r.ReadInt32() with
            | 1 ->
                let fc = r.ReadInt32()
                let files = List.init fc (fun _ -> r.ReadString())
                let mc = r.ReadInt32()
                let warnings = 
                    List.init mc (fun _ ->
                        
                        let kind = 
                            match r.ReadInt32() with
                            | 1 -> MessageKind.Info
                            | 2 -> MessageKind.Warning (r.ReadString())
                            | 3 -> MessageKind.Error (r.ReadString())
                            | _ -> MessageKind.Debug

                        {
                            kind = kind
                            file = r.ReadString()
                            startLine = r.ReadInt32()
                            startCol = r.ReadInt32()
                            endLine = r.ReadInt32()
                            endCol = r.ReadInt32()
                            message = r.ReadString()
                        }
                    )
                Some (Success(files, warnings))

            | 2 ->
                r.ReadString() |> Fatal |> Some
            | 3 ->
                Some Shutdown
            | _ ->
                None



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


module Server =
    let private processMessage (cid : int) (log : ILog) (checker : FSharpChecker) project cache lenses =
        try
            log.debug range0 "  %04d: %s %A %A" cid (Path.GetFileName project.project) cache lenses
            let w = obj()
            let mutable messages = []

            let addMessage (kind : IPC.MessageKind) (r : range) (str : string) =
                lock w (fun () ->
                    let message =
                        {
                            IPC.file = r.FileName
                            IPC.kind = kind
                            IPC.startLine = r.StartLine
                            IPC.startCol = r.StartColumn
                            IPC.endLine = r.EndLine
                            IPC.endCol = r.EndColumn
                            IPC.message = str
                        }
                    messages <- messages @ [message]
                )

            let innerLog =
                { new ILog with
                    member x.debug r fmt = 
                        fmt |> Printf.kprintf (fun str -> 
                            addMessage (IPC.MessageKind.Debug) r str
                            log.debug r "  %04d: %s" cid str
                        )
                    member x.info r fmt = 
                        fmt |> Printf.kprintf (fun str -> 
                            addMessage (IPC.MessageKind.Info) r str
                            log.debug r "  %04d: %s" cid str
                        )
                    member x.warn r c fmt =
                        fmt |> Printf.kprintf (fun str -> 
                            addMessage (IPC.MessageKind.Warning c) r str
                            log.debug r "  %04d: warning %s" cid str
                        )
                    member x.error r c fmt =
                        fmt |> Printf.kprintf (fun str -> 
                            addMessage (IPC.MessageKind.Error c) r str
                            log.debug r  "  %04d: error %s" cid str
                        )
                }

            let newFiles = Adaptify.run checker cache lenses innerLog project
            let reply = IPC.Reply.Success(newFiles, messages)
            reply

        with e ->
            IPC.Reply.Fatal (string e)
    
    open System.Net

    let start (log : ILog) =
        let mutable currentId = 0
        let newId () = Interlocked.Increment(&currentId)
        let checker = newChecker()

        let server = 
            TCP.Server(IPAddress.Loopback, fun self msg ->
                let cid = newId()
                async {
                    log.debug range0 "%04d: %d bytes" cid msg.Length
                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    do! Async.SwitchToThreadPool()
                    let reply = 
                        match IPC.Command.tryUnpickle msg with
                        | Some msg ->
                            match msg with
                            | IPC.Command.Exit ->
                                self.Stop()
                                IPC.Reply.Shutdown
                            | IPC.Command.Adaptify(project, cache, lenses) -> 
                                processMessage cid log checker project cache lenses
                        | None ->
                            IPC.Reply.Fatal "could not parse command"
                    sw.Stop()
                    log.info range0 "%04d took %.3fms" cid sw.Elapsed.TotalMilliseconds

                    let mem = System.GC.GetTotalMemory(false)
                    if mem > 4L * 1073741824L then
                        let gb = float mem / 1073741824.0 
                        log.warn range0 "" "shutdown due to large memory: %.3fGB" gb
                        Process.releasePort self.Port
                        Process.startAdaptifyServer log |> ignore
                        self.Stop()

                    return IPC.Reply.pickle reply
                }
            )

        match Process.trySetPort server.Port with
        | Choice1Of2 () ->
            log.info range0 "server running on port %d" server.Port
            Some server
        | Choice2Of2(otherPid, otherPort) ->
            log.info range0 "server already running with pid %d and port %d" otherPid otherPort
            server.Stop()
            server.WaitForExit()
            None



module Client =
    open System.Net
    open System.Net.Sockets
    open System.Threading
    open System.Threading.Tasks

    let tryAdaptivfyAsync (log : ILog) (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        async {
            match Process.readProcessAndPort() with
            | Some (_proc, port) ->
                let cmd = IPC.Command.Adaptify(project, useCache, generateLenses)
                let data = IPC.Command.pickle cmd
                match! TCP.Client.tryGetAsync IPAddress.Loopback port 60000 data with
                | Some reply ->
                    match IPC.Reply.tryUnpickle reply with
                    | Some (IPC.Reply.Success(files, messages)) ->
                        for m in messages do
                            let range = mkRange m.file (mkPos m.startLine m.startCol) (mkPos m.endLine m.endCol)
                            match m.kind with
                            | IPC.MessageKind.Debug -> log.debug range "%s" m.message
                            | IPC.MessageKind.Info -> log.info range "%s" m.message
                            | IPC.MessageKind.Warning c -> log.warn range c "%s" m.message
                            | IPC.MessageKind.Error c -> log.error range c "%s" m.message

                        return Some files

                    | Some (IPC.Reply.Fatal msg) ->
                        log.debug range0 "server: %s" msg
                        return None

                    | Some IPC.Reply.Shutdown ->
                        log.debug range0 "server: shutdown"
                        return None

                    | None ->
                        return None
                | None ->
                    return None
            | None ->
                return None
        }
      
    let tryAdaptify (log : ILog)  (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        tryAdaptivfyAsync log project useCache generateLenses |> Async.RunSynchronously

    let adaptify (log : ILog) (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        let rec run (retries : int) =
            if retries = 0 then
                log.warn range0 "" "falling back to local adaptify"
                let checker = newChecker()
                Adaptify.run checker useCache generateLenses log project
            else
                match tryAdaptify log project useCache generateLenses with
                | Some files -> 
                    files
                | None -> 
                    log.debug range0 "connection failed"
                    Process.startAdaptifyServer log |> ignore

                    let sw = System.Diagnostics.Stopwatch.StartNew()
                    let rec wait (timeout : int) =
                        if sw.Elapsed.TotalMilliseconds > float timeout then    
                            false
                        else
                            match Process.readProcessAndPort() with
                            | Some _ -> true
                            | None ->
                                Threading.Thread.Sleep 100
                                wait timeout

                    wait 2000 |> ignore
                    run (retries - 1)
                            

        run 5
    
    let shutdownAsync (log : ILog) =
        async {
            match Process.readProcessAndPort() with
            | Some(proc, port) ->
                match! TCP.Client.tryGetAsync IPAddress.Loopback port 10000 (IPC.Command.pickle IPC.Command.Exit) with
                | Some res ->
                    match IPC.Reply.tryUnpickle res with
                    | Some IPC.Reply.Shutdown ->
                        if proc.WaitForExit(60000) then
                            log.info range0 "server exited"
                        else
                            log.warn range0 "" "shutdown taking very long -> killing process"
                            proc.Kill()
                    | Some r ->
                        log.warn range0 "" "unexpected response %A -> killing process" r
                        proc.Kill()
                    | None ->
                        log.warn range0 "" "cannot parse server response -> killing process"
                        proc.Kill()
                        
                | None ->
                    log.warn range0 "" "server does not respond -> killing process"
                    proc.Kill()
            | None ->
                log.info range0 "no server running"
            
    }

    let shutdown (log : ILog) =
        shutdownAsync log |> Async.RunSynchronously
    