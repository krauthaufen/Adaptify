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





module Server =
    let private processMessage (cid : int) (log : ILog) (checker : FSharpChecker) (data : IPC.Command) =
        try
            match data with
            | IPC.Command.Adaptify(project, cache, lenses) ->
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
            | IPC.Command.Exit ->
                IPC.Reply.Shutdown

        with e ->
            IPC.Reply.Fatal (string e)
    
    open System.Net
    open System.Net.Sockets
    open System.Threading.Tasks

    type TaskSet() =
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
                Task.WaitAll(all)

        member x.WaitAsync() =
            async {
                let all = lock tasks (fun () -> Seq.toArray tasks.Values)
                if all.Length > 0 then
                    do! Task.WhenAll(all) |> Async.AwaitTask
                    return! x.WaitAsync()

            }


    let private startServer (log : ILog) (listener : TcpListener) =
        let port = (unbox<IPEndPoint> listener.LocalEndpoint).Port
        let cancel = new CancellationTokenSource()
        
        let workingLock = obj()
        let mutable working = 0
        let mutable lastRequest = DateTime.Now

        let timeout = TimeSpan.FromMinutes 8.0
        let tick _ =
            let dt = DateTime.Now - lastRequest
            if working = 0 && dt > timeout then 
                log.info range0 "exiting due to timeout (inactive for %A)" dt
                listener.Stop()
                cancel.Cancel()
                Environment.Exit 0

        let period = int (timeout.TotalMilliseconds / 10.0) |> max 1000
        let timer =
            new Timer(TimerCallback tick, null, period, period)
        
        let mutable currentId = 0
        let newId() = Interlocked.Increment(&currentId)

        let readyLock = obj()
        let mutable ready = false
        let checker = newChecker()

        let addWorking() =
            lock workingLock (fun () ->
                working <- working + 1
                Monitor.PulseAll workingLock
            )
        let removeWorking() =
            lock workingLock (fun () ->
                working <- working - 1
                Monitor.PulseAll workingLock
            )

        let waitWorking() =
            lock workingLock (fun () ->
                while working > 0 do
                    Monitor.Wait workingLock |> ignore
            )
            

        
        let tasks = TaskSet()
        let run = 
            async {
                try
                    let mutable keepRunning = true
                    while keepRunning do
                        if not ready then
                            lock readyLock (fun () ->
                                ready <- true
                                Monitor.PulseAll readyLock
                            )
                        let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                        
                        addWorking()
                        
                        let cid = newId()
                        lastRequest <- DateTime.Now
                        
                        let task = 
                            Async.StartAsTask <| async {
                                try
                                    try
                                        do! Async.SwitchToThreadPool()
                                        use stream = client.GetStream()
                                        let! msg = stream.ReadMessageAsync()
                                        log.debug range0 "got %04d with %d bytes" cid msg.Length
                                        let sw = System.Diagnostics.Stopwatch.StartNew()
                                        do! Async.SwitchToThreadPool()
                                        let reply = 
                                            match IPC.Command.tryUnpickle msg with
                                            | Some msg ->
                                                processMessage cid log checker msg
                                            | None ->
                                                IPC.Reply.Fatal "could not parse command"

                                        do! stream.WriteMessageAsync(IPC.Reply.pickle reply)
                                        client.Dispose()
                                        sw.Stop()
                                        log.info range0 "%04d took %.3fms" cid sw.Elapsed.TotalMilliseconds
                                        match reply with
                                        | IPC.Reply.Shutdown ->
                                            log.info range0 "shutdown requested"
                                            listener.Stop()
                                            timer.Dispose()
                                            Process.releasePort port
                                            Process.startAdaptifyServer log |> ignore
                                        | _ ->
                                            ()
                                    with e ->
                                        log.warn range0 "" "%04d failed: %A" cid e
                                        ()
                                finally
                                    removeWorking()
                                    lastRequest <- DateTime.Now
                            }

                        tasks.Add task |> ignore

                        let mem = System.GC.GetTotalMemory(false)
                        if mem > 3L * 1073741824L then
                            let gb = float mem / 1073741824.0 
                            log.warn range0 "" "shutdown due to large memory: %.3fGB" gb
                            Process.releasePort port
                            Process.startAdaptifyServer log |> ignore
                            keepRunning <- false
                            do! tasks.WaitAsync()
                finally
                    listener.Stop()
                    timer.Dispose()
            }

        let task = Async.StartAsTask(run, cancellationToken = cancel.Token)

        lock readyLock (fun () ->
            while not ready do
                Monitor.Wait readyLock |> ignore
        )

        let cancel() =
            try listener.Stop() with _ -> ()
            try cancel.Cancel() with _ -> ()
            try task.Wait() with _ -> ()

        let wait() =
            try task.Wait() with _ -> ()

        wait, cancel

    let start (log : ILog) =
        let listener = new TcpListener(IPAddress.Loopback, 0)
        listener.Start()
        let port = (unbox<IPEndPoint> listener.LocalEndpoint).Port
        
        let wait, cancel = startServer log listener

        match Process.trySetPort port with
        | Choice1Of2 () ->
            log.info range0 "server running on port %d" port
            Some (wait, cancel)
        | Choice2Of2(otherPid, otherPort) ->
            log.info range0 "server already running with pid %d and port %d" otherPid otherPort
            cancel()
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
                use client = new TcpClient()
                match! client.TryConnectAsync(IPAddress.Loopback, port, 500) with
                | true ->
                    let cmd = IPC.Command.Adaptify(project, useCache, generateLenses)
                    let data = IPC.Command.pickle cmd
                    use stream = client.GetStream()
                    do! stream.WriteMessageAsync data
                    let! reply = stream.ReadMessageAsync()

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
                | false ->
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
    
    let shutdownAsync () =
        async {
            match Process.readProcessAndPort() with
            | Some(proc, port) ->
                use client = new TcpClient()
                match! client.TryConnectAsync(IPAddress.Loopback, port, 500) with
                | true ->
                    use stream = client.GetStream()
                    do! stream.WriteMessageAsync(IPC.Command.pickle IPC.Command.Exit)
                    let! res = stream.ReadMessageAsync()
                    match IPC.Reply.tryUnpickle res with
                    | Some IPC.Reply.Shutdown ->
                        if proc.WaitForExit(1000) then
                            return true
                        else
                            proc.Kill()
                            return true
                    | _ ->
                        return false
                | false ->
                    proc.Kill()
                    return true
            | None ->
                return true
            
    }

    let shutdown () =
        shutdownAsync () |> Async.RunSynchronously
    