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
            | _ ->
                None

    type Reply =
        | Success of files : list<string> * messages : list<AdaptifyMessage>
        | Fatal of message : string

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
            | _ ->
                None



[<AutoOpen>]
module internal NetworkStreamExtensions =
    open System.Net
    open System.Net.Sockets

    type NetworkStream with
        member x.ReadInt32() =
            let data = Array.zeroCreate 4
            let mutable r = 0
            while r < 4 do
                r <- r + x.Read(data, r, 4 - r)
            BitConverter.ToInt32(data, 0)
            
        member x.ReadInt32Async() =
            async {
                let mutable canceled = false
                let! ct = Async.CancellationToken
                try
                    use reg = ct.Register (fun () -> canceled <- true; x.Dispose())
                    let data = Array.zeroCreate 4
                    let mutable r = 0
                    while r < 4 do
                        let! rr = x.ReadAsync(data, r, 4 - r) |> Async.AwaitTask
                        r <- r + rr
                    return BitConverter.ToInt32(data, 0)
                with e ->
                    if canceled then raise <| OperationCanceledException()
                    else raise e
                    return -1
                    
            }

        member x.ReadMessage(ct : CancellationToken) =
            let mutable canceled = false
            try
                let mutable offset = 0

                use reg = ct.Register (fun () -> canceled <- true; x.Dispose())
                let len = x.ReadInt32()
                let buffer = Array.zeroCreate len
                let mutable length = buffer.Length

                while offset < len do
                    let read = x.Read(buffer, offset, length)
                    offset <- offset + read
                    length <- length - read

                buffer
            with :? ObjectDisposedException ->
                if canceled then
                    raise <| OperationCanceledException()
                else
                    reraise()
                    
        member x.ReadMessage() =
            x.ReadMessage(CancellationToken.None)

        member x.ReadMessageAsync() =
            async {
                let! ct = Async.CancellationToken
                let mutable canceled = false
                try
                    let mutable offset = 0

                    use reg = ct.Register (fun () -> canceled <- true; x.Dispose())
                    let! len = x.ReadInt32Async()
                    let buffer = Array.zeroCreate len
                    let mutable length = buffer.Length

                    while offset < len do
                        let! read = x.ReadAsync(buffer, offset, length) |> Async.AwaitTask
                        offset <- offset + read
                        length <- length - read

                    return buffer
                with :? ObjectDisposedException as e ->
                    if canceled then
                        raise <| OperationCanceledException()
                    else
                        raise e
                    return [||]
            }



module Server =
    let private processMessage (cid : int) (log : ILog) (checker : FSharpChecker) (data : byte[]) =
        try
            match IPC.Command.tryUnpickle data with
            | Some (IPC.Command.Adaptify(project, cache, lenses)) ->
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

                let newFiles = Adaptify.run (Some checker) cache lenses innerLog project
                let reply = IPC.Reply.Success(newFiles, messages)
                IPC.Reply.pickle reply
            | None ->
                IPC.Reply.pickle (IPC.Reply.Fatal "could not parse command")
        with e ->
            IPC.Reply.pickle (IPC.Reply.Fatal (string e))
    
    open System.Net
    open System.Net.Sockets

    let startTcp (log : ILog) =
        let listener = new TcpListener(IPAddress.Loopback, 0)
        listener.Start()
        
        let cancel = new CancellationTokenSource()
        
        let port = (unbox<IPEndPoint> listener.LocalEndpoint).Port
        if not (Process.setPort port) then
            log.info range0 "server already running"
            listener.Stop()
            cancel.Cancel()
            id
        else
            log.info range0 "server running on port %d" port

            let sw = System.Diagnostics.Stopwatch.StartNew()
            let checker = FSharpChecker.Create(keepAssemblyContents = true, keepAllBackgroundResolutions = true)
            sw.Stop()
            log.debug range0 "creating FSharpChecker took: %.3fms" sw.Elapsed.TotalMilliseconds

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

            let run = 
                async {
                    try
                        while true do
                            let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                            let cid = newId()

                            lastRequest <- DateTime.Now
                            Async.Start <|
                                async {
                                    Interlocked.Increment(&working) |> ignore
                                    try
                                        try
                                            use stream = client.GetStream()
                                            let! msg = stream.ReadMessageAsync()
                                            log.debug range0 "got %04d with %d bytes" cid msg.Length
                                            let sw = System.Diagnostics.Stopwatch.StartNew()
                                            do! Async.SwitchToThreadPool()
                                            let reply = processMessage cid log checker msg
                                            do! stream.WriteAsync(BitConverter.GetBytes reply.Length, 0, 4) |> Async.AwaitTask
                                            do! stream.WriteAsync(reply, 0, reply.Length) |> Async.AwaitTask
                                            client.Dispose()
                                            sw.Stop()
                                            log.info range0 "%04d took %.3fms" cid sw.Elapsed.TotalMilliseconds
                                        with e ->
                                            log.warn range0 "" "%04d failed: %A" cid e
                                            ()
                                    finally
                                        Interlocked.Decrement(&working) |> ignore
                                        lastRequest <- DateTime.Now
                                }
                    finally
                        log.info range0 "shutdown"
                        listener.Stop()
                        timer.Dispose()
                }

            let task = Async.StartAsTask(run, cancellationToken = cancel.Token)
            fun () ->
                listener.Stop()
                cancel.Cancel()
                try task.Wait()
                with _ -> ()



module Client =
    open System.Net
    open System.Net.Sockets

    type NetworkStream with
        member stream.ReadMessageAsync() =
            async {
                let mutable offset = 0
                let mutable cont = true

                let len = Array.zeroCreate 4
                let! _ = stream.ReadAsync(len, 0, 4) |> Async.AwaitTask
                let len = BitConverter.ToInt32(len, 0)
                let buffer = Array.zeroCreate len
                let mutable length = buffer.Length

                while offset < len do
                    let! read = stream.ReadAsync(buffer, offset, length) |> Async.AwaitTask
                    offset <- offset + read
                    length <- length - read

                return buffer
            }


    let tryAdaptifyAsyncTcp (log : ILog) (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        match Process.readPort 1000 with
        | None -> 
            async { return None }
        | Some port ->
            let client = new TcpClient()
            async {
                let sw = System.Diagnostics.Stopwatch.StartNew()
                while sw.Elapsed.TotalMilliseconds < 1000.0 && not client.Connected do
                    try
                        do! client.ConnectAsync(IPAddress.Loopback, port) |> Async.AwaitTask
                    with _ ->
                        do! Async.Sleep 100
                        ()

                let cmd = IPC.Command.Adaptify(project, useCache, generateLenses)
                let data = IPC.Command.pickle cmd
                use stream = client.GetStream()
                do! stream.WriteAsync(BitConverter.GetBytes data.Length, 0, 4) |> Async.AwaitTask
                do! stream.WriteAsync(data, 0, data.Length) |> Async.AwaitTask
                do! stream.FlushAsync() |> Async.AwaitTask

                let! reply = stream.ReadMessageAsync()
                client.Dispose()

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

                | None ->
                    return None
            }
      
    let tryAdaptifyTcp (log : ILog)  (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        tryAdaptifyAsyncTcp log project useCache generateLenses |> Async.RunSynchronously

    let adaptifyTcp (log : ILog)  (project : ProjectInfo) (useCache : bool) (generateLenses : bool) =
        let rec run (retries : int) =
            if retries = 0 then
                log.warn range0 "" "falling back to local adaptify"
                Adaptify.run None useCache generateLenses log project
            else
                match tryAdaptifyTcp log project useCache generateLenses with
                | Some files -> 
                    files
                | None -> 
                    log.debug range0 "connection failed"
                    Process.startAdaptifyServer log |> ignore
                    Threading.Thread.Sleep 100
                    run (retries - 1)
                            

        run 5
    
    
    