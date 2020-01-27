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
        | Adaptify of project : ProjectInfo * outputPath : string * designTime : bool * useCache : bool * lenses : bool
        | Exit

    module Command =
        let pickle (cmd : Command) =
            use ms = new MemoryStream()
            use w = new BinaryWriter(ms)
            match cmd with
            | Adaptify(p, path, d, c, l) ->  
                w.Write 1
                ProjectInfo.pickleTo ms p
                w.Write path
                w.Write(d)
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
                    let path = r.ReadString()
                    let d = r.ReadBoolean()
                    let c = r.ReadBoolean()
                    let l = r.ReadBoolean()
                    Some (Adaptify(p, path, d, c, l))
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
    let private processMessage (cid : int) (log : ILog) (checker : FSharpChecker) project outputPath designTime cache lenses = 
        async {
            try
                log.debug range0 "  %04d: %s %A %A %A" cid (Path.GetFileName project.project) designTime cache lenses
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

                let! newFiles = Adaptify.runAsync checker outputPath designTime cache lenses innerLog project
                let reply = IPC.Reply.Success(newFiles, messages)
                return reply

            with e ->
                return IPC.Reply.Fatal (string e)
        }
    
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
                    let! reply = 
                        match IPC.Command.tryUnpickle msg with
                        | Some msg ->
                            match msg with
                            | IPC.Command.Exit ->
                                self.Stop()
                                async { return IPC.Reply.Shutdown }
                            | IPC.Command.Adaptify(project, path, designTime, cache, lenses) -> 
                                processMessage cid log checker project path designTime cache lenses
                        | None ->
                            async { return IPC.Reply.Fatal "could not parse command" }
                    sw.Stop()
                    log.info range0 "%04d took %.3fms" cid sw.Elapsed.TotalMilliseconds

                    let mem = System.GC.GetTotalMemory(false)
                    if mem > 4L * 1073741824L then
                        let gb = float mem / 1073741824.0 
                        log.warn range0 "" "shutdown due to large memory: %.3fGB" gb
                        ProcessManagement.releasePort self.Port
                        ProcessManagement.startAdaptifyServer log |> ignore
                        self.Stop()

                    return IPC.Reply.pickle reply
                }
            )

        match ProcessManagement.trySetPort server.Port with
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

    let tryAdaptivfyAsync (log : ILog) (project : ProjectInfo) (outputPath : string) (designTime : bool) (useCache : bool) (generateLenses : bool) =
        async {
            match ProcessManagement.readProcessAndPort() with
            | Some (_proc, port) ->
                let cmd = IPC.Command.Adaptify(project, outputPath, designTime, useCache, generateLenses)
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
      
    let tryAdaptify (ct : CancellationToken) (log : ILog) (project : ProjectInfo) (outputPath : string) (designTime : bool) (useCache : bool) (generateLenses : bool) =
        let run = tryAdaptivfyAsync log project outputPath designTime useCache generateLenses
        try Async.RunSynchronously(run, cancellationToken = ct)
        with _ -> None

    let adaptify (ct : CancellationToken) (log : ILog) (project : ProjectInfo) (outputPath : string) (designTime : bool) (useCache : bool) (generateLenses : bool) =
        let rec run (retries : int) =
            if retries = 0 then
                log.warn range0 "" "falling back to local adaptify"
                let checker = newChecker()
                let run = Adaptify.runAsync checker outputPath designTime useCache generateLenses log project
                try Async.RunSynchronously(run, cancellationToken = ct)
                with _ -> project.files
            else
                match tryAdaptify ct log project outputPath designTime useCache generateLenses with
                | Some files -> 
                    files
                | None -> 
                    if not ct.IsCancellationRequested then
                        log.debug range0 "connection failed"
                        ProcessManagement.startAdaptifyServer log |> ignore

                        let sw = System.Diagnostics.Stopwatch.StartNew()
                        let rec wait (timeout : int) =
                            if sw.Elapsed.TotalMilliseconds > float timeout then    
                                false
                            else
                                match ProcessManagement.readProcessAndPort() with
                                | Some _ -> true
                                | None ->
                                    Threading.Thread.Sleep 100
                                    wait timeout

                        wait 2000 |> ignore
                        run (retries - 1)
                    else
                        project.files
                            

        run 5
    
    let shutdownAsync (log : ILog) =
        async {
            match ProcessManagement.readProcessAndPort() with
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
    