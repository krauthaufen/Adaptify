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

    static let lockLength = 4096L

    member x.Enter() =
        Monitor.Enter lockObj

        if isNull stream then   
            File.ensureDirectory fileName
            stream <- new FileStream(fileName, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Inheritable ||| FileShare.ReadWrite, 4096, FileOptions.WriteThrough)

        isEntered <- isEntered + 1
        if isEntered = 1 then
            let mutable entered = false
            while not entered do
                try 
                    stream.Lock(0L, lockLength)
                    entered <- true
                with _ ->
                    Threading.Thread.Sleep 5

    member x.Exit() =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        isEntered <- isEntered - 1
        if isEntered = 0 then
            stream.Unlock(0L, lockLength)
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
                stream.Unlock(0L, lockLength)
            if not (isNull stream) then
                stream.Dispose()
                stream <- null
        )

    interface IDisposable with
        member x.Dispose() = x.Dispose()

[<AutoOpen>]
module internal NetworkStreamExtensions =
    open System.Net
    open System.Net.Sockets
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

    type TcpClient with
        member x.TryConnectAsync(address : IPAddress, port : int, ?timeout : int) =
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
                
            //async {
            //    match timeout with
            //    | Some timeout when timeout > 0 ->
            //        let timeout = Task.Delay(timeout).ContinueWith(fun _ -> false)

            //        let connect = 
            //            Async.StartAsTask <| async {
                            
            //                try
            //                    do! x.ConnectAsync(address, port) |> Async.AwaitTask
            //                    return true
            //                with _ ->
            //                    return false
            //            }

            //        let! t = Task.WhenAny(connect, timeout) |> Async.AwaitTask
            //        return! Async.AwaitTask t
            //    | _ -> 
            //        try
            //            do! x.ConnectAsync(address, port) |> Async.AwaitTask
            //            return true
            //        with _ ->
            //            return false
                
            //}

    type NetworkStream with
        member x.ReadInt32() =
            let data = Array.zeroCreate 4
            let mutable r = 0
            while r < 4 do
                let rr = x.Read(data, r, 4 - r)
                if rr = 0 then raise <| ObjectDisposedException("stream")
                r <- r + rr
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
                        if rr = 0 then raise <| ObjectDisposedException("stream")
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
                    if read = 0 then raise <| ObjectDisposedException("stream")
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
                        if read = 0 then raise <| ObjectDisposedException("stream")
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

        member x.WriteMessageAsync(message : byte[]) =
            async {
                let len = BitConverter.GetBytes message.Length
                do! x.WriteAsync(len, 0, len.Length) |> Async.AwaitTask
                do! x.WriteAsync(message, 0, message.Length) |> Async.AwaitTask
                do! x.FlushAsync() |> Async.AwaitTask
            }


module Process = 
    open System.Threading
    open System.Threading.Tasks

    let private executableExtension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe"
        else ""

    let private executableName =
        "adaptify" + executableExtension

    let private directory() =
        let path = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
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
        let start = ProcessStartInfo("dotnet", String.concat " " args, CreateNoWindow = true, UseShellExecute = false,  RedirectStandardOutput = true, RedirectStandardError = true)
        let proc = Process.Start(start)
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            while not proc.StandardOutput.EndOfStream do
                let line = proc.StandardOutput.ReadLine()
                log.debug range0 "dotnet: %s" line
            while not proc.StandardError.EndOfStream do
                let line = proc.StandardError.ReadLine()
                log.debug range0 "dotnet: %s" line

            failwith "dotnet failed"

    let startAdaptifyServer (log : ILog) =
        let entryPath = try Assembly.GetEntryAssembly().Location with _ -> "foo.dll"
        let executablePath = try Process.GetCurrentProcess().MainModule.FileName with _ -> "bar"

        if Path.GetFileName(executablePath) = executableName then
            log.info range0 "starting self-executable"
            let info = ProcessStartInfo(executablePath, "--server", UseShellExecute = false, CreateNoWindow = true) 
            let proc = Process.Start(info)
            proc

        elif Path.GetFileName(entryPath) = "adaptify.dll" then  
            log.info range0 "starting self-dll" 
            let info = ProcessStartInfo("dotnet", "\"" + entryPath + "\" --server", UseShellExecute = false, CreateNoWindow = true) 
            let proc = Process.Start(info)
            proc

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
                let info = ProcessStartInfo(toolPath, "--server", UseShellExecute = false, CreateNoWindow = true)
                let proc = Process.Start(info)
                proc
            | Some(otherProc, _) ->
                otherProc
                

