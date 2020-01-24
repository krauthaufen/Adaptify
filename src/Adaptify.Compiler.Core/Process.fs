namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.Range
open System.Reflection
open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading


type IPCLock(fileName : string, dataSize : int) =
    let stream = new FileStream(fileName, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Delete ||| FileShare.Inheritable ||| FileShare.ReadWrite, dataSize, FileOptions.WriteThrough)
    let lockObj = obj()
    let mutable isEntered = 0

    member x.Enter() =
        Monitor.Enter lockObj
        isEntered <- isEntered + 1
        if isEntered = 1 then
            let mutable entered = false
            while not entered do
                try 
                    stream.Lock(0L, int64 dataSize)
                    entered <- true
                with _ ->
                    Threading.Thread.Sleep 5
            stream.SetLength(int64 dataSize)

    member x.Exit() =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        isEntered <- isEntered - 1
        if isEntered = 0 then
            stream.Unlock(0L, int64 dataSize)
        Monitor.Exit lockObj

    member x.Write(data : byte[], offset : int, count : int) =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        stream.Write(data, offset, count)
        stream.Flush()

    member x.Write(str : string) =
        let mutable bytes = System.Text.Encoding.UTF8.GetBytes str
        if bytes.Length > dataSize then failwithf "string too long (%d vs %d)" bytes.Length dataSize
        x.Write(bytes, 0, bytes.Length)

    member x.ReadAll() =
        if not (Monitor.IsEntered lockObj) then failwith "lock not entered"
        let arr = Array.zeroCreate dataSize
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
                stream.Unlock(0L, int64 dataSize)
        )
        stream.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()


module Process = 

    let private executableExtension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe"
        else ""

    let private directory =
        let path = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
        while not (Directory.Exists path) do 
            try Directory.CreateDirectory path |> ignore
            with _ -> ()
        path

    let ipc = 
        let path = Path.Combine(directory, "process.lock")
        new IPCLock(path, 4096)
        
    let logFile = Path.Combine(directory, "log.txt")

    let rec locked (action : unit -> 'r) =
        ipc.Enter()
        try 
            action()
        finally
            ipc.Exit()

    let private (|Int32|_|) (str : string) =
        match Int32.TryParse str with
        | (true, v) -> Some v
        | _ -> None

    let readPort (timeout : int) =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let read() =
            locked (fun () -> 
                let parts = ipc.ReadString().Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
                match parts with
                | [| Int32 pid; Int32 port |] ->
                    if port = 0 then 
                        None
                    else
                        try 
                            let proc = Process.GetProcessById(pid)
                            Some port
                        with _ ->
                            None
                | _ ->
                    None
            )

        let rec run() =
            if timeout > 0 && sw.Elapsed.TotalMilliseconds > float timeout then
                None
            else
                try 
                    read()
                with _ -> 
                    Threading.Thread.Sleep 100
                    run()

        run()

    let kill (log : ILog) =
        locked (fun () -> 
            let parts = ipc.ReadString().Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)
            match parts with
            | [| Int32 pid; Int32 port |] ->
                if port <> 0 then 
                    try 
                        let proc = Process.GetProcessById(pid)
                        proc.Kill()
                        log.info range0 "killed process %d" pid
                    with _ ->
                        ()
            | _ ->
                ()
        )


    let setPort (port : int) =
        let pid = Process.GetCurrentProcess().Id
        locked (fun () ->
            match readPort 0 with
            | Some other ->
                false
            | None -> 
                ipc.Write(sprintf "%d;%d" pid port)
                true
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
        let entry = Assembly.GetEntryAssembly()
        if entry.GetName().Name = "adaptify" then   
            let info = ProcessStartInfo("dotnet", entry.Location + " --server", UseShellExecute = false, CreateNoWindow = true) 
            let proc = Process.Start(info)
            proc
        else    
            let toolPath = Path.Combine(directory, "adaptify" + executableExtension)
            if File.Exists toolPath then
                log.debug range0 "found tool at %s" toolPath
            else
                while not (File.Exists toolPath) do
                    locked (fun () ->
                        try
                            dotnet log [ 
                                "tool"; "install"; "adaptify"
                                "--no-cache"
                                "--tool-path"; sprintf "\"%s\"" directory
                                "--version"; sprintf "[%s]" selfVersion
                            ]
                            log.debug range0 "installed tool at %s" toolPath
                        with _ ->
                            ()
                    )
            let info = ProcessStartInfo(toolPath, "--server", UseShellExecute = false, CreateNoWindow = true)
            let proc = Process.Start(info)
            proc

