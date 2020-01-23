namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.Range
open System.Reflection
open System.Runtime.InteropServices
open System.Diagnostics


module Process = 

    let private executableExtension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".exe"
        else ""

    let logFile =
        let folder = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
        if not (Directory.Exists folder) then Directory.CreateDirectory folder |> ignore
        Path.Combine(folder, "log.txt")

    let getProcesses() =
        let folder = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
        if Directory.Exists folder then 
            let selfId = Process.GetCurrentProcess().Id
            Directory.GetFiles(folder, "*.proc")
            |> Array.choose (fun file ->
                match Int32.TryParse (Path.GetFileNameWithoutExtension file) with
                | (true, pid) ->  
                    if pid <> selfId then
                        try 
                            let proc = Process.GetProcessById(pid)
                            if isNull proc then 
                                try File.Delete file with _ -> ()
                                None
                            else 
                                Some proc
                        with _ -> 
                            try File.Delete file with _ -> ()
                            None
                    else
                        None
                | _ -> 
                    None
            )
        else
            [||]
            
    //let internal getPort() =
    //    let folder = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
    //    if Directory.Exists folder then 
    //        let selfId = Process.GetCurrentProcess().Id
    //        Directory.GetFiles(folder, "*.proc")
    //        |> Array.tryPick (fun file ->
    //            match Int32.TryParse (Path.GetFileNameWithoutExtension file) with
    //            | (true, pid) ->  
    //                if true || pid <> selfId then
    //                    try 
    //                        let proc = Process.GetProcessById(pid)
    //                        if isNull proc then 
    //                            try File.Delete file with _ -> ()
    //                            None
    //                        else 
    //                            match Int32.TryParse((File.ReadAllText file)) with
    //                            | (true, port) -> Some port
    //                            | _ -> None
    //                    with _ -> 
    //                        try File.Delete file with _ -> ()
    //                        None
    //                else
    //                    None
    //            | _ -> 
    //                None
    //        )
    //    else
    //        None

    //let internal createProcFile(port : int) =
    //    let folder = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
    //    let pid = System.Diagnostics.Process.GetCurrentProcess().Id
    //    let procFile = Path.Combine(folder, sprintf "%d.proc" pid)
    //    if not (Directory.Exists folder) then Directory.CreateDirectory folder |> ignore
    //    File.WriteAllText(procFile, string port)

    //let internal deleteProcFile() =
    //    let folder = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
    //    let pid = System.Diagnostics.Process.GetCurrentProcess().Id
    //    let procFile = Path.Combine(folder, sprintf "%d.proc" pid)
    //    if File.Exists procFile then File.Delete procFile

    open System.IO.MemoryMappedFiles

    let private sem = 
        let mutexName = "adaptify_" + selfVersion + "_mutex"
        match System.Threading.Semaphore.TryOpenExisting mutexName with
        | (true, sem) -> sem
        | _ -> new System.Threading.Semaphore(1, 1, mutexName)

    let rec locked (action : unit -> 'r) =
        try 
            let mutable worked = sem.WaitOne()
            while not worked do
                worked <- sem.WaitOne()
            action()
        finally
            sem.Release() |> ignore

    let file = MemoryMappedFile.CreateOrOpen("adaptify_" + selfVersion, 8L, MemoryMappedFileAccess.ReadWrite, MemoryMappedFileOptions.None, HandleInheritability.Inheritable)
                
    let readPort (timeout : int) =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let read() =
            locked (fun () -> 
                use view = file.CreateViewAccessor()
                if view.Capacity >= 8L then
                    let port = view.ReadInt32(0L)
                    let pid = view.ReadInt32(4L)
                    if port = 0 then 
                        None
                    else
                        try 
                            let proc = Process.GetProcessById(pid)
                            Some port
                        with _ ->
                            None
                else
                    None
            )

        let rec run() =
            if sw.Elapsed.TotalMilliseconds > float timeout then
                None
            else
                try 
                    read()
                with _ -> 
                    Threading.Thread.Sleep 100
                    run()

        run()

    let setPort (port : int) =
        let pid = Process.GetCurrentProcess().Id
        let mapName = "adaptify_" + selfVersion


        locked (fun () ->
            let file = MemoryMappedFile.CreateOrOpen(mapName, 8L, MemoryMappedFileAccess.ReadWrite, MemoryMappedFileOptions.None, HandleInheritability.Inheritable)
            use view = file.CreateViewAccessor()
            
            let runningPort = view.ReadInt32(0L)
            let running = view.ReadInt32(4L)
            if running <> 0 && runningPort <> 0 then
                let working = 
                    try Process.GetProcessById running |> ignore; true
                    with _ -> false
                if working then 
                    file.Dispose()
                    failwith "dead"


            view.Write(0L, port)
            view.Write(4L, pid)
            view.Flush()
            
            file :> IDisposable
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
        if false && entry.GetName().Name = "adaptify" then   
            let info = ProcessStartInfo("dotnet", entry.Location + " --server", UseShellExecute = false, CreateNoWindow = true) 
            let proc = Process.Start(info)
            proc
        else    
            let toolDir = Path.Combine(Path.GetTempPath(), "adaptify", string selfVersion)
            let toolPath = Path.Combine(toolDir, "adaptify" + executableExtension)
            if File.Exists toolPath then
                log.debug range0 "found tool at %s" toolPath
            else
                while not (File.Exists toolPath) do
                    locked (fun () ->
                        try
                            dotnet log [ 
                                "tool"; "install"; "adaptify"
                                "--no-cache"
                                "--tool-path"; sprintf "\"%s\"" toolDir
                                "--version"; sprintf "[%s]" selfVersion
                            ]
                            log.debug range0 "installed tool at %s" toolPath
                        with _ ->
                            ()
                    )
            let info = ProcessStartInfo(toolPath, "--server", UseShellExecute = false, CreateNoWindow = true)
            let proc = Process.Start(info)
            proc

