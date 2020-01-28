namespace Adaptify.Compiler

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range

module Directory =
    let ensure (path : string) =
        while not (Directory.Exists path) do 
            try Directory.CreateDirectory path |> ignore
            with _ -> ()
        path

module File =
    let ensureDirectory (path : string) =
        Directory.ensure (Path.GetDirectoryName path) |> ignore


[<AutoOpen>]
module StringTools =
    let lines (str : string) =
        str.Split([|"\r\n"|], StringSplitOptions.None)

    let indent (lines : string[]) =
        lines |> Array.map (fun s -> "    " + s)
        
    let indentStr (str : string) =
        str |> lines |> indent |> String.concat "\r\n"


module Map =
    let union (l : Map<_,_>) (r : Map<_,_>) =
        let mutable res = l
        for (KeyValue(k,v)) in r do
            res <- Map.add k v res
        res

module FSharpAttribute =
    let isAutoOpen (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = typeof<AutoOpenAttribute>.FullName
        | None ->
            false

    let isModelType (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.ModelTypeAttribute"
        | None ->
            false
            
    let isTreatAsValue (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.TreatAsValueAttribute"
        | None ->
            false

    let isNonAdaptive (a : FSharpAttribute) =
        match a.AttributeType.TryFullName with
        | Some name ->
            name = "Adaptify.NonAdaptiveAttribute"
        | None ->
            false

type ILog =
    abstract member debug : range -> Printf.StringFormat<'T, unit> -> 'T
    abstract member info : range -> Printf.StringFormat<'T, unit> -> 'T
    abstract member warn : range -> string -> Printf.StringFormat<'T, unit> -> 'T
    abstract member error : range -> string -> Printf.StringFormat<'T, unit> -> 'T

module Log =
    open System.IO

    let empty =
        { new ILog with
            member x.debug _ fmt = Printf.kprintf ignore fmt
            member x.info _ fmt = Printf.kprintf ignore fmt
            member x.warn _ _ fmt = Printf.kprintf ignore fmt
            member x.error _ _ fmt = Printf.kprintf ignore fmt
        }
    
    let private consoleLock = obj()

    let console (verbose : bool) =  
        let colored = 
            try ignore Console.WindowHeight; true
            with _ -> false
        let useColor (c : ConsoleColor) (f : unit -> 'a) =
            let o = Console.ForegroundColor
            Console.ForegroundColor <- c
            try f()
            finally Console.ForegroundColor <- o

        let writeRange (r : FSharp.Compiler.Range.range) =  
            if r <> range0 then
                Console.WriteLine(" @ {0} ({1},{2}--{3},{4})", r.FileName, r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
            else
                Console.WriteLine()
                
        let rangeString (r : FSharp.Compiler.Range.range) =  
            if r <> range0 then
                String.Format(" @ {0} ({1},{2}--{3},{4})", r.FileName, r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
            else
                ""

        { new ILog with
            member x.debug range fmt =
                fmt |> Printf.kprintf (fun str ->   
                    if verbose then 
                        lock consoleLock (fun () ->
                            if colored then
                                Console.Write("> ")
                                useColor ConsoleColor.DarkGray (fun () ->
                                    Console.Write(str)
                                )
                                writeRange range
                            else
                                Console.WriteLine("> {0}{1}", str, rangeString range)
                        )
                )

            
            member x.info range fmt =
                fmt |> Printf.kprintf (fun str ->   
                    lock consoleLock (fun () ->
                        if colored then
                            Console.Write("> ")
                            useColor ConsoleColor.Gray (fun () ->
                                Console.Write(str)
                            )
                            writeRange range
                        else
                            Console.WriteLine("> {0}{1}", str, rangeString range)
                    )
                )
            
            member x.warn range code fmt =
                fmt |> Printf.kprintf (fun str ->  
                    lock consoleLock (fun () ->
                        if colored then
                            Console.Write("> ")
                            useColor ConsoleColor.DarkYellow (fun () ->
                                Console.Write(str)
                            )
                            writeRange range
                        else
                            Console.WriteLine("> {0}{1}", str, rangeString range)
                    )
                )
            
            member x.error range code fmt =
                fmt |> Printf.kprintf (fun str ->  
                    lock consoleLock (fun () ->
                        if colored then
                            Console.Write("> ")
                            useColor ConsoleColor.Red (fun () ->
                                Console.Write(str)
                            )
                            writeRange range
                        else
                            Console.WriteLine("> {0}{1}", str, rangeString range)
                    )
                )
        }

    let file (verbose : bool) (file : string) =

        File.ensureDirectory file
        let stream = new FileStream(file, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite, 4096, FileOptions.WriteThrough)

        let enter() =
            let mutable entered = false
            while not entered do
                try
                    stream.Lock(0L, 0L)
                    entered <- true
                with _ ->
                    Threading.Thread.Sleep 0

        let exit() =
            stream.Unlock(0L, 0L)

        let pid = System.Diagnostics.Process.GetCurrentProcess().Id
        let appendLine (prefix : string) (str : string) =
            let now = DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
            let str = sprintf "%06d %s %s %s\r\n" pid now prefix str 
            let bytes = System.Text.Encoding.UTF8.GetBytes str
            enter()
            stream.Seek(0L, SeekOrigin.End) |> ignore
            stream.Write(bytes, 0, bytes.Length)
            stream.Flush()
            exit()

        { new ILog with
            member x.debug _range fmt =
                fmt |> Printf.kprintf (fun str -> if verbose then appendLine "DEBUG " str)
            member x.info _range fmt =
                fmt |> Printf.kprintf (fun str -> appendLine "INFO  " str)
            member x.warn _code _range fmt =
                fmt |> Printf.kprintf (fun str -> appendLine "WARN  " str)
            member x.error _code _range fmt =
                fmt |> Printf.kprintf (fun str -> appendLine "ERR   " str)
        }

    let ofList (logs : list<ILog>) =
        match logs with
        | [] -> empty
        | [s] -> s
        | l :: ls ->
            let merge (l : ILog) (r : ILog) =
                { new ILog with
                    member x.debug range fmt =
                        fmt |> Printf.kprintf (fun str -> l.debug range "%s" str; r.debug range "%s" str)
                    member x.info range fmt =
                        fmt |> Printf.kprintf (fun str -> l.info range "%s" str; r.info range "%s" str)
                    member x.warn c range fmt =
                        fmt |> Printf.kprintf (fun str -> l.warn c range "%s" str; r.warn c range "%s" str)
                    member x.error c range fmt =
                        fmt |> Printf.kprintf (fun str -> l.error c range "%s" str; r.error c range "%s" str)
                }

            ls |> List.fold merge l




[<RequireQualifiedAccess>]
type OutputStream =
    | Stdout
    | Stderr

[<RequireQualifiedAccess>]
type OutputMode =
    | None
    | Inline
    | Custom of (OutputStream -> string -> unit)

type ProcessConfig =
    {
        file        : string
        workDir     : string
        args        : list<string>
        output      : OutputMode
    }

module Process =
    open System.Diagnostics
    open System.Runtime.InteropServices

    let startDaemon (file : string) (args : list<string>) =
        let info = 
            ProcessStartInfo(
                file, String.concat " " (Seq.map (sprintf "\"%s\"") args),
                UseShellExecute = false,
                WorkingDirectory = Path.GetTempPath()
            )
        info.RedirectStandardInput <- true
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        
        let proc = new Process()
        
        proc.StartInfo <- info
        if proc.Start() then
            proc.StandardOutput.Close()
            proc.StandardError.Close()
            proc.StandardInput.Close()

    let tryStart (cfg : ProcessConfig) =
        try
            let cwd = if String.IsNullOrWhiteSpace cfg.workDir then Environment.CurrentDirectory else cfg.workDir
            let info = 
                ProcessStartInfo(
                    cfg.file, String.concat " " cfg.args,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true,
                    WorkingDirectory = cwd
                )

            let proc = new Process()
            proc.StartInfo <- info

            match cfg.output with
            | OutputMode.None -> ()
            | OutputMode.Inline ->
                proc.OutputDataReceived.Add (fun e ->
                    if not (String.IsNullOrEmpty e.Data) then
                        Console.WriteLine("{0}", e.Data)                    
                )
                proc.ErrorDataReceived.Add (fun e ->
                    if not (String.IsNullOrEmpty e.Data) then
                        Console.Error.WriteLine("{0}", e.Data)                    
                )
            | OutputMode.Custom write ->
                proc.OutputDataReceived.Add (fun e ->
                    if not (String.IsNullOrEmpty e.Data) then
                        write OutputStream.Stdout e.Data              
                )
                proc.ErrorDataReceived.Add (fun e ->
                    if not (String.IsNullOrEmpty e.Data) then
                        write OutputStream.Stderr e.Data                           
                )


            if proc.Start() then
                match cfg.output with
                | OutputMode.None -> 
                    proc.StandardOutput.Close()
                    proc.StandardError.Close()
                | _ ->
                    proc.BeginOutputReadLine()
                    proc.BeginErrorReadLine()

                Some proc
            else
                None 
        with e -> 
            None               




[<AutoOpen>]
module Versions =   
    open System.Reflection
    open System.Threading

    let newChecker() =
        let c = FSharpChecker.Create(projectCacheSize = 200, keepAssemblyContents = true)
        c.ImplicitlyStartBackgroundWork <- false
        c

    let startThread (run : unit -> unit) =
        let thread = Thread(ThreadStart(run), IsBackground = true)
        thread.Start()
        thread


    let selfVersion = 
        let version = 
            typeof<ILog>.Assembly.GetCustomAttributes(typeof<AssemblyVersionAttribute>, true)
            |> Array.choose (function :? AssemblyVersionAttribute as a -> Some a.Version | _ -> None)
            |> Array.tryHead
        match version with
        | Some v -> v
        | None -> "0.0.0.0"

