namespace Adaptify.Compiler

open System
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Range


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
    
    let console (verbose : bool) =  

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

        { new ILog with
            member x.debug range fmt =
                fmt |> Printf.kprintf (fun str ->   
                    if verbose then 
                        lock x (fun () ->
                            Console.Write("> ")
                            useColor ConsoleColor.DarkGray (fun () ->
                                Console.Write(str)
                            )
                            writeRange range
                        )
                )

            
            member x.info range fmt =
                fmt |> Printf.kprintf (fun str ->   
                    lock x (fun () ->
                        Console.Write("> ")
                        useColor ConsoleColor.Gray (fun () ->
                            Console.Write(str)
                        )
                        writeRange range
                    )
                )
            
            member x.warn range code fmt =
                fmt |> Printf.kprintf (fun str ->  
                    lock x (fun () -> 
                        Console.Write("> ")
                        useColor ConsoleColor.DarkYellow (fun () ->
                            Console.Write(str)
                        )
                        writeRange range
                    )
                )
            
            member x.error range code fmt =
                fmt |> Printf.kprintf (fun str ->  
                    lock x (fun () ->
                        Console.Write("> ")
                        useColor ConsoleColor.Red (fun () ->
                            Console.Write(str)
                        )
                        writeRange range
                    )
                )
        }

    let file (verbose : bool) (file : string) =
        let lines (prefix : string) (str : string) = 
            let now = DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")
            [ sprintf "%s %s %s" now prefix str ]

        { new ILog with
            member x.debug range fmt =
                fmt |> Printf.kprintf (fun str -> if verbose then File.AppendAllLines(file, lines "DEBUG " str))
            member x.info range fmt =
                fmt |> Printf.kprintf (fun str -> File.AppendAllLines(file, lines "INFO  " str))
            member x.warn c range fmt =
                fmt |> Printf.kprintf (fun str -> File.AppendAllLines(file, lines "WARN  " str))
            member x.error c range fmt =
                fmt |> Printf.kprintf (fun str -> File.AppendAllLines(file, lines "ERR   " str))
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


[<AutoOpen>]
module Versions =   
    open System.Reflection

    let selfVersion =
        let version = 
            typeof<ILog>.Assembly.GetCustomAttributes(typeof<AssemblyVersionAttribute>, true)
            |> Array.choose (function :? AssemblyVersionAttribute as a -> Some a.Version | _ -> None)
            |> Array.tryHead
        match version with
        | Some v -> v
        | None -> "0.0.0.0"

    //let pipeName = "adaptifypipe_" + selfVersion
