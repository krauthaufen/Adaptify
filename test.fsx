#r "./bin/Adaptify.Compiler.Core.dll"

open Adaptify.Compiler
open System.Reflection

 let version = 
        typeof<ILog>.Assembly.GetCustomAttributes(typeof<AssemblyVersionAttribute>, true)
        |> Array.choose (function :? AssemblyVersionAttribute as a -> Some a.Version | _ -> None)
        |> Array.tryHead
    
    match version with
    | Some v -> 
        log.info range0 "version: %s" v
        v
    | None -> 
        log.info range0 "no version: 0.0.0.0"
        "0.0.0.0"
