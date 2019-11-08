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
    abstract member warn : range -> Printf.StringFormat<'T, unit> -> 'T
    abstract member error : range -> Printf.StringFormat<'T, unit> -> 'T

