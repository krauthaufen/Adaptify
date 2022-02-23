namespace Adaptify.Compiler

open System
open FSharp.Compiler.Symbols


type Scope =
    | Global
    | Namespace of string
    | Module of parent : Scope * name : string * isAutoOpen : bool * hasModuleSuffix : bool

module Scope =
    let rec ofFSharpEntity (log : ILog) (e : FSharpEntity) =
        if e.IsNamespace then
            match e.DeclaringEntity with
            | Some d ->
                match ofFSharpEntity log d with
                | Namespace p -> p + "." + e.DisplayName |> Namespace
                | _ -> failwith "invalid F#"

            | None ->
                Namespace(e.DisplayName)

        elif e.IsFSharpModule then
            let autoOpen = e.Attributes |> Seq.exists (FSharpAttribute.isAutoOpen log)
            let moduleSuffix = e.HasFSharpModuleSuffix
            let name = e.DisplayName
            match e.DeclaringEntity with
            | Some d ->
                let parent = ofFSharpEntity log d
                Module(parent, name, autoOpen, moduleSuffix)
            | None ->
                match e.Namespace with
                | Some ns -> Module(Namespace ns, name, autoOpen, moduleSuffix)
                | None -> Module(Global, name, autoOpen, moduleSuffix)
        elif e.IsClass then
            let ns = e.Namespace
            let name = e.DisplayName
            match e.Namespace with
                | Some ns -> 
                    Module(Namespace ns, name, false, false)
                | None -> 
                    Module(Global, name, false, false)
        else
            failwithf "%s is neither a module nor a namespace" e.DisplayName
         
    let rec ofFSharpEntityOpt (log : ILog) (e : option<FSharpEntity>) (ns : option<string>) =  
        match e with
        | None ->
            match ns with
            | Some ns -> Namespace ns
            | None -> Global
        | Some e -> ofFSharpEntity log e

    let rec fullName (scope : Scope) =
        match scope with
        | Global -> None
        | Namespace ns -> Some ns
        | Module(parent, name, _, _) ->
            match fullName parent with
            | None -> Some name
            | Some v -> Some (v + "." + name)
        
    let private opened =
        [
            "System"
            "Microsoft.FSharp.Core"
            "Microsoft.FSharp.Collections"
            "FSharp.Data.Adaptive"
            "Adaptify"
        ]

    let rec shortName (scope : Scope) =
        match fullName scope with
        | Some full ->
            let shortened = 
                opened |> Seq.tryPick (fun o ->
                    if full.StartsWith o then Some (full.Substring o.Length)
                    else None
                )
            match shortened with
            | Some s -> 
                if String.IsNullOrWhiteSpace s then None
                else Some s
            | None -> 
                Some full
        | None ->
            None

    let rec relativeName (relativeTo : Scope) (scope : Scope) =
        let rec find (s : Scope) =
            if s = relativeTo then
                None
            else
                match s with
                | Module(parent, name, _, _) ->
                    match find parent with
                    | Some str -> str + "." + name |> Some
                    | None -> name |> Some
                | Namespace ns ->
                    Some ns
                | Global ->
                    None
        match find scope with
        | Some str -> Some str
        | None -> None
