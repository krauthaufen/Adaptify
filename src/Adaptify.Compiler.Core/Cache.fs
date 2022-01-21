namespace Aardvark.Compiler

open System.IO
open Adaptify.Compiler



type Warning =
    {
        isError : bool
        startLine : int
        startCol : int
        endLine : int
        endCol : int
        message : string
        code : string
    }

[<AutoOpen>]
module private StringHelpers =

    let toBase64 (str : string) =
        System.Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes str)

    let fromBase64 (str : string) =
        System.Convert.FromBase64String str |> System.Text.Encoding.UTF8.GetString


module Warning =

    let parse (str : string) : list<Warning> =
        if str.Length = 0 then 
            []
        else
            let str = fromBase64 str
            str.Split([|"\r\n" |], System.StringSplitOptions.None)
            |> Array.toList
            |> List.map (fun (l : string) ->
                let c : string[] = l.Split([| ";" |], System.StringSplitOptions.None)
                {
                    isError = false
                    startLine = int c.[0]
                    startCol = int c.[1]
                    endLine = int c.[2]
                    endCol = int c.[3]
                    code = fromBase64 c.[4]
                    message = fromBase64 c.[5]
                }
            )

    let pickle (l : list<Warning>) =
        let string =
            l |> List.map (fun w ->
                sprintf "%d;%d;%d;%d;%s;%s" w.startLine w.startCol w.endLine w.endCol (toBase64 w.code) (toBase64 w.message)
            ) |> String.concat "\r\n"
            
        string
        |> toBase64


type FileCacheEntry =
    {
        fileHash    : string
        hasModels   : bool
        warnings    : list<Warning>
    }


type CacheFile =
    {
        lenses : bool
        projectHash : string
        fileHashes : Map<string, FileCacheEntry>
    }

module CacheFile =
    let tryRead (log : ILog) (path : string) =
        try
            let lines = File.ReadAllLines path
            if lines.Length >= 3 && lines.[0].Trim() = selfVersion then
                let fileHashes =
                    Array.skip 3 lines |> Seq.map (fun l ->
                        let comp = l.Split([|";"|], System.StringSplitOptions.None)

                        let warnings =
                            if comp.Length > 3 then Warning.parse comp.[3]
                            else []

                        let hasModels = comp.[2].ToLower().Trim() = "true"

                        fromBase64 comp.[0], { fileHash = comp.[1]; hasModels = hasModels; warnings = warnings }
                    )
                    |> Map.ofSeq
                Some {
                    lenses = lines.[1].ToLower().Trim() = "true"
                    projectHash = lines.[2]
                    fileHashes = fileHashes
                }
            else
                None
        with e ->
            log.debug FSharp.Compiler.Text.Range.range0 "could not read cache file: %A" e
            None

    let save (cache : CacheFile) (path : string) =
        try
            File.WriteAllLines(path, [|
                yield selfVersion
                yield if cache.lenses then "true" else "false"
                yield cache.projectHash
                for (file, entry) in Map.toSeq cache.fileHashes do
                    let wrn = Warning.pickle entry.warnings
                    yield sprintf "%s;%s;%s;%s" (toBase64 file) entry.fileHash (if entry.hasModels then "true" else "false") wrn
            |])
        with _ ->
            ()
