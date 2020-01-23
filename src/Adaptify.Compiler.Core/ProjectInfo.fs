﻿namespace Adaptify.Compiler

open System.IO


[<RequireQualifiedAccess>]
type Target =   
    | Exe
    | Library
    | WinExe
    | Module

[<RequireQualifiedAccess>]
type DebugType =
    | Off
    | Full
    | PdbOnly
    | Portable


type ProjectInfo =
    {
        project     : string
        isNewStyle  : bool
        references  : list<string>
        files       : list<string>
        defines     : list<string>
        target      : Target
        output      : Option<string>
        additional  : list<string>
        debug       : DebugType
    }

module ProjectInfo = 

    let ofFscArgs (isNewStyle : bool) (path : string) (args : list<string>) =
        let mutable parsed = Set.empty

        let removeArg (a : string) = parsed <- Set.add a parsed

        let references = 
            args |> List.choose (fun a ->
                if a.StartsWith "-r:" then removeArg a; Some (a.Substring 3)
                elif a.StartsWith "--reference:" then removeArg a; Some (a.Substring 12)
                else None
            )

        let files =
            args |> List.filter (fun a -> 
                if not (a.StartsWith "-") then
                    let isAssemblyInfo = (Path.GetFileName(a).ToLower().EndsWith "assemblyinfo.fs")
                    removeArg a
                    not isAssemblyInfo
                else
                    false
            ) 

        let output =
            args |> List.tryPick (fun a ->
                if a.StartsWith "-o:" then removeArg a; Some (a.Substring 3)
                elif a.StartsWith "--out:" then removeArg a; Some (a.Substring 6)
                else None
            )

        let target =
            args |> List.tryPick (fun a ->
                if a.StartsWith "--target:" then
                    removeArg a
                    let target = a.Substring(9).Trim().ToLower()
                    match target with
                    | "exe" -> Some Target.Exe
                    | "library" -> Some Target.Library
                    | "winexe" -> Some Target.WinExe
                    | "module" -> Some Target.Module
                    | _ -> None
                else
                    None
            )

        let defines =
            args |> List.choose (fun a ->
                if a.StartsWith "-d:" then removeArg a; Some (a.Substring 3)
                elif a.StartsWith "--define:" then removeArg a; Some (a.Substring 9)
                else None
            )

        let hasDebug =
            args |> List.tryPick (fun a ->
                let rest = 
                    if a.StartsWith "-g" then Some (a.Substring(2).Replace(" ", ""))
                    elif a.StartsWith "--debug" then Some (a.Substring(7).Replace(" ", ""))
                    else None
                        
                match rest with
                | Some "" | Some "+" -> removeArg a; Some true
                | Some "-" -> removeArg a; Some false
                | _ -> None
            )

        let debugType =
            args |> List.tryPick (fun a ->
                let rest = 
                    if a.StartsWith "-g" then Some (a.Substring(2).Replace(" ", ""))
                    elif a.StartsWith "--debug" then Some (a.Substring(7).Replace(" ", ""))
                    else None
                        
                match rest with
                | Some ":full" -> removeArg a; Some DebugType.Full
                | Some ":pdbonly" -> removeArg a; Some DebugType.PdbOnly
                | Some ":portable" -> removeArg a; Some DebugType.Portable
                | _ -> None
            )

        let additional =
            args |> List.filter (fun a -> not (Set.contains a parsed))

        let debug =
            match hasDebug with
            | Some true -> defaultArg debugType DebugType.Full
            | Some false -> DebugType.Off
            | None -> defaultArg debugType DebugType.Full 

        {
            isNewStyle  = isNewStyle
            project = path
            //fscArgs     = args
            references  = references
            files       = files
            target      = defaultArg target Target.Library
            defines     = defines
            additional  = additional
            output      = output
            debug       = debug
        }

    let toFscArgs (info : ProjectInfo) =
        [
            match info.output with
            | Some o -> yield sprintf "-o:%s" o
            | None -> ()

            match info.debug with
            | DebugType.Off ->
                yield "--debug-"
            | DebugType.Full -> 
                yield "--debug:full"
            | DebugType.Portable -> 
                yield "--debug:portable"
            | DebugType.PdbOnly -> 
                yield "--debug:pdbonly"
                
            for a in info.additional do
                yield a

            for r in info.references do
                yield sprintf "-r:%s" r

            for f in info.files do
                yield f

        ]

    let computeHash (info : ProjectInfo) =
        use ms = new MemoryStream()
        use w = new StreamWriter(ms)

        w.WriteLine(info.project)
        w.WriteLine(
            if info.isNewStyle then "newstyle"
            else "oldstyle"
        )
        w.WriteLine "references"
        for r in List.sort info.references do w.WriteLine(r)
        w.WriteLine "files"
        for f in info.files do w.WriteLine f
        w.WriteLine "defines"
        for f in List.sort info.defines do w.WriteLine f
        w.WriteLine(
            match info.target with
            | Target.Exe -> "exe"
            | Target.WinExe -> "winexe"
            | Target.Library -> "library"
            | Target.Module -> "module"
        )
        w.WriteLine "additional"
        for a in List.sort info.additional do w.WriteLine a
        w.WriteLine(
            match info.debug with
            | DebugType.Full -> "full"
            | DebugType.Off -> "off"
            | DebugType.PdbOnly -> "pdbonly"
            | DebugType.Portable -> "portable"
        )

        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let md5 = System.Security.Cryptography.MD5.Create()
        let hash = md5.ComputeHash(ms)
        System.Guid hash |> string

    [<AutoOpen>]
    module private PickleHelpers = 
        let pickleTarget (t : Target) =
            match t with
            | Target.Exe -> 0
            | Target.Library -> 1
            | Target.Module -> 2
            | Target.WinExe -> 3
        let pickleDebugType (t : DebugType) =
            match t with
            | DebugType.Off -> 0
            | DebugType.PdbOnly -> 1
            | DebugType.Portable -> 2
            | DebugType.Full -> 3

        let unpickleTarget (v : int) =
            match v with
            | 0 -> Target.Exe
            | 2 -> Target.Module
            | 3 -> Target.WinExe
            | _ -> Target.Library

        let unpickleDebugType (v : int) =
            match v with
            | 1 -> DebugType.PdbOnly
            | 2 -> DebugType.Portable
            | 3 -> DebugType.Full
            | _ -> DebugType.Off






    let tryUnpickleOf (stream : Stream) =
        let p = stream.Position
        try
            use r = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
        
            let project = r.ReadString()
            let isNewStyle = r.ReadBoolean()
            let c = r.ReadInt32()
            let references = List.init c (fun _ -> r.ReadString())
        
            let c = r.ReadInt32()
            let files = List.init c (fun _ -> r.ReadString())
        
            let c = r.ReadInt32()
            let defines = List.init c (fun _ -> r.ReadString())

            let target = r.ReadInt32() |> unpickleTarget

            let has = r.ReadBoolean()
            let output = if has then r.ReadString() |> Some else None
        
            let c = r.ReadInt32()
            let additional = List.init c (fun _ -> r.ReadString())

            let debug = r.ReadInt32() |> unpickleDebugType

            Some {
                isNewStyle  = isNewStyle
                project     = project
                references  = references
                files       = files
                target      = target
                defines     = defines
                additional  = additional
                output      = output
                debug       = debug
            }
        with _ ->
            stream.Position <- p
            None

    let pickleTo (stream : Stream) (info : ProjectInfo) =
        use w = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)

        w.Write info.project
        w.Write info.isNewStyle

        w.Write (List.length info.references)
        for r in info.references do w.Write r

        w.Write (List.length info.files)
        for f in info.files do w.Write f

        w.Write (List.length info.defines)
        for f in info.defines do w.Write f

        w.Write (pickleTarget info.target)

        match info.output with
        | Some o -> 
            w.Write(true)
            w.Write(o)
        | None ->
            w.Write(false)

        w.Write (List.length info.additional)
        for f in info.additional do w.Write f

        w.Write(pickleDebugType info.debug)


    let pickle (info : ProjectInfo) =
        use ms = new System.IO.MemoryStream()
        pickleTo ms info
        ms.ToArray()

    let tryUnpickle (arr : byte[]) =
        use ms = new System.IO.MemoryStream(arr)
        tryUnpickleOf ms
        

