namespace Adaptify.Compiler

open System
open FSharp.Compiler.Range
open System.IO
open System.IO.Pipes
open System.Threading

[<RequireQualifiedAccess>]
type Severity =
    | Debug     = 0
    | Info      = 1
    | Warn      = 2
    | Error     = 3

type Message =
    {
        range       : range
        code        : Option<string>
        severity    : Severity
        message     : string
        time        : DateTime
    }

module Message =
    let pickleTo (stream : Stream) (msg : Message) =
        use w = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)

        w.Write(msg.range.FileName)
        w.Write(msg.range.StartLine)
        w.Write(msg.range.StartColumn)
        w.Write(msg.range.EndLine)
        w.Write(msg.range.EndColumn)

        match msg.code with
        | Some code ->
            w.Write true
            w.Write code
        | None ->
            w.Write false

        w.Write (int msg.severity)
        w.Write msg.message
        w.Write (msg.time.ToString("s", System.Globalization.CultureInfo.InvariantCulture))

    let tryUnpickle (stream : Stream) =
        let p = stream.Position
        try
            use r = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
        

            let file = r.ReadString()
            let startLine = r.ReadInt32()
            let startCol = r.ReadInt32()
            let endLine = r.ReadInt32()
            let endCol = r.ReadInt32()

            let range = mkRange file (mkPos startLine startCol) (mkPos endLine endCol)

            let has = r.ReadBoolean()
            let code = if has then r.ReadString() |> Some else None
            let severity = r.ReadInt32() |> unbox<Severity>
            let message = r.ReadString()
            let time = DateTime.Parse(r.ReadString(), System.Globalization.CultureInfo.InvariantCulture)

            Some {
                range = range
                severity = severity
                code = code
                message = message
                time = time
            }
        with _ ->
            stream.Position <- p
            None

[<RequireQualifiedAccess>]
type Command =
    | AdaptifyFile of rebuild : bool * projectFile : string * msbuildprops : list<string * string>
    | AdaptifyInfo of rebuild : bool * projectInfo : ProjectInfo
    
module Command =
    let pickleTo (stream : Stream) (cmd : Command) =
        use w = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)
        match cmd with
        | Command.AdaptifyFile(rebuild, file, props) ->
            w.Write("AdaptifyFile")
            w.Write rebuild
            w.Write(file)
            w.Write(List.length props)
            for (k,v) in props do 
                w.Write k
                w.Write v
        | Command.AdaptifyInfo(rebuild, info) ->
            w.Write("AdaptifyInfo")
            w.Write rebuild
            ProjectInfo.pickleTo stream info

    let tryUnpickle (stream : Stream) =
        let p = stream.Position
        try
            use r = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
            let case = r.ReadString()
            match case with
            | "AdaptifyFile" -> 
                let rebuild = r.ReadBoolean()
                let file = r.ReadString()
                let len = r.ReadInt32()
                let props = List.init len (fun _ -> r.ReadString(), r.ReadString())
                Command.AdaptifyFile(rebuild, file, props) |> Some
            | "AdaptifyInfo" ->
                let rebuild = r.ReadBoolean()
                match ProjectInfo.tryUnpickle stream with
                | Some info -> 
                    Command.AdaptifyInfo(rebuild, info) |> Some
                | None -> 
                    stream.Position <- p
                    None
            | _ ->
                stream.Position <- p
                None
        with _ ->
            stream.Position <- p
            None


[<RequireQualifiedAccess>]
type Reply =
    | Success of info : ProjectInfo * messages : list<Message>
    | Error of messages : list<Message>
    
module Reply =
    let pickleTo (stream : Stream) (reply : Reply) =
        use w = new BinaryWriter(stream, System.Text.Encoding.UTF8, true)
        match reply with
        | Reply.Success(info, messages) ->
            w.Write "Success"
            ProjectInfo.pickleTo stream info
            w.Write (List.length messages)
            for m in messages do Message.pickleTo stream m
        | Reply.Error messages ->
            w.Write "Error"
            w.Write (List.length messages)
            for m in messages do Message.pickleTo stream m
            
    let tryUnpickle (stream : Stream) =
        let p = stream.Position
        try
            use r = new BinaryReader(stream, System.Text.Encoding.UTF8, true)
            match r.ReadString() with
            | "Success" ->
                match ProjectInfo.tryUnpickle stream with
                | Some info ->
                    let len = r.ReadInt32()
                    let messages = List.init len (fun _ -> Message.tryUnpickle stream)

                    if List.forall Option.isSome messages then
                        let messages = List.map Option.get messages
                        Some (Reply.Success(info, messages))
                    else
                        stream.Position <- p
                        None
                | None ->
                    stream.Position <- p
                    None
            | "Error" ->
                let len = r.ReadInt32()
                let messages = List.init len (fun _ -> Message.tryUnpickle stream)

                if List.forall Option.isSome messages then
                    let messages = List.map Option.get messages
                    Some (Reply.Error messages)
                else
                    stream.Position <- p
                    None
            | _ ->
                stream.Position <- p
                None
        with _ ->
            stream.Position <- p
            None


module internal PipeStreamHelpers =
    let rec read (ct : CancellationToken) (buffer : ref<byte[]>) (offset : int) (size : int) (pipe : PipeStream) =
        async {
            let! bytes = pipe.ReadAsync(!buffer, offset, size, ct) |> Async.AwaitTask
            let e = offset + bytes
            if pipe.IsMessageComplete then
                if e < buffer.Value.Length then 
                    Array.Resize(&buffer.contents, e)
                return !buffer
            else
                let free = buffer.Value.Length - e
                if free <= 0 then 
                    Array.Resize(&buffer.contents, e + 4096)
                    return! read ct buffer e 4096 pipe
                else
                    return! read ct buffer e free pipe
                    
        }

[<AutoOpen>]
module PipeStreamExtensions =
    type PipeStream with
        member x.ReadMessage(?ct : CancellationToken) : Async<byte[]> =
            let ct = defaultArg ct CancellationToken.None
            async {
                let buffer : byte[] = Array.zeroCreate 4096
                return! PipeStreamHelpers.read ct (ref buffer) 0 4096 x
            }

        member x.Send(msg : Command) =
            use ms = new MemoryStream()
            Command.pickleTo ms msg
            let data = ms.ToArray()
            x.Write(data, 0, data.Length)

        member x.SendAsync(msg : Command) =
            use ms = new MemoryStream()
            Command.pickleTo ms msg
            let data = ms.ToArray()
            x.WriteAsync(data, 0, data.Length) |> Async.AwaitTask
            
        member x.SendAsync(msg : Reply) =
            use ms = new MemoryStream()
            Reply.pickleTo ms msg
            let data = ms.ToArray()
            x.WriteAsync(data, 0, data.Length) |> Async.AwaitTask

        member x.ReceiveCommandAsync(?ct : CancellationToken) =
            async {
                let! data = x.ReadMessage(?ct = ct)
                use ms = new MemoryStream(data)
                return Command.tryUnpickle ms
            }
        
        member x.ReceiveReplyAsync(?ct : CancellationToken) =
            async {
                let! data = x.ReadMessage(?ct = ct)
                use ms = new MemoryStream(data)
                return Reply.tryUnpickle ms
            }



module Client =
    open System.Diagnostics

    let l = obj()

    let rec connectOrStart (path : string) (tries : int) (pipeName : string) (client : NamedPipeClientStream) =
        lock l (fun () ->
            if tries > 0 then
                try client.Connect(1000)
                with :? TimeoutException ->
                    let full = Path.Combine(path, "tools", "Adaptify.Server.dll")
                    let start = ProcessStartInfo("dotnet", sprintf "\"%s\" %s" full pipeName)
                    start.UseShellExecute <- false
                    let proc = Process.Start(start)
                    try client.Connect(5000)
                    with :? TimeoutException -> connectOrStart path (tries - 1) pipeName client
            else
                raise <| TimeoutException()
        )

    let run (path : string) (pipeName : string) (cmd : Command) =
        try
            use client =
                new NamedPipeClientStream(
                    "127.0.0.1", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous
                )
            connectOrStart path 2 pipeName client 
            client.ReadMode <- PipeTransmissionMode.Message
            client.Send(cmd)
            client.ReceiveReplyAsync() |> Async.RunSynchronously
        with _ ->
            None

    //let runAsync (pipeName : string) (cmd : Command) =
    //    async {
    //        try
    //            let! ct = Async.CancellationToken
    //            use client =
    //                new NamedPipeClientStream(
    //                    "127.0.0.1", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous 
    //                )
    //            do! client.ConnectAsync(ct) |> Async.AwaitTask
    //            client.ReadMode <- PipeTransmissionMode.Message
    //            do! client.SendAsync(cmd)
    //            return! client.ReceiveReplyAsync(ct)
    //        with _ ->
    //            return None
    //    }

type CacheFile =
    {
        projectHash : string
        fileHashes : Map<string, string * bool>
    }

module CacheFile =
    let tryRead (path : string) =
        try
            let lines = File.ReadAllLines path
            let fileHashes =
                Array.skip 1 lines |> Seq.map (fun l ->
                    let comp = l.Split([|";"|], System.StringSplitOptions.None)
                    comp.[0], (comp.[1], System.Boolean.Parse comp.[2])
                )
                |> Map.ofSeq
            Some {
                projectHash = lines.[0]
                fileHashes = fileHashes
            }
        with _ ->
            None

    let save (cache : CacheFile) (path : string) =
        try
            File.WriteAllLines(path, [|
                yield cache.projectHash
                for (file, (hash, modelTypes)) in Map.toSeq cache.fileHashes do
                    yield sprintf "%s;%s;%A" file hash modelTypes
            |])
        with _ ->
            ()
