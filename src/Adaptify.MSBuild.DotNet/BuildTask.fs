namespace Adaptify.MSBuild

open Adaptify.Compiler
open Microsoft.Build.Utilities
open Microsoft.Build.Framework
open System.IO
open FSharp.Compiler.SourceCodeServices

type AdaptifyTask() =
    inherit Task()

    let mutable debug = false
    let mutable files : string[] = [||]
    let mutable references : string[] = [||]
    let mutable projectFile = ""
    let mutable results : string[] = [||]
    let mutable framework : string = ""
    let mutable outputType : string = ""

    override x.Execute() =
        if debug then
            System.Diagnostics.Debugger.Launch() |> ignore
            
        match Path.GetExtension projectFile with
            | ".fsproj" -> 
                try
                    let targetType = 
                        match outputType.ToLower() with
                            | "winexe" -> Target.WinExe
                            | "exe" -> Target.Exe
                            | _ -> Target.Library

                    let isNetFramework = references |> Array.exists (fun r -> Path.GetFileNameWithoutExtension(r).ToLower() = "mscorlib")
                    let checker = FSharpChecker.Create(keepAssemblyContents = true)
                    

                    let inFiles =
                        let rec appendGenerated (f : list<string>) =
                            match f with
                            | [] -> []
                            | [s] when targetType = Target.Exe -> [s]
                            | [s] -> [s; Path.ChangeExtension(s, ".g.fs")]
                            | h :: t ->
                                h :: Path.ChangeExtension(h, ".g.fs") :: appendGenerated t
                        appendGenerated (Array.toList files)

                    let projInfo =
                        {
                            project = projectFile
                            isNewStyle = not isNetFramework
                            references = Array.toList references
                            files = inFiles
                            defines = []
                            target = targetType
                            output = Some (Path.GetTempFileName() + ".dll")
                            additional = []
                            debug = DebugType.Off
                        }

                    let options = 
                        checker.GetProjectOptionsFromCommandLineArgs(
                            projectFile, 
                            ProjectInfo.toFscArgs projInfo |> List.toArray
                        )
                    let projDir = Path.GetDirectoryName projectFile

                    let newFiles = System.Collections.Generic.List<string>()


                    for file in files do
                        let name = Path.GetFileNameWithoutExtension file

                        let path = Path.Combine(projDir, file)
                        let content = File.ReadAllText path
                        let text = FSharp.Compiler.Text.SourceText.ofString content
                        let (_parseResult, answer) = checker.ParseAndCheckFileInProject(file, 0, text, options) |> Async.RunSynchronously
        
                        match answer with
                        | FSharpCheckFileAnswer.Succeeded res ->
                            let adaptors = 
                                res.PartialAssemblySignature.Entities
                                |> Seq.toList
                                |> List.collect (fun e -> Adaptor.generate { qualifiedPath = Option.toList e.Namespace; file = path } e)
                                |> List.groupBy (fun (f, _, _) -> f)
                                |> List.map (fun (f, els) -> 
                                    f, els |> List.map (fun (_,m,c) -> m, c) |> List.groupBy fst |> List.map (fun (m,ds) -> m, List.map snd ds) |> Map.ofList
                                )
                                |> Map.ofList

                            let builder = System.Text.StringBuilder()

                            for (ns, modules) in Map.toSeq adaptors do
                                sprintf "namespace %s" ns |> builder.AppendLine |> ignore
                                sprintf "open FSharp.Data.Adaptive" |> builder.AppendLine |> ignore
                                for (m, def) in Map.toSeq modules do
                                    sprintf "[<AutoOpen>]" |> builder.AppendLine |> ignore
                                    sprintf "module rec %s =" m |> builder.AppendLine |> ignore
                                    for d in def do
                                        for l in d do
                                            sprintf "    %s" l |> builder.AppendLine |> ignore


                            newFiles.Add file
                            if builder.Length > 0 then
                                let file = Path.ChangeExtension(path, ".g.fs")
                                File.WriteAllText(file, builder.ToString())
                                newFiles.Add (Path.ChangeExtension(file, ".g.fs"))

                        | FSharpCheckFileAnswer.Aborted ->
                            ()


                    results <- Seq.toArray newFiles
                    true
                with e ->
                    false
              
             | other -> 
                results <- files
                true
                


    member x.Debug
        with get() = debug
        and set i = debug <- i
        
    member x.TargetFramework
        with get() = framework
        and set i = framework <- i

    member x.OutputType
        with get() = outputType
        and set t = outputType <- t

    [<Required>]
    member x.Files
        with get() = files
        and set i = files <- i

    [<Required>]
    member x.References
        with get() = references
        and set c = references <- c

    [<Required>]
    member x.ProjectFile
        with get() = projectFile
        and set f = projectFile <- f

   
    [<Output>]
    member x.Results
        with get() = results
        and set r = results <- r
