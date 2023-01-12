namespace Adaptify

open System
open Adaptify.Compiler
open System.IO
open FSharp.Compiler.Text

module private Parsing =
    open System.Xml.Linq
    open System.Text.RegularExpressions

    let compileInclude = Regex("""(<Compile (Include="(.*\.g\.fs)") (\/)>)""")
    let addVisibleAttribute (s : string) =
        let m = compileInclude.Match s
        if m.Success then
            let a = XElement.Parse(s)
            a.SetAttributeValue("Visible", "false")
            a.ToString(SaveOptions.None) |> Some
        else
            None
    

    let addVisbleToAllCompileIncludes (fsprojContent : string) =
        let r (m : Match) = 
            match m.Value |> addVisibleAttribute with
            | None -> m.Value
            | Some v -> v
        compileInclude.Replace(fsprojContent, r)


    module Test =
        let test () = """<Compile Include="ab/b/f\cSceneObjects-Model.g.fs" />"""
        let result () = test () |> addVisibleAttribute  
        let all () = 
            File.ReadAllText @"C:\Users\steinlechner\Desktop\PRo3D-nomsbuild\src\PRo3D.Core\PRo3D.Core.fsproj"
            |> addVisbleToAllCompileIncludes
            |> printfn "%s"




module PatchProject =
   open Microsoft.Build.Construction

   let patchProject (log : ILog) (projectFileName : string) (hideGFilesInProjects : bool) (adaptifyFiles : array<string * string>) = 
        let p = ProjectRootElement.Open(projectFileName)
        let mutable changed = false
        for g in p.ItemGroups do
            for i in g.Items do
                if i.ItemType = "Compile" && Path.GetExtension(i.Include) = ".fs" then
                    let toSlash (s : string) = s.Replace("\\","/")
                    match Array.tryFind (fun (source,gen) -> toSlash i.Include = toSlash source) adaptifyFiles with
                    | None -> ()
                    | Some (source, generatedFileName) -> 
                        let correspondingGFile =
                            g.Items |> Seq.tryFind (fun o -> 
                                o.Include = generatedFileName
                            )
                        match correspondingGFile with
                        | Some gFile -> 
                            log.info Range.range0 "[PatchProject] skipped %s (already in project)" gFile.Include
                        | None -> 
                            let newElement = i.Clone() |> unbox<ProjectItemElement>
                            newElement.Include <- generatedFileName
                            g.InsertAfterChild(newElement,i)
                            changed <- true
        if changed then
            p.Save()
            log.info Range.range0 "[PatchProject] added .g files to project %s." projectFileName
            if hideGFilesInProjects then
                let patched = Parsing.addVisbleToAllCompileIncludes (File.ReadAllText projectFileName)
                File.WriteAllText(projectFileName, patched)
        else
            log.info Range.range0 "[PatchProject] skipped all .g files in project -> nothing was added."