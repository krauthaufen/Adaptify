#load ".paket/load/net6.0/build/build.group.fsx"

open System
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools
open System.Text.RegularExpressions

do  // init fake
    let args = System.Environment.GetCommandLineArgs() |> Array.toList |> List.skip 2
    let ctx = Context.FakeExecutionContext.Create false "build.fsx" args
    Context.setExecutionContext (Context.RuntimeContext.Fake ctx)
    Target.initEnvironment()

do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let notes = ReleaseNotes.load "RELEASE_NOTES.md"

let isWindows =
    Environment.OSVersion.Platform <> PlatformID.Unix && Environment.OSVersion.Platform <> PlatformID.MacOSX

Target.create "Clean" (fun _ ->
    if Directory.Exists "bin/Debug" then
        Trace.trace "deleting bin/Debug"
        Directory.delete "bin/Debug"
    if Directory.Exists "bin/Release" then
        Trace.trace "deleting bin/Release"
        Directory.delete "bin/Release"

    let pkgs = !!"bin/*.nupkg" |> Seq.toList
    if not (List.isEmpty pkgs) then
        Trace.tracefn "deleting packages: %s" (pkgs |> Seq.map Path.GetFileNameWithoutExtension |> String.concat ", ")
        File.deleteAll pkgs


    let dirs = Directory.EnumerateDirectories("src", "obj", SearchOption.AllDirectories) |> Seq.toList

    if not (List.isEmpty dirs) then 
        for d in dirs do    
            let parent = Path.GetDirectoryName d
            let proj = Directory.GetFiles(parent, "*.fsproj") |> Seq.isEmpty |> not
            if proj then
                Trace.tracefn "deleting %s" d
                Directory.delete d
)

Target.create "Compile" (fun _ ->
    let options (o : DotNet.BuildOptions) =
        let v = sprintf "%d.%d.%d.%s" notes.SemVer.Major notes.SemVer.Minor notes.SemVer.Patch (string notes.SemVer.Build)

        { o with 
            Configuration = DotNet.BuildConfiguration.Release
            
            MSBuildParams = 
                { o.MSBuildParams with 
                    DisableInternalBinLog = true
                    Properties = 
                        [
                            "GenerateAssemblyInfo", "true"
                            "AssemblyVersion", v
                            "FileVersion", v
                            "AssemblyFileVersion", v
                            "ProductVersion", v
                            "InformationalVersion", v
                        ] 
                }
        }
    DotNet.build options "Adaptify.sln"
)

Target.create "Pack" (fun _ ->
    let args = 
        [
            "paket"
            "pack"
            "--version"
            notes.NugetVersion
            "--interproject-references"
            "fix"
            "--release-notes"
            sprintf "\"%s\"" (String.concat "\\n" notes.Notes)
            "--project-url"
            "\"https://github.com/krauthaufen/Adaptify\""
            sprintf "\"%s\"" (Path.Combine(__SOURCE_DIRECTORY__, "bin"))
        ]
    let ret = 
        Process.shellExec {
            ExecParams.Program = "dotnet"
            WorkingDir = __SOURCE_DIRECTORY__
            CommandLine = String.concat " " args
            Args = []
        }
    if ret <> 0 then failwithf "paket failed with exit code %d" ret

    "src/Adaptify.Compiler/adaptify.fsproj" |> DotNet.pack (fun o -> 
        { o with        
            NoRestore = true
            NoBuild = true
            MSBuildParams = { o.MSBuildParams with DisableInternalBinLog = true; Properties = ["Version", notes.NugetVersion] }
        }
    )

)

Target.create "Push" (fun _ ->
    let packageNameRx = Regex @"^(?<name>[a-zA-Z_0-9\.-]+?)\.(?<version>([0-9]+\.)*[0-9]+.*?)\.nupkg$"
    
    if not (Git.Information.isCleanWorkingCopy ".") then
        Git.Information.showStatus "."
        failwith "repo not clean"

    
    if File.exists "deploy.targets" then
        let packages =
            !!"bin/*.nupkg"
            |> Seq.filter (fun path ->
                let name = Path.GetFileName path
                let m = packageNameRx.Match name
                if m.Success then
                    m.Groups.["version"].Value = notes.NugetVersion
                else
                    false
            )
            |> Seq.toList

        let targetsAndKeys =
            File.ReadAllLines "deploy.targets"
            |> Array.map (fun l -> l.Split(' '))
            |> Array.choose (function [|dst; key|] -> Some (dst, key) | _ -> None)
            |> Array.choose (fun (dst, key) ->
                let path = 
                    Path.Combine(
                        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                        ".ssh",
                        key
                    )
                if File.exists path then
                    let key = File.ReadAllText(path).Trim()
                    Some (dst, key)
                else
                    None
            )
            |> Map.ofArray
            
        
        Git.CommandHelper.directRunGitCommandAndFail "." "fetch --tags"
        Git.Branches.tag "." notes.NugetVersion

        let branch = Git.Information.getBranchName "."
        Git.Branches.pushBranch "." "origin" branch

        if List.isEmpty packages then
            failwith "no packages produced"

        if Map.isEmpty targetsAndKeys then
            failwith "no deploy targets"
            
        for (dst, key) in Map.toSeq targetsAndKeys do
            Trace.tracefn "pushing to %s" dst
            let options (o : Paket.PaketPushParams) =
                { o with 
                    PublishUrl = dst
                    ApiKey = key 
                    WorkingDir = "bin"
                }

            Paket.pushFiles options packages

        Git.Branches.pushTag "." "origin" notes.NugetVersion
    ()
)

Target.create "Default" ignore

Target.create "Docs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)
Target.create "GenerateDocs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "--define:RELEASE generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)


let cleanDir (dir : string) =
    let info = DirectoryInfo(dir)
    if info.Exists then
        let children = info.GetFileSystemInfos()
        for c in children do
            Trace.tracefn "delete %s" c.Name
            match c with
            | :? DirectoryInfo as d -> 
                if d.Name <> ".git" then d.Delete(true)
            | _ -> c.Delete()

let rec copyRecursive (src : string) (dst : string) =
    let info = DirectoryInfo(src)
    if info.Exists && not (info.Name.StartsWith ".") then
        Directory.ensure dst
        let children = info.GetFileSystemInfos()
        for c in children do
            Trace.tracefn "copy %s" c.Name
            match c with
            | :? DirectoryInfo as d -> copyRecursive d.FullName (Path.Combine(dst, d.Name))
            | :? FileInfo as f -> f.CopyTo(Path.Combine(dst, f.Name)) |> ignore
            | _ -> ()

let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FSharp.Data.Adaptive"

Target.create "ReleaseDocs" (fun _ ->
    let name = Guid.NewGuid() |> string
    let tempDocsDir = "temp/" + name
    let outputDir = "docs/output"

    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
    cleanDir tempDocsDir
    
    Directory.ensure outputDir
    copyRecursive outputDir tempDocsDir 
    Git.Staging.stageAll tempDocsDir
    let info = Git.Information.getCurrentSHA1 __SOURCE_DIRECTORY__
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s/%s" notes.NugetVersion info)
    Git.Branches.push tempDocsDir

    let rec reallyDelete (iter : int) =
        if iter >= 0 then 
            try Directory.Delete(tempDocsDir, true)
            with _ -> reallyDelete (iter - 1)

    reallyDelete 5
)


let ilrepack (outFolder : string) (args : list<string>) =
    let path = Path.GetFullPath(Path.Combine("packages", "build", "ILRepack", "tools", "ILRepack.exe"))
    let program, args =
        match Environment.OSVersion.Platform with
        | PlatformID.Unix | PlatformID.MacOSX ->
            "mono", path :: args
        | _ ->
            path, args

    let paramters = 
        { 
            Program = program
            WorkingDir = outFolder
            CommandLine = String.concat " " args
            Args = []
        }

    Fake.Core.Process.shellExec paramters


"Compile" ==> "Default" 

"Compile" ==> 
    "Docs"
    
"Compile" ==> 
    "GenerateDocs" ==> 
    "ReleaseDocs"

"Compile" ==> 
    "Pack" ==>
    "Push"



Target.runOrDefault "Default"


