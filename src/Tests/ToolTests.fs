namespace Adaptify.Tests

open System.IO
open System.Runtime.InteropServices
open System.Threading
open FsUnit
open NUnit.Framework

[<TestFixture>]
module ToolTests =

    let config =
    #if DEBUG
        "Debug"
    #else
        "Release"
    #endif

#if NET8_0
    let netRuntime = 8
#endif

#if NET9_0
    let netRuntime = 9
#endif

    let net8Proj =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "Examples", "Net8", "Net8.fsproj")

    let net9Proj =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "Examples", "Net9", "Net9.fsproj")

    let netFrameworkProj =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "Examples", "NetFramework", "NetFramework.fsproj")

    let adaptifyPath =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "bin", config, "dotnet-tool", $"net{netRuntime}.0", "adaptify.dll")

    let globalJsonData =
        sprintf """{ "sdk": { "version": "%d.0.0", "rollForward": "latestFeature" } }""" netRuntime

    module private File =

        let deleteSafe (path: string) =
            try if File.Exists path then File.Delete path
            with _ -> ()

    module private Directory =

        let containsGFiles (directory: string) =
            let files = Directory.EnumerateFiles(directory, "*.g.fs", SearchOption.TopDirectoryOnly)
            not <| Seq.isEmpty files

        let deleteGFiles (directory: string) =
            let files = Directory.EnumerateFiles(directory, "*.g.fs", SearchOption.TopDirectoryOnly)
            for f in files do File.deleteSafe f

        let lock (action: unit -> 'T) (projectPath: string) =
            let dir = Path.GetDirectoryName(projectPath)
            let lockFile = Path.Combine(dir, "test.lock")

            let mutable stream : FileStream = null
            while isNull stream do
                try
                    let s = new FileStream(lockFile, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
                    stream <- s
                with _ ->
                    printfn $"Waiting to lock '{lockFile}..."
                    Thread.Sleep 400

            try
                action()
            finally
                stream.Close()
                File.deleteSafe lockFile

    module private Adaptify =

        let run (projectPath: string) =
            projectPath |> Directory.lock (fun () ->
                // Ionide is really picky about the .NET SDK version, so we have to put an appropriate global.json
                // next to the project file before trying to load it....
                let globalJson = Path.Combine(Path.GetDirectoryName(projectPath), "global.json")

                try
                    File.WriteAllLines(globalJson, [| globalJsonData |])
                    Process.run true None "dotnet" $"{adaptifyPath} --lenses --local --verbose --force \"{projectPath}\"" |> ignore
                finally
                    File.deleteSafe globalJson
            )

    [<SetUp>]
    let Setup() =
        Assert.That(net8Proj, Does.Exist)
        Assert.That(net9Proj, Does.Exist)
        Assert.That(netFrameworkProj, Does.Exist)

    let private test (projectPath: string) =
        let dir = Path.GetDirectoryName projectPath
        if Directory.containsGFiles dir then
            Assert.Ignore($"Directory '{dir}' contains *.g.fs files.")

        try
            Adaptify.run projectPath
            dir |> Directory.containsGFiles |> should be True
        finally
            Directory.deleteGFiles dir

    [<Test>]
    let ``[Adaptify] NET 8``() =
        test net8Proj

#if NET9_0
    [<Test>]
    let ``[Adaptify] NET 9``() =
        test net9Proj
#endif

    [<Test>]
    let ``[Adaptify] NET Framework``() =
        if not <| RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            Assert.Ignore("Windows-only")

        test netFrameworkProj
