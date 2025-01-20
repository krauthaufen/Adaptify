namespace Adaptify.Tests

open System
open System.Diagnostics
open System.Text

module internal Process =

    let run (writeOutputToConsole: bool) (directory: string option) (cmd: string) (args: string) =
        use p = new Process()
        p.StartInfo.FileName <- cmd
        p.StartInfo.Arguments <- args
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.RedirectStandardError <- true
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.CreateNoWindow <- true
        p.StartInfo.WorkingDirectory <- directory |> Option.defaultValue null

        let output = ResizeArray<string>()
        p.OutputDataReceived.Add (fun args ->
            if writeOutputToConsole then
                Console.WriteLine args.Data

            if not <| String.IsNullOrWhiteSpace args.Data then
                lock output (fun _ -> output.Add args.Data)
        )

        let errors = ResizeArray<string>()
        p.ErrorDataReceived.Add (fun args ->
            if not <| String.IsNullOrWhiteSpace args.Data then
                lock errors (fun _ -> errors.Add args.Data)
        )

        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        if p.ExitCode <> 0 || errors.Count > 0 then
            let sb = StringBuilder()
            sb.Append $"Command '{cmd} {args}' failed (status code = {p.ExitCode})" |> ignore
            if errors.Count > 0 then
                sb.Append $":" |> ignore

                for e in errors do
                    sb.Append $"{Environment.NewLine}    {e}" |> ignore
            else
                sb.Append "." |> ignore

            failwith <| sb.ToString()

        output