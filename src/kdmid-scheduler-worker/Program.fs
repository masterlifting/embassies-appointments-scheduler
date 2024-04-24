open System
open System.Threading

[<EntryPoint>]
let main args =

    let seconds =
        match args.Length with
        | 1 ->
            match args.[0] with
            | Infrastructure.DSL.AP.IsFloat value -> value
            | _ -> (TimeSpan.FromDays 1).TotalSeconds
        | _ -> (TimeSpan.FromDays 1).TotalSeconds

    Infrastructure.Logging.useConsoleLogger
    <| KdmidScheduler.Worker.Configuration.AppSettings

    let duration = TimeSpan.FromSeconds seconds
    use cts = new CancellationTokenSource(duration)

    $"The worker will be running for %d{duration.Days}d %02d{duration.Hours}h %02d{duration.Minutes}m %02d{duration.Seconds}s"
    |> Infrastructure.Logging.Log.warning

    cts.Token
    |> KdmidScheduler.Worker.Core.configure
    |> Worker.Core.start
    |> Async.RunSynchronously

    0
