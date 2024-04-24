open System
open System.Threading
open Infrastructure.Logging

[<EntryPoint>]
let main args =

    let duration =
        match args.Length with
        | 1 ->
            match args.[0] with
            | Infrastructure.DSL.AP.IsFloat seconds -> seconds
            | _ -> (TimeSpan.FromDays 1).TotalSeconds
        | _ -> (TimeSpan.FromDays 1).TotalSeconds


    useConsoleLogger <| KdmidScheduler.Worker.Configuration.AppSettings

    use cts = new CancellationTokenSource(TimeSpan.FromSeconds duration)

    let duration = TimeSpan.FromSeconds duration

    $"The worker will be running for %d{duration.Days}d %02d{duration.Hours}h %02d{duration.Minutes}m %02d{duration.Seconds}s."
    |> Log.info

    cts.Token
    |> KdmidScheduler.Worker.Core.configureWorker
    |> Worker.Core.startWorker
    |> Async.RunSynchronously

    0
