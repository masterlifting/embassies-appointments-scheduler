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

    useConsoleLogger <| KdmidScheduler.Worker.Configuration.appSettings

    $"The worker will be running for {duration} seconds." |> Log.warning
    use cts = new CancellationTokenSource(TimeSpan.FromSeconds duration)

    cts.Token
    |> KdmidScheduler.Worker.Core.configureWorker
    |> Worker.Core.startWorker
    |> Async.RunSynchronously

    0
