open Infrastructure
open KdmidScheduler.Worker

[<EntryPoint>]
let main _ =
    Logging.useConsoleLogger Configuration.AppSettings

    Core.configure |> Worker.Core.start |> Async.RunSynchronously

    Async.Sleep(System.TimeSpan.FromSeconds 300) |> Async.RunSynchronously

    0
