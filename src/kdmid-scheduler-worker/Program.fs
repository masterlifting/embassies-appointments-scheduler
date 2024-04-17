[<EntryPoint>]
let main args =
    args
    |> KdmidScheduler.Worker.Core.configWorker
    |> Worker.Core.startWorker
    |> Async.RunSynchronously

    0
